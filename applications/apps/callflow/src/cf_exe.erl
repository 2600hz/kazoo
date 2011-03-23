%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow executioner, executes the head node and waits for further
%%% instructions
%%%
%%% @end
%%% Created:       21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 19 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================

-module ( cf_exe ).

%% API
-export ( [start/2] ).

-import ( logger, [format_log/3] ).
-import ( props, [get_value/2, get_value/3] ).

-import(cf_call_command, [hangup/1]).

-include ( "callflow.hrl" ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets up the call flow executioner for this call, and starts 
%% execution of the first node
%% @end
%%--------------------------------------------------------------------
-spec(start/2 :: (Call :: #cf_call{}, Flow :: json_object()) -> ok).
start ( Call, Flow ) ->
    process_flag(trap_exit, true),
    AmqpQ = init_amqp(Call),
    C1 = parse_from(Call),
    C2 = parse_to(C1),
    next ( C2#cf_call{cf_pid=self(), amqp_q=AmqpQ}, Flow ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Executes the top most call flow node on a given call,
%% then waits for the modules reply, unexpected death, or timeout.
%% @end
%%--------------------------------------------------------------------
-spec(next/2 :: (Call :: #cf_call{}, Flow :: json_object()) -> ok).
next ( Call, Flow ) ->
    Module = <<"cf_", (get_value(<<"module">>, Flow))/binary>>,
    Data = get_value(<<"data">>, Flow),   
    try list_to_existing_atom(binary_to_list(Module)) of
        CF_Module ->
            format_log(info, "CF EXECUTIONER (~p): Executing ~p...", [self(), Module]),
            wait(Call, Flow, spawn_link(CF_Module, handle, [Data, Call]))
    catch
        _:_ ->
            format_log(error, "CF EXECUTIONER (~p): Module ~p doesn't exist!", [self(), Module]),
            self() ! { continue, 1 },
            wait(Call, Flow, undefined)
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for the module handling the current call flow node to  reply, 
%% unexpectly die, or timeout and advance the call flow accordingly
%% @end
%%--------------------------------------------------------------------
-spec(wait/3 :: (Call :: #cf_call{}, Flow :: json_object(), Pid :: pid() | undefined) -> ok).
wait ( Call, Flow, Pid ) ->
   receive
       {'EXIT', _Pid, normal} ->
           wait(Call, Flow, Pid);
       {'EXIT', _Pid, Reason} ->
           format_log(info, "CF EXECUTIONER (~p): Module died unexpectedly (~p)", [self(), Reason]),
           self() ! { continue, <<"_">> },
           wait(Call, Flow, Pid);
       { continue } -> 
           self() ! { continue, <<"_">> }, 
           wait(Call, Flow, Pid);
       { continue, Key } ->
           format_log(info, "CF EXECUTIONER (~p): Advancing to the next node...", [self()]),
           {struct, NewFlow} = case get_value(<<"children">>, Flow) of
                                   {struct, []} ->
                                       { struct, [] };
                                   {struct, Children} ->
                                       proplists:get_value(Key, Children);
                                   _ ->
                                       format_log(error, "CF EXECUTIONER (~p): Unexpected end of callflow...", [self()]),
                                       hangup(Call),
                                       exit(invalid_child)
                               end,
           case NewFlow of
               [] ->
                   format_log(info, "CF EXECUTIONER (~p): Child node doesn't exist, hanging up...", [self()]),
                   hangup(Call),
                   exit(missing_child);
               _  -> 
                   next (Call, NewFlow)
           end;
       { stop } ->
           format_log(info, "CF EXECUTIONER (~p): Callflow execution has been stopped", [self()]),
           hangup(Call),
           exit(normal);
       { heartbeat } ->
           wait( Call, Flow, Pid );
       {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
           Msg = mochijson2:decode(binary_to_list(Payload)),
           if 
               is_pid(Pid) -> Pid ! {amqp_msg, Msg};
               true -> ok
           end,           
           case whapps_json:get_value(<<"Event-Name">>, Msg) of
               <<"CHANNEL_HANGUP_COMPLETE">> ->
                   receive
                       {_, #amqp_msg{props = Props, payload = Payload}}=Message when Props#'P_basic'.content_type == <<"application/json">> ->
                           self() ! Message, 
                           wait( Call, Flow, Pid );                          
                       { heartbeat } ->
                           self() ! { heartbeat },
                           wait( Call, Flow, Pid )
                   after
                       1000 ->
                           format_log(info, "CF EXECUTIONER (~p): Channel was hung up", [self()]),                          
                           exit(normal)
                   end;
               _Else ->
                   wait( Call, Flow, Pid )
           end
   after
       120000 -> 
           format_log(info, "CF EXECUTIONER (~p): Callflow timeout!", [self()]),
           hangup(Call),
           exit(timeout)
   end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes a AMQP queue and consumer to recieve call events
%% @end
%%--------------------------------------------------------------------
-spec(init_amqp/1 :: (Call :: #cf_call{}) -> binary()).                          
init_amqp(#cf_call{amqp_h=AHost, call_id=CallId}) ->
    AmqpQ = amqp_util_old:new_queue(AHost),
    amqp_util_old:bind_q_to_callevt(AHost, AmqpQ, CallId),
    amqp_util_old:bind_q_to_targeted(AHost, AmqpQ),
    amqp_util_old:basic_consume(AHost, AmqpQ),
    AmqpQ.    

parse_from(#cf_call{route_request=RR}=Call) ->
    case binary:split(get_value(<<"From">>, RR), <<"@">>) of
        [Number|_] ->
            Call#cf_call{from_number = Number, from_domain = <<>>};
        _ ->
            Call
    end.    

parse_to(#cf_call{route_request=RR}=Call) ->
    case binary:split(get_value(<<"To">>, RR), <<"@">>) of
        [Number|_] ->
            Call#cf_call{to_number = Number, to_domain = <<>>};
        _ ->
            Call
    end.    

%%%============================================================================
%%%== END =====
%%%============================================================================
