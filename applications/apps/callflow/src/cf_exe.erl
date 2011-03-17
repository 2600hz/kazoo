%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow executioner, executes the head node and waits for further
%%% instructions
%%%
%%% @end
%%% Created:       21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 23 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================

-module ( cf_exe ).

%% API
-export ( [start/2] ).

-import ( logger, [format_log/3] ).
-import ( props, [get_value/2, get_value/3] ).

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
    NewCall = Call#cf_call{cf_pid=self()},
    init_amqp(NewCall),
    next ( NewCall, Flow )
.

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
            format_log(info, "CF EXECUTIONER (~p): Executing ~p...~n", [self(), Module]),
            wait(Call, Flow, spawn_link(CF_Module, handle, [Data, Call]))
    catch
        _:_ ->
            format_log(error, "CF EXECUTIONER (~p): Module ~p doesn't exist!~n", [self(), Module]),
            self() ! { continue, 1 },
            wait(Call, Flow, undefined)
    end
.    

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
           format_log(info, "CF EXECUTIONER (~p): Module died unexpectedly (~p)~n", [self(), Reason]),
           self() ! { continue, <<"_">> },
           wait(Call, Flow, Pid);
       { continue } -> 
           self() ! { continue, <<"_">> }, 
           wait(Call, Flow, Pid);
       { continue, Key } ->
           format_log(info, "CF EXECUTIONER (~p): Advancing to the next node...~n", [self()]),
           {struct, NewFlow} = case get_value(<<"children">>, Flow) of
                                   {struct, []} ->
                                       { struct, [] };
                                   {struct, Children} ->
                                       proplists:get_value(Key, Children);
                                   _ ->
                                       format_log(error, "CF EXECUTIONER (~p): Unexpected end of callflow...~n", [self()]),
                                       hangup(Call),
                                       exit("Bad things happened...")
                               end,
           case NewFlow of
               [] ->
                   start (Call, [{<<"module">>, <<"dialplan">>}, {<<"data">>, {struct, [{<<"action">>, <<"hangup">>}, {<<"data">>, {struct, []}}]}}]),
                   format_log(info, "CF EXECUTIONER (~p): Child node doesn't exist, hanging up...~n", [self()]);
               _  -> 
                   next (Call, NewFlow)
           end;
       { stop } ->
           format_log(info, "CF EXECUTIONER (~p): Callflow execution has been stopped~n", [self()]),
           hangup(Call),
           exit("End of execution");
       { heartbeat } ->
           wait( Call, Flow, Pid );
       {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
           Msg = mochijson2:decode(binary_to_list(Payload)),
           if 
               is_pid(Pid) -> Pid ! {call_event, Msg};
               true -> ok
           end,
           wait( Call, Flow, Pid )
   after
       120000 -> 
           format_log(info, "CF EXECUTIONER (~p): Callflow timeout!~n", [self()]),
           hangup(Call),
           exit("No call events in 2 mintues")
   end
.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes a AMQP queue and consumer to recieve call events
%% @end
%%--------------------------------------------------------------------
-spec(init_amqp/1 :: (Call :: #cf_call{}) -> no_return()).                          
init_amqp(#cf_call{amqp_h=AHost, call_id=CallId}) ->
    AmqpQ = amqp_util:new_queue(AHost),
    amqp_util:bind_q_to_callevt(AHost, AmqpQ, CallId),
    amqp_util:basic_consume(AHost, AmqpQ).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to hangup the channel 
%% @end
%%--------------------------------------------------------------------
-spec(hangup/1 :: (Call :: #cf_call{}) -> no_return()).
hangup(#cf_call{amqp_h=AHost, call_id=CallId, ctrl_q=CtrlQ}) ->
    Command = [
                {<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(CallId, <<"call">>, <<"command">>, <<"cf_exe">>, <<"1.0">>)
              ],    
    {ok, Json} = whistle_api:hangup_req(Command),
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).

%%%============================================================================
%%%== END =====
%%%============================================================================
