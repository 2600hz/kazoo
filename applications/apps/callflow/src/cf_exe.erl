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
-module(cf_exe).

%% API
-export([start/2]).

-import(cf_call_command, [hangup/1]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets up the call flow executioner for this call, and starts
%% execution of the first node
%% @end
%%--------------------------------------------------------------------
-spec(start/2 :: (Call :: #cf_call{}, Flow :: json_object()) -> ok).
start(Call, Flow) ->
    process_flag(trap_exit, true),
    AmqpQ = init_amqp(Call),
    next(Call#cf_call{cf_pid=self(), amqp_q=AmqpQ}, Flow).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Executes the top most call flow node on a given call,
%% then waits for the modules reply, unexpected death, or timeout.
%% @end
%%--------------------------------------------------------------------
-spec(next/2 :: (Call :: #cf_call{}, Flow :: json_object()) -> ok).
next(Call, Flow) ->
    Module = <<"cf_", (wh_json:get_value(<<"module">>, Flow))/binary>>,
    Data = wh_json:get_value(<<"data">>, Flow),
    try list_to_existing_atom(binary_to_list(Module)) of
        CF_Module ->
            logger:format_log(info, "CF EXECUTIONER (~p): Executing ~p...", [self(), Module]),
            wait(Call, Flow, spawn_link(CF_Module, handle, [Data, Call]))
    catch
        _:_ ->
            logger:format_log(error, "CF EXECUTIONER (~p): Module ~p doesn't exist!", [self(), Module]),
            self() ! {continue, <<"_">>},
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
wait(Call, Flow, Pid) ->
   receive
       {'EXIT', Pid, Reason} when Reason =/= normal ->
           logger:format_log(info, "CF EXECUTIONER (~p): Module died unexpectedly (~p)", [self(), Reason]),
           self() ! {continue, <<"_">>},
           wait(Call, Flow, Pid);
       {continue} ->
           self() ! {continue, <<"_">>},
           wait(Call, Flow, Pid);
       {continue, Key} ->
           logger:format_log(info, "CF EXECUTIONER (~p): Advancing to the next node...", [self()]),
           case wh_json:get_value([<<"children">>, Key], Flow) of
               undefined ->
                   logger:format_log(error, "CF EXECUTIONER (~p): Unexpected end of callflow...", [self()]),
                   hangup(Call);
               ?EMPTY_JSON_OBJECT ->
                   logger:format_log(info, "CF EXECUTIONER (~p): Child node doesn't exist, hanging up...", [self()]),
                   hangup(Call);
               NewFlow ->
                   next(Call, NewFlow)
           end;
       {attempt} when not is_pid(Pid) ->
           self() ! {continue, <<"_">>},
           wait(Call, Flow, Pid);
       {attempt, Key} when not is_pid(Pid) ->
           self() ! {continue, Key},
           wait(Call, Flow, Pid);
       {attempt} ->
           self() ! {attempt, <<"_">>},
           wait(Call, Flow, Pid);
       {attempt, Key} ->
           case wh_json:get_value([<<"children">>, Key], Flow) of
               undefined ->
                   Pid ! {attempt_resp, {error, undefined}},
                   wait(Call, Flow, Pid);
               ?EMPTY_JSON_OBJECT ->
                   Pid ! {attempt_resp, {error, empty}},
                   wait(Call, Flow, Pid);
               NewFlow ->
                   Pid ! {attempt_resp, ok},
                   next(Call, NewFlow)
           end;
       {branch, NewFlow} ->
           next(Call, NewFlow);
       {stop} ->
           logger:format_log(info, "CF EXECUTIONER (~p): Callflow execution has been stopped", [self()]),
           hangup(Call);
       {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
           Msg = mochijson2:decode(Payload),
           is_pid(Pid) andalso Pid ! {amqp_msg, Msg},
           case wh_json:get_value(<<"Event-Name">>, Msg) of
               <<"CHANNEL_HANGUP_COMPLETE">> ->
                   receive
                       {_, #amqp_msg{props = Props, payload = Payload}}=Message when Props#'P_basic'.content_type == <<"application/json">> ->
                           self() ! Message,
                           wait(Call, Flow, Pid);
                       {heartbeat} ->
                           self() ! {heartbeat},
                           wait(Call, Flow, Pid)
                   after
                       1000 ->
                           logger:format_log(info, "CF EXECUTIONER (~p): Channel was hung up", [self()])
                   end;
               _Else ->
                   wait(Call, Flow, Pid)
           end
   end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes a AMQP queue and consumer to recieve call events
%% @end
%%--------------------------------------------------------------------
-spec(init_amqp/1 :: (Call :: #cf_call{}) -> binary()).
init_amqp(#cf_call{call_id=CallId}) ->
    AmqpQ = amqp_util:new_queue(),
    amqp_util:bind_q_to_callevt(AmqpQ, CallId),
    amqp_util:bind_q_to_targeted(AmqpQ),
    amqp_util:basic_consume(AmqpQ),
    AmqpQ.
