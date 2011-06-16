%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
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
-export([start_link/2]).
-export([init/3]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets up the call flow executioner for this call, and starts
%% execution of the first node
%% @end
%%--------------------------------------------------------------------
-spec(start_link/2 :: (Call :: #cf_call{}, Flow :: json_object()) -> tuple(ok, pid())).
start_link(Call, Flow) ->
    proc_lib:start_link(?MODULE, init, [self(), Call, Flow]).

-spec(init/3 :: (Parent :: pid(), Call :: #cf_call{}, Flow :: json_object()) -> no_return()).
init(Parent, #cf_call{call_id=CallId}=Call, Flow) ->
    put(callid, CallId),
    ?LOG("executing callflow ~s", [Call#cf_call.flow_id]),
    process_flag(trap_exit, true),
    AmqpQ = init_amqp(Call),
    cache_account(Call),
    proc_lib:init_ack(Parent, {ok, self()}),
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
    try
        CF_Module = whistle_util:to_atom(Module, true),
        Pid = spawn_link(CF_Module, handle, [Data, Call]),
        ?LOG("moving to action ~s", [Module]),
        wait(Call, Flow, Pid)
    catch
        _:_ ->
            ?LOG("unknown action ~s, skipping", [Module]),
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
           ?LOG("action ~w died unexpectedly", [Pid]),
           self() ! {continue, <<"_">>},
           wait(Call, Flow, Pid);
       {continue} ->
           self() ! {continue, <<"_">>},
           wait(Call, Flow, Pid);
       {continue, Key} ->
           case wh_json:get_value([<<"children">>, Key], Flow) of
               undefined ->
                   ?LOG_END("unexpected end of callflow"),
                   cf_call_command:hangup(Call);
               ?EMPTY_JSON_OBJECT ->
                   ?LOG_END("child node doesn't exist"),
                   cf_call_command:hangup(Call);
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
           ?LOG_END("execution has been stopped");
       {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
           Msg = mochijson2:decode(Payload),
           is_pid(Pid) andalso Pid ! {amqp_msg, Msg},
           wait(Call, Flow, Pid);
       _Msg ->
           wait(Call, Flow, Pid)
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the account into the cache for use by the callflow modules
%% (primarily for default values)
%% @end
%%--------------------------------------------------------------------
-spec(cache_account/1 :: (Call :: #cf_call{}) -> no_return()).
cache_account(#cf_call{account_db=Db}) ->
    spawn(fun() ->
                  case wh_cache:fetch({account, Db}) =/= {error, not_found}
                      orelse couch_mgr:get_results(Db, <<"account/listing_by_id">>, [{<<"include_docs">>, true}]) of
                      {ok, [JObj]} -> wh_cache:store({account, Db}, wh_json:get_value(<<"doc">>, JObj));
                      true -> ok;
                      {error, _} -> ok
                  end
          end).
