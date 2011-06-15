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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% initialize the new call flow execution process
%% @end
%%-----------------------------------------------------------------------------
-spec(init/3 :: (Parent :: pid(), Call :: #cf_call{}, Flow :: json_object()) -> no_return()).
init(Parent, Call, Flow) ->
    process_flag(trap_exit, true),
    call_info(Call),
    AmqpQ = init_amqp(Call),
    cache_account(Call),
    proc_lib:init_ack(Parent, {ok, self()}),
    next(Call#cf_call{cf_pid=self(), amqp_q=AmqpQ}, Flow).

%%--------------------------------------------------------------------
%% @private
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
            ?LOG("moving to action ~s", [Module]),
            wait(Call, Flow, spawn_link(CF_Module, handle, [Data, Call]))
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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% load the callid into to the process dictionary then log information about
%% the call that is about to be processed
%% @end
%%-----------------------------------------------------------------------------
-spec(call_info/1 :: (Call :: #cf_call{}) -> no_return()).
call_info(#cf_call{flow_id=FlowId, call_id=CallId, cid_name=CIDName, cid_number=CIDNumber
               ,request=Request, from=From, to=To, inception=Inception, authorizing_id=AuthorizingId }) ->
    put(callid, CallId),
    ?LOG_START("executing callflow ~s", [FlowId]),
    ?LOG("request ~s", [Request]),
    ?LOG("to ~s", [To]),
    ?LOG("from ~s", [From]),
    ?LOG("CID ~s ~s", [CIDNumber, CIDName]),
    ?LOG("inception ~s", [Inception]),
    ?LOG("authorizing id ~s", [AuthorizingId]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the account into the cache for use by the callflow modules
%% (primarily for default values)
%% @end
%%--------------------------------------------------------------------
-spec(cache_account/1 :: (Call :: #cf_call{}) -> no_return()).
cache_account(#cf_call{account_id=AccountId, account_db=Db}) ->
    spawn(fun() ->
                  case wh_cache:fetch({account, AccountId}) =/= {error, not_found}
                      orelse couch_mgr:get_results(Db, <<"account/listing_by_id">>, [{<<"include_docs">>, true}]) of
                      {ok, [JObj]} -> wh_cache:store({account, AccountId}, wh_json:get_value(<<"doc">>, JObj));
                      true -> ok;
                      {error, _} -> ok
                  end
          end).
