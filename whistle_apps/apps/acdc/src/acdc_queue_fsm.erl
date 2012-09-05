%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Controls how a queue process progresses a member_call
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

%% Event injectors
-export([member_call/3
         ,member_connect_resp/2
         ,member_accepted/2
         ,member_connect_retry/2
         ,call_event/4
        ]).

%% State handlers
-export([sync/2
         ,ready/2
         ,connect_req/2
         ,connecting/2
        ]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("acdc.hrl").

%% How long should we wait for a response to our member_connect_req
-define(COLLECT_RESP_TIMEOUT, 3000).
-define(COLLECT_RESP_MESSAGE, collect_timer_expired).

-define(SYNC_TIMEOUT, 3000).
-define(SYNC_MESSAGE, sync_timer_expired).

-record(state, {
          queue_proc :: pid()
         ,connect_resps = [] :: wh_json:json_objects()
         ,collect_ref :: reference()
         ,acct_id :: ne_binary()
         ,acct_db :: ne_binary()
         ,queue_id :: ne_binary()

         ,timer_ref :: reference() % for tracking timers

         ,member_call :: whapps_call:call()

         %% Config options
         ,name :: ne_binary()
         ,connection_timeout = 0 :: integer() % how long can a caller wait in the queue
         ,agent_ring_timeout = 5 :: pos_integer() % how long to ring an agent before giving up
         ,max_queue_size = 0 :: integer() % restrict the number of the queued callers
         ,ring_simultaneously = 1 :: integer() % how many agents to try ringing at a time (first one wins)
         ,enter_when_empty = true :: boolean() % if a queue is agent-less, can the caller enter?
         ,agent_wrapup_time = 0 :: integer() % forced wrapup time for an agent after a call
         ,moh :: ne_binary() % media to play to customer while on hold
         ,announce :: ne_binary() % media to play to customer when about to be connected to agent

         ,caller_exit_key :: ne_binary() % DTMF a caller can press to leave the queue
         ,record_caller = false :: boolean() % record the caller
         ,cdr_url :: ne_binary() % optional URL to request for extra CDR data

         ,strategy = 'rr' :: queue_strategy() % round-robin | most-idle
         ,strategy_state :: queue_strategy_state() % based on the strategy

         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (pid(), wh_json:json_object()) -> startlink_ret().
start_link(QueuePid, QueueJObj) ->
    gen_fsm:start_link(?MODULE, [QueuePid, QueueJObj], []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_call/3 :: (pid(), wh_json:json_object(), #'basic.deliver'{}) -> 'ok'.
member_call(FSM, CallJObj, Delivery) ->
    gen_fsm:send_event(FSM, {member_call, CallJObj, Delivery}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_connect_resp/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_resp(FSM, Resp) ->
    gen_fsm:send_event(FSM, {agent_resp, Resp}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_accepted/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_accepted(FSM, AcceptJObj) ->
    gen_fsm:send_event(FSM, {accepted, AcceptJObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec member_connect_retry/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_retry(FSM, RetryJObj) ->
    gen_fsm:send_event(FSM, {retry, RetryJObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue is processing a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for hangup events).
%% @end
%%--------------------------------------------------------------------
-spec call_event/4 :: (pid(), ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
call_event(FSM, <<"call_event">>, <<"CHANNEL_DESTROY">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {member_hungup, EvtJObj});
call_event(_, _, _, _) -> ok.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([QueuePid, QueueJObj]) ->
    QueueId = wh_json:get_value(<<"_id">>, QueueJObj),
    AcctId = wh_json:get_value(<<"pvt_account_id">>, QueueJObj),
    AcctDb = wh_json:get_value(<<"pvt_account_db">>, QueueJObj),

    put(callid, <<"fsm_", QueueId/binary>>),

    gen_fsm:send_event(self(), {strategy_sync}),

    {ok, sync, #state{queue_proc=QueuePid
                      ,acct_id=AcctId
                      ,acct_db=AcctDb
                      ,queue_id=QueueId

                      ,name = wh_json:get_value(<<"name">>, QueueJObj)
                      ,connection_timeout = wh_json:get_integer_value(<<"connection_timeout">>, QueueJObj)
                      ,agent_ring_timeout = wh_json:get_integer_value(<<"agent_ring_timeout">>, QueueJObj)
                      ,max_queue_size = wh_json:get_integer_value(<<"max_queue_size">>, QueueJObj)
                      ,ring_simultaneously = wh_json:get_value(<<"ring_simultaneously">>, QueueJObj)
                      ,enter_when_empty = wh_json:is_true(<<"enter_when_empty">>, QueueJObj, true)
                      ,agent_wrapup_time = wh_json:get_integer_value(<<"agent_wrapup_time">>, QueueJObj)
                      ,moh = wh_json:get_value(<<"moh">>, QueueJObj)
                      ,announce = wh_json:get_value(<<"announce">>, QueueJObj)
                      ,caller_exit_key = wh_json:get_value(<<"caller_exit_key">>, QueueJObj, <<"#">>)
                      ,record_caller = wh_json:is_true(<<"record_caller">>, QueueJObj, false)
                      ,cdr_url = wh_json:get_value(<<"cdr_url">>, QueueJObj)

                      ,strategy = get_strategy(wh_json:get_value(<<"strategy">>, QueueJObj))
                      ,strategy_state = undefined

                      ,member_call = undefined
                     }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
sync({strategy_sync}, #state{strategy=Type
                             ,queue_proc=Srv
                            }=State) ->
    lager:debug("sync strategy ~s", [Type]),
    %% send sync_req
    case maybe_send_sync_req(Type, Srv) of
        {true, SyncRef} ->
            {next_state, sync, State#state{timer_ref=SyncRef}}; % we're waiting for a sync_resp
        false ->
            acdc_queue:accept_member_calls(Srv),
            {next_state, ready, State} % no sync needed, let's go!
    end;
sync({member_call, CallJObj, Delivery}, #state{queue_proc=Srv}=State) ->
    lager:debug("member_call recv, still syncing, cancel for redelivery"),
    acdc_queue:cancel_member_call(Srv, CallJObj, Delivery),
    {next_state, sync, State};

sync({timeout, SyncRef, ?SYNC_MESSAGE}, #state{timer_ref=SyncRef
                                               ,strategy=Strategy
                                               ,acct_db=AcctDb
                                               ,queue_id=QueueId
                                              }=State) ->
    lager:debug("sync timeout, creating strategy state"),
    {next_state, ready, State#state{timer_ref=undefined
                                    ,strategy_state=create_strategy_state(Strategy, AcctDb, QueueId)
                                   }};

sync(_E, State) ->
    lager:debug("recv unhandled event: ~p", [_E]),
    {next_state, sync, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
ready({member_call, CallJObj, Delivery}, #state{queue_proc=Srv}=State) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, CallJObj)),
    lager:debug("member call received: ~s", [whapps_call:call_id(Call)]),
    acdc_queue:member_connect_req(Srv, CallJObj, Delivery),
    {next_state, connect_req, State#state{collect_ref=start_collect_timer()
                                          ,member_call=Call
                                         }};
ready({agent_resp, _Resp}, State) ->
    lager:debug("someone jumped the gun, or was slow on the draw: ~p", [_Resp]),
    {next_state, ready, State};
ready({accepted, _AcceptJObj}, State) ->
    lager:debug("weird to receive an acceptance: ~p", [_AcceptJObj]),
    {next_state, ready, State};
ready({retry, _RetryJObj}, State) ->
    lager:debug("weird to receive a retry when we're just hanging here: ~p", [_RetryJObj]),
    {next_state, ready, State};
ready({member_hungup, _CallEvt}, State) ->
    lager:debug("member hungup from previous call, failed to unbind"),
    {next_state, ready, State};
ready(_Event, State) ->
    lager:debug("unhandled event: ~p", [_Event]),
    {next_state, ready, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
connect_req({member_call, CallJObj, Delivery}, #state{queue_proc=Srv}=State) ->
    lager:debug("recv a member_call while processing a different member: ~p", [CallJObj]),
    acdc_queue:cancel_member_call(Srv, CallJObj, Delivery),
    {next_state, connect_req, State};
connect_req({agent_resp, Resp}, #state{connect_resps=CRs}=State) ->
    lager:debug("recv another resp: ~p", [Resp]),
    {next_state, connect_req, State#state{connect_resps=[Resp | CRs]}};
connect_req({timeout, Ref, ?COLLECT_RESP_MESSAGE}, #state{collect_ref=Ref
                                                          ,connect_resps=[]
                                                          ,queue_proc=Srv
                                                         }=State) ->
    lager:debug("done waiting for agents to respond, no one responded"),
    acdc_queue:cancel_member_call(Srv),
    {next_state, ready, State#state{connect_resps=[]
                                    ,collect_ref=undefined
                                    ,member_call=undefined
                                   }};
connect_req({timeout, Ref, ?COLLECT_RESP_MESSAGE}, #state{collect_ref=Ref
                                                          ,connect_resps=CRs
                                                          ,queue_proc=Srv
                                                          ,strategy=Strategy
                                                          ,strategy_state=StrategyState
                                                         }=State) ->
    lager:debug("done waiting for agents to respond, picking a winner"),
    case pick_winner(CRs, Strategy, StrategyState) of
        {Winner, Monitors, Rest, StrategyState1} ->
            acdc_queue:member_connect_win(Srv, Winner),
            _ = [acdc_queue:member_connect_monitor(Srv, M) || M <- Monitors],

            lager:debug("sending win to ~p", [Winner]),
            {next_state, connecting, State#state{connect_resps=Rest
                                                 ,collect_ref=undefined
                                                 ,strategy_state=StrategyState1
                                                }};
        undefined ->
            lager:debug("no more responses to choose from"),
            acdc_queue:cancel_member_call(Srv),
            {next_state, ready, State#state{collect_ref=undefined
                                            ,member_call=undefined
                                           }}
    end;
connect_req({accepted, _AcceptJObj}, State) ->
    lager:debug("recv accept response before win sent (are you magical): ~p", [_AcceptJObj]),
    {next_state, connect_req, State};
connect_req({retry, _RetryJObj}, State) ->
    lager:debug("recv retry response before win sent: ~p", [_RetryJObj]),
    {next_state, connect_req, State};
connect_req({member_hungup, JObj}, #state{queue_proc=Srv}=State) ->
    lager:debug("member hungup before we could assign an agent"),
    acdc_queue:finish_member_call(Srv, JObj),
    {next_state, ready, State#state{connect_resps=[]
                                    ,collect_ref=undefined
                                    ,member_call=undefined
                                   }};
connect_req(_Event, State) ->
    lager:debug("unhandled event: ~p", [_Event]),
    {next_state, connect_req, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
connecting({member_call, CallJObj, Delivery}, #state{queue_proc=Srv}=State) ->
    lager:debug("recv a member_call while connecting: ~p", [CallJObj]),
    acdc_queue:cancel_member_call(Srv, CallJObj, Delivery),
    {next_state, connecting, State};
connecting({agent_resp, _Resp}, State) ->
    lager:debug("agent resp must have just missed cutoff: ~p", [_Resp]),
    {next_state, connecting, State};
connecting({accepted, AcceptJObj}, #state{queue_proc=Srv}=State) ->
    lager:debug("recv acceptance from agent: ~p", [AcceptJObj]),
    acdc_queue:finish_member_call(Srv, AcceptJObj),
    {next_state, ready, State#state{member_call=undefined}};
connecting({retry, RetryJObj}, #state{queue_proc=Srv, connect_resps=[]}=State) ->
    lager:debug("recv retry from agent: ~p", [RetryJObj]),
    acdc_queue:cancel_member_call(Srv, RetryJObj),
    {next_state, ready, State#state{member_call=undefined}};
connecting({retry, RetryJObj}, State) ->
    lager:debug("recv retry from agent: ~p", [RetryJObj]),
    lager:debug("but wait, we have others who wanted to try"),
    gen_fsm:send_event(self(), {timeout, undefined, ?COLLECT_RESP_MESSAGE}),
    {next_state, connect_req, State};

connecting({member_hungup, CallEvt}, #state{queue_proc=Srv}=State) ->
    lager:debug("caller hungup while we waited for the agent to connect"),
    acdc_queue:cancel_member_call(Srv, CallEvt),
    {next_state, ready, State#state{member_call=undefined}};

connecting(_Event, State) ->
    lager:debug("unhandled event: ~p", [_Event]),
    {next_state, connecting, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, _Event]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    lager:debug("unhandled sync event in ~s: ~p", [StateName, _Event]),
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled message in state ~s: ~p", [StateName, _Info]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    lager:debug("acdc queue fsm terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_collect_timer() ->
    gen_fsm:start_timer(?COLLECT_RESP_TIMEOUT, ?COLLECT_RESP_MESSAGE).

%% Really sophisticated selection algorithm
-spec pick_winner/3 :: (wh_json:json_objects(), queue_strategy(), queue_strategy_state()) ->
                               'undefined' |
                               {wh_json:json_object()
                                ,wh_json:json_objects()
                                ,wh_json:json_objects()
                               }.
pick_winner([], _, _) -> undefined;
pick_winner(CRs, 'rr', AgentQ) ->
    {{value, AgentId}, AgentQ1} = queue:out(AgentQ),

    case split_agents(AgentId, CRs) of
        {[Resp|SameAgents], OtherAgents} ->
            {Resp, SameAgents, OtherAgents, queue:in(AgentId, AgentQ1)};
        {[], Others} ->
            case pick_winner(Others, 'rr', AgentQ1) of
                {Resp, Same, Other, AgentQ2} ->
                    {Resp, Same, Other, queue:ing(AgentId, AgentQ2)};
                undefined -> undefined
            end
    end;
pick_winner(CRs, 'mi', _) ->
    [MostIdle | Rest] = lists:usort(fun sort_agent/2, CRs),
    AgentId = wh_json:get_value(<<"Agent-ID">>, MostIdle),
    {Same, Other} = split_agents(AgentId, Rest),
    {MostIdle, Same, Other, undefined}.

%% If A's idle time is greater, it should come before B
-spec sort_agent/2 :: (wh_json:json_object(), wh_json:json_object()) -> boolean().
sort_agent(A, B) ->
    wh_json:get_integer_value(<<"Idle-Time">>, A, 0) >
        wh_json:get_integer_value(<<"Idle-Time">>, B, 0).

-spec split_agents/2 :: (ne_binary(), wh_json:json_objects()) ->
                                {wh_json:json_objects(), wh_json:json_objects()}.
split_agents(AgentId, Rest) ->
    lists:partition(fun(R) ->
                            AgentId =:= wh_json:get_value(<<"Agent-ID">>, R)
                    end, Rest).

-spec maybe_send_sync_req/2 :: (queue_strategy(), pid()) -> 'false' | {'true', reference()}.
maybe_send_sync_req(mi, _) -> false;
maybe_send_sync_req(rr, Srv) ->
    acdc_queue:send_sync_req(Srv, rr),
    {true, gen_fsm:start_timer(?SYNC_TIMEOUT, ?SYNC_MESSAGE)}.

-spec get_strategy/1 :: (api_binary()) -> queue_strategy().
get_strategy(<<"round_robin">>) -> 'rr';
get_strategy(<<"most_idle">>) -> 'mi';
get_strategy(_) -> 'rr'.

create_strategy_state('rr', AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {ok, []} -> lager:debug("no agents around"), queue:new();
        {ok, JObjs} -> queue:from_list([wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs]);
        {error, _E} -> lager:debug("error: ~p", [_E]), queue:new()
    end.
