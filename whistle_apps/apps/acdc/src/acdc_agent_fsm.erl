%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Tracks the agent's state, responds to messages from the corresponding
%%% acdc_agent gen_listener process.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2, start_link/3, start_link/4
         ,call_event/4
         ,member_connect_req/2
         ,member_connect_win/2
         ,originate_ready/2
         ,originate_failed/2
         ,sync_req/2, sync_resp/2
         ,pause/2
         ,resume/1
         ,refresh/2
         ,current_call/1
         ,status/1
        ]).

-export([wait_for_listener/3]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

%% Agent states
-export([wait/2
         ,sync/2
         ,ready/2
         ,ringing/2
         ,answered/2
         ,wrapup/2
         ,paused/2

         ,wait/3
         ,sync/3
         ,ready/3
         ,ringing/3
         ,answered/3
         ,wrapup/3
         ,paused/3
        ]).

-include("acdc.hrl").

%% When an agent starts up, how long do we wait for other agents to respond with their status?
-define(SYNC_RESPONSE_TIMEOUT, 5000).
-define(SYNC_RESPONSE_MESSAGE, sync_response_timeout).

%% We weren't able to join our brethern, how long to wait to check again
-define(RESYNC_RESPONSE_TIMEOUT, 15000).
-define(RESYNC_RESPONSE_MESSAGE, resync_response_timeout).

-define(PAUSE_MESSAGE, pause_expired).

-define(CALL_STATUS_TIMEOUT, 10000).
-define(CALL_STATUS_MESSAGE, call_status_timeout).

-record(state, {
          acct_id :: ne_binary()
         ,acct_db :: ne_binary()
         ,agent_id :: ne_binary()
         ,agent_proc :: pid()
         ,agent_proc_id :: ne_binary()
         ,wrapup_timeout = 0 :: integer() % optionally set on win
         ,wrapup_ref :: reference()
         ,sync_ref :: reference()
         ,call_status_ref :: reference()
         ,call_status_failures = 0 :: integer()
         ,member_call :: whapps_call:call()
         ,member_call_id :: ne_binary()
         ,member_call_queue_id :: ne_binary()
         ,member_call_start :: wh_now()
         ,caller_exit_key = <<"#">> :: ne_binary()
         ,agent_call_id :: ne_binary()
         ,next_status :: ne_binary()
         ,fsm_call_id :: ne_binary() % used when no call-ids are available
         ,endpoints = [] :: wh_json:objects()
         }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%   When a queue receives a call and needs an agent, it will send a 
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%--------------------------------------------------------------------
-spec member_connect_req/2 :: (pid(), wh_json:object()) -> 'ok'.
member_connect_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {member_connect_req, JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue receives a call and needs an agent, it will send a 
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%--------------------------------------------------------------------
-spec member_connect_win/2 :: (pid(), wh_json:object()) -> 'ok'.
member_connect_win(FSM, JObj) ->
    gen_fsm:send_event(FSM, {member_connect_win, JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When an agent is involved in a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for bridge and hangup events).
%% @end
%%--------------------------------------------------------------------
-spec call_event/4 :: (pid(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
call_event(FSM, <<"call_event">>, <<"CHANNEL_BRIDGE">>, JObj) ->
    gen_fsm:send_event(FSM, {channel_bridged, callid(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, JObj) ->
    gen_fsm:send_event(FSM, {channel_unbridged, callid(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_DESTROY">>, JObj) ->
    gen_fsm:send_event(FSM, {channel_hungup, callid(JObj)});
call_event(FSM, <<"call_event">>, <<"LEG_CREATED">>, JObj) ->
    gen_fsm:send_event(FSM, {leg_created, callid(JObj)});
call_event(FSM, <<"call_event">>, <<"LEG_DESTROYED">>, JObj) ->
    gen_fsm:send_event(FSM, {leg_destroyed, callid(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_ANSWER">>, JObj) ->
    gen_fsm:send_event(FSM, {channel_answered, callid(JObj)});
call_event(FSM, <<"call_event">>, <<"DTMF">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {dtmf_pressed, wh_json:get_value(<<"DTMF-Digit">>, EvtJObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, JObj) ->
    maybe_send_execute_complete(FSM, wh_json:get_value(<<"Application-Name">>, JObj), JObj);

call_event(FSM, <<"call_event">>, <<"call_status_resp">>, JObj) ->
    gen_fsm:send_event(FSM, {call_status, JObj});

call_event(FSM, <<"error">>, <<"dialplan">>, JObj) ->
    _ = wh_util:put_callid(JObj),
    lager:debug("error event: ~s", [wh_json:get_value(<<"Error-Message">>, JObj)]),

    Req = wh_json:get_value(<<"Request">>, JObj),

    gen_fsm:send_event(FSM, {dialplan_error, wh_json:get_value(<<"Application-Name">>, Req)});
call_event(_, _C, _E, _) ->
    ok.
    %% lager:debug("unhandled call event: ~s: ~s", [_C, _E]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_execute_complete/3 :: (pid(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_send_execute_complete(FSM, <<"bridge">>, JObj) ->
    gen_fsm:send_event(FSM, {channel_unbridged, callid(JObj)});
maybe_send_execute_complete(FSM, <<"call_pickup">>, JObj) ->
    gen_fsm:send_event(FSM, {channel_bridged, callid(JObj)});
maybe_send_execute_complete(_, _, _) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
originate_ready(FSM, JObj) ->
    gen_fsm:send_event(FSM, {originate_ready, JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
originate_failed(FSM, JObj) ->
    gen_fsm:send_event(FSM, {originate_failed, JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
sync_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {sync_req, JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
sync_resp(FSM, JObj) ->
    gen_fsm:send_event(FSM, {sync_resp, JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
pause(FSM, Timeout) ->
    gen_fsm:send_event(FSM, {pause, Timeout}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
resume(FSM) ->
    gen_fsm:send_event(FSM, {resume}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
refresh(FSM, AgentJObj) ->
    gen_fsm:send_all_state_event(FSM, {refresh, AgentJObj}).

-spec current_call/1 :: (pid()) -> 'undefined' | wh_json:object().
current_call(FSM) ->
    gen_fsm:sync_send_event(FSM, current_call).

status(FSM) ->
    gen_fsm:sync_send_event(FSM, status).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (pid(), wh_json:object()) -> startlink_ret().
-spec start_link/3 :: (pid(), whapps_call:call(), ne_binary()) -> startlink_ret().
-spec start_link/4 :: (ne_binary(), ne_binary(), pid(), wh_proplist()) -> startlink_ret().

start_link(Supervisor, AgentJObj) when is_pid(Supervisor) ->
    start_link(wh_json:get_value(<<"pvt_account_id">>, AgentJObj)
               ,wh_json:get_value(<<"_id">>, AgentJObj)
               ,Supervisor
               ,[]
              ).
start_link(Supervisor, ThiefCall, _QueueId) ->
    start_link(whapps_call:account_id(ThiefCall)
               ,whapps_call:owner_id(ThiefCall)
               ,Supervisor
               ,[]
              ).
start_link(AcctId, AgentId, Supervisor, Props) ->
    gen_fsm:start_link(?MODULE, [AcctId, AgentId, Supervisor, Props], []).

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
init([AcctId, AgentId, Supervisor, Props]) ->
    FSMCallId = <<"fsm_", AcctId/binary, "_", AgentId/binary>>,
    put(callid, FSMCallId),
    lager:debug("started acdc agent fsm"),

    acdc_stats:agent_active(AcctId, AgentId),

    Self = self(),
    _P = spawn(?MODULE, wait_for_listener, [Supervisor, Self, Props]),
    lager:debug("waiting for listener in ~p", [_P]),

    {ok, wait, #state{acct_id=AcctId
                      ,acct_db=wh_util:format_account_id(AcctId, encoded)
                      ,agent_id=AgentId
                      ,fsm_call_id=FSMCallId
                     }}.


wait_for_listener(Supervisor, FSM, Props) ->
    case acdc_agent_sup:agent(Supervisor) of
        undefined ->
            lager:debug("listener not ready yet, waiting"),
            timer:sleep(100),
            wait_for_listener(Supervisor, FSM, Props);
        P when is_pid(P) ->
            lager:debug("listener retrieved: ~p", [P]),

            {NextState, SyncRef} = case props:get_value(skip_sync, Props) of
                                       true -> {ready, undefined};
                                       _ ->
                                           gen_fsm:send_event(FSM, send_sync_event),
                                           gen_fsm:send_all_state_event(FSM, load_endpoints),
                                           {sync, start_sync_timer(FSM)}
                                   end,

            gen_fsm:send_event(FSM, {listener, P, NextState, SyncRef})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
wait({listener, AgentProc, NextState, SyncRef}, State) ->
    lager:debug("setting agent proc to ~p", [AgentProc]),
    acdc_agent:fsm_started(AgentProc, self()),
    {next_state, NextState, State#state{
                              agent_proc=AgentProc
                              ,sync_ref=SyncRef
                              ,agent_proc_id=acdc_util:proc_id(AgentProc)
                             }};
wait(send_sync_event, State) ->
    gen_fsm:send_event(self(), send_sync_event),
    {next_state, wait, State};
wait(_Msg, State) ->
    lager:debug("unhandled msg in wait: ~p", [_Msg]),
    {next_state, wait, State}.

wait(status, _, State) ->
    {reply, undefined, wait, State};
wait(current_call, _, State) ->
    {reply, undefined, wait, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
sync({timeout, Ref, ?SYNC_RESPONSE_MESSAGE}, #state{sync_ref=Ref
                                                    ,acct_id=AcctId
                                                    ,agent_id=AgentId
                                                   }=State) when is_reference(Ref) ->
    lager:debug("done waiting for sync responses"),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, State#state{sync_ref=Ref}};
sync({timeout, Ref, ?RESYNC_RESPONSE_MESSAGE}, #state{sync_ref=Ref}=State) when is_reference(Ref) ->
    lager:debug("resync timer expired, lets check with the others again"),
    SyncRef = start_sync_timer(),
    gen_fsm:send_event(self(), send_sync_event),
    {next_state, sync, State#state{sync_ref=SyncRef}};

sync(send_sync_event, #state{agent_proc=Srv, agent_proc_id=_AProcId}=State) ->
    lager:debug("sending sync_req event to other agent processes: ~s", [_AProcId]),
    acdc_agent:send_sync_req(Srv),
    {next_state, sync, State};

sync({sync_req, JObj}, #state{agent_proc=Srv
                              ,agent_proc_id=AProcId
                             }=State) ->
    case wh_json:get_value(<<"Process-ID">>, JObj) of
        AProcId ->
            lager:debug("recv sync req from ourself"),
            {next_state, sync, State};
        _OtherProcId ->
            lager:debug("recv sync_req from ~s (we are ~s)", [_OtherProcId, AProcId]),
            acdc_agent:send_sync_resp(Srv, sync, JObj),
            {next_state, sync, State}
    end;

sync({sync_resp, JObj}, #state{sync_ref=Ref
                               ,acct_id=AcctId
                               ,agent_id=AgentId
                              }=State) ->
    case catch wh_util:to_atom(wh_json:get_value(<<"Status">>, JObj)) of
        sync ->
            lager:debug("other agent is in sync too"),
            {next_state, sync, State};
        ready ->
            lager:debug("other agent is in ready state, joining"),
            _ = erlang:cancel_timer(Ref),
            acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
            {next_state, ready, State#state{sync_ref=undefined}};
        {'EXIT', _} ->
            lager:debug("other agent sent unusable state, ignoring"),
            {next_state, sync, State};
        Status ->
            lager:debug("other agent is in ~s, delaying", [Status]),
            _ = erlang:cancel_timer(Ref),
            {next_state, sync, State#state{sync_ref=start_resync_timer()}}
    end;

sync({member_connect_req, _}, State) ->
    lager:debug("member_connect_req recv, not ready"),
    {next_state, sync, State};

sync({pause, Timeout}, #state{acct_id=AcctId
                              ,agent_id=AgentId
                              }=State) ->
    lager:debug("recv status update:, pausing for up to ~b s", [Timeout]),
    Ref = start_pause_timer(Timeout),
    acdc_stats:agent_paused(AcctId, AgentId, Timeout),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_RED_FLASH),
    {next_state, paused, State#state{sync_ref=Ref}};

sync(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {next_state, sync, State}.

sync(status, _, State) ->
    {reply, undefined, sync, State};
sync(current_call, _, State) ->
    {reply, undefined, sync, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
ready({pause, Timeout}, #state{acct_id=AcctId
                               ,agent_id=AgentId
                              }=State) ->
    lager:debug("recv status update: pausing for up to ~b s", [Timeout]),
    Ref = start_pause_timer(Timeout),
    acdc_stats:agent_paused(AcctId, AgentId, Timeout),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_RED_FLASH),
    {next_state, paused, State#state{sync_ref=Ref}};

ready({sync_req, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("recv sync_req from ~s", [wh_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent:send_sync_resp(Srv, ready, JObj),
    {next_state, ready, State};

ready({member_connect_win, JObj}, #state{agent_proc=Srv
                                         ,endpoints=EPs
                                         ,agent_proc_id=MyId
                                         ,agent_id=AgentId
                                        }=State) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    CallId = whapps_call:call_id(Call),

    put(callid, CallId),

    WrapupTimer = wh_json:get_integer_value(<<"Wrapup-Timeout">>, JObj, 0),
    CallerExitKey = wh_json:get_value(<<"Caller-Exit-Key">>, JObj, <<"#">>),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    case wh_json:get_value(<<"Agent-Process-ID">>, JObj) of
        MyId ->
            lager:debug("trying to ring agent ~s to connect to caller", [AgentId]),

            acdc_agent:bridge_to_member(Srv, Call, JObj, EPs),

            {next_state, ringing, State#state{wrapup_timeout=WrapupTimer
                                              ,member_call=Call
                                              ,member_call_id=CallId
                                              ,member_call_start=erlang:now()
                                              ,member_call_queue_id=QueueId
                                              ,caller_exit_key=CallerExitKey
                                             }};
        _OtherId ->
            lager:debug("monitoring agent ~s connecting to caller", [AgentId]),

            acdc_agent:monitor_call(Srv, Call),

            {next_state, ringing, State#state{
                                    wrapup_timeout=WrapupTimer
                                    ,member_call_id=CallId
                                    ,member_call_start=erlang:now()
                                    ,member_call_queue_id=QueueId
                                    ,caller_exit_key=CallerExitKey
                                   }}
    end;

ready({member_connect_req, JObj}, #state{agent_proc=Srv}=State) ->
    acdc_agent:member_connect_resp(Srv, JObj),
    {next_state, ready, State};

ready({channel_hungup, CallId}, #state{agent_proc=Srv}=State) ->
    lager:debug("channel hungup for ~s", [CallId]),
    acdc_agent:channel_hungup(Srv, CallId),
    {next_state, ready, State};
ready({channel_unbridged, CallId}, #state{agent_proc=_Srv}=State) ->
    lager:debug("channel unbridged: ~s", [CallId]),
    {next_state, ready, State};
ready({leg_destroyed, CallId}, #state{agent_proc=_Srv}=State) ->
    lager:debug("channel unbridged: ~s", [CallId]),
    {next_state, ready, State};

ready({resume}, State) ->
    {next_state, ready, State};

ready({dtmf_pressed, _}, State) ->
    {next_state, ready, State};

ready({originate_failed, _E}, State) ->
    {next_state, ready, State};

ready(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {next_state, ready, State}.

ready(status, _, State) ->
    {reply, <<"ready">>, ready, State};
ready(current_call, _, State) ->
    {reply, undefined, ready, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
ringing({member_connect_req, _}, State) ->
    {next_state, ringing, State};

ringing({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("agent won, but can't process this right now (already ringing)"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, ringing, State};

ringing({originate_ready, JObj}, #state{agent_proc=Srv}=State) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    lager:debug("ringing agent's phone with call-id ~s", [CallId]),
    acdc_agent:originate_execute(Srv, JObj),
    {next_state, ringing, State#state{agent_call_id=CallId}};

ringing({originate_failed, _E}, #state{agent_proc=Srv
                                         ,acct_id=AcctId
                                         ,agent_id=AgentId
                                         ,member_call_queue_id=QueueId
                                         ,member_call_id=CallId
                                        }=State) ->
    lager:debug("ringing agent failed: ~p", [_E]),
    acdc_agent:member_connect_retry(Srv, CallId),

    acdc_stats:call_missed(AcctId, QueueId, AgentId, CallId),

    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, clear_call(State)};

ringing({channel_bridged, CallId}, #state{member_call_id=CallId
                                          ,agent_proc=Srv
                                          ,acct_id=AcctId
                                          ,agent_id=AgentId
                                         }=State) ->
    lager:debug("agent phone has been connected to caller"),
    acdc_agent:member_connect_accepted(Srv),
    acdc_stats:agent_oncall(AcctId, AgentId, CallId),
    {next_state, answered, State#state{call_status_ref=start_call_status_timer()
                                      ,call_status_failures=0
                                      }};

ringing({channel_hungup, CallId}, #state{agent_proc=Srv
                                         ,agent_call_id=CallId
                                         ,acct_id=AcctId
                                         ,agent_id=AgentId
                                         ,member_call_queue_id=QueueId
                                         ,member_call_id=MCallId
                                        }=State) ->
    lager:debug("agent did not answer their phone in time: ~s", [CallId]),

    acdc_agent:channel_hungup(Srv, CallId),
    acdc_agent:member_connect_retry(Srv, MCallId),
    acdc_stats:call_missed(AcctId, QueueId, AgentId, MCallId),

    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, clear_call(State)};

ringing({channel_hungup, CallId}, #state{agent_proc=Srv
                                         ,acct_id=AcctId
                                         ,agent_id=AgentId
                                         ,member_call_id=CallId
                                         ,member_call_queue_id=QueueId
                                         ,agent_call_id=AgentCallId
                                        }=State
       ) ->
    lager:debug("caller's channel (~s) has gone down, stop agent's call", [CallId]),
    acdc_agent:channel_hungup(Srv, AgentCallId),

    acdc_stats:call_abandoned(AcctId, QueueId, CallId, ?ABANDON_HANGUP),

    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, clear_call(State)};

ringing({dtmf_pressed, DTMF}, #state{caller_exit_key=DTMF
                                     ,agent_proc=Srv
                                     ,agent_call_id=AgentCallId
                                     ,acct_id=AcctId
                                     ,agent_id=AgentId
                                     ,member_call_queue_id=QueueId
                                     ,member_call_id=CallId
                                    }=State) when is_binary(DTMF) ->
    lager:debug("caller exit key pressed: ~s", [DTMF]),
    acdc_agent:channel_hungup(Srv, AgentCallId),
    acdc_stats:call_abandoned(AcctId, QueueId, CallId, ?ABANDON_EXIT),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, clear_call(State)};

ringing({channel_answered, ACallId}, #state{agent_call_id=ACallId
                                            ,agent_proc=Srv
                                           }=State) ->
    lager:debug("agent answered phone on ~s, connecting to caller", [ACallId]),
    acdc_agent:join_agent(Srv, ACallId),
    {next_state, answered, State#state{call_status_ref=start_call_status_timer()
                                       ,call_status_failures=0
                                      }};

ringing({channel_answered, MCallId}, #state{member_call_id=MCallId}=State) ->
    lager:debug("caller's channel answered"),
    {next_state, ringing, State};

ringing({sync_req, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("recv sync_req from ~s", [wh_json:get_value(<<"Process-ID">>, JObj)]),
    acdc_agent:send_sync_resp(Srv, ringing, JObj),
    {next_state, ringing, State};

ringing(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {next_state, ringing, State}.

ringing(status, _, State) ->
    {reply, <<"ringing">>, ringing, State};
ringing(current_call, _, #state{member_call=Call
                                ,member_call_queue_id=QueueId
                               }=State) ->
    {reply, current_call(Call, ringing, QueueId, undefined), ringing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
answered({member_connect_req, _}, State) ->
    {next_state, answered, State};
answered({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("agent won, but can't process this right now (on the phone with someone)"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, answered, State};

answered({dialplan_error, _App}, #state{agent_proc=Srv
                                        ,acct_id=AcctId
                                        ,agent_id=AgentId
                                        ,member_call_queue_id=QueueId
                                        ,member_call_id=CallId
                                       }=State) ->
    lager:debug("connecting agent to caller failed, clearing call"),
    acdc_agent:member_connect_retry(Srv, CallId),

    acdc_stats:call_missed(AcctId, QueueId, AgentId, CallId),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, clear_call(State)};

answered({channel_bridged, CallId}, #state{member_call_id=CallId
                                           ,agent_proc=Srv
                                           ,acct_id=AcctId
                                           ,agent_id=AgentId
                                           ,member_call=_Call
                                          }=State) ->
    lager:debug("agent has connected to caller"),
    acdc_agent:member_connect_accepted(Srv),
    acdc_stats:agent_oncall(AcctId, AgentId, CallId),

    {next_state, answered, State};

answered({channel_bridged, CallId}, #state{agent_call_id=CallId
                                           ,agent_proc=Srv
                                          }=State) ->
    lager:debug("agent has connected to caller"),
    acdc_agent:member_connect_accepted(Srv),
    {next_state, answered, State};

answered({channel_hungup, CallId}, #state{member_call_id=CallId}=State) ->
    lager:debug("caller's channel hung up"),
    {next_state, wrapup, State#state{wrapup_timeout=0, wrapup_ref=hangup_call(State)}};

answered({channel_hungup, CallId}, #state{agent_call_id=CallId}=State) ->
    lager:debug("agent's channel has hung up"),
    {next_state, wrapup, State#state{wrapup_timeout=0, wrapup_ref=hangup_call(State)}};

answered({channel_hungup, CallId}, #state{agent_proc=Srv}=State) ->
    lager:debug("someone(~s) hungup, ignoring", [CallId]),
    acdc_agent:channel_hungup(Srv, CallId),
    {next_state, answered, State};

answered({sync_req, JObj}, #state{agent_proc=Srv
                                  ,member_call_id=CallId
                                 }=State) ->
    lager:debug("recv sync_req from ~s", [wh_json:get_value(<<"Process-ID">>, JObj)]),
    acdc_agent:send_sync_resp(Srv, answered, JObj, [{<<"Call-ID">>, CallId}]),
    {next_state, answered, State};

answered({channel_unbridged, CallId}, #state{member_call_id=CallId}=State) ->
    lager:debug("caller channel unbridged"),
    {next_state, wrapup, State#state{wrapup_timeout=0, wrapup_ref=hangup_call(State)}};
answered({channel_unbridged, CallId}, #state{agent_call_id=CallId}=State) ->
    lager:debug("agent channel unbridged"),
    {next_state, wrapup, State#state{wrapup_timeout=0, wrapup_ref=hangup_call(State)}};

answered({timeout, CRef, ?CALL_STATUS_MESSAGE}, #state{call_status_ref=CRef
                                                       ,call_status_failures=Failures
                                                      }=State) when Failures > 3 ->
    lager:debug("call status failed ~b times, call is probably down", [Failures]),
    {next_state, wrapup, State#state{wrapup_timeout=0, wrapup_ref=hangup_call(State)}};

answered({timeout, CRef, ?CALL_STATUS_MESSAGE}, #state{call_status_ref=CRef
                                                       ,call_status_failures=Failures
                                                       ,agent_proc=Srv
                                                      }=State) ->
    acdc_agent:call_status_req(Srv),
    {next_state, answered, State#state{call_status_ref=start_call_status_timer()
                                       ,call_status_failures=Failures+1
                                      }};
answered({call_status, JObj}, #state{call_status_failures=Failures}=State) ->
    case wh_json:get_value(<<"Status">>, JObj) of
        <<"active">> -> {next_state, answered, State#state{call_status_failures=0}};
        _S ->
            {next_state, answered, State#state{call_status_failures=Failures+1}}
    end;
answered(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {next_state, answered, State}.

answered(status, _, State) ->
    {reply, <<"answered">>, answered, State};
answered(current_call, _, #state{member_call=Call
                                 ,member_call_start=Start
                                 ,member_call_queue_id=QueueId
                                }=State) ->
    {reply, current_call(Call, answered, QueueId, Start), answered, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
wrapup({member_connect_req, _}, State) ->
    {next_state, wrapup, State#state{wrapup_timeout=0}};
wrapup({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("agent won, but can't process this right now (in wrapup)"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, wrapup, State#state{wrapup_timeout=0}};

wrapup({timeout, Ref, wrapup_expired}, #state{wrapup_ref=Ref
                                              ,acct_id=AcctId
                                              ,agent_id=AgentId
                                             }=State) ->
    lager:debug("wrapup timer expired, ready for action!"),
    acdc_stats:agent_ready(AcctId, AgentId),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),

    {next_state, ready, clear_call(State)};

wrapup({sync_req, JObj}, #state{agent_proc=Srv
                                ,wrapup_ref=Ref
                               }=State) ->
    lager:debug("recv sync_req from ~s", [wh_json:get_value(<<"Process-ID">>, JObj)]),

    acdc_agent:send_sync_resp(Srv, wrapup, JObj, [{<<"Time-Left">>, time_left(Ref)}]),
    {next_state, wrapup, State};

wrapup({channel_hungup, CallId}, #state{agent_proc=Srv}=State) ->
    lager:debug("channel ~s hungup", [CallId]),
    acdc_agent:channel_hungup(Srv, CallId),
    {next_state, wrapup, State};

wrapup({leg_destroyed, CallId}, #state{agent_proc=Srv}=State) ->
    lager:debug("leg ~s destroyed", [CallId]),
    acdc_agent:channel_hungup(Srv, CallId),
    {next_state, wrapup, State};

wrapup(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {next_state, wrapup, State#state{wrapup_timeout=0}}.

wrapup(status, _, State) ->
    {reply, <<"wrapup">>, wrapup, State};
wrapup(current_call, _, #state{member_call=Call
                               ,member_call_start=Start
                               ,member_call_queue_id=QueueId
                              }=State) ->
    {reply, current_call(Call, wrapup, QueueId, Start), wrapup, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
paused({timeout, Ref, ?PAUSE_MESSAGE}, #state{sync_ref=Ref
                                              ,acct_id=AcctId
                                              ,agent_id=AgentId
                                              ,agent_proc=Srv
                                             }=State) when is_reference(Ref) ->
    lager:debug("pause timer expired, putting agent back into action"),

    _ = update_agent_status_to_resume(AcctId, AgentId),

    acdc_agent:send_status_resume(Srv),

    acdc_stats:agent_resume(AcctId, AgentId),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),

    {next_state, ready, clear_call(State#state{sync_ref=undefined})};
paused({resume}, #state{acct_id=AcctId
                        ,agent_id=AgentId
                        ,agent_proc=Srv
                        ,sync_ref=Ref
                       }=State) ->
    lager:debug("resume received, putting agent back into action"),
    _ = erlang:cancel_timer(Ref),

    _ = update_agent_status_to_resume(AcctId, AgentId),

    acdc_agent:send_status_resume(Srv),

    acdc_stats:agent_resume(AcctId, AgentId),

    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_GREEN),
    {next_state, ready, clear_call(State)};

paused({sync_req, JObj}, #state{agent_proc=Srv
                                ,wrapup_ref=Ref
                               }=State) ->
    lager:debug("recv sync_req from ~s", [wh_json:get_value(<<"Process-ID">>, JObj)]),

    acdc_agent:send_sync_resp(Srv, paused, JObj, [{<<"Time-Left">>, time_left(Ref)}]),

    {next_state, paused, State};

paused({member_connect_req, _}, State) ->
    {next_state, paused, State};
paused({member_connect_win, JObj}, #state{agent_proc=Srv}=State) ->
    lager:debug("agent won, but can't process this right now"),
    acdc_agent:member_connect_retry(Srv, JObj),
    {next_state, paused, State};
paused(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {next_state, paused, State}.

paused(status, _, State) ->
    {reply, <<"paused">>, paused, State};
paused(current_call, _, State) ->
    {reply, undefined, paused, State}.

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
handle_event({refresh, AgentJObj}, StateName, State) ->
    lager:debug("refresh agent config: ~p", [AgentJObj]),
    {next_state, StateName, State};
handle_event(load_endpoints, StateName, #state{acct_db=AcctDb
                                               ,agent_id=AgentId
                                               ,acct_id=AcctId
                                               ,agent_proc=Srv
                                              }=State) ->
    Setters = [fun(C) -> whapps_call:set_account_id(AcctId, C) end
               ,fun(C) -> whapps_call:set_account_db(AcctDb, C) end
               ,fun(C) -> whapps_call:set_owner_id(AgentId, C) end
              ],

    Call = lists:foldl(fun(F, C) -> F(C) end
                       ,whapps_call:new(), Setters
                      ),
    case catch acdc_util:get_endpoints(Call, AgentId) of
        [] ->
            lager:debug("no endpoints, going down"),
            acdc_agent:stop(Srv),
            {stop, normal, State};
        [_|_]=EPs ->
            lager:debug("endpoints loaded and registered"),

            {next_state, StateName, State#state{endpoints=EPs}};
        {'EXIT', _E} ->
            lager:debug("failed to load endpoints: ~p", [_E]),
            acdc_agent:stop(Srv),
            {stop, normal, State}
    end;
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
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, _Event]),
    Reply = ok,
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
handle_info({timeout, _Ref, ?SYNC_RESPONSE_MESSAGE}=Msg, StateName, State) ->
    gen_fsm:send_event(self(), Msg),
    {next_state, StateName, State};
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
terminate(_Reason, _StateName, #state{agent_proc=Srv
                                      ,acct_id=AcctId
                                      ,agent_id=AgentId
                                     }) ->
    acdc_agent:stop(Srv),
    acdc_util:presence_update(AcctId, AgentId, ?PRESENCE_RED_SOLID),
    lager:debug("acdc agent fsm terminating while in ~s: ~p", [_StateName, _Reason]).

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
-spec start_wrapup_timer/1 :: (integer()) -> reference().
start_wrapup_timer(Timeout) when Timeout < 0 -> start_wrapup_timer(0); % send immediately
start_wrapup_timer(Timeout) -> gen_fsm:start_timer(Timeout*1000, wrapup_expired).

-spec start_sync_timer/0 :: () -> reference().
-spec start_sync_timer/1 :: (pid()) -> reference().
start_sync_timer() ->
    lager:debug("sync timer start"),
    gen_fsm:start_timer(?SYNC_RESPONSE_TIMEOUT, ?SYNC_RESPONSE_MESSAGE).
start_sync_timer(P) ->
    lager:debug("sync timer start: ~p", [P]),
    erlang:start_timer(?SYNC_RESPONSE_TIMEOUT, P, ?SYNC_RESPONSE_MESSAGE).

-spec start_resync_timer/0 :: () -> reference().
start_resync_timer() ->
    gen_fsm:start_timer(?RESYNC_RESPONSE_TIMEOUT, ?RESYNC_RESPONSE_MESSAGE).

-spec start_pause_timer/1 :: (pos_integer()) -> reference().
start_pause_timer(undefined) -> start_pause_timer(1);
start_pause_timer(Timeout) ->
    gen_fsm:start_timer(Timeout * 1000, ?PAUSE_MESSAGE).

start_call_status_timer() ->
    gen_fsm:start_timer(?CALL_STATUS_TIMEOUT, ?CALL_STATUS_MESSAGE).

-spec callid/1 :: (wh_json:object()) -> api_binary().
callid(JObj) ->
    case wh_json:get_value(<<"Call-ID">>, JObj) of
        undefined -> wh_json:get_value([<<"Call">>, <<"Call-ID">>], JObj);
        CallId -> CallId
    end.

-spec update_agent_status_to_resume/2 :: (ne_binary(), ne_binary()) ->
                                                 {'ok', wh_json:object()}.
update_agent_status_to_resume(AcctId, AgentId) ->
    AcctDb = wh_util:format_account_id(AcctId, encoded),
    Doc = wh_json:from_list([{<<"agent_id">>, AgentId}
                             ,{<<"action">>, <<"resume">>}
                             ,{<<"node">>, wh_util:to_binary(node())}
                             ,{<<"pid">>, wh_util:to_binary(pid_to_list(self()))}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                            ]),
    {ok, _D} = couch_mgr:save_doc(AcctDb, wh_doc:update_pvt_parameters(Doc, AcctDb)).

%% returns time left in seconds
time_left(Ref) when is_reference(Ref) ->
    time_left(erlang:read_timer(Ref));
time_left(false) -> undefined;
time_left(Ms) when is_integer(Ms) -> Ms div 1000.

clear_call(#state{fsm_call_id=FSMCallId}=State) ->
    put(callid, FSMCallId),
    State#state{wrapup_timeout = 0
                ,wrapup_ref = undefined
                ,member_call = undefined
                ,member_call_id = undefined
                ,member_call_start = undefined
                ,member_call_queue_id = undefined
                ,agent_call_id = undefined
                ,caller_exit_key = <<"#">>
                ,call_status_ref = undefined
               }.

-spec current_call/4 :: (whapps_call:call() | 'undefined', atom(), ne_binary(), 'undefined' | wh_now()) ->
                                wh_json:object() | 'undefined'.
current_call(undefined, _, _, _) -> undefined;
current_call(Call, AgentState, QueueId, Start) -> 
    wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                       ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                       ,{<<"caller_id_number">>, whapps_call:caller_id_name(Call)}
                       ,{<<"to">>, whapps_call:to_user(Call)}
                       ,{<<"from">>, whapps_call:from_user(Call)}
                       ,{<<"agent_state">>, wh_util:to_binary(AgentState)}
                       ,{<<"duration">>, elapsed(Start)}
                       ,{<<"queue_id">>, QueueId}
                      ]).

elapsed(undefined) -> undefined;
elapsed(Start) -> wh_util:elapsed_s(Start).

hangup_call(#state{wrapup_timeout=WrapupTimeout
                   ,agent_proc=Srv
                   ,member_call_id=CallId
                   ,member_call_queue_id=QueueId
                   ,member_call_start=Started
                   ,acct_id=AcctId
                   ,agent_id=AgentId
                   ,call_status_ref=CRef
                  }) ->
    lager:debug("going into a wrapup period ~p: ~s", [WrapupTimeout, CallId]),

    acdc_stats:call_processed(AcctId, QueueId, AgentId
                              ,CallId, elapsed(Started)
                             ),

    _ = maybe_stop_timer(CRef),

    acdc_agent:channel_hungup(Srv, CallId),

    acdc_stats:agent_wrapup(AcctId, AgentId, WrapupTimeout),
    start_wrapup_timer(WrapupTimeout).

-spec maybe_stop_timer/1 :: (reference() | 'undefined') -> 'ok'.
maybe_stop_timer(undefined) -> ok;
maybe_stop_timer(ConnRef) ->
    _ = gen_fsm:cancel_timer(ConnRef),
    ok.
