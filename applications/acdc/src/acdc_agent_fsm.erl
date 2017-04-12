%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%% Tracks the agent's state, responds to messages from the corresponding
%%% acdc_agent gen_listener process.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(acdc_agent_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/2, start_link/3, start_link/4, start_link/5
        ,call_event/4
        ,member_connect_req/2
        ,member_connect_win/2
        ,agent_timeout/2
        ,originate_ready/2
        ,originate_resp/2, originate_started/2, originate_uuid/2
        ,originate_failed/2
        ,sync_req/2, sync_resp/2
        ,pause/2
        ,resume/1

        ,add_acdc_queue/2, rm_acdc_queue/2
        ,update_presence/3
        ,agent_logout/1
        ,refresh/2
        ,current_call/1
        ,status/1

        ,new_endpoint/2
        ,edited_endpoint/2
        ,deleted_endpoint/2
        ]).

-export([wait_for_listener/4]).

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
        ,outbound/2

        ,wait/3
        ,sync/3
        ,ready/3
        ,ringing/3
        ,answered/3
        ,wrapup/3
        ,paused/3
        ,outbound/3
        ]).

-ifdef(TEST).
-export([changed_endpoints/2]).
-endif.

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% When an agent starts up, how long do we wait for other agents to respond with their status?
-define(SYNC_RESPONSE_TIMEOUT, 5000).
-define(SYNC_RESPONSE_MESSAGE, 'sync_response_timeout').

%% We weren't able to join our brethern, how long to wait to check again
-define(RESYNC_RESPONSE_TIMEOUT, 15000).
-define(RESYNC_RESPONSE_MESSAGE, 'resync_response_timeout').

-define(PAUSE_MESSAGE, 'pause_expired').

-define(WRAPUP_FINISHED, 'wrapup_finished').

-define(MAX_CONNECT_FAILURES, <<"max_connect_failures">>).
-define(MAX_FAILURES, kapps_config:get_integer(?CONFIG_CAT, ?MAX_CONNECT_FAILURES, 3)).

-define(NOTIFY_PICKUP, <<"pickup">>).
-define(NOTIFY_HANGUP, <<"hangup">>).
-define(NOTIFY_CDR, <<"cdr">>).
-define(NOTIFY_RECORDING, <<"recording">>).
-define(NOTIFY_ALL, <<"all">>).

-define(RESOURCE_TYPE_AUDIO, <<"audio">>).

-record(state, {account_id :: ne_binary()
               ,account_db :: ne_binary()
               ,agent_id :: ne_binary()
               ,agent_listener :: server_ref()
               ,agent_listener_id :: api_ne_binary()
               ,agent_name :: api_binary()

               ,wrapup_timeout = 0 :: integer() % optionally set on win
               ,wrapup_ref :: api_reference()

               ,sync_ref :: api_reference()
               ,pause_ref :: api_reference()

               ,member_call :: kapps_call:call()
               ,member_call_id :: api_binary()
               ,member_call_queue_id :: api_binary()
               ,member_call_start :: kz_now() | undefined
               ,caller_exit_key = <<"#">> :: ne_binary()
               ,queue_notifications :: api_object()

               ,agent_call_id :: api_binary()
               ,next_status :: api_binary()
               ,fsm_call_id :: api_binary() % used when no call-ids are available
               ,endpoints = [] :: kz_json:objects()
               ,outbound_call_ids = [] :: ne_binaries()
               ,max_connect_failures :: kz_timeout()
               ,connect_failures = 0 :: non_neg_integer()
               ,agent_state_updates = [] :: list()
               }).
-type state() :: #state{}.

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
-spec member_connect_req(pid(), kz_json:object()) -> 'ok'.
member_connect_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'member_connect_req', JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When a queue receives a call and needs an agent, it will send a
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%--------------------------------------------------------------------
-spec member_connect_win(pid(), kz_json:object()) -> 'ok'.
member_connect_win(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'member_connect_win', JObj}).

-spec agent_timeout(pid(), kz_json:object()) -> 'ok'.
agent_timeout(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'agent_timeout', JObj}).

%%--------------------------------------------------------------------
%% @doc
%%   When an agent is involved in a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for bridge and hangup events).
%% @end
%%--------------------------------------------------------------------
-spec call_event(pid(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
call_event(FSM, <<"call_event">>, <<"CHANNEL_BRIDGE">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_bridged', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_unbridged', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"usurp_control">>, JObj) ->
    gen_fsm:send_event(FSM, {'usurp_control', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_DESTROY">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_hungup', call_id(JObj), hangup_cause(JObj)});
call_event(FSM, <<"call_event">>, <<"LEG_CREATED">>, JObj) ->
    gen_fsm:send_event(FSM, {'leg_created', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"LEG_DESTROYED">>, JObj) ->
    gen_fsm:send_event(FSM, {'leg_destroyed', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_ANSWER">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_answered', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"DTMF">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {'dtmf_pressed', kz_json:get_value(<<"DTMF-Digit">>, EvtJObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, JObj) ->
    maybe_send_execute_complete(FSM, kz_json:get_value(<<"Application-Name">>, JObj), JObj);
call_event(FSM, <<"error">>, <<"dialplan">>, JObj) ->
    _ = kz_util:put_callid(JObj),
    lager:debug("error event: ~s", [kz_json:get_value(<<"Error-Message">>, JObj)]),

    Req = kz_json:get_value(<<"Request">>, JObj),

    gen_fsm:send_event(FSM, {'dialplan_error', kz_json:get_value(<<"Application-Name">>, Req)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_REPLACED">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_replaced', JObj});
call_event(FSM, <<"call_event">>, <<"CHANNEL_TRANSFEREE">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_unbridged', call_id(JObj)});
call_event(_, _C, _E, _) ->
    lager:info("Unhandled combo: ~s/~s", [_C, _E]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_execute_complete(pid(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_execute_complete(FSM, <<"bridge">>, JObj) ->
    lager:info("Send EXECUTE_COMPLETE,bridge to ~p with ci: ~s, olci: ~s",
               [FSM
               ,call_id(JObj)
               ,kz_call_event:other_leg_call_id(JObj)
               ]),
    gen_fsm:send_event(FSM, {'channel_unbridged', call_id(JObj)});
maybe_send_execute_complete(FSM, <<"call_pickup">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_bridged', call_id(JObj)});
maybe_send_execute_complete(_, _, _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec originate_ready(server_ref(), kz_json:object()) -> 'ok'.
originate_ready(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_ready', JObj}).

-spec originate_resp(server_ref(), kz_json:object()) -> 'ok'.
originate_resp(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_resp', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec originate_started(server_ref(), kz_json:object()) -> 'ok'.
originate_started(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_started', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec originate_uuid(server_ref(), kz_json:object()) -> 'ok'.
originate_uuid(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_uuid'
                            ,kz_json:get_value(<<"Outbound-Call-ID">>, JObj)
                            ,kz_json:get_value(<<"Outbound-Call-Control-Queue">>, JObj)
                            }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec originate_failed(server_ref(), kz_json:object()) -> 'ok'.
originate_failed(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_failed', JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sync_req(server_ref(), kz_json:object()) -> 'ok'.
sync_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'sync_req', JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sync_resp(server_ref(), kz_json:object()) -> 'ok'.
sync_resp(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'sync_resp', JObj}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec pause(server_ref(), kz_timeout()) -> 'ok'.
pause(FSM, Timeout) ->
    gen_fsm:send_all_state_event(FSM, {'pause', Timeout}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec resume(server_ref()) -> 'ok'.
resume(FSM) ->
    gen_fsm:send_all_state_event(FSM, {'resume'}).

%%--------------------------------------------------------------------
%% @doc
%% Request the agent listener bind to queue and conditionally send an
%% availability update depending on agent state
%% @end
%%--------------------------------------------------------------------
-spec add_acdc_queue(server_ref(), ne_binary()) -> 'ok'.
add_acdc_queue(FSM, QueueId) ->
    gen_fsm:send_all_state_event(FSM, {'add_acdc_queue', QueueId}).

%%--------------------------------------------------------------------
%% @doc
%% Request the agent listener unbind from queue and send an
%% unavailability update
%% @end
%%--------------------------------------------------------------------
-spec rm_acdc_queue(server_ref(), ne_binary()) -> 'ok'.
rm_acdc_queue(FSM, QueueId) ->
    gen_fsm:send_all_state_event(FSM, {'rm_acdc_queue', QueueId}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_presence(server_ref(), ne_binary(), ne_binary()) -> 'ok'.
update_presence(FSM, PresenceId, PresenceState) ->
    gen_fsm:send_all_state_event(FSM, {'update_presence', PresenceId, PresenceState}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec agent_logout(server_ref()) -> 'ok'.
agent_logout(FSM) ->
    gen_fsm:send_all_state_event(FSM, {'agent_logout'}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(FSM, AgentJObj) -> gen_fsm:send_all_state_event(FSM, {'refresh', AgentJObj}).

-spec current_call(pid()) -> api_object().
current_call(FSM) -> gen_fsm:sync_send_event(FSM, 'current_call').

-spec status(pid()) -> kz_proplist().
status(FSM) -> gen_fsm:sync_send_event(FSM, 'status').

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), kz_json:object()) -> startlink_ret().
-spec start_link(pid(), kapps_call:call(), ne_binary()) -> startlink_ret().
-spec start_link(ne_binary(), ne_binary(), pid(), kz_proplist()) -> startlink_ret().

start_link(Supervisor, AgentJObj) when is_pid(Supervisor) ->
    pvt_start_link(kz_doc:account_id(AgentJObj)
                  ,kz_doc:id(AgentJObj)
                  ,Supervisor
                  ,[]
                  ,'false'
                  ).
start_link(Supervisor, ThiefCall, _QueueId) ->
    pvt_start_link(kapps_call:account_id(ThiefCall)
                  ,kapps_call:owner_id(ThiefCall)
                  ,Supervisor
                  ,[]
                  ,'true'
                  ).
start_link(AccountId, AgentId, Supervisor, Props) ->
    pvt_start_link(AccountId, AgentId, Supervisor, Props, 'false').

-spec start_link(pid(), any(), ne_binary(), ne_binary(), any()) -> startlink_ret().
start_link(Supervisor, _AgentJObj, AccountId, AgentId, _Queues) ->
    pvt_start_link(AccountId, AgentId, Supervisor, [], 'false').

pvt_start_link('undefined', _AgentId, Supervisor, _, _) ->
    lager:debug("agent ~s trying to start with no account id", [_AgentId]),
    _ = kz_util:spawn(fun acdc_agent_sup:stop/1, [Supervisor]),
    'ignore';
pvt_start_link(_AccountId, 'undefined', Supervisor, _, _) ->
    lager:debug("undefined agent id trying to start in account ~s", [_AccountId]),
    _ = kz_util:spawn(fun acdc_agent_sup:stop/1, [Supervisor]),
    'ignore';
pvt_start_link(AccountId, AgentId, Supervisor, Props, IsThief) ->
    gen_fsm:start_link(?SERVER, [AccountId, AgentId, Supervisor, Props, IsThief], []).

-spec new_endpoint(pid(), kz_json:object()) -> 'ok'.
-spec edited_endpoint(pid(), kz_json:object()) -> 'ok'.
-spec deleted_endpoint(pid(), kz_json:object()) -> 'ok'.
new_endpoint(FSM, EP) ->
    lager:debug("sending EP to ~p: ~p", [FSM, EP]).
edited_endpoint(FSM, EP) ->
    lager:debug("sending EP to ~p: ~p", [FSM, EP]),
    gen_fsm:send_all_state_event(FSM, {'edited_endpoint', kz_doc:id(EP), EP}).
deleted_endpoint(FSM, EP) ->
    lager:debug("sending EP to ~p: ~p", [FSM, EP]).

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
%% @spec init(Args) -> {'ok', StateName, State} |
%%                     {'ok', StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {'ok', atom(), state()}.
init([AccountId, AgentId, Supervisor, Props, IsThief]) ->
    FSMCallId = <<"fsm_", AccountId/binary, "_", AgentId/binary>>,
    kz_util:put_callid(FSMCallId),
    lager:debug("started acdc agent fsm"),

    _P = kz_util:spawn(fun wait_for_listener/4, [Supervisor, self(), Props, IsThief]),
    lager:debug("waiting for listener in ~p", [_P]),

    {'ok'
    ,'wait'
    ,#state{account_id = AccountId
           ,account_db = kz_util:format_account_id(AccountId, 'encoded')
           ,agent_id = AgentId
           ,fsm_call_id = FSMCallId
           ,max_connect_failures = max_failures(AccountId)
           }
    }.

-spec max_failures(ne_binary() | kz_json:object()) -> non_neg_integer().
max_failures(Account) when is_binary(Account) ->
    case kz_account:fetch(Account) of
        {'ok', AccountJObj} -> max_failures(AccountJObj);
        {'error', _} -> ?MAX_FAILURES
    end;
max_failures(JObj) ->
    kz_json:get_integer_value(?MAX_CONNECT_FAILURES, JObj, ?MAX_FAILURES).

-spec wait_for_listener(pid(), pid(), kz_proplist(), boolean()) -> 'ok'.
wait_for_listener(Supervisor, FSM, Props, IsThief) ->
    case acdc_agent_sup:listener(Supervisor) of
        'undefined' ->
            lager:debug("listener not ready yet, waiting"),
            timer:sleep(100),
            wait_for_listener(Supervisor, FSM, Props, IsThief);
        P when is_pid(P) ->
            lager:debug("listener retrieved: ~p", [P]),

            {NextState, SyncRef} =
                case props:get_value('skip_sync', Props) =:= 'true'
                    orelse IsThief
                of
                    'true' -> {'ready', 'undefined'};
                    _ ->
                        gen_fsm:send_event(FSM, 'send_sync_event'),
                        gen_fsm:send_all_state_event(FSM, 'load_endpoints'),
                        {'sync', start_sync_timer(FSM)}
                end,

            gen_fsm:send_event(FSM, {'listener', P, NextState, SyncRef})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wait(any(), state()) -> handle_fsm_ret(state()).
-spec wait(any(), atom(), state()) -> handle_sync_event_ret(state()).
wait({'listener', AgentListener, NextState, SyncRef}, #state{account_id=AccountId
                                                            ,agent_id=AgentId
                                                            }=State) ->
    lager:debug("setting agent proc to ~p", [AgentListener]),
    acdc_agent_listener:fsm_started(AgentListener, self()),
    acdc_agent_stats:agent_ready(AccountId, AgentId),
    {'next_state', NextState, State#state{agent_listener=AgentListener
                                         ,sync_ref=SyncRef
                                         ,agent_listener_id=acdc_util:proc_id()
                                         }};
wait('send_sync_event', State) ->
    gen_fsm:send_event(self(), 'send_sync_event'),
    {'next_state', 'wait', State};
wait(_Msg, State) ->
    lager:debug("unhandled event in wait: ~p", [_Msg]),
    {'next_state', 'wait', State}.

wait('status', _, State) ->
    {'reply', [{'state', <<"wait">>}], 'wait', State};
wait('current_call', _, State) ->
    {'reply', 'undefined', 'wait', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sync(any(), state()) -> handle_fsm_ret(state()).
-spec sync(any(), atom(), state()) -> handle_sync_event_ret(state()).
sync({'timeout', Ref, ?SYNC_RESPONSE_MESSAGE}, #state{sync_ref=Ref
                                                     ,agent_listener=AgentListener
                                                     }=State) when is_reference(Ref) ->
    lager:debug("done waiting for sync responses"),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    apply_state_updates(State#state{sync_ref=Ref});
sync({'timeout', Ref, ?RESYNC_RESPONSE_MESSAGE}, #state{sync_ref=Ref}=State) when is_reference(Ref) ->
    lager:debug("resync timer expired, lets check with the others again"),
    SyncRef = start_sync_timer(),
    gen_fsm:send_event(self(), 'send_sync_event'),
    {'next_state', 'sync', State#state{sync_ref=SyncRef}};
sync('send_sync_event', #state{agent_listener=AgentListener
                              ,agent_listener_id=_AProcId
                              }=State) ->
    lager:debug("sending sync_req event to other agent processes: ~s", [_AProcId]),
    acdc_agent_listener:send_sync_req(AgentListener),
    {'next_state', 'sync', State};
sync({'sync_req', JObj}, #state{agent_listener=AgentListener
                               ,agent_listener_id=AProcId
                               }=State) ->
    case kz_json:get_value(<<"Process-ID">>, JObj) of
        AProcId ->
            lager:debug("recv sync req from ourself"),
            {'next_state', 'sync', State};
        _OtherProcId ->
            lager:debug("recv sync_req from ~s (we are ~s)", [_OtherProcId, AProcId]),
            acdc_agent_listener:send_sync_resp(AgentListener, 'sync', JObj),
            {'next_state', 'sync', State}
    end;
sync({'sync_resp', JObj}, #state{sync_ref=Ref
                                ,agent_listener=AgentListener
                                }=State) ->
    case catch kz_term:to_atom(kz_json:get_value(<<"Status">>, JObj)) of
        'sync' ->
            lager:debug("other agent is in sync too"),
            {'next_state', 'sync', State};
        'ready' ->
            lager:debug("other agent is in ready state, joining"),
            _ = erlang:cancel_timer(Ref),
            acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
            {Next, SwitchTo, State1} =
                apply_state_updates(State#state{sync_ref='undefined'}),
            {Next, SwitchTo, State1, 'hibernate'};
        {'EXIT', _} ->
            lager:debug("other agent sent unusable state, ignoring"),
            {'next_state', 'sync', State};
        Status ->
            lager:debug("other agent is in ~s, delaying", [Status]),
            _ = erlang:cancel_timer(Ref),
            {'next_state', 'sync', State#state{sync_ref=start_resync_timer()}}
    end;
sync({'member_connect_req', _}, State) ->
    lager:debug("member_connect_req recv, not ready"),
    {'next_state', 'sync', State};
sync(?NEW_CHANNEL_FROM(CallId), State) ->
    lager:debug("sync call_from outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
sync(?NEW_CHANNEL_TO(CallId, _), State) ->
    lager:debug("sync call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
sync(_Evt, State) ->
    lager:debug("unhandled event while syncing: ~p", [_Evt]),
    {'next_state', 'sync', State}.

sync('status', _, State) ->
    {'reply', [{'state', <<"sync">>}], 'sync', State};
sync('current_call', _, State) ->
    {'reply', 'undefined', 'sync', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ready(any(), state()) -> handle_fsm_ret(state()).
-spec ready(any(), atom(), state()) -> handle_sync_event_ret(state()).
ready({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'ready', JObj),
    {'next_state', 'ready', State};
ready({'member_connect_win', JObj}, #state{agent_listener=AgentListener
                                          ,endpoints=OrigEPs
                                          ,agent_listener_id=MyId
                                          ,account_id=AccountId
                                          ,agent_id=AgentId
                                          ,connect_failures=CF
                                          }=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    CallId = kapps_call:call_id(Call),

    kz_util:put_callid(CallId),

    WrapupTimer = kz_json:get_integer_value(<<"Wrapup-Timeout">>, JObj, 0),
    CallerExitKey = kz_json:get_value(<<"Caller-Exit-Key">>, JObj, <<"#">>),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    CDRUrl = cdr_url(JObj),
    RecordingUrl = recording_url(JObj),

    case kz_json:get_value(<<"Agent-Process-ID">>, JObj) of
        MyId ->
            lager:debug("trying to ring agent ~s to connect to caller in queue ~s", [AgentId, QueueId]),

            case get_endpoints(OrigEPs, AgentListener, Call, AgentId, QueueId) of
                {'error', 'no_endpoints'} ->
                    lager:info("agent ~s has no endpoints assigned; logging agent out", [AgentId]),
                    acdc_agent_stats:agent_logged_out(AccountId, AgentId),
                    agent_logout(self()),
                    acdc_agent_listener:member_connect_retry(AgentListener, JObj),
                    {'next_state', 'paused', State};
                {'error', _E} ->
                    lager:debug("can't take the call, skip me: ~p", [_E]),
                    acdc_agent_listener:member_connect_retry(AgentListener, JObj),
                    {'next_state', 'ready', State#state{connect_failures=CF+1}};
                {'ok', UpdatedEPs} ->
                    acdc_agent_listener:bridge_to_member(AgentListener, Call, JObj, UpdatedEPs, CDRUrl, RecordingUrl),

                    CIDName = kapps_call:caller_id_name(Call),
                    CIDNum = kapps_call:caller_id_number(Call),

                    acdc_agent_stats:agent_connecting(AccountId, AgentId, CallId, CIDName, CIDNum),
                    lager:info("trying to ring agent endpoints(~p)", [length(UpdatedEPs)]),
                    lager:debug("notifications for the queue: ~p", [kz_json:get_value(<<"Notifications">>, JObj)]),
                    {'next_state', 'ringing', State#state{wrapup_timeout=WrapupTimer
                                                         ,member_call=Call
                                                         ,member_call_id=CallId
                                                         ,member_call_start=kz_time:now()
                                                         ,member_call_queue_id=QueueId
                                                         ,caller_exit_key=CallerExitKey
                                                         ,endpoints=UpdatedEPs
                                                         ,queue_notifications=kz_json:get_value(<<"Notifications">>, JObj)
                                                         }}
            end;
        _OtherId ->
            lager:debug("monitoring agent ~s to connect to caller in queue ~s", [AgentId, QueueId]),

            acdc_agent_listener:monitor_call(AgentListener, Call, CDRUrl, RecordingUrl),

            {'next_state', 'ringing', State#state{
                                        wrapup_timeout=WrapupTimer
                                                 ,member_call_id=CallId
                                                 ,member_call_start=kz_time:now()
                                                 ,member_call_queue_id=QueueId
                                                 ,caller_exit_key=CallerExitKey
                                                 ,agent_call_id='undefined'
                                       }}
    end;
ready({'member_connect_req', _}, #state{max_connect_failures=Max
                                       ,connect_failures=Fails
                                       ,account_id=AccountId
                                       ,agent_id=AgentId
                                       }=State) when is_integer(Max), Fails >= Max ->
    lager:info("agent has failed to connect ~b times, logging out", [Fails]),
    acdc_agent_stats:agent_logged_out(AccountId, AgentId),
    agent_logout(self()),
    {'next_state', 'paused', State};
ready({'member_connect_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:member_connect_resp(AgentListener, JObj),
    {'next_state', 'ready', State};
ready({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("ignoring an outbound call that is the result of a failed originate"),
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    acdc_agent_listener:channel_hungup(AgentListener, ACallId),
    {'next_state', 'ready', State};
ready({'channel_answered', CallId}, #state{outbound_call_ids=OutboundCallIds}=State) ->
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent picked up outbound call ~s", [CallId]),
            {'next_state', 'outbound', start_outbound_call_handling(CallId, clear_call(State, 'ready')), 'hibernate'};
        'false' ->
            lager:debug("unexpected answer of ~s while in ready", [CallId]),
            {'next_state', 'ready', State}
    end;
ready({'channel_hungup', CallId, _Cause}, #state{agent_listener=AgentListener
                                                ,outbound_call_ids=OutboundCallIds
                                                }=State) ->
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent outbound channel ~s down", [CallId]),
            acdc_util:unbind_from_call_events(CallId, AgentListener),
            {'next_state', 'ready', State#state{outbound_call_ids=lists:delete(CallId, OutboundCallIds)}};
        'false' ->
            lager:debug("unexpected channel ~s down", [CallId]),
            acdc_agent_listener:channel_hungup(AgentListener, CallId),
            {'next_state', 'ready', State}
    end;
ready({'channel_unbridged', CallId}, #state{agent_listener=_AgentListener}=State) ->
    lager:debug("channel unbridged: ~s", [CallId]),
    {'next_state', 'ready', State};
ready({'leg_destroyed', CallId}, #state{agent_listener=_AgentListener}=State) ->
    lager:debug("channel unbridged: ~s", [CallId]),
    {'next_state', 'ready', State};
ready({'dtmf_pressed', _}, State) ->
    {'next_state', 'ready', State};
ready({'originate_failed', _E}, State) ->
    {'next_state', 'ready', State};
ready(?NEW_CHANNEL_FROM(CallId), State) ->
    lager:debug("ready call_from outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
ready(?NEW_CHANNEL_TO(CallId, 'undefined'), State) ->
    lager:debug("ready call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
ready(?NEW_CHANNEL_TO(_CallId, _MemberCallId), State) ->
    {'next_state', 'ready', State};
ready(_Evt, State) ->
    lager:debug("unhandled event while ready: ~p", [_Evt]),
    {'next_state', 'ready', State}.

ready('status', _, State) ->
    {'reply', [{'state', <<"ready">>}], 'ready', State};
ready('current_call', _, State) ->
    {'reply', 'undefined', 'ready', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ringing(any(), state()) -> handle_fsm_ret(state()).
-spec ringing(any(), atom(), state()) -> handle_sync_event_ret(state()).
ringing({'member_connect_req', _}, State) ->
    {'next_state', 'ringing', State};
ringing({'member_connect_win', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (already ringing)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'ringing', State};
ringing({'originate_ready', JObj}, #state{agent_listener=AgentListener}=State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),

    lager:debug("ringing agent's phone with call-id ~s", [CallId]),
    acdc_agent_listener:originate_execute(AgentListener, JObj),
    {'next_state', 'ringing', State};
ringing({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv originate_uuid for agent call ~s(~s)", [ACallId, ACtrlQ]),
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    {'next_state', 'ringing', State};
ringing({'originate_started', ACallId}, #state{agent_listener=AgentListener
                                              ,member_call_id=MemberCallId
                                              ,member_call=MemberCall
                                              ,account_id=AccountId
                                              ,agent_id=AgentId
                                              ,queue_notifications=Ns
                                              }=State) ->
    lager:debug("originate resp on ~s, connecting to caller", [ACallId]),
    acdc_agent_listener:member_connect_accepted(AgentListener, ACallId),

    maybe_notify(Ns, ?NOTIFY_PICKUP, State),

    CIDName = kapps_call:caller_id_name(MemberCall),
    CIDNum = kapps_call:caller_id_number(MemberCall),

    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNum),

    {'next_state', 'answered', State#state{agent_call_id=ACallId
                                          ,connect_failures=0
                                          }};
ringing({'originate_failed', E}, #state{agent_listener=AgentListener
                                       ,account_id=AccountId
                                       ,agent_id=AgentId
                                       ,member_call_queue_id=QueueId
                                       ,member_call_id=CallId
                                       ,connect_failures=Fails
                                       ,max_connect_failures=MaxFails
                                       }=State) ->
    acdc_agent_listener:member_connect_retry(AgentListener, CallId),

    ErrReason = missed_reason(kz_json:get_value(<<"Error-Message">>, E)),

    lager:debug("ringing agent failed: ~s", [ErrReason]),

    acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, ErrReason),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing({'agent_timeout', _JObj}, #state{agent_listener=AgentListener
                                        ,account_id=AccountId
                                        ,agent_id=AgentId
                                        ,member_call_queue_id=QueueId
                                        ,member_call_id=CallId
                                        ,connect_failures=Fails
                                        ,max_connect_failures=MaxFails
                                        }=State) ->
    acdc_agent_listener:agent_timeout(AgentListener),
    lager:debug("recv timeout from queue process"),
    acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, <<"timeout">>),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing({'channel_bridged', MemberCallId}, #state{member_call_id=MemberCallId
                                                 ,member_call=MemberCall
                                                 ,agent_listener=AgentListener
                                                 ,account_id=AccountId
                                                 ,agent_id=AgentId
                                                 ,queue_notifications=Ns
                                                 }=State) ->
    lager:debug("agent phone has been connected to caller"),
    acdc_agent_listener:member_connect_accepted(AgentListener),

    maybe_notify(Ns, ?NOTIFY_PICKUP, State),

    CIDName = kapps_call:caller_id_name(MemberCall),
    CIDNum = kapps_call:caller_id_number(MemberCall),

    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNum),

    {'next_state', 'answered', State#state{connect_failures=0}};
ringing({'channel_bridged', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing({'channel_hungup', AgentCallId, Cause}, #state{agent_listener=AgentListener
                                                      ,agent_call_id=AgentCallId
                                                      ,account_id=AccountId
                                                      ,agent_id=AgentId
                                                      ,member_call_queue_id=QueueId
                                                      ,member_call_id=MemberCallId
                                                      ,connect_failures=Fails
                                                      ,max_connect_failures=MaxFails
                                                      }=State) ->
    lager:debug("ringing agent failed: timeout on ~s ~s", [AgentCallId, Cause]),

    acdc_agent_listener:member_connect_retry(AgentListener, MemberCallId),
    acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),

    acdc_stats:call_missed(AccountId, QueueId, AgentId, MemberCallId, Cause),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing({'channel_hungup', MemberCallId, _Cause}, #state{agent_listener=AgentListener
                                                        ,member_call_id=MemberCallId
                                                        }=State) ->
    lager:debug("caller's channel (~s) has gone down, stop agent's call: ~s", [MemberCallId, _Cause]),
    acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    apply_state_updates(clear_call(State, 'ready'));
ringing({'channel_hungup', CallId, _Cause}, #state{agent_listener=AgentListener
                                                  ,outbound_call_ids=OutboundCallIds
                                                  }=State) ->
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent outbound channel ~s down", [CallId]),
            acdc_util:unbind_from_call_events(CallId, AgentListener),
            {'next_state', 'ringing', State#state{outbound_call_ids=lists:delete(CallId, OutboundCallIds)}};
        'false' ->
            lager:debug("unexpected channel ~s down", [CallId]),
            {'next_state', 'ringing', State}
    end;
ringing({'dtmf_pressed', DTMF}, #state{caller_exit_key=DTMF
                                      ,agent_listener=AgentListener
                                      ,agent_call_id=AgentCallId
                                      }=State) when is_binary(DTMF) ->
    lager:debug("caller exit key pressed: ~s", [DTMF]),
    acdc_agent_listener:channel_hungup(AgentListener, AgentCallId),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    apply_state_updates(clear_call(State, 'ready'));
ringing({'dtmf_pressed', DTMF}, #state{caller_exit_key=_ExitKey}=State) ->
    lager:debug("caller pressed ~s, exit key is ~s", [DTMF, _ExitKey]),
    {'next_state', 'ringing', State};
ringing({'channel_answered', MemberCallId}, #state{member_call_id=MemberCallId}=State) ->
    lager:debug("caller's channel answered"),
    {'next_state', 'ringing', State};
ringing({'channel_answered', OtherCallId}, #state{account_id=AccountId
                                                 ,agent_id=AgentId
                                                 ,member_call=MemberCall
                                                 ,member_call_id=MemberCallId
                                                 ,agent_listener=AgentListener
                                                 ,outbound_call_ids=OutboundCallIds
                                                 }=State) ->
    case lists:member(OtherCallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent picked up outbound call ~s instead of the queue call ~s", [OtherCallId, MemberCallId]),
            acdc_agent_listener:hangup_call(AgentListener),
            {'next_state', 'outbound', start_outbound_call_handling(OtherCallId, clear_call(State, 'ready')), 'hibernate'};
        'false' ->
            lager:debug("recv answer for ~s, probably the agent's call", [OtherCallId]),

            CIDName = kapps_call:caller_id_name(MemberCall),
            CIDNum = kapps_call:caller_id_number(MemberCall),

            acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNum),

            acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_SOLID),

            {'next_state', 'answered', State#state{agent_call_id=OtherCallId
                                                  ,connect_failures=0
                                                  }}
    end;
ringing({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Process-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'ringing', JObj),
    {'next_state', 'ringing', State};
ringing({'originate_resp', ACallId}, #state{agent_listener=AgentListener
                                           ,member_call_id=MemberCallId
                                           ,member_call=MemberCall
                                           ,account_id=AccountId
                                           ,agent_id=AgentId
                                           ,queue_notifications=Ns
                                           }=State) ->
    lager:debug("originate resp on ~s, connecting to caller", [ACallId]),
    acdc_agent_listener:member_connect_accepted(AgentListener, ACallId),

    maybe_notify(Ns, ?NOTIFY_PICKUP, State),

    CIDName = kapps_call:caller_id_name(MemberCall),
    CIDNum = kapps_call:caller_id_number(MemberCall),

    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNum),

    {'next_state', 'answered', State#state{agent_call_id=ACallId
                                          ,connect_failures=0
                                          }};
ringing(?NEW_CHANNEL_FROM(CallId), #state{agent_listener=AgentListener}=State) ->
    lager:debug("ringing call_from outbound: ~s", [CallId]),
    acdc_agent_listener:hangup_call(AgentListener),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, clear_call(State, 'ready')), 'hibernate'};
ringing(?NEW_CHANNEL_TO(CallId, 'undefined'), #state{agent_listener=AgentListener
                                                    ,outbound_call_ids=OutboundCallIds
                                                    }=State) ->
    lager:debug("ringing call_to outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'ringing', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
ringing(?NEW_CHANNEL_TO(CallId, MemberCallId), #state{member_call_id=MemberCallId}=State) ->
    lager:debug("new channel ~s for agent", [CallId]),
    {'next_state', 'ringing', State};
ringing(?NEW_CHANNEL_TO(CallId, _MemberCallId), #state{agent_listener=AgentListener}=State) ->
    lager:debug("found a uuid ~s that was from a previous queue call", [CallId]),
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    {'next_state', 'ringing', State};
ringing({'leg_created', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing({'leg_destroyed', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing({'usurp_control', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing(_Evt, State) ->
    lager:debug("unhandled event while ringing: ~p", [_Evt]),
    {'next_state', 'ringing', State}.

ringing('status', _, #state{member_call_id=MemberCallId
                           ,agent_call_id=ACallId
                           }=State) ->
    {'reply', [{'state', <<"ringing">>}
              ,{'member_call_id', MemberCallId}
              ,{'agent_call_id', ACallId}
              ]
    ,'ringing', State};
ringing('current_call', _, #state{member_call=Call
                                 ,member_call_queue_id=QueueId
                                 }=State) ->
    {'reply', current_call(Call, 'ringing', QueueId, 'undefined'), 'ringing', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec answered(any(), state()) -> handle_fsm_ret(state()).
-spec answered(any(), atom(), state()) -> handle_sync_event_ret(state()).
answered({'member_connect_req', _}, State) ->
    {'next_state', 'answered', State};
answered({'member_connect_win', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (on the phone with someone)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'answered', State};
answered({'dialplan_error', _App}, #state{agent_listener=AgentListener
                                         ,account_id=AccountId
                                         ,agent_id=AgentId
                                         ,member_call_queue_id=QueueId
                                         ,member_call_id=CallId
                                         ,agent_call_id=ACallId
                                         }=State) ->
    lager:debug("connecting agent to caller failed(~p), clearing call", [_App]),
    acdc_agent_listener:channel_hungup(AgentListener, ACallId),
    acdc_agent_listener:member_connect_retry(AgentListener, CallId),

    acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, <<"dialplan_error">>),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    apply_state_updates(clear_call(State, 'ready'));
answered({'channel_bridged', CallId}, #state{member_call_id=CallId
                                            ,agent_listener=AgentListener
                                            ,queue_notifications=Ns
                                            }=State) ->
    lager:debug("agent has connected to member"),
    acdc_agent_listener:member_connect_accepted(AgentListener),
    maybe_notify(Ns, ?NOTIFY_PICKUP, State),
    {'next_state', 'answered', State};
answered({'channel_bridged', CallId}, #state{agent_call_id=CallId
                                            ,agent_listener=AgentListener
                                            ,queue_notifications=Ns
                                            }=State) ->
    lager:debug("agent has connected (~s) to caller", [CallId]),
    acdc_agent_listener:member_connect_accepted(AgentListener, CallId),
    maybe_notify(Ns, ?NOTIFY_PICKUP, State),
    {'next_state', 'answered', State};
answered({'channel_replaced', JObj}, #state{agent_listener=AgentListener}=State) ->
    CallId = kz_call_event:call_id(JObj),
    ReplacedBy = kz_call_event:replaced_by(JObj),
    acdc_agent_listener:rebind_events(AgentListener, CallId, ReplacedBy),
    kz_util:put_callid(ReplacedBy),
    lager:info("channel ~s replaced by ~s", [CallId, ReplacedBy]),
    {'next_state', 'answered', State#state{member_call_id = ReplacedBy}};
answered({'channel_hungup', CallId, Cause}, #state{member_call_id=CallId
                                                  ,outbound_call_ids=[]
                                                  }=State) ->
    lager:debug("caller's channel hung up: ~s", [Cause]),
    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'member')}};
answered({'channel_hungup', CallId, _Cause}, #state{account_id=AccountId
                                                   ,agent_id=AgentId
                                                   ,agent_listener=AgentListener
                                                   ,member_call_id=CallId
                                                   ,member_call_queue_id=QueueId
                                                   ,queue_notifications=Ns
                                                   ,outbound_call_ids=[OutboundCallId|_]
                                                   }=State) ->
    lager:debug("caller's channel hung up, but there are still some outbounds"),
    acdc_stats:call_processed(AccountId, QueueId, AgentId, CallId, 'member'),
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    {'next_state', 'outbound', start_outbound_call_handling(OutboundCallId, clear_call(State, 'ready')), 'hibernate'};
answered({'channel_hungup', CallId, Cause}, #state{agent_call_id=CallId
                                                  ,outbound_call_ids=[]
                                                  }=State) ->
    lager:debug("agent's channel has hung up: ~s", [Cause]),
    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'agent')}};
answered({'channel_hungup', CallId, _Cause}, #state{account_id=AccountId
                                                   ,agent_id=AgentId
                                                   ,agent_listener=AgentListener
                                                   ,member_call_id=MemberCallId
                                                   ,member_call_queue_id=QueueId
                                                   ,queue_notifications=Ns
                                                   ,agent_call_id=CallId
                                                   ,outbound_call_ids=[OutboundCallId|_]
                                                   }=State) ->
    lager:debug("agent's channel hung up, but there are still some outbounds"),
    acdc_stats:call_processed(AccountId, QueueId, AgentId, CallId, 'agent'),
    acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    {'next_state', 'outbound', start_outbound_call_handling(OutboundCallId, clear_call(State, 'ready')), 'hibernate'};
answered({'channel_hungup', CallId, _Cause}, #state{agent_listener=AgentListener
                                                   ,outbound_call_ids=OutboundCallIds
                                                   }=State) ->
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent outbound channel ~s down", [CallId]),
            acdc_util:unbind_from_call_events(CallId, AgentListener),
            {'next_state', 'answered', State#state{outbound_call_ids=lists:delete(CallId, OutboundCallIds)}};
        'false' ->
            lager:debug("unexpected channel ~s down", [CallId]),
            {'next_state', 'answered', State}
    end;
answered({'sync_req', JObj}, #state{agent_listener=AgentListener
                                   ,member_call_id=CallId
                                   }=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Process-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'answered', JObj, [{<<"Call-ID">>, CallId}]),
    {'next_state', 'answered', State};
answered({'channel_unbridged', CallId}, #state{member_call_id=CallId}=State) ->
    lager:info("caller channel ~s unbridged", [CallId]),
    {'next_state', 'answered', State};
answered({'channel_unbridged', CallId}, #state{agent_call_id=CallId}=State) ->
    lager:info("agent channel unbridged"),
    {'next_state', 'answered', State};
answered({'channel_answered', MemberCallId}, #state{member_call_id=MemberCallId}=State) ->
    lager:debug("member's channel has answered"),
    {'next_state', 'answered', State};
answered({'channel_answered', AgentCallId}, #state{agent_call_id=AgentCallId}=State) ->
    lager:debug("agent's channel ~s has answered", [AgentCallId]),
    {'next_state', 'answered', State};
answered({'channel_answered', OtherCallId}=Evt, #state{outbound_call_ids=OutboundCallIds}=State) ->
    case lists:member(OtherCallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent answered outbound call ~s", [OtherCallId]),
            {'next_state', 'answered', State};
        'false' ->
            lager:debug("unexpected event while answered: ~p", [Evt]),
            {'next_state', 'answered', State}
    end;
answered({'originate_started', _CallId}, State) ->
    {'next_state', 'answered', State};
answered(?NEW_CHANNEL_FROM(CallId), #state{agent_listener=AgentListener
                                          ,outbound_call_ids=OutboundCallIds
                                          }=State) ->
    lager:debug("answered call_from outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'answered', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
answered(?NEW_CHANNEL_TO(CallId, 'undefined'), #state{agent_listener=AgentListener
                                                     ,outbound_call_ids=OutboundCallIds
                                                     }=State) ->
    lager:debug("answered call_to outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'answered', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
answered(?NEW_CHANNEL_TO(CallId, MemberCallId), #state{member_call_id=MemberCallId}=State) ->
    lager:debug("new channel ~s for agent", [CallId]),
    {'next_state', 'answered', State};
answered({'leg_created', _CallId}, State) ->
    {'next_state', 'answered', State};
answered({'usurp_control', _CallId}, State) ->
    {'next_state', 'answered', State};
answered(_Evt, State) ->
    lager:debug("unhandled event while answered: ~p", [_Evt]),
    {'next_state', 'answered', State}.

answered('status', _, #state{member_call_id=MemberCallId
                            ,agent_call_id=ACallId
                            }=State) ->
    {'reply', [{'state', <<"answered">>}
              ,{'member_call_id', MemberCallId}
              ,{'agent_call_id', ACallId}
              ]
    ,'answered', State};
answered('current_call', _, #state{member_call=Call
                                  ,member_call_start=Start
                                  ,member_call_queue_id=QueueId
                                  }=State) ->
    {'reply', current_call(Call, 'answered', QueueId, Start), 'answered', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec wrapup(any(), state()) -> handle_fsm_ret(state()).
-spec wrapup(any(), atom(), state()) -> handle_sync_event_ret(state()).
wrapup({'pause', Timeout}, #state{account_id=AccountId
                                 ,agent_id=AgentId
                                 ,agent_listener=AgentListener
                                 }=State) ->
    lager:debug("recv status update: pausing for up to ~b s", [Timeout]),
    Ref = start_pause_timer(Timeout),
    acdc_agent_stats:agent_paused(AccountId, AgentId, Timeout),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_FLASH),

    {'next_state', 'paused', State#state{pause_ref=Ref}};
wrapup({'member_connect_req', _}, State) ->
    {'next_state', 'wrapup', State#state{wrapup_timeout=0}};
wrapup({'member_connect_win', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (in wrapup)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'wrapup', State#state{wrapup_timeout=0}};
wrapup({'timeout', Ref, ?WRAPUP_FINISHED}, #state{wrapup_ref=Ref
                                                 ,agent_listener=AgentListener
                                                 }=State) ->
    lager:debug("wrapup timer expired, ready for action!"),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    apply_state_updates(clear_call(State, 'ready'));
wrapup({'sync_req', JObj}, #state{agent_listener=AgentListener
                                 ,wrapup_ref=Ref
                                 }=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Process-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'wrapup', JObj, [{<<"Time-Left">>, time_left(Ref)}]),
    {'next_state', 'wrapup', State};
wrapup({'channel_hungup', _, _}, State) ->
    {'next_state', 'wrapup', State};
wrapup({'leg_destroyed', CallId}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("leg ~s destroyed", [CallId]),
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    {'next_state', 'wrapup', State};
wrapup(?NEW_CHANNEL_FROM(CallId), State) ->
    lager:debug("wrapup call_from outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
wrapup(?NEW_CHANNEL_TO(CallId, _), State) ->
    lager:debug("wrapup call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
wrapup({'originate_resp', _}, State) ->
    {'next_state', 'wrapup', State};
wrapup(_Evt, State) ->
    lager:debug("unhandled event while in wrapup: ~p", [_Evt]),
    {'next_state', 'wrapup', State#state{wrapup_timeout=0}}.

wrapup('status', _, #state{wrapup_ref=Ref}=State) ->
    {'reply', [{'state', <<"wrapup">>}
              ,{'wrapup_left', time_left(Ref)}
              ]
    ,'wrapup', State};
wrapup('current_call', _, #state{member_call=Call
                                ,member_call_start=Start
                                ,member_call_queue_id=QueueId
                                }=State) ->
    {'reply', current_call(Call, 'wrapup', QueueId, Start), 'wrapup', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec paused(any(), state()) -> handle_fsm_ret(state()).
-spec paused(any(), atom(), state()) -> handle_sync_event_ret(state()).
paused({'timeout', Ref, ?PAUSE_MESSAGE}, #state{pause_ref=Ref
                                               ,agent_listener=AgentListener
                                               }=State) when is_reference(Ref) ->
    lager:debug("pause timer expired, putting agent back into action"),

    acdc_agent_listener:update_agent_status(AgentListener, <<"resume">>),

    acdc_agent_listener:send_status_resume(AgentListener),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    apply_state_updates(clear_call(State#state{sync_ref='undefined'}, 'ready'));
paused({'sync_req', JObj}, #state{agent_listener=AgentListener
                                 ,pause_ref=Ref
                                 }=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Process-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'paused', JObj, [{<<"Time-Left">>, time_left(Ref)}]),
    {'next_state', 'paused', State};
paused({'member_connect_req', _}, State) ->
    {'next_state', 'paused', State};
paused({'member_connect_win', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'paused', State};
paused({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("ignoring an outbound call that is the result of a failed originate"),
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    acdc_agent_listener:channel_hungup(AgentListener, ACallId),
    {'next_state', 'paused', State};
paused(?NEW_CHANNEL_FROM(CallId), State) ->
    lager:debug("paused call_from outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
paused(?NEW_CHANNEL_TO(CallId, 'undefined'), State) ->
    lager:debug("paused call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, State), 'hibernate'};
paused(?NEW_CHANNEL_TO(_CallId, _MemberCallId), State) ->
    {'next_state', 'paused', State};
paused(_Evt, State) ->
    lager:debug("unhandled event while paused: ~p", [_Evt]),
    {'next_state', 'paused', State}.

paused('status', _, #state{pause_ref=Ref}=State) ->
    {'reply', [{'state', <<"paused">>}
              ,{'pause_left', time_left(Ref)}
              ]
    ,'paused', State};
paused('current_call', _, State) ->
    {'reply', 'undefined', 'paused', State}.

-spec outbound(any(), state()) -> handle_fsm_ret(state()).
-spec outbound(any(), atom(), state()) -> handle_sync_event_ret(state()).
outbound({'channel_hungup', CallId, Cause}, #state{agent_listener=AgentListener
                                                  ,outbound_call_ids=OutboundCallIds
                                                  }=State) ->
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent outbound channel ~s down: ~s", [CallId, Cause]),
            outbound_hungup(State#state{outbound_call_ids=lists:delete(CallId, OutboundCallIds)});
        'false' ->
            lager:debug("unexpected channel ~s down", [CallId]),
            {'next_state', 'outbound', State}
    end;
outbound({'member_connect_win', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (on outbound call)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),
    {'next_state', 'outbound', State};
outbound({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("ignoring an outbound call that is the result of a failed originate"),
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    acdc_agent_listener:channel_hungup(AgentListener, ACallId),
    {'next_state', 'outbound', State};
outbound({'originate_failed', _E}, State) ->
    {'next_state', 'outbound', State};
outbound({'timeout', Ref, ?PAUSE_MESSAGE}, #state{pause_ref=Ref}=State) ->
    lager:debug("pause timer expired while outbound"),
    {'next_state', 'outbound', State#state{pause_ref='undefined'}};
outbound({'timeout', WRef, ?WRAPUP_FINISHED}, #state{wrapup_ref=WRef}=State) ->
    lager:debug("wrapup timer ended while on outbound call"),
    {'next_state', 'outbound', State#state{wrapup_ref='undefined'}, 'hibernate'};
outbound({'member_connect_req', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'leg_created', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_answered', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_bridged', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_unbridged', _}, State) ->
    {'next_state', 'outbound', State};
outbound(?NEW_CHANNEL_TO(CallId, _), #state{outbound_call_ids=[CallId]}=State) ->
    {'next_state', 'outbound', State};
outbound(?NEW_CHANNEL_FROM(CallId), #state{agent_listener=AgentListener
                                          ,outbound_call_ids=OutboundCallIds
                                          }=State) ->
    lager:debug("outbound call_from outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'outbound', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
outbound(?NEW_CHANNEL_TO(CallId, 'undefined'), #state{agent_listener=AgentListener
                                                     ,outbound_call_ids=OutboundCallIds
                                                     }=State) ->
    lager:debug("outbound call_to outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'outbound', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
outbound(?NEW_CHANNEL_TO(_CallId, _MemberCallId), State) ->
    {'next_state', 'outbound', State};
outbound({'leg_destroyed', _CallId}, State) ->
    {'next_state', 'outbound', State};
outbound({'usurp_control', _CallId}, State) ->
    {'next_state', 'outbound', State};
outbound(_Msg, State) ->
    lager:debug("ignoring msg in outbound: ~p", [_Msg]),
    {'next_state', 'outbound', State}.

outbound('status', _, #state{wrapup_ref=Ref
                            ,outbound_call_ids=OutboundCallIds
                            }=State) ->
    {'reply', [{'state', <<"outbound">>}
              ,{'wrapup_left', time_left(Ref)}
              ,{'outbound_call_id', hd(OutboundCallIds)}
              ]
    ,'outbound', State};
outbound('current_call', _, State) ->
    {'reply', 'undefined', 'outbound', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_event({'agent_logout'}=Event, StateName, #state{agent_state_updates=Queue}=State) ->
    case valid_state_for_logout(StateName) of
        'true' -> handle_agent_logout(State);
        'false' ->
            NewQueue = [Event | Queue],
            {'next_state', StateName, State#state{agent_state_updates=NewQueue}}
    end;
handle_event({'resume'}, 'ready', State) ->
    {'next_state', 'ready', State};
handle_event({'resume'}=Event, 'paused', #state{agent_state_updates=Queue}=State) ->
    NewQueue = [Event | Queue],
    apply_state_updates(State#state{agent_state_updates=NewQueue});
handle_event({'resume'}=Event, StateName, #state{agent_state_updates=Queue}=State) ->
    lager:debug("recv resume during ~p, delaying", [StateName]),
    NewQueue = [Event | Queue],
    {'next_state', StateName, State#state{agent_state_updates=NewQueue}};
handle_event({'pause', Timeout}=Event, 'ready', #state{agent_state_updates=Queue}=State) ->
    lager:debug("recv status update: pausing for up to ~b s", [Timeout]),
    NewQueue = [Event | Queue],
    apply_state_updates(State#state{agent_state_updates=NewQueue});
handle_event({'pause', Timeout}, 'paused', State) ->
    handle_event({'pause', Timeout}, 'ready', State);
handle_event({'pause', _}=Event, StateName, #state{agent_state_updates=Queue}=State) ->
    lager:debug("recv pause during ~p, delaying", [StateName]),
    NewQueue = [Event | Queue],
    {'next_state', StateName, State#state{agent_state_updates=NewQueue}};
handle_event({'add_acdc_queue', QueueId}, StateName, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:add_acdc_queue(AgentListener, QueueId, StateName),
    {'next_state', StateName, State};
handle_event({'rm_acdc_queue', QueueId}, StateName, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:rm_acdc_queue(AgentListener, QueueId),
    {'next_state', StateName, State};
handle_event({'update_presence', PresenceId, PresenceState}, 'ready', State) ->
    handle_presence_update(PresenceId, PresenceState, State),
    {'next_state', 'ready', State};
handle_event({'update_presence', _, _}=Event, StateName, #state{agent_state_updates=Queue}=State) ->
    NewQueue = [Event | Queue],
    {'next_state', StateName, State#state{agent_state_updates=NewQueue}};
handle_event({'refresh', AgentJObj}, StateName, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:refresh_config(AgentListener, kz_json:get_value(<<"queues">>, AgentJObj), StateName),
    {'next_state', StateName, State};
handle_event('load_endpoints', StateName, #state{agent_listener='undefined'}=State) ->
    lager:debug("agent proc not ready, not loading endpoints yet"),
    gen_fsm:send_all_state_event(self(), 'load_endpoints'),
    {'next_state', StateName, State};
handle_event('load_endpoints', StateName, #state{agent_id=AgentId
                                                ,agent_listener=AgentListener
                                                ,account_id=AccountId
                                                ,account_db=AccountDb
                                                }=State) ->
    Setters = [{fun kapps_call:set_account_id/2, AccountId}
              ,{fun kapps_call:set_account_db/2, AccountDb}
              ,{fun kapps_call:set_owner_id/2, AgentId}
              ,{fun kapps_call:set_resource_type/2, ?RESOURCE_TYPE_AUDIO}
              ],

    Call = kapps_call:exec(Setters, kapps_call:new()),

    %% Inform us of things with us as owner
    catch gproc:reg(?OWNER_UPDATE_REG(AccountId, AgentId)),

    case get_endpoints([], AgentListener, Call, AgentId, 'undefined') of
        {'error', 'no_endpoints'} -> {'next_state', StateName, State};
        {'ok', EPs} -> {'next_state', StateName, State#state{endpoints=EPs}};
        {'error', E} -> {'stop', E, State}
    end;
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in state ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {'reply', Reply, NextStateName, NextState} |
%%                   {'reply', Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> handle_sync_event_ret(state()).
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, _Event]),
    {'reply', 'ok', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {'next_state', NextStateName, NextState} |
%%                   {'next_state', NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), atom(), state()) -> handle_fsm_ret(state()).
handle_info({'timeout', _Ref, ?SYNC_RESPONSE_MESSAGE}=Msg, StateName, State) ->
    gen_fsm:send_event(self(), Msg),
    {'next_state', StateName, State};
handle_info({'endpoint_edited', EP}, StateName, #state{endpoints=EPs
                                                      ,account_id=AccountId
                                                      ,agent_id=AgentId
                                                      ,agent_listener=AgentListener
                                                      }=State) ->
    EPId = kz_doc:id(EP),
    case kz_json:get_value(<<"owner_id">>, EP) of
        AgentId ->
            lager:debug("device ~s edited, we're the owner, maybe adding it", [EPId]),
            {'next_state', StateName, State#state{endpoints=maybe_add_endpoint(EPId, EP, EPs, AccountId, AgentListener)}, 'hibernate'};
        _OwnerId ->
            lager:debug("device ~s edited, owner now ~s, maybe removing it", [EPId, _OwnerId]),
            {'next_state', StateName, State#state{endpoints=maybe_remove_endpoint(EPId, EPs, AccountId, AgentListener)}, 'hibernate'}
    end;
handle_info({'endpoint_deleted', EP}, StateName, #state{endpoints=EPs
                                                       ,account_id=AccountId
                                                       ,agent_listener=AgentListener
                                                       }=State) ->
    EPId = kz_doc:id(EP),
    lager:debug("device ~s deleted, maybe removing it", [EPId]),
    {'next_state', StateName, State#state{endpoints=maybe_remove_endpoint(EPId, EPs, AccountId, AgentListener)}, 'hibernate'};
handle_info({'endpoint_created', EP}, StateName, #state{endpoints=EPs
                                                       ,account_id=AccountId
                                                       ,agent_id=AgentId
                                                       ,agent_listener=AgentListener
                                                       }=State) ->
    EPId = kz_doc:id(EP),
    case kz_json:get_value(<<"owner_id">>, EP) of
        AgentId ->
            lager:debug("device ~s created, we're the owner, maybe adding it", [EPId]),
            {'next_state', StateName, State#state{endpoints=maybe_add_endpoint(EPId, EP, EPs, AccountId, AgentListener)}, 'hibernate'};
        _OwnerId ->
            lager:debug("device ~s created, owner is ~s, maybe ignoring", [EPId, _OwnerId]),

            case kz_json:get_value([<<"hotdesk">>, <<"users">>, AgentId], EP) of
                'undefined' -> {'next_state', StateName, State};
                _ ->
                    lager:debug("device ~s created, we're a hotdesk user, maybe adding it", [EPId]),
                    {'next_state', StateName, State#state{endpoints=maybe_add_endpoint(EPId, EP, EPs, AccountId, AgentListener)}, 'hibernate'}
            end
    end;
handle_info(?NEW_CHANNEL_FROM(_CallId)=Evt, StateName, State) ->
    gen_fsm:send_event(self(), Evt),
    {'next_state', StateName, State};
handle_info(?NEW_CHANNEL_TO(_CallId, _)=Evt, StateName, State) ->
    gen_fsm:send_event(self(), Evt),
    {'next_state', StateName, State};
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled message in state ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

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
-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(_Reason, _StateName, #state{agent_listener=AgentListener}) ->
    lager:debug("acdc agent fsm terminating while in ~s: ~p", [_StateName, _Reason]),
    acdc_agent_listener:stop(AgentListener),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_SOLID).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {'ok', StateName, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), atom(), state(), any()) -> {'ok', atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_wrapup_timer(integer()) -> reference().
start_wrapup_timer(Timeout) when Timeout =< 0 -> start_wrapup_timer(1); % send immediately
start_wrapup_timer(Timeout) -> gen_fsm:start_timer(Timeout*1000, ?WRAPUP_FINISHED).

-spec start_sync_timer() -> reference().
-spec start_sync_timer(pid()) -> reference().
start_sync_timer() ->
    gen_fsm:start_timer(?SYNC_RESPONSE_TIMEOUT, ?SYNC_RESPONSE_MESSAGE).
start_sync_timer(P) ->
    erlang:start_timer(?SYNC_RESPONSE_TIMEOUT, P, ?SYNC_RESPONSE_MESSAGE).

-spec start_resync_timer() -> reference().
start_resync_timer() ->
    gen_fsm:start_timer(?RESYNC_RESPONSE_TIMEOUT, ?RESYNC_RESPONSE_MESSAGE).

-spec start_pause_timer(pos_integer()) -> api_reference().
start_pause_timer('undefined') -> start_pause_timer(1);
start_pause_timer(0) -> 'undefined';
start_pause_timer(Timeout) ->
    gen_fsm:start_timer(Timeout * 1000, ?PAUSE_MESSAGE).

-spec call_id(kz_json:object()) -> api_binary().
call_id(JObj) ->
    case kz_json:get_value(<<"Call-ID">>, JObj) of
        'undefined' -> kz_json:get_value([<<"Call">>, <<"Call-ID">>], JObj);
        CallId -> CallId
    end.

-spec hangup_cause(kz_json:object()) -> ne_binary().
hangup_cause(JObj) ->
    case kz_json:get_value(<<"Hangup-Cause">>, JObj) of
        'undefined' -> <<"unknown">>;
        Cause -> Cause
    end.

%% returns time left in seconds
-spec time_left(reference() | 'false' | api_integer()) -> api_integer().
time_left(Ref) when is_reference(Ref) ->
    time_left(erlang:read_timer(Ref));
time_left('false') -> 'undefined';
time_left('undefined') -> 'undefined';
time_left(Ms) when is_integer(Ms) -> Ms div 1000.

-spec clear_call(state(), atom()) -> state().
clear_call(#state{connect_failures=Fails
                 ,max_connect_failures=Max
                 ,account_id=AccountId
                 ,agent_id=AgentId
                 }=State, 'failed') when is_integer(Max), (Max - Fails) =< 1 ->
    acdc_agent_stats:agent_logged_out(AccountId, AgentId),
    agent_logout(self()),
    lager:debug("agent has failed to connect ~b times, logging out", [Fails+1]),
    clear_call(State#state{connect_failures=Fails+1}, 'paused');
clear_call(#state{connect_failures=Fails
                 ,max_connect_failures=_MaxFails
                 }=State, 'failed') ->
    lager:debug("agent has failed to connect ~b times(~b)", [Fails+1, _MaxFails]),
    clear_call(State#state{connect_failures=Fails+1}, 'ready');
clear_call(#state{fsm_call_id=FSMemberCallId
                 ,wrapup_ref=WRef
                 ,pause_ref=PRef
                 }=State, NextState)->
    kz_util:put_callid(FSMemberCallId),

    ReadyForAction = NextState =/= 'wrapup'
        andalso NextState =/= 'paused',
    lager:debug("ready for action: ~s: ~s", [NextState, ReadyForAction]),

    _ = maybe_stop_timer(WRef, ReadyForAction),
    _ = maybe_stop_timer(PRef, ReadyForAction),

    State#state{wrapup_timeout = 0
               ,wrapup_ref = case ReadyForAction of 'true' -> 'undefined'; 'false' -> WRef end
               ,pause_ref = case ReadyForAction of 'true' -> 'undefined'; 'false' -> PRef end
               ,member_call = 'undefined'
               ,member_call_id = 'undefined'
               ,member_call_start = 'undefined'
               ,member_call_queue_id = 'undefined'
               ,agent_call_id = 'undefined'
               ,caller_exit_key = <<"#">>
               }.

-spec current_call(kapps_call:call() | 'undefined', atom(), ne_binary(), 'undefined' | kz_now()) ->
                          api_object().
current_call('undefined', _, _, _) -> 'undefined';
current_call(Call, AgentState, QueueId, Start) ->
    kz_json:from_list([{<<"call_id">>, kapps_call:call_id(Call)}
                      ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
                      ,{<<"caller_id_number">>, kapps_call:caller_id_name(Call)}
                      ,{<<"to">>, kapps_call:to_user(Call)}
                      ,{<<"from">>, kapps_call:from_user(Call)}
                      ,{<<"agent_state">>, kz_term:to_binary(AgentState)}
                      ,{<<"duration">>, elapsed(Start)}
                      ,{<<"queue_id">>, QueueId}
                      ]).

-spec elapsed('undefined' | kz_now()) -> api_integer().
elapsed('undefined') -> 'undefined';
elapsed(Start) -> kz_time:elapsed_s(Start).

-spec wrapup_timer(state()) -> reference().
wrapup_timer(#state{agent_listener=AgentListener
                   ,wrapup_timeout = WrapupTimeout
                   ,account_id = AccountId
                   ,agent_id = AgentId
                   ,member_call_id = CallId
                   ,member_call_start=_Started
                   }) ->
    acdc_agent_listener:unbind_from_events(AgentListener, CallId),
    lager:info("call lasted ~b s", [elapsed(_Started)]),
    lager:info("going into a wrapup period ~p: ~s", [WrapupTimeout, CallId]),
    acdc_agent_stats:agent_wrapup(AccountId, AgentId, WrapupTimeout),
    start_wrapup_timer(WrapupTimeout).

-spec hangup_call(state(), 'member' | 'agent') -> reference().
hangup_call(#state{agent_listener=AgentListener
                  ,member_call_id=CallId
                  ,member_call_queue_id=QueueId
                  ,account_id=AccountId
                  ,agent_id=AgentId
                  ,queue_notifications=Ns
                  }=State, Initiator) ->
    acdc_stats:call_processed(AccountId, QueueId, AgentId, CallId, Initiator),

    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    wrapup_timer(State).

-spec maybe_stop_timer(api_reference()) -> 'ok'.
-spec maybe_stop_timer(api_reference(), boolean()) -> 'ok'.
maybe_stop_timer('undefined') -> 'ok';
maybe_stop_timer(ConnRef) when is_reference(ConnRef) ->
    _ = gen_fsm:cancel_timer(ConnRef),
    'ok'.

maybe_stop_timer(TimerRef, 'true') -> maybe_stop_timer(TimerRef);
maybe_stop_timer(_, 'false') -> 'ok'.

-spec start_outbound_call_handling(ne_binary() | kapps_call:call(), state()) -> state().
start_outbound_call_handling(CallId, #state{agent_listener=AgentListener
                                           ,account_id=AccountId
                                           ,agent_id=AgentId
                                           ,outbound_call_ids=OutboundCallIds
                                           }=State) when is_binary(CallId) ->
    kz_util:put_callid(CallId),
    lager:debug("agent making outbound call, not receiving ACDc calls"),
    acdc_agent_listener:outbound_call(AgentListener, CallId),
    acdc_agent_stats:agent_outbound(AccountId, AgentId, CallId),
    State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]};
start_outbound_call_handling(Call, State) ->
    start_outbound_call_handling(kapps_call:call_id(Call), State).

-spec outbound_hungup(state()) -> handle_fsm_ret(state()).
outbound_hungup(#state{agent_listener=AgentListener
                      ,wrapup_ref=WRef
                      ,pause_ref=PRef
                      ,outbound_call_ids=[]
                      }=State) ->
    case time_left(WRef) of
        N when is_integer(N), N > 0 -> apply_state_updates(clear_call(State, 'wrapup'));
        _W ->
            case time_left(PRef) of
                N when is_integer(N), N > 0 -> apply_state_updates(clear_call(State, 'paused'));
                _P ->
                    lager:debug("wrapup left: ~p pause left: ~p", [_W, _P]),
                    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
                    apply_state_updates(clear_call(State, 'ready'))
            end
    end;
outbound_hungup(State) ->
    lager:debug("agent still has some outbound calls active"),
    {'next_state', 'outbound', State}.

-spec missed_reason(ne_binary()) -> ne_binary().
missed_reason(<<"-ERR ", Reason/binary>>) ->
    missed_reason(binary:replace(Reason, <<"\n">>, <<>>, ['global']));
missed_reason(<<"ALLOTTED_TIMEOUT">>) -> <<"timeout">>;
missed_reason(<<"NO_USER_RESPONSE">>) -> <<"rejected">>;
missed_reason(<<"CALL_REJECTED">>) -> <<"rejected">>;
missed_reason(<<"USER_BUSY">>) -> <<"rejected">>;
missed_reason(Reason) -> Reason.

-spec find_username(kz_json:object()) -> api_binary().
find_username(EP) ->
    find_sip_username(EP, kz_device:sip_username(EP)).

-spec find_sip_username(kz_json:object(), api_binary()) -> api_binary().
find_sip_username(EP, 'undefined') -> kz_json:get_value(<<"To-User">>, EP);
find_sip_username(_EP, Username) -> Username.

-spec find_endpoint_id(kz_json:object()) -> api_binary().
-spec find_endpoint_id(kz_json:object(), api_binary()) -> api_binary().

find_endpoint_id(EP) ->
    find_endpoint_id(EP, kz_doc:id(EP)).

find_endpoint_id(EP, 'undefined') -> kz_json:get_value(<<"Endpoint-ID">>, EP);
find_endpoint_id(_EP, EPId) -> EPId.

-spec monitor_endpoint(kz_json:object(), ne_binary(), server_ref()) -> any().
monitor_endpoint(EP, AccountId, AgentListener) ->
    %% Bind for outbound call requests
    acdc_agent_listener:add_endpoint_bindings(AgentListener
                                             ,kz_endpoint:get_sip_realm(EP, AccountId)
                                             ,find_username(EP)
                                             ),
    %% Inform us of device changes
    catch gproc:reg(?ENDPOINT_UPDATE_REG(AccountId, find_endpoint_id(EP))),
    catch gproc:reg(?NEW_CHANNEL_REG(AccountId, find_username(EP))).

-spec unmonitor_endpoint(kz_json:object(), ne_binary(), server_ref()) -> any().
unmonitor_endpoint(EP, AccountId, AgentListener) ->
    %% Bind for outbound call requests
    acdc_agent_listener:remove_endpoint_bindings(AgentListener
                                                ,kz_endpoint:get_sip_realm(EP, AccountId)
                                                ,find_username(EP)
                                                ),
    %% Inform us of device changes
    catch gproc:unreg(?ENDPOINT_UPDATE_REG(AccountId, kz_doc:id(EP))),
    catch gproc:unreg(?NEW_CHANNEL_REG(AccountId, find_username(EP))).

-spec maybe_add_endpoint(ne_binary(), kz_json:object(), kz_json:objects(), ne_binary(), server_ref()) -> any().
maybe_add_endpoint(EPId, EP, EPs, AccountId, AgentListener) ->
    case lists:partition(fun(E) -> kz_doc:id(E) =:= EPId end, EPs) of
        {[], _} ->
            lager:debug("endpoint ~s not in our list, adding it", [EPId]),
            [begin monitor_endpoint(EP, AccountId, AgentListener), EP end | EPs];
        {_, _} -> EPs
    end.

-spec maybe_remove_endpoint(ne_binary(), kz_json:objects(), ne_binary(), server_ref()) -> kz_json:objects().
maybe_remove_endpoint(EPId, EPs, AccountId, AgentListener) ->
    case lists:partition(fun(EP) -> kz_doc:id(EP) =:= EPId end, EPs) of
        {[], _} -> EPs; %% unknown endpoint
        {[RemoveEP], EPs1} ->
            lager:debug("endpoint ~s in our list, removing it", [EPId]),
            _ = unmonitor_endpoint(RemoveEP, AccountId, AgentListener),
            EPs1
    end.

-spec get_endpoints(kz_json:objects(), server_ref(), kapps_call:call(), api_binary(), api_binary()) ->
                           {'ok', kz_json:objects()} |
                           {'error', any()}.
get_endpoints(OrigEPs, AgentListener, Call, AgentId, QueueId) ->
    case catch acdc_util:get_endpoints(Call, AgentId) of
        [] ->
            {'error', 'no_endpoints'};
        [_|_]=EPs ->
            AccountId = kapps_call:account_id(Call),

            {Add, Rm} = changed_endpoints(OrigEPs, EPs),
            _ = [monitor_endpoint(EP, AccountId, AgentListener) || EP <- Add],
            _ = [unmonitor_endpoint(EP, AccountId, AgentListener) || EP <- Rm],

            {'ok', [kz_json:set_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], QueueId, EP) || EP <- EPs]};
        {'EXIT', E} ->
            lager:debug("failed to load endpoints: ~p", [E]),
            {'error', E}
    end.

-spec return_to_state(non_neg_integer(), pos_integer()) -> 'paused' | 'ready'.
return_to_state(Fails, MaxFails) ->
    lager:debug("fails ~b max ~b going to pause", [Fails, MaxFails]),
    case is_integer(MaxFails)
        andalso Fails >= MaxFails of
        'true' -> 'paused';
        'false' -> 'ready'
    end.

%% {Add, Rm}
%% Orig [] Curr [] => {[], []}
%% Orig [X] Curr [X] => {[], []}
%% Orig [X] Curr [Y] => {[Y], [X]}
%% Orig [X, Y] Curr [Y] => {[], [X]}
%% Orig [X] Curr [X, Y] => {[Y], []}
changed_endpoints([], EPs) -> {EPs, []};
changed_endpoints(OrigEPs, EPs) ->
    changed_endpoints(OrigEPs, EPs, []).

changed_endpoints([], [], Add) -> {Add, []};
changed_endpoints(OrigEPs, [], Add) -> {Add, OrigEPs};
changed_endpoints(OrigEPs, [EP|EPs], Add) ->
    EPId = find_endpoint_id(EP),
    case lists:partition(fun(OEP) ->
                                 find_endpoint_id(OEP) =:= EPId
                         end, OrigEPs)
    of
        {[], _} -> changed_endpoints(OrigEPs, EPs, [EP|Add]);
        {_, RestOrigEPs} -> changed_endpoints(RestOrigEPs, EPs, Add)
    end.

maybe_notify('undefined', _, _) -> 'ok';
maybe_notify(Ns, Key, State) ->
    case kz_json:get_value(Key, Ns) of
        'undefined' ->
            case kz_json:get_value(?NOTIFY_ALL, Ns) of
                'undefined' -> 'ok';
                Url ->
                    lager:debug("send update for ~s to ~s", [?NOTIFY_ALL, Url]),
                    _ = kz_util:spawn(fun notify/4, [Url, get_method(Ns), Key, State]),
                    'ok'
            end;
        Url ->
            lager:debug("send update for ~s to ~s", [Key, Url]),
            _ = kz_util:spawn(fun notify/4, [Url, get_method(Ns), Key, State]),
            'ok'
    end.

-spec get_method(kz_json:object()) -> 'get' | 'post'.
get_method(Ns) ->
    case kz_json:get_value(<<"method">>, Ns) of
        'undefined' -> 'get';
        M -> standardize_method(kz_term:to_lower_binary(M))
    end.

-spec standardize_method(ne_binary()) -> 'get' | 'post'.
standardize_method(<<"post">>) -> 'post';
standardize_method(_) -> 'get'.

-spec notify(ne_binary(), 'get' | 'post', ne_binary(), state()) -> 'ok'.
notify(Url, Method, Key, #state{account_id=AccountId
                               ,agent_id=AgentId
                               ,member_call=MemberCall
                               ,agent_call_id=AgentCallId
                               ,member_call_queue_id=QueueId
                               }) ->
    kz_util:put_callid(kapps_call:call_id(MemberCall)),
    Data = kz_json:from_list(
             props:filter_undefined(
               [{<<"account_id">>, AccountId}
               ,{<<"agent_id">>, AgentId}
               ,{<<"agent_call_id">>, AgentCallId}
               ,{<<"queue_id">>, QueueId}
               ,{<<"member_call_id">>, kapps_call:call_id(MemberCall)}
               ,{<<"caller_id_name">>, kapps_call:caller_id_name(MemberCall)}
               ,{<<"caller_id_number">>, kapps_call:caller_id_number(MemberCall)}
               ,{<<"call_state">>, Key}
               ,{<<"now">>, kz_time:current_tstamp()}
               ])),
    notify(Url, Method, Data).

-spec notify(ne_binary(), 'get' | 'post', kz_json:object()) -> 'ok'.
notify(Url, 'post', Data) ->
    notify(Url, [], 'post', kz_json:encode(Data)
          ,[{'content_type', "application/json"}]
          );
notify(Url, 'get', Data) ->
    notify(uri(Url, kz_http_util:json_to_querystring(Data))
          ,[], 'get', <<>>, []
          ).

-spec notify(iolist(), [], 'get' | 'post', binary(), kz_proplist()) -> 'ok'.
notify(Uri, Headers, Method, Body, Opts) ->
    Options = [{'connect_timeout', 200}
              ,{'timeout', 1000}
               | Opts
              ],
    URI = kz_term:to_list(Uri),
    case kz_http:req(Method, URI, Headers, Body, Options) of
        {'ok', _Status, _ResponseHeaders, _ResponseBody} ->
            lager:debug("~s req to ~s: ~p", [Method, Uri, _Status]);
        {'error', _E} ->
            lager:debug("failed to send request to ~s: ~p", [Uri, _E])
    end.

-spec cdr_url(kz_json:object()) -> api_binary().
cdr_url(JObj) ->
    case kz_json:get_value([<<"Notifications">>, ?NOTIFY_CDR], JObj) of
        'undefined' -> kz_json:get_ne_value(<<"CDR-Url">>, JObj);
        Url -> Url
    end.

-spec recording_url(kz_json:object()) -> api_binary().
recording_url(JObj) ->
    case kz_json:get_value([<<"Notifications">>, ?NOTIFY_RECORDING], JObj) of
        'undefined' -> kz_json:get_ne_value(<<"Recording-URL">>, JObj);
        Url -> Url
    end.

-spec uri(ne_binary(), iolist()) -> iolist().
uri(URI, QueryString) ->
    case kz_http_util:urlsplit(URI) of
        {Scheme, Host, Path, <<>>, Fragment} ->
            kz_http_util:urlunsplit({Scheme, Host, Path, QueryString, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            kz_http_util:urlunsplit({Scheme, Host, Path, <<QS/binary, "&", (kz_term:to_binary(QueryString))/binary>>, Fragment})
    end.

-spec apply_state_updates(state()) -> handle_fsm_ret(state()).
apply_state_updates(#state{agent_state_updates=Q
                          ,wrapup_ref=WRef
                          ,pause_ref=PRef
                          }=State) ->
    FoldDefaultState = case time_left(WRef) of
                           N when is_integer(N), N > 0 -> 'wrapup';
                           _W ->
                               case time_left(PRef) of
                                   N when is_integer(N), N > 0 -> 'paused';
                                   _P -> 'ready'
                               end
                       end,
    lager:debug("default state for applying state updates ~s", [FoldDefaultState]),
    apply_state_updates_fold({'next_state', FoldDefaultState, State#state{agent_state_updates=[]}}, lists:reverse(Q)).

-spec apply_state_updates_fold({'next_state', atom(), state()}, list()) -> handle_fsm_ret(state()).
apply_state_updates_fold({_, StateName, #state{account_id=AccountId
                                              ,agent_id=AgentId
                                              ,agent_listener=AgentListener
                                              ,wrapup_ref=WRef
                                              ,pause_ref=PRef
                                              }}=Acc, []) ->
    lager:debug("resulting agent state ~s", [StateName]),
    case StateName of
        'ready' ->
            acdc_agent_listener:send_agent_available(AgentListener),
            acdc_agent_stats:agent_ready(AccountId, AgentId);
        'wrapup' -> acdc_agent_stats:agent_wrapup(AccountId, AgentId, time_left(WRef));
        'paused' ->
            acdc_agent_listener:send_agent_busy(AgentListener),
            acdc_agent_stats:agent_paused(AccountId, AgentId, time_left(PRef))
    end,
    Acc;
apply_state_updates_fold({_, _, State}, [{'pause', Timeout}|Updates]) ->
    apply_state_updates_fold(handle_pause(Timeout, State), Updates);
apply_state_updates_fold({_, _, State}, [{'resume'}|Updates]) ->
    apply_state_updates_fold(handle_resume(State), Updates);
apply_state_updates_fold({_, _, State}, [{'agent_logout'}|_]) ->
    lager:debug("agent logging out"),
    %% Do not continue fold, stop FSM
    handle_agent_logout(State);
apply_state_updates_fold({_, _, State}=Acc, [{'update_presence', PresenceId, PresenceState}|Updates]) ->
    handle_presence_update(PresenceId, PresenceState, State),
    apply_state_updates_fold(Acc, Updates).

-spec valid_state_for_logout(atom()) -> boolean().
valid_state_for_logout('ready') -> 'true';
valid_state_for_logout('wrapup') -> 'true';
valid_state_for_logout('paused') -> 'true';
valid_state_for_logout(_) -> 'false'.

-spec handle_agent_logout(state()) -> handle_fsm_ret(state()).
handle_agent_logout(#state{account_id = AccountId
                          ,agent_id = AgentId
                          }=State) ->
    acdc_agent_stats:agent_logged_out(AccountId, AgentId),
    {'stop', 'normal', State}.

-spec handle_presence_update(ne_binary(), ne_binary(), state()) -> 'ok'.
handle_presence_update(PresenceId, PresenceState, #state{agent_id = AgentId
                                                        ,account_id = AccountId
                                                        }) ->
    Super = acdc_agents_sup:find_agent_supervisor(AccountId, AgentId),
    Listener = acdc_agent_sup:listener(Super),
    acdc_agent_listener:maybe_update_presence_id(Listener, PresenceId),
    acdc_agent_listener:presence_update(Listener, PresenceState).

-spec handle_resume(state()) -> handle_fsm_ret(state()).
handle_resume(#state{agent_listener=AgentListener
                    ,pause_ref=Ref
                    }=State) ->
    lager:debug("resume received, putting agent back into action"),
    maybe_stop_timer(Ref),

    acdc_agent_listener:update_agent_status(AgentListener, <<"resume">>),

    acdc_agent_listener:send_status_resume(AgentListener),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    {'next_state', 'ready', State#state{pause_ref='undefined'}}.

-spec handle_pause(integer(), state()) -> handle_fsm_ret(state()).
handle_pause(Timeout, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_FLASH),
    Ref = start_pause_timer(Timeout),
    State1 = State#state{pause_ref=Ref},
    {'next_state', 'paused', State1}.
