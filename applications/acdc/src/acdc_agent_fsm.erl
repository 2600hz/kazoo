%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Tracks the agent's state, responds to messages from the corresponding
%%% acdc_agent gen_listener process.
%%%
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/2, start_link/3, start_link/4, start_link/5
        ,call_event/4
        ,member_connect_req/2
        ,member_connect_win/3
        ,member_connect_satisfied/3
        ,agent_timeout/2
        ,shared_failure/2
        ,shared_call_id/2
        ,originate_ready/2
        ,originate_resp/2, originate_started/2, originate_uuid/2
        ,originate_failed/2
        ,sync_req/2, sync_resp/2
        ,pause/3
        ,resume/1
        ,end_wrapup/1

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
        ,ringing_callback/2
        ,awaiting_callback/2
        ,answered/2
        ,wrapup/2
        ,paused/2
        ,outbound/2
        ,inbound/2

        ,wait/3
        ,sync/3
        ,ready/3
        ,ringing/3
        ,ringing_callback/3
        ,awaiting_callback/3
        ,answered/3
        ,wrapup/3
        ,paused/3
        ,outbound/3
        ,inbound/3
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

-record(state, {account_id :: kz_term:ne_binary()
               ,account_db :: kz_term:ne_binary()
               ,agent_id :: kz_term:ne_binary()
               ,agent_listener :: kz_term:server_ref()
               ,agent_listener_id :: api_kz_term:ne_binary()
               ,agent_name :: kz_term:api_binary()

               ,wrapup_timeout = 0 :: integer() % optionally set on win
               ,wrapup_ref :: kz_term:api_reference()

               ,sync_ref :: kz_term:api_reference()
               ,pause_ref :: kz_term:api_reference() | 'infinity'
               ,pause_alias :: kz_term:api_binary()

               ,member_call :: kapps_call:call()
               ,member_call_id :: kz_term:api_binary()
               ,member_callback_candidates = [] :: kz_term:proplist()
               ,member_original_call :: kapps_call:call()
               ,member_original_call_id :: kz_term:api_binary()
               ,member_call_queue_id :: kz_term:api_binary()
               ,member_call_start :: kz_term:kz_now() | undefined
               ,caller_exit_key = <<"#">> :: kz_term:ne_binary()
               ,queue_notifications :: kz_term:api_object()

               ,agent_call_id :: kz_term:api_binary()
               ,agent_callback_call = 'undefined' :: kapps_call:call() | 'undefined'
               ,next_status :: kz_term:api_binary()
               ,fsm_call_id :: kz_term:api_binary() % used when no call-ids are available
               ,endpoints = [] :: kz_json:objects()
               ,outbound_call_ids = [] :: kz_term:ne_binaries()
               ,inbound_call_ids = [] :: kz_term:ne_binaries()
               ,max_connect_failures :: kz_term:timeout() | 'infinity'
               ,connect_failures = 0 :: non_neg_integer()
               ,agent_state_updates = [] :: list()
               ,monitoring = 'false' :: boolean()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================
%%------------------------------------------------------------------------------
%% @doc When a queue receives a call and needs an agent, it will send a
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%------------------------------------------------------------------------------
-spec member_connect_req(pid(), kz_json:object()) -> 'ok'.
member_connect_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'member_connect_req', JObj}).

%%------------------------------------------------------------------------------
%% @doc When a queue receives a call and needs an agent, it will send a
%%   member_connect_req. The agent will respond (if possible) with a
%%   member_connect_resp payload or ignore the request
%% @end
%%------------------------------------------------------------------------------
-spec member_connect_win(pid(), kz_json:object(), 'same_node' | 'different_node') -> 'ok'.
member_connect_win(FSM, JObj, Node) ->
    gen_fsm:send_event(FSM, {'member_connect_win', JObj, Node}).

-spec member_connect_satisfied(pid(), kz_json:object(), 'same_node'|'different_node') -> 'ok'.
member_connect_satisfied(FSM, JObj, Node) ->
    gen_fsm:send_event(FSM, {'member_connect_satisfied', JObj, Node}).

-spec agent_timeout(pid(), kz_json:object()) -> 'ok'.
agent_timeout(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'agent_timeout', JObj}).

-spec shared_failure(pid(), kz_json:object()) -> 'ok'.
shared_failure(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'shared_failure', JObj}).

-spec shared_call_id(pid(), kz_json:object()) -> 'ok'.
shared_call_id(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'shared_call_id', JObj}).

%%------------------------------------------------------------------------------
%% @doc When an agent is involved in a call, it will receive call events.
%%   Pass the call event to the FSM to see if action is needed (usually
%%   for bridge and hangup events).
%% @end
%%------------------------------------------------------------------------------
-spec call_event(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
call_event(FSM, <<"call_event">>, <<"CHANNEL_BRIDGE">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_bridged', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_UNBRIDGE">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_unbridged', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"usurp_control">>, JObj) ->
    gen_fsm:send_event(FSM, {'usurp_control', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_DESTROY">>, JObj) ->
    gen_fsm:send_event(FSM, ?DESTROYED_CHANNEL(call_id(JObj), acdc_util:hangup_cause(JObj)));
call_event(FSM, <<"call_event">>, <<"CHANNEL_DISCONNECTED">>, JObj) ->
    gen_fsm:send_event(FSM, ?DESTROYED_CHANNEL(call_id(JObj), <<"MEDIA_SERVER_UNREACHABLE">>));
call_event(FSM, <<"call_event">>, <<"LEG_CREATED">>, JObj) ->
    %% Due to change in kapi_call to send events based on Origination-Call-ID,
    %% we do not want to bind to any of the loopback legs
    case kz_json:get_ne_binary_value(<<"Channel-Loopback-Leg">>, JObj) of
        'undefined' ->
            gen_fsm:send_event(FSM, {'leg_created'
                                    ,call_id(JObj)
                                    ,kz_call_event:other_leg_call_id(JObj)
                                    });
        _ -> 'ok'
    end;
call_event(FSM, <<"call_event">>, <<"LEG_DESTROYED">>, JObj) ->
    gen_fsm:send_event(FSM, {'leg_destroyed', call_id(JObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_ANSWER">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_answered', JObj});
call_event(FSM, <<"call_event">>, <<"DTMF">>, EvtJObj) ->
    gen_fsm:send_event(FSM, {'dtmf_pressed', kz_json:get_value(<<"DTMF-Digit">>, EvtJObj)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, JObj) ->
    maybe_send_execute_complete(FSM, kz_json:get_value(<<"Application-Name">>, JObj), JObj);
call_event(FSM, <<"error">>, <<"dialplan">>, JObj) ->
    _ = kz_log:put_callid(JObj),
    lager:debug("error event: ~s", [kz_json:get_value(<<"Error-Message">>, JObj)]),

    Req = kz_json:get_value(<<"Request">>, JObj),

    gen_fsm:send_event(FSM, {'dialplan_error', kz_json:get_value(<<"Application-Name">>, Req)});
call_event(FSM, <<"call_event">>, <<"CHANNEL_REPLACED">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_replaced', JObj});
call_event(FSM, <<"call_event">>, <<"CHANNEL_TRANSFEREE">>, JObj) ->
    Transferor = kz_call_event:other_leg_call_id(JObj),
    Transferee = kz_call_event:call_id(JObj),
    gen_fsm:send_event(FSM, {'channel_transferee', Transferor, Transferee});
call_event(FSM, <<"call_event">>, <<"PLAYBACK_STOP">>, JObj) ->
    gen_fsm:send_event(FSM, {'playback_stop', JObj});
call_event(_FSM, <<"call_event">>, <<"CALL_UPDATE">>, _JObj) ->
                                                % lager:debug("unhandled CALL_UPDATE: ~p", [JObj]);
    'ok';
call_event(_, _C, _E, _) ->
    lager:info("unhandled combo: ~s/~s", [_C, _E]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_execute_complete(pid(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_send_execute_complete(FSM, <<"bridge">>, JObj) ->
    lager:info("send EXECUTE_COMPLETE,bridge to ~p with ci: ~s, olci: ~s",
               [FSM
               ,call_id(JObj)
               ,kz_call_event:other_leg_call_id(JObj)
               ]),
    gen_fsm:send_event(FSM, {'channel_unbridged', call_id(JObj)});
maybe_send_execute_complete(FSM, <<"call_pickup">>, JObj) ->
    gen_fsm:send_event(FSM, {'channel_bridged', call_id(JObj)});
maybe_send_execute_complete(_, _, _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec originate_ready(kz_term:server_ref(), kz_json:object()) -> 'ok'.
originate_ready(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_ready', JObj}).

-spec originate_resp(kz_term:server_ref(), kz_json:object()) -> 'ok'.
originate_resp(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_resp', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec originate_started(kz_term:server_ref(), kz_json:object()) -> 'ok'.
originate_started(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_started', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec originate_uuid(kz_term:server_ref(), kz_json:object()) -> 'ok'.
originate_uuid(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_uuid'
                            ,kz_json:get_value(<<"Outbound-Call-ID">>, JObj)
                            ,kz_json:get_value(<<"Outbound-Call-Control-Queue">>, JObj)
                            }).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec originate_failed(kz_term:server_ref(), kz_json:object()) -> 'ok'.
originate_failed(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'originate_failed', JObj}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync_req(kz_term:server_ref(), kz_json:object()) -> 'ok'.
sync_req(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'sync_req', JObj}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync_resp(kz_term:server_ref(), kz_json:object()) -> 'ok'.
sync_resp(FSM, JObj) ->
    gen_fsm:send_event(FSM, {'sync_resp', JObj}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pause(kz_term:server_ref(), kz_term:timeout(), kz_term:api_binary()) -> 'ok'.
pause(FSM, Timeout, Alias) ->
    gen_fsm:send_all_state_event(FSM, {'pause', Timeout, Alias}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resume(kz_term:server_ref()) -> 'ok'.
resume(FSM) ->
    gen_fsm:send_all_state_event(FSM, {'resume'}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec end_wrapup(kz_term:server_ref()) -> 'ok'.
end_wrapup(FSM) ->
    gen_fsm:send_all_state_event(FSM, {'end_wrapup'}).

%%------------------------------------------------------------------------------
%% @doc Request the agent listener bind to queue and conditionally send an
%% availability update depending on agent state
%% @end
%%------------------------------------------------------------------------------
-spec add_acdc_queue(kz_term:server_ref(), kz_term:ne_binary()) -> 'ok'.
add_acdc_queue(FSM, QueueId) ->
    gen_fsm:send_all_state_event(FSM, {'add_acdc_queue', QueueId}).

%%------------------------------------------------------------------------------
%% @doc Request the agent listener unbind from queue and send an
%% unavailability update
%% @end
%%------------------------------------------------------------------------------
-spec rm_acdc_queue(kz_term:server_ref(), kz_term:ne_binary()) -> 'ok'.
rm_acdc_queue(FSM, QueueId) ->
    gen_fsm:send_all_state_event(FSM, {'rm_acdc_queue', QueueId}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_presence(kz_term:server_ref(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_presence(FSM, PresenceId, PresenceState) ->
    gen_fsm:send_all_state_event(FSM, {'update_presence', PresenceId, PresenceState}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec agent_logout(kz_term:server_ref()) -> 'ok'.
agent_logout(FSM) ->
    gen_fsm:send_all_state_event(FSM, {'agent_logout'}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(FSM, AgentJObj) -> gen_fsm:send_all_state_event(FSM, {'refresh', AgentJObj}).

-spec current_call(pid()) -> kz_term:api_object().
current_call(FSM) -> gen_fsm:sync_send_event(FSM, 'current_call').

-spec status(pid()) -> kz_term:proplist().
status(FSM) -> gen_fsm:sync_send_event(FSM, 'status').

%%------------------------------------------------------------------------------
%% @doc Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% @end
%%------------------------------------------------------------------------------

-spec start_link(pid(), kz_json:object()) -> kz_term:startlink_ret().
start_link(Supervisor, AgentJObj) when is_pid(Supervisor) ->
    pvt_start_link(kz_doc:account_id(AgentJObj)
                  ,kz_doc:id(AgentJObj)
                  ,Supervisor
                  ,[]
                  ,'false'
                  ).

-spec start_link(pid(), kapps_call:call(), kz_term:ne_binary()) -> kz_term:startlink_ret().
start_link(Supervisor, ThiefCall, _QueueId) ->
    pvt_start_link(kapps_call:account_id(ThiefCall)
                  ,kapps_call:owner_id(ThiefCall)
                  ,Supervisor
                  ,[]
                  ,'true'
                  ).

-spec start_link(kz_term:ne_binary(), kz_term:ne_binary(), pid(), kz_term:proplist()) -> kz_term:startlink_ret().
start_link(AccountId, AgentId, Supervisor, Props) ->
    pvt_start_link(AccountId, AgentId, Supervisor, Props, 'false').

-spec start_link(pid(), any(), kz_term:ne_binary(), kz_term:ne_binary(), any()) -> kz_term:startlink_ret().
start_link(Supervisor, _AgentJObj, AccountId, AgentId, _Queues) ->
    pvt_start_link(AccountId, AgentId, Supervisor, [], 'false').

pvt_start_link('undefined', _AgentId, Supervisor, _, _) ->
    lager:debug("agent ~s trying to start with no account id", [_AgentId]),
    _ = kz_process:spawn(fun acdc_agent_sup:stop/1, [Supervisor]),
    'ignore';
pvt_start_link(_AccountId, 'undefined', Supervisor, _, _) ->
    lager:debug("undefined agent id trying to start in account ~s", [_AccountId]),
    _ = kz_process:spawn(fun acdc_agent_sup:stop/1, [Supervisor]),
    'ignore';
pvt_start_link(AccountId, AgentId, Supervisor, Props, IsThief) ->
    gen_fsm:start_link(?SERVER, [AccountId, AgentId, Supervisor, Props, IsThief], []).

-spec new_endpoint(pid(), kz_json:object()) -> 'ok'.
new_endpoint(FSM, EP) ->
    lager:debug("sending EP to ~p: ~p", [FSM, EP]).

-spec edited_endpoint(pid(), kz_json:object()) -> 'ok'.
edited_endpoint(FSM, EP) ->
    lager:debug("sending EP to ~p: ~p", [FSM, EP]),
    gen_fsm:send_all_state_event(FSM, {'edited_endpoint', kz_doc:id(EP), EP}).

-spec deleted_endpoint(pid(), kz_json:object()) -> 'ok'.
deleted_endpoint(FSM, EP) ->
    lager:debug("sending EP to ~p: ~p", [FSM, EP]).

%%%=============================================================================
%%% gen_fsm callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', atom(), state()}.
init([AccountId, AgentId, Supervisor, Props, IsThief]) ->
    FSMCallId = <<"fsm_", AccountId/binary, "_", AgentId/binary>>,
    kz_log:put_callid(FSMCallId),
    lager:debug("started acdc agent fsm"),

    _P = kz_process:spawn(fun wait_for_listener/4, [Supervisor, self(), Props, IsThief]),
    lager:debug("waiting for listener in ~p", [_P]),

    AccountDb = kzs_util:format_account_db(AccountId),
    {'ok', UserDoc} = kz_datamgr:open_cache_doc(AccountDb, AgentId),

    {'ok'
    ,'wait'
    ,#state{account_id = AccountId
           ,account_db = AccountDb
           ,agent_id = AgentId
           ,agent_name = kz_json:get_value(<<"username">>, UserDoc)
           ,fsm_call_id = FSMCallId
           ,max_connect_failures = max_failures(AccountId)
           }
    }.

-spec max_failures(kz_term:ne_binary() | kz_json:object()) -> non_neg_integer() | 'infinity'.
max_failures(Account) when is_binary(Account) ->
    case kzd_accounts:fetch(Account) of
        {'ok', AccountJObj} -> max_failures(AccountJObj);
        {'error', _} -> ?MAX_FAILURES
    end;
max_failures(0) ->
    'infinity';
max_failures(V) when is_integer(V), V > 0 ->
    V;
max_failures(JObj) ->
    max_failures(kz_json:get_integer_value(?MAX_CONNECT_FAILURES, JObj, ?MAX_FAILURES)).

-spec wait_for_listener(pid(), pid(), kz_term:proplist(), boolean()) -> 'ok'.
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec wait(any(), state()) -> kz_term:handle_fsm_ret(state()).
wait({'listener', AgentListener, NextState, SyncRef}, State) ->
    lager:debug("setting agent proc to ~p", [AgentListener]),
    acdc_agent_listener:fsm_started(AgentListener, self()),
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

-spec wait(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
wait('status', _, State) ->
    {'reply', [{'state', <<"wait">>}], 'wait', State};
wait('current_call', _, State) ->
    {'reply', 'undefined', 'wait', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sync(any(), state()) -> kz_term:handle_fsm_ret(state()).
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

sync(?NEW_CHANNEL_FROM(CallId, Number, Name, _), State) ->
    lager:debug("sync call_from inbound: ~s", [CallId]),
    {'next_state', 'inbound', start_inbound_call_handling(CallId, Number, Name, State), 'hibernate'};
sync(?NEW_CHANNEL_TO(CallId, Number, Name), State) ->
    lager:debug("sync call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, Number, Name, State), 'hibernate'};
sync(_Evt, State) ->
    lager:debug("unhandled event while syncing: ~p", [_Evt]),
    {'next_state', 'sync', State}.

-spec sync(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
sync('status', _, State) ->
    {'reply', [{'state', <<"sync">>}], 'sync', State};
sync('current_call', _, State) ->
    {'reply', 'undefined', 'sync', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ready(any(), state()) -> kz_term:handle_fsm_ret(state()).
ready({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'ready', JObj),
    {'next_state', 'ready', State};
ready({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener
                                                       ,endpoints=OrigEPs
                                                       ,account_id=AccountId
                                                       ,agent_id=AgentId
                                                       ,connect_failures=CF
                                                       }=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    CallId = kapps_call:call_id(Call),

    kz_log:put_callid(CallId),

    WrapupTimer = kz_json:get_integer_value(<<"Wrapup-Timeout">>, JObj, 0),
    CallerExitKey = kz_json:get_value(<<"Caller-Exit-Key">>, JObj, <<"#">>),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    CDRUrl = cdr_url(JObj),
    RecordingUrl = recording_url(JObj),

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
            acdc_util:bind_to_call_events(Call, AgentListener),

            %% Need to check if a callback is required to the caller
            NextState = case kz_json:get_value(<<"Callback-Details">>, JObj) of
                            'undefined' ->
                                acdc_agent_listener:bridge_to_member(AgentListener, Call, JObj, UpdatedEPs, CDRUrl, RecordingUrl),
                                'ringing';
                            Details ->
                                acdc_agent_listener:originate_callback_to_agent(AgentListener, Call, JObj, UpdatedEPs, CDRUrl, RecordingUrl, Details),
                                'ringing_callback'
                        end,

            {CIDNumber, CIDName} = acdc_util:caller_id(Call),

            acdc_agent_stats:agent_connecting(AccountId, AgentId, CallId, CIDName, CIDNumber),
            lager:info("trying to ring agent endpoints(~p)", [length(UpdatedEPs)]),
            lager:debug("notifications for the queue: ~p", [kz_json:get_value(<<"Notifications">>, JObj)]),
            {'next_state', NextState, State#state{wrapup_timeout=WrapupTimer
                                                 ,member_call=Call
                                                 ,member_call_id=CallId
                                                 ,member_call_start=kz_time:now()
                                                 ,member_call_queue_id=QueueId
                                                 ,caller_exit_key=CallerExitKey
                                                 ,endpoints=UpdatedEPs
                                                 ,queue_notifications=kz_json:get_value(<<"Notifications">>, JObj)
                                                 }}
    end;
ready({'member_connect_win', JObj, 'different_node'}, #state{agent_listener=AgentListener
                                                            ,endpoints=OrigEPs
                                                            ,agent_id=AgentId
                                                            ,connect_failures=CF
                                                            }=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    CallId = kapps_call:call_id(Call),

    kz_log:put_callid(CallId),

    WrapupTimer = kz_json:get_integer_value(<<"Wrapup-Timeout">>, JObj, 0),
    CallerExitKey = kz_json:get_value(<<"Caller-Exit-Key">>, JObj, <<"#">>),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    RecordingUrl = recording_url(JObj),

    %% Only start monitoring if the agent can actually take the call
    case get_endpoints(OrigEPs, AgentListener, Call, AgentId, QueueId) of
        {'error', 'no_endpoints'} ->
            lager:info("agent ~s has no endpoints assigned; logging agent out", [AgentId]),
            {'next_state', 'paused', State};
        {'error', _E} ->
            lager:debug("can't take the call, skip me: ~p", [_E]),
            {'next_state', 'ready', State#state{connect_failures=CF+1}};
        {'ok', UpdatedEPs} ->
            acdc_util:bind_to_call_events(Call, AgentListener),

            acdc_agent_listener:monitor_call(AgentListener, Call, JObj, RecordingUrl),
            %% Need to check if a callback is required to the caller
            NextState = case kz_json:get_value(<<"Callback-Details">>, JObj) of
                            'undefined' -> 'ringing';
                            _Details -> 'ringing_callback'
                        end,

            lager:debug("monitoring agent ~s to connect to caller in queue ~s", [AgentId, QueueId]),
            {'next_state', NextState, State#state{wrapup_timeout=WrapupTimer
                                                 ,member_call=Call
                                                 ,member_call_id=CallId
                                                 ,member_call_start=kz_time:now()
                                                 ,member_call_queue_id=QueueId
                                                 ,caller_exit_key=CallerExitKey
                                                 ,endpoints=UpdatedEPs
                                                 ,queue_notifications=kz_json:get_value(<<"Notifications">>, JObj)
                                                 ,monitoring = 'true'
                                                 }}
    end;
ready({'member_connect_satisfied', _, _Node}, State) ->
    lager:debug("unexpected connect_satisfied in state 'ready'"),
    {'next_state', 'ready', State};

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
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    {'next_state', 'ready', State};
ready({'channel_answered', JObj}, #state{outbound_call_ids=OutboundCallIds}=State) ->
    CallId = call_id(JObj),
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent picked up outbound call ~s", [CallId]),
            {'next_state', 'outbound', start_outbound_call_handling(CallId, clear_call(State, 'ready')), 'hibernate'};
        'false' ->
            lager:debug("unexpected answer of ~s while in ready", [CallId]),
            {'next_state', 'ready', State}
    end;
ready(?DESTROYED_CHANNEL(CallId, _Cause), #state{agent_listener=AgentListener
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
ready(?NEW_CHANNEL_FROM(CallId, Number, Name, 'undefined'), State) ->
    lager:debug("ready call_from inbound: ~s", [CallId]),
    {'next_state', 'inbound', start_inbound_call_handling(CallId, Number, Name, State), 'hibernate'};
ready(?NEW_CHANNEL_FROM(CallId,_,_, MemberCallId), State) ->
    cancel_if_failed_originate(CallId, MemberCallId, 'ready', State);
ready(?NEW_CHANNEL_TO(CallId,Number,Name), State) ->
    lager:debug("ready call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, Number, Name, State), 'hibernate'};
ready({'playback_stop', _JObj}, State) ->
    {'next_state', 'ready', State};
ready(_Evt, State) ->
    lager:debug("unhandled event while ready: ~p", [_Evt]),
    {'next_state', 'ready', State}.

-spec ready(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
ready('status', _, State) ->
    {'reply', [{'state', <<"ready">>}], 'ready', State};
ready('current_call', _, State) ->
    {'reply', 'undefined', 'ready', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ringing(any(), state()) -> kz_term:handle_fsm_ret(state()).
ringing({'member_connect_req', _}, State) ->
    {'next_state', 'ringing', State};
ringing({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (already ringing)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'ringing', State};
ringing({'member_connect_win', _, 'different_node'}, State) ->
    lager:debug("received member_connect_win for different node (ringing)"),
    {'next_state', 'ringing', State};
ringing({'originate_ready', JObj}, #state{agent_listener=AgentListener}=State) ->
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),

    lager:debug("ringing agent's phone with call-id ~s", [CallId]),
    acdc_agent_listener:originate_execute(AgentListener, JObj),
    {'next_state', 'ringing', State};
ringing({'member_connect_satisfied', JObj, Node}, #state{agent_listener=AgentListener
                                                        ,member_call_id=MemberCallId
                                                        ,account_id=AccountId
                                                        ,member_call_queue_id=QueueId
                                                        ,agent_id=AgentId
                                                        ,connect_failures=Fails
                                                        ,max_connect_failures=MaxFails
                                                        }=State) ->
    CallId = kz_json:get_ne_binary_value([<<"Call">>, <<"Call-ID">>], JObj, []),
    AcceptedAgentId = kz_json:get_binary_value(<<"Accept-Agent-ID">>, JObj),
    lager:info("received connect_satisfied: check if I should hangup:~p  MemberCall:~p CallId:~p AgentId:~p AcceptedAgentId:~p", [Node, MemberCallId, CallId, AgentId, AcceptedAgentId]),
    case CallId =:= MemberCallId
        andalso AgentId /= AcceptedAgentId of
        true ->
            lager:info("agent ~p hanging up: agent ~p is handling the call", [AgentId, AcceptedAgentId]),
            acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),
            _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, MemberCallId, <<"LOSE_RACE">>),
            acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

            State1 = clear_call(State, 'failed'),
            StateName1 = return_to_state(Fails+1, MaxFails),
            case StateName1 of
                'paused' -> {'next_state', 'paused', State1};
                'ready' -> apply_state_updates(State1)
            end;
        _ -> {'next_state', 'ringing', State}
    end;
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

    {CIDNumber, CIDName} = acdc_util:caller_id(MemberCall),

    acdc_util:bind_to_call_events(ACallId, AgentListener),

    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNumber),

    {'next_state', 'answered', State#state{agent_call_id=ACallId
                                          ,connect_failures=0
                                          }};
ringing({'originate_failed', E}, #state{agent_listener=AgentListener
                                       ,account_id=AccountId
                                       ,agent_id=AgentId
                                       ,member_call_queue_id=QueueId
                                       ,member_call_id=CallId
                                       }=State) ->
    ErrReason = missed_reason(kz_json:get_value(<<"Error-Message">>, E)),
    lager:debug("originate failed (~s), broadcasting", [ErrReason]),
    kapi_acdc_agent:publish_shared_originate_failure([{<<"Account-ID">>, AccountId}
                                                     ,{<<"Agent-ID">>, AgentId}
                                                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                     ]),

    acdc_agent_listener:member_connect_retry(AgentListener, CallId),

    _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, ErrReason),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    {'next_state', 'ringing', State};
ringing({'agent_timeout', _JObj}, #state{agent_listener=AgentListener
                                        ,account_id=AccountId
                                        ,agent_id=AgentId
                                        ,member_call_queue_id=QueueId
                                        ,member_call_id=CallId
                                        }=State) ->
    ErrReason = <<"timeout">>,
    lager:debug("agent timeout, publishing originate failed"),
    kapi_acdc_agent:publish_shared_originate_failure([{<<"Account-ID">>, AccountId}
                                                     ,{<<"Agent-ID">>, AgentId}
                                                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                     ]),

    acdc_agent_listener:agent_timeout(AgentListener),

    _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, ErrReason),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    {'next_state', 'ringing', State};
ringing({'playback_stop', _JObj}, State) ->
    {'next_state', 'ringing', State};
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

    {CIDNumber, CIDName} = acdc_util:caller_id(MemberCall),

    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNumber),

    {'next_state', 'answered', State#state{connect_failures=0}};
ringing({'channel_bridged', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing(?DESTROYED_CHANNEL(AgentCallId, _Cause), #state{agent_listener=AgentListener
                                                       ,agent_call_id=AgentCallId
                                                       ,connect_failures=Fails
                                                       ,max_connect_failures=MaxFails
                                                       }=State) ->
    lager:debug("agent's channel (~s) down", [AgentCallId]),

    acdc_agent_listener:hangup_call(AgentListener),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing(?DESTROYED_CHANNEL(MemberCallId, _Cause), #state{agent_listener=AgentListener
                                                        ,account_id=AccountId
                                                        ,member_call_id=MemberCallId
                                                        ,member_call_queue_id=QueueId
                                                        }=State) ->
    lager:debug("caller's channel (~s) has gone down, stop agent's call: ~s", [MemberCallId, _Cause]),
    acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),

    _ = acdc_stats:call_abandoned(AccountId, QueueId, MemberCallId, ?ABANDON_HANGUP),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    apply_state_updates(clear_call(State, 'ready'));
ringing(?DESTROYED_CHANNEL(CallId, _Cause), #state{agent_listener=AgentListener
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
ringing({'channel_answered', JObj}, #state{member_call_id=MemberCallId
                                          ,agent_listener=AgentListener
                                          ,outbound_call_ids=OutboundCallIds
                                          }=State) ->
    case call_id(JObj) of
        MemberCallId ->
            lager:debug("caller's channel answered"),
            {'next_state', 'ringing', State};
        OtherCallId ->
            case lists:member(OtherCallId, OutboundCallIds) of
                'true' ->
                    lager:debug("agent picked up outbound call ~s instead of the queue call ~s", [OtherCallId, MemberCallId]),
                    acdc_agent_listener:hangup_call(AgentListener),
                    {'next_state', 'outbound', start_outbound_call_handling(OtherCallId, clear_call(State, 'ready')), 'hibernate'};
                'false' ->
                    lager:debug("recv answer for ~s, probably the agent's call", [OtherCallId]),
                    {'next_state', 'ringing', State#state{agent_call_id=OtherCallId}}
            end
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
    lager:debug("originate resp on ~s, broadcasting", [ACallId]),
    kapi_acdc_agent:publish_shared_call_id([{<<"Account-ID">>, AccountId}
                                           ,{<<"Agent-ID">>, AgentId}
                                           ,{<<"Agent-Call-ID">>, ACallId}
                                            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                           ]),

    maybe_notify(Ns, ?NOTIFY_PICKUP, State),

    {CIDNumber, CIDName} = acdc_util:caller_id(MemberCall),

    acdc_agent_listener:member_connect_accepted(AgentListener, ACallId),
    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNumber),

    {'next_state', 'ringing', State};
ringing({'shared_failure', _JObj}, #state{connect_failures=Fails
                                         ,max_connect_failures=MaxFails
                                         }=State) ->
    lager:debug("shared originate failure"),

    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing({'shared_call_id', JObj}, #state{agent_listener=AgentListener}=State) ->
    ACallId = kz_json:get_value(<<"Agent-Call-ID">>, JObj),

    lager:debug("shared call id ~s acquired, connecting to caller", [ACallId]),

    acdc_util:bind_to_call_events(ACallId, AgentListener),
    acdc_agent_listener:monitor_connect_accepted(AgentListener, ACallId),

    {'next_state', 'answered', State#state{agent_call_id=ACallId
                                          ,connect_failures=0
                                          }};
ringing(?NEW_CHANNEL_FROM(CallId,_,_, MemberCallId), #state{member_call_id=MemberCallId}=State) ->
    lager:debug("new channel ~s for agent", [CallId]),
    {'next_state', 'ringing', State};
ringing(?NEW_CHANNEL_FROM(CallId, Number, Name, _), #state{agent_listener=AgentListener}=State) ->
    lager:debug("ringing call_from inbound: ~s", [CallId]),
    acdc_agent_listener:hangup_call(AgentListener),
    {'next_state', 'inbound', start_inbound_call_handling(CallId, Number, Name, clear_call(State, 'ready')), 'hibernate'};
ringing(?NEW_CHANNEL_TO(CallId,_,_), #state{agent_listener=AgentListener
                                           ,outbound_call_ids=OutboundCallIds
                                           }=State) ->
    lager:debug("ringing call_to outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'ringing', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
ringing({'leg_created', _, _}, State) ->
    {'next_state', 'ringing', State};
ringing({'leg_destroyed', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing({'usurp_control', _CallId}, State) ->
    {'next_state', 'ringing', State};
ringing(_Evt, State) ->
    lager:debug("unhandled event while ringing: ~p", [_Evt]),
    {'next_state', 'ringing', State}.

-spec ringing(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
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

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ringing_callback(any(), state()) -> kz_term:handle_fsm_ret(state()).
ringing_callback({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'ringing_callback', JObj),
    {'next_state', 'ringing_callback', State};
ringing_callback({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv originate_uuid for agent call ~s(~s)", [ACallId, ACtrlQ]),
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    {'next_state', 'ringing_callback', State};
ringing_callback({'originate_resp', ACallId}, #state{account_id=AccountId
                                                    ,agent_id=AgentId
                                                    ,agent_listener=AgentListener
                                                    ,member_call_id=MemberCallId
                                                    ,member_call=MemberCall
                                                    ,queue_notifications=Ns
                                                    ,agent_call_id=ACallId
                                                    ,agent_callback_call=ACall
                                                    }=State) ->
    lager:debug("originate resp on ~s, broadcasting", [ACallId]),
    kapi_acdc_agent:publish_shared_call_id([{<<"Account-ID">>, AccountId}
                                           ,{<<"Agent-ID">>, AgentId}
                                           ,{<<"Agent-Call-ID">>, ACallId}
                                            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                           ]),

    maybe_notify(Ns, ?NOTIFY_PICKUP, State),

    {CIDNumber, CIDName} = acdc_util:caller_id(MemberCall),

    %% TODO: remove if unnecessary
                                                % acdc_util:b_bind_to_call_events(ACallId, AgentListener),
    acdc_agent_listener:member_callback_accepted(AgentListener, ACall),
    acdc_agent_stats:agent_connected(AccountId, AgentId, MemberCallId, CIDName, CIDNumber),

    {'next_state', 'ringing_callback', State#state{connect_failures=0}};
ringing_callback({'originate_failed', JObj}, #state{agent_listener=AgentListener
                                                   ,account_id=AccountId
                                                   ,agent_id=AgentId
                                                   ,member_call_queue_id=QueueId
                                                   ,member_call_id=CallId
                                                   }=State) ->
    ErrReason = missed_reason(kz_json:get_value(<<"Error-Message">>, JObj)),
    lager:debug("originate failed (~s), broadcasting", [ErrReason]),
    kapi_acdc_agent:publish_shared_originate_failure([{<<"Account-ID">>, AccountId}
                                                     ,{<<"Agent-ID">>, AgentId}
                                                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                     ]),

    acdc_agent_listener:member_connect_retry(AgentListener, CallId),

    _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, ErrReason),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    {'next_state', 'ringing_callback', State};
ringing_callback({'shared_failure', _JObj}, #state{connect_failures=Fails
                                                  ,max_connect_failures=MaxFails
                                                  }=State) ->
    lager:debug("shared originate failure"),

    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
%% For the monitoring processes, fake the agent_callback_call so playback_stop isn't ignored
ringing_callback({'shared_call_id', JObj}, #state{agent_callback_call='undefined'}=State) ->
    ACallId = kz_json:get_value(<<"Agent-Call-ID">>, JObj),
    ACall = kapps_call:set_call_id(ACallId, kapps_call:new()),
    ringing_callback({'shared_call_id', JObj}, State#state{agent_callback_call=ACall});
ringing_callback({'shared_call_id', JObj}, #state{agent_listener=AgentListener}=State) ->
    ACallId = kz_json:get_value(<<"Agent-Call-ID">>, JObj),

    lager:debug("shared call id ~s acquired, connecting to caller", [ACallId]),

    acdc_util:b_bind_to_call_events(ACallId, AgentListener),
    acdc_agent_listener:monitor_connect_accepted(AgentListener, ACallId),

    {'next_state', 'ringing_callback', State#state{agent_call_id=ACallId
                                                  ,connect_failures=0
                                                  }};
ringing_callback({'member_connect_satisfied', JObj, Node}, #state{agent_listener=AgentListener
                                                                 ,member_call_id=MemberCallId
                                                                 ,account_id=AccountId
                                                                 ,member_call_queue_id=QueueId
                                                                 ,agent_id=AgentId
                                                                 ,connect_failures=Fails
                                                                 ,max_connect_failures=MaxFails
                                                                 }=State) ->
    CallId = kz_json:get_ne_binary_value([<<"Call">>, <<"Call-ID">>], JObj, []),
    AcceptedAgentId = kz_json:get_binary_value(<<"Accept-Agent-ID">>, JObj),
    lager:info("received connect_satisfied: check if I should hangup:~p  MemberCall:~p CallId:~p AgentId:~p AcceptedAgentId:~p", [Node, MemberCallId, CallId, AgentId, AcceptedAgentId]),
    case CallId =:= MemberCallId
        andalso AgentId /= AcceptedAgentId of
        true ->
            lager:info("agent ~p hanging up: agent ~p is handling the call", [AgentId, AcceptedAgentId]),
            acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),
            _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, MemberCallId, <<"LOSE_RACE">>),
            acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

            State1 = clear_call(State, 'failed'),
            StateName1 = return_to_state(Fails+1, MaxFails),
            case StateName1 of
                'paused' -> {'next_state', 'paused', State1};
                'ready' -> apply_state_updates(State1)
            end;
        _ -> {'next_state', 'ringing_callback', State}
    end;
ringing_callback({'channel_answered', JObj}, State) ->
    CallId = call_id(JObj),
    lager:debug("agent answered phone on ~s", [CallId]),
    ACall = kapps_call:set_account_id(kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj), kapps_call:from_json(JObj)),
    {'next_state', 'ringing_callback', State#state{agent_call_id=CallId
                                                  ,agent_callback_call=ACall
                                                  }};
ringing_callback(?DESTROYED_CHANNEL(ACallId, _Cause), #state{agent_call_id=ACallId
                                                            ,connect_failures=Fails
                                                            ,max_connect_failures=MaxFails
                                                            ,monitoring='true'
                                                            }=State) ->
    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing_callback(?DESTROYED_CHANNEL(ACallId, Cause), #state{account_id=AccountId
                                                           ,agent_id=AgentId
                                                           ,agent_listener=AgentListener
                                                           ,member_call_id=CallId
                                                           ,member_call_queue_id=QueueId
                                                           ,agent_call_id=ACallId
                                                           ,max_connect_failures=MaxFails
                                                           ,connect_failures=Fails
                                                           }=State) ->
    lager:info("agent hungup ~s while they were supposed to wait for a callback", [ACallId]),

    acdc_agent_listener:member_connect_retry(AgentListener, CallId),

    _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, CallId, Cause),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    NewFSMState = clear_call(State, 'failed'),
    NextState = return_to_state(Fails+1, MaxFails),
    case NextState of
        'paused' -> {'next_state', 'paused', NewFSMState};
        'ready' -> apply_state_updates(NewFSMState)
    end;
ringing_callback(?NEW_CHANNEL_TO(CallId,_,_), State) ->
    lager:debug("new channel ~s for agent", [CallId]),
    {'next_state', 'ringing_callback', State};
ringing_callback({'playback_stop', _}, #state{agent_callback_call='undefined'}=State) ->
    {'next_state', 'ringing_callback', State};
ringing_callback({'playback_stop', _JObj}, #state{member_call=Call
                                                 ,member_call_id=MemberCallId
                                                 ,monitoring='true'
                                                 }=State) ->
    {'next_state', 'awaiting_callback', State#state{member_original_call=Call
                                                   ,member_original_call_id=MemberCallId
                                                   }};
ringing_callback({'playback_stop', _JObj}, #state{agent_listener=AgentListener
                                                 ,member_call=Call
                                                 ,member_call_id=MemberCallId
                                                 ,agent_callback_call=AgentCallbackCall
                                                 }=State) ->
    NewMemberCallId = acdc_agent_listener:originate_callback_return(AgentListener, AgentCallbackCall),
    kz_log:put_callid(NewMemberCallId),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_SOLID),

    %% Preserve old call information for sake of stats
    {'next_state', 'awaiting_callback', State#state{member_call_id=NewMemberCallId
                                                   ,member_original_call=Call
                                                   ,member_original_call_id=MemberCallId
                                                   }};
ringing_callback({'usurp_control', _}, State) ->
    {'next_state', 'ringing_callback', State};
ringing_callback(_Evt, State) ->
    lager:debug("unhandled event: ~p", [_Evt]),
    {'next_state', 'ringing_callback', State}.

-spec ringing_callback(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
ringing_callback('status', _, State) ->
    {'reply', [{'state', <<"ringing_callback">>}]
    ,'ringing', State};
ringing_callback('current_call', _, #state{member_call=Call
                                          ,member_call_queue_id=QueueId
                                          }=State) ->
    {'reply', current_call(Call, 'ringing_callback', QueueId, 'undefined'), 'ringing_callback', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec awaiting_callback(any(), state()) -> kz_term:handle_fsm_ret(state()).
awaiting_callback({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'awaiting_callback', JObj),
    {'next_state', 'awaiting_callback', State};
awaiting_callback({'originate_uuid', MemberCallbackCallId, CtrlQ}, #state{member_callback_candidates=Candidates}=State) ->
    lager:debug("recv originate_uuid for member callback call ~s(~s)", [MemberCallbackCallId, CtrlQ]),
    {'next_state', 'awaiting_callback', State#state{member_callback_candidates=props:set_value(MemberCallbackCallId, CtrlQ, Candidates)}};
awaiting_callback({'originate_resp', _}, State) ->
    {'next_state', 'awaiting_callback', State};
awaiting_callback({'originate_failed', JObj}, #state{account_id=AccountId
                                                    ,agent_id=AgentId
                                                    ,agent_listener=AgentListener
                                                    ,member_call=MemberCall
                                                    ,member_original_call_id=OriginalMemberCallId
                                                    ,member_call_queue_id=QueueId
                                                    ,agent_call_id=ACallId
                                                    }=State) ->
    ErrReason = missed_reason(kz_json:get_value(<<"Error-Message">>, JObj)),
    lager:debug("originate failed (~s), broadcasting", [ErrReason]),
    kapi_acdc_agent:publish_shared_originate_failure([{<<"Account-ID">>, AccountId}
                                                     ,{<<"Agent-ID">>, AgentId}
                                                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                     ]),

    acdc_agent_listener:member_connect_accepted(AgentListener, ACallId, MemberCall),

    _ = acdc_stats:call_handled(AccountId, QueueId, OriginalMemberCallId, AgentId),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    {'next_state', 'awaiting_callback', State#state{wrapup_ref=hangup_call(State, 'member')}};
awaiting_callback({'shared_failure', _}, #state{agent_listener=AgentListener
                                               ,agent_call_id=ACallId
                                               }=State) ->
    lager:debug("shared originate failure"),
    acdc_agent_listener:channel_hungup(AgentListener, ACallId),

    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'member')}};
awaiting_callback({'shared_call_id', JObj}, #state{agent_listener=AgentListener
                                                  ,member_call=MemberCall
                                                  ,member_callback_candidates=Candidates
                                                  ,monitoring='true'
                                                  }=State) ->
    NewMemberCallId = kz_json:get_value(<<"Member-Call-ID">>, JObj),
    acdc_util:bind_to_call_events(NewMemberCallId, AgentListener),
    NewMemberCall = kapps_call:exec([fun(Call) -> kapps_call:set_account_id(kapps_call:account_id(MemberCall), Call) end
                                    ,fun(Call) -> kapps_call:set_call_id(NewMemberCallId, Call) end
                                    ], kapps_call:new()),

    {'next_state', 'answered', State#state{member_call=NewMemberCall
                                          ,member_call_id=NewMemberCallId
                                          ,member_callback_candidates=props:set_value(NewMemberCallId, NewMemberCall, Candidates)
                                          ,connect_failures=0
                                          }};
awaiting_callback({'shared_call_id', _}, State) ->
    {'next_state', 'answered', State};
awaiting_callback({'channel_answered', JObj}=Evt, #state{account_id=AccountId
                                                        ,agent_id=AgentId
                                                        ,agent_listener=AgentListener
                                                        ,member_callback_candidates=Candidates
                                                        ,member_original_call=OriginalMemberCall
                                                        ,member_original_call_id=OriginalMemberCallId
                                                        ,member_call_queue_id=QueueId
                                                        ,agent_call_id=ACallId
                                                        }=State) ->
    CallId = call_id(JObj),
    case props:get_value(CallId, Candidates) of
        'undefined' -> awaiting_callback_unhandled_event(Evt, State);
        CtrlQ ->
            lager:info("member answered phone on ~s", [CallId]),

            %% Update control queue so call recordings work
            %% Also preserve some metadata included in call recordings
            MemberCall = kapps_call:exec([fun(Call) -> kapps_call:set_account_id(AccountId, Call) end
                                         ,fun(Call) -> kapps_call:set_control_queue(CtrlQ, Call) end
                                         ,fun(Call) -> kapps_call:set_custom_channel_var(<<"Queue-ID">>, QueueId, Call) end
                                         ], kapps_call:from_json(JObj)),

            kapi_acdc_agent:publish_shared_call_id([{<<"Account-ID">>, AccountId}
                                                   ,{<<"Agent-ID">>, AgentId}
                                                   ,{<<"Member-Call-ID">>, CallId}
                                                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                   ]),

            %% Notify the queue_fsm that the call is now fully accepted
            acdc_agent_listener:member_connect_accepted(AgentListener, ACallId, MemberCall),

            {CIDNumber, CIDName} = acdc_util:caller_id(OriginalMemberCall),

            acdc_agent_stats:agent_connected(AccountId, AgentId, OriginalMemberCallId, CIDName, CIDNumber),
            _ = acdc_stats:call_handled(AccountId, QueueId, OriginalMemberCallId, AgentId),

            {'next_state', 'awaiting_callback', State#state{member_call=MemberCall
                                                           ,member_call_id=CallId
                                                           ,connect_failures=0
                                                           }}
    end;
awaiting_callback(?DESTROYED_CHANNEL(OriginalMemberCallId, _Cause), #state{member_original_call_id=OriginalMemberCallId
                                                                          ,monitoring='true'
                                                                          }=State) ->
    {'next_state', 'awaiting_callback', State};
awaiting_callback(?DESTROYED_CHANNEL(ACallId, _Cause), #state{agent_listener=AgentListener
                                                             ,agent_call_id=ACallId
                                                             ,monitoring='true'
                                                             }=State) ->
    lager:debug("agent hungup ~s while waiting for a callback to connect", [ACallId]),
    acdc_agent_listener:channel_hungup(AgentListener, ACallId),

    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'member')}};
awaiting_callback(?DESTROYED_CHANNEL(ACallId, _Cause), #state{account_id=AccountId
                                                             ,agent_id=AgentId
                                                             ,agent_listener=AgentListener
                                                             ,member_call=MemberCall
                                                             ,member_original_call_id=OriginalMemberCallId
                                                             ,member_call_queue_id=QueueId
                                                             ,agent_call_id=ACallId
                                                             }=State) ->
    lager:info("agent hungup ~s while waiting for a callback to connect", [ACallId]),

    acdc_agent_listener:member_connect_accepted(AgentListener, ACallId, MemberCall),

    _ = acdc_stats:call_handled(AccountId, QueueId, OriginalMemberCallId, AgentId),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),

    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'member')}};
awaiting_callback(?DESTROYED_CHANNEL(CallId, Cause), State) ->
    maybe_member_no_answer(CallId, Cause, State);
awaiting_callback({'leg_created', CallId, OtherLegCallId}=Evt, #state{agent_listener=AgentListener
                                                                     ,member_callback_candidates=Candidates
                                                                     }=State) ->
    case props:get_value(CallId, Candidates) of
        'undefined' -> awaiting_callback_unhandled_event(Evt, State);
        CtrlQ ->
            %% Unbind from originate UUID, bind to bridge of loopback
            lager:debug("rebinding from ~s to ~s due to loopback", [CallId, OtherLegCallId]),
            acdc_agent_listener:rebind_events(AgentListener, CallId, OtherLegCallId),

            Candidates1 = props:set_value(OtherLegCallId, CtrlQ, []),
            {'next_state', 'awaiting_callback', State#state{member_callback_candidates=Candidates1}}
    end;
awaiting_callback({'leg_destroyed', _}, State) ->
    {'next_state', 'awaiting_callback', State};
awaiting_callback({'playback_stop', _JObj}, State) ->
    {'next_state', 'awaiting_callback', State};
awaiting_callback({'usurp_control', _}, State) ->
    {'next_state', 'awaiting_callback', State};
awaiting_callback(Evt, State) ->
    awaiting_callback_unhandled_event(Evt, State).

-spec awaiting_callback(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
awaiting_callback('status', _, #state{member_call_id=MemberCallId
                                     ,agent_call_id=ACallId
                                     }=State) ->
    {'reply', [{'state', <<"awaiting_callback">>}
              ,{'member_call_id', MemberCallId}
              ,{'agent_call_id', ACallId}
              ]
    ,'ringing', State};
awaiting_callback('current_call', _, #state{member_call=Call
                                           ,member_call_queue_id=QueueId
                                           }=State) ->
    {'reply', current_call(Call, 'awaiting_callback', QueueId, 'undefined'), 'awaiting_callback', State}.

-spec awaiting_callback_unhandled_event(any(), state()) ->
          {'next_state', 'awaiting_callback', state()}.
awaiting_callback_unhandled_event(Evt, State) ->
    lager:debug("unhandled event while awaiting callback: ~p", [Evt]),
    {'next_state', 'awaiting_callback', State}.

-spec maybe_member_no_answer(kz_term:ne_binary(), kz_term:ne_binary(), state()) ->
          {'next_state', atom(), state()}.
maybe_member_no_answer(CallId, Cause, #state{account_id=AccountId
                                            ,agent_id=AgentId
                                            ,agent_listener=AgentListener
                                            ,member_call=MemberCall
                                            ,member_callback_candidates=Candidates
                                            ,member_original_call_id=OriginalMemberCallId
                                            ,member_call_queue_id=QueueId
                                            ,agent_call_id=ACallId
                                            }=State) ->
    _ = case props:get_value(CallId, Candidates) of
        'undefined' -> 'ok';
        _ ->
            ErrReason = missed_reason(Cause),
            lager:debug("member did not answer callback ~s (~s)", [CallId, ErrReason]),

            acdc_agent_listener:member_connect_accepted(AgentListener, ACallId, MemberCall),

            acdc_stats:call_handled(AccountId, QueueId, OriginalMemberCallId, AgentId)
    end,
    {'next_state', 'awaiting_callback', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec answered(any(), state()) -> kz_term:handle_fsm_ret(state()).
answered({'member_connect_req', _}, State) ->
    {'next_state', 'answered', State};
answered({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (on the phone with someone)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),
    {'next_state', 'answered', State};
answered({'member_connect_win', _, 'different_node'}, State) ->
    lager:debug("received member_connect_win for different node (answered)"),
    {'next_state', 'answered', State};
answered({'member_connect_satisfied', _, Node}, State) ->
    lager:debug("received member_connect_satisfied for ~p (answered)", [Node]),
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

    _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, original_call_id(State), <<"dialplan_error">>),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    apply_state_updates(clear_call(State, 'ready'));
answered({'playback_stop', _JObj}, State) ->
    {'next_state', 'answered', State};
answered(?DESTROYED_CHANNEL(CallId, Cause), #state{member_call_id=CallId
                                                  ,outbound_call_ids=[]
                                                  }=State) ->
    lager:debug("caller's channel hung up: ~s", [Cause]),
    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'member')}};
answered(?DESTROYED_CHANNEL(CallId, _Cause), #state{account_id=AccountId
                                                   ,agent_id=AgentId
                                                   ,agent_listener=AgentListener
                                                   ,member_call_id=CallId
                                                   ,member_call_queue_id=QueueId
                                                   ,queue_notifications=Ns
                                                   ,outbound_call_ids=[OutboundCallId|_]
                                                   }=State) ->
    lager:debug("caller's channel hung up, but there are still some outbounds"),
    _ = acdc_stats:call_processed(AccountId, QueueId, AgentId, original_call_id(State), 'member'),
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    {'next_state', 'outbound', start_outbound_call_handling(OutboundCallId, clear_call(State, 'ready')), 'hibernate'};
answered(?DESTROYED_CHANNEL(CallId, Cause), #state{agent_call_id=CallId
                                                  ,outbound_call_ids=[]
                                                  }=State) ->
    lager:debug("agent's channel has hung up: ~s", [Cause]),
    {'next_state', 'wrapup', State#state{wrapup_ref=hangup_call(State, 'agent')}};
answered(?DESTROYED_CHANNEL(CallId, _Cause), #state{account_id=AccountId
                                                   ,agent_id=AgentId
                                                   ,agent_listener=AgentListener
                                                   ,member_call_id=MemberCallId
                                                   ,member_call_queue_id=QueueId
                                                   ,queue_notifications=Ns
                                                   ,agent_call_id=CallId
                                                   ,outbound_call_ids=[OutboundCallId|_]
                                                   }=State) ->
    lager:debug("agent's channel hung up, but there are still some outbounds"),
    _ = acdc_stats:call_processed(AccountId, QueueId, AgentId, original_call_id(State), 'agent'),
    acdc_agent_listener:channel_hungup(AgentListener, MemberCallId),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    {'next_state', 'outbound', start_outbound_call_handling(OutboundCallId, clear_call(State, 'ready')), 'hibernate'};
answered(?DESTROYED_CHANNEL(CallId, _Cause), #state{agent_listener=AgentListener
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
answered({'channel_answered', JObj}=Evt, #state{agent_call_id=AgentCallId
                                               ,member_call_id=MemberCallId
                                               ,outbound_call_ids=OutboundCallIds
                                               }=State) ->
    case call_id(JObj) of
        AgentCallId ->
            lager:debug("agent's channel ~s has answered", [AgentCallId]),
            {'next_state', 'answered', State};
        MemberCallId ->
            lager:debug("member's channel has answered"),
            {'next_state', 'answered', State};
        OtherCallId ->
            case lists:member(OtherCallId, OutboundCallIds) of
                'true' ->
                    lager:debug("agent answered outbound call ~s", [OtherCallId]),
                    {'next_state', 'answered', State};
                'false' ->
                    lager:debug("unexpected event while answered: ~p", [Evt]),
                    {'next_state', 'answered', State}
            end
    end;
answered({'channel_bridged', _}, State) ->
    {'next_state', 'answered', State};
answered({'channel_unbridged', _}, State) ->
    {'next_state', 'answered', State};
answered({'channel_transferee', Transferor, Transferee}, #state{account_id=AccountId
                                                               ,agent_id=AgentId
                                                               ,member_call_id=Transferor
                                                               ,member_call_queue_id=QueueId
                                                               ,queue_notifications=Ns
                                                               ,agent_call_id=Transferee
                                                               }=State) ->
    lager:info("caller transferred the agent"),
    _ = acdc_stats:call_processed(AccountId, QueueId, AgentId, Transferor, 'member'),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    {'next_state', 'outbound', start_outbound_call_handling(Transferee, clear_call(State, 'ready'))};
answered({'channel_transferee', _, _}, State) ->
    {'next_state', 'answered', State};
answered({'channel_replaced', _}, State) ->
    {'next_state', 'answered', State};
answered({'originate_started', _CallId}, State) ->
    {'next_state', 'answered', State};
answered(?NEW_CHANNEL_FROM(CallId,_,_, MemberCallId), #state{member_call_id=MemberCallId}=State) ->
    lager:debug("new channel ~s for agent", [CallId]),
    {'next_state', 'answered', State};
answered(?NEW_CHANNEL_FROM(CallId,_,_, _), #state{agent_listener=AgentListener
                                                 ,outbound_call_ids=OutboundCallIds
                                                 }=State) ->
    lager:debug("answered call_from inbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'answered', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
answered(?NEW_CHANNEL_TO(CallId,_,_), #state{agent_listener=AgentListener
                                            ,outbound_call_ids=OutboundCallIds
                                            }=State) ->
    lager:debug("answered call_to outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'answered', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
answered({'leg_created', _, _}, State) ->
    {'next_state', 'answered', State};
answered({'usurp_control', _CallId}, State) ->
    {'next_state', 'answered', State};
answered(_Evt, State) ->
    lager:debug("unhandled event while answered: ~p", [_Evt]),
    {'next_state', 'answered', State}.

-spec answered(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
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

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec wrapup(any(), state()) -> kz_term:handle_fsm_ret(state()).
wrapup({'member_connect_req', _}, State) ->
    {'next_state', 'wrapup', State#state{wrapup_timeout=0}};
wrapup({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (in wrapup)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'wrapup', State#state{wrapup_timeout=0}};
wrapup({'member_connect_win', _, 'different_node'}, State) ->
    lager:debug("received member_connect_win for different node (wrapup)"),
    {'next_state', 'wrapup', State#state{wrapup_timeout=0}};
wrapup({'member_connect_satisfied', _, Node}, State) ->
    lager:info("unexpected connect_satisfied for ~p", [Node]),
    {'next_state', 'wrapup', State};

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
wrapup({'channel_bridged', _}, State) ->
    {'next_state', 'wrapup', State};
wrapup({'channel_unbridged', _}, State) ->
    {'next_state', 'wrapup', State};
wrapup({'channel_transferee', _, _}, State) ->
    {'next_state', 'wrapup', State};
wrapup(?DESTROYED_CHANNEL(_, _), State) ->
    {'next_state', 'wrapup', State};
wrapup({'leg_destroyed', CallId}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("leg ~s destroyed", [CallId]),
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    {'next_state', 'wrapup', State};
wrapup(?NEW_CHANNEL_FROM(CallId, Number, Name, _), State) ->
    lager:debug("wrapup call_from inbound: ~s", [CallId]),
    {'next_state', 'inbound', start_inbound_call_handling(CallId, Number, Name, State), 'hibernate'};
wrapup(?NEW_CHANNEL_TO(CallId,Number,Name), State) ->
    lager:debug("wrapup call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, Number, Name, State), 'hibernate'};
wrapup({'playback_stop', _}, State) ->
    {'next_state', 'wrapup', State};
wrapup({'originate_resp', _}, State) ->
    {'next_state', 'wrapup', State};
wrapup(_Evt, State) ->
    lager:debug("unhandled event while in wrapup: ~p", [_Evt]),
    {'next_state', 'wrapup', State#state{wrapup_timeout=0}}.

-spec wrapup(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
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

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec paused(any(), state()) -> kz_term:handle_fsm_ret(state()).
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
paused({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),

    {'next_state', 'paused', State};
paused({'member_connect_win', _, 'different_node'}, State) ->
    lager:debug("received member_connect_win for different node (paused)"),
    {'next_state', 'paused', State};

paused({'member_connect_satisfied', _, Node}, State) ->
    lager:info("unexpected connect_satisfied for ~p", [Node]),
    {'next_state', 'paused', State};

paused({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    {'next_state', 'paused', State};
paused(?NEW_CHANNEL_FROM(CallId,_,_, MemberCallId), #state{member_call_id = MemberCallId} = State) ->
    cancel_if_failed_originate(CallId, MemberCallId, 'paused', State);
paused(?NEW_CHANNEL_FROM(CallId, Number, Name, _), State) ->
    lager:debug("paused call_from inbound: ~s", [CallId]),
    {'next_state', 'inbound', start_inbound_call_handling(CallId, Number, Name, State), 'hibernate'};
paused(?NEW_CHANNEL_TO(CallId, Number, Name), State) ->
    lager:debug("paused call_to outbound: ~s", [CallId]),
    {'next_state', 'outbound', start_outbound_call_handling(CallId, Number, Name, State), 'hibernate'};
paused(_Evt, State) ->
    lager:debug("unhandled event while paused: ~p", [_Evt]),
    {'next_state', 'paused', State}.

-spec paused(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
paused('status', _, #state{pause_ref=Ref}=State) ->
    {'reply', [{'state', <<"paused">>}
              ,{'pause_left', time_left(Ref)}
              ]
    ,'paused', State};
paused('current_call', _, State) ->
    {'reply', 'undefined', 'paused', State}.

-spec outbound(any(), state()) -> kz_term:handle_fsm_ret(state()).
outbound({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'outbound', JObj),
    {'next_state', 'outbound', State};
outbound({'playback_stop', _JObj}, State) ->
    {'next_state', 'outbound', State};
outbound(?DESTROYED_CHANNEL(CallId, Cause), #state{agent_listener=AgentListener
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
outbound({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (on outbound call)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),
    {'next_state', 'outbound', State};
outbound({'member_connect_win', _, 'different_node'}, State) ->
    lager:debug("received member_connect_win for different node (outbound)"),
    {'next_state', 'outbound', State};

outbound({'member_connect_satisfied', _, Node}, State) ->
    lager:info("unexpected connect_satisfied for ~p", [Node]),
    {'next_state', 'wrapup', State};

outbound({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
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
outbound({'leg_created', _, _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_answered', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_bridged', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_unbridged', _}, State) ->
    {'next_state', 'outbound', State};
outbound({'channel_replaced', _}, State) ->
    {'next_state', 'outbound', State};
outbound(?NEW_CHANNEL_FROM(CallId,_,_, MemberCallId), #state{member_call_id = MemberCallId} = State) ->
    cancel_if_failed_originate(CallId, MemberCallId, 'outbound', State);
outbound(?NEW_CHANNEL_FROM(CallId,_,_, _), #state{outbound_call_ids=[CallId]}=State) ->
    {'next_state', 'outbound', State};
outbound(?NEW_CHANNEL_FROM(CallId,_,_, _), #state{agent_listener=AgentListener
                                                 ,outbound_call_ids=OutboundCallIds
                                                 }=State) ->
    lager:debug("outbound call_from outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'outbound', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
outbound(?NEW_CHANNEL_TO(CallId,_,_), #state{agent_listener=AgentListener
                                            ,outbound_call_ids=OutboundCallIds
                                            }=State) ->
    lager:debug("outbound call_to outbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'outbound', State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
outbound({'leg_destroyed', _CallId}, State) ->
    {'next_state', 'outbound', State};
outbound({'usurp_control', _CallId}, State) ->
    {'next_state', 'outbound', State};
outbound(_Msg, State) ->
    lager:debug("ignoring msg in outbound: ~p", [_Msg]),
    {'next_state', 'outbound', State}.

-spec outbound(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
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

-spec inbound(any(), state()) -> kz_term:handle_fsm_ret(state()).
inbound({'sync_req', JObj}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("recv sync_req from ~s", [kz_json:get_value(<<"Server-ID">>, JObj)]),
    acdc_agent_listener:send_sync_resp(AgentListener, 'inbound', JObj),
    {'next_state', 'inbound', State};
inbound({'playback_stop', _JObj}, State) ->
    {'next_state', 'inbound', State};
inbound(?DESTROYED_CHANNEL(CallId, Cause), #state{agent_listener=AgentListener
                                                 ,inbound_call_ids=OutboundCallIds
                                                 }=State) ->
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    case lists:member(CallId, OutboundCallIds) of
        'true' ->
            lager:debug("agent inbound channel ~s down: ~s", [CallId, Cause]),
            inbound_hungup(State#state{inbound_call_ids=lists:delete(CallId, OutboundCallIds)});
        'false' ->
            lager:debug("unexpected channel ~s down", [CallId]),
            {'next_state', 'inbound', State}
    end;
inbound({'member_connect_win', JObj, 'same_node'}, #state{agent_listener=AgentListener}=State) ->
    lager:debug("agent won, but can't process this right now (on inbound call)"),
    acdc_agent_listener:member_connect_retry(AgentListener, JObj),
    {'next_state', 'inbound', State};
inbound({'member_connect_win', _, 'different_node'}, State) ->
    lager:debug("received member_connect_win for different node (inbound)"),
    {'next_state', 'inbound', State};

inbound({'member_connect_satisfied', _, Node}, State) ->
    lager:info("unexpected connect_satisfied for ~p", [Node]),
    {'next_state', 'wrapup', State};

inbound({'originate_uuid', ACallId, ACtrlQ}, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:originate_uuid(AgentListener, ACallId, ACtrlQ),
    {'next_state', 'inbound', State};
inbound({'originate_failed', _E}, State) ->
    {'next_state', 'inbound', State};
inbound({'timeout', Ref, ?PAUSE_MESSAGE}, #state{pause_ref=Ref}=State) ->
    lager:debug("pause timer expired while inbound"),
    {'next_state', 'inbound', State#state{pause_ref='undefined'}};
inbound({'timeout', WRef, ?WRAPUP_FINISHED}, #state{wrapup_ref=WRef}=State) ->
    lager:debug("wrapup timer ended while on inbound call"),
    {'next_state', 'inbound', State#state{wrapup_ref='undefined'}, 'hibernate'};
inbound({'member_connect_req', _}, State) ->
    {'next_state', 'inbound', State};
inbound({'leg_created', _, _}, State) ->
    {'next_state', 'inbound', State};
inbound({'channel_answered', _}, State) ->
    {'next_state', 'inbound', State};
inbound({'channel_bridged', _}, State) ->
    {'next_state', 'inbound', State};
inbound({'channel_unbridged', _}, State) ->
    {'next_state', 'inbound', State};
inbound({'channel_replaced', _}, State) ->
    {'next_state', 'inbound', State};
inbound(?NEW_CHANNEL_FROM(CallId,_,_, MemberCallId), #state{member_call_id = MemberCallId} = State) ->
    cancel_if_failed_originate(CallId, MemberCallId, 'inbound', State);
inbound(?NEW_CHANNEL_FROM(CallId,_,_, _), #state{inbound_call_ids=[CallId]}=State) ->
    {'next_state', 'inbound', State};
inbound(?NEW_CHANNEL_FROM(CallId,_,_, _), #state{agent_listener=AgentListener
                                                ,inbound_call_ids=OutboundCallIds
                                                }=State) ->
    lager:debug("inbound call_from inbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'inbound', State#state{inbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
inbound(?NEW_CHANNEL_TO(CallId,_,_), #state{agent_listener=AgentListener
                                           ,inbound_call_ids=OutboundCallIds
                                           }=State) ->
    lager:debug("inbound call_to inbound: ~s", [CallId]),
    acdc_util:bind_to_call_events(CallId, AgentListener),
    {'next_state', 'inbound', State#state{inbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}};
inbound({'leg_destroyed', _CallId}, State) ->
    {'next_state', 'inbound', State};
inbound({'usurp_control', _CallId}, State) ->
    {'next_state', 'inbound', State};
inbound(_Msg, State) ->
    lager:debug("ignoring msg in inbound: ~p", [_Msg]),
    {'next_state', 'inbound', State}.

-spec inbound(any(), atom(), state()) -> kz_term:handle_sync_event_ret(state()).
inbound('status', _, #state{wrapup_ref=Ref
                           ,inbound_call_ids=OutboundCallIds
                           }=State) ->
    {'reply', [{'state', <<"inbound">>}
              ,{'wrapup_left', time_left(Ref)}
              ,{'inbound_call_id', hd(OutboundCallIds)}
              ]
    ,'inbound', State};
inbound('current_call', _, State) ->
    {'reply', 'undefined', 'inbound', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(any(), atom(), state()) -> kz_types:handle_fsm_ret(state()).
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
handle_event({'pause', Timeout, Alias}, 'ringing_callback', State) ->
    handle_event({'pause', Timeout, Alias}, 'ringing', State);
handle_event({'pause', Timeout, Alias}, 'ringing', #state{agent_listener=AgentListener
                                                         ,account_id=AccountId
                                                         ,agent_id=AgentId
                                                         ,member_call_queue_id=QueueId
                                                         }=State) ->
    %% Give up the current ringing call
    acdc_agent_listener:hangup_call(AgentListener),
    lager:debug("stopping ringing agent in order to move to pause"),
    _ = acdc_stats:call_missed(AccountId, QueueId, AgentId, original_call_id(State), <<"agent pausing">>),
    NewFSMState = clear_call(State, 'failed'),
    %% After clearing we are basically 'ready' state, pause from that state
    handle_event({'pause', Timeout, Alias}, 'ready', NewFSMState);
handle_event({'pause', 0, _}=Event, 'ready', #state{agent_state_updates=Queue}=State) ->
    lager:debug("recv status update:, pausing for up to infinity s"),
    NewQueue = [Event | Queue],
    apply_state_updates(State#state{agent_state_updates=NewQueue});
handle_event({'pause', Timeout, _}=Event, 'ready', #state{agent_state_updates=Queue}=State) ->
    lager:debug("recv status update: pausing for up to ~b s", [Timeout]),
    NewQueue = [Event | Queue],
    apply_state_updates(State#state{agent_state_updates=NewQueue});
handle_event({'pause', Timeout, Alias}, 'paused', State) ->
    handle_event({'pause', Timeout, Alias}, 'ready', State);
handle_event({'pause', _, _}=Event, StateName, #state{agent_state_updates=Queue}=State) ->
    lager:debug("recv pause during ~p, delaying", [StateName]),
    NewQueue = [Event | Queue],
    {'next_state', StateName, State#state{agent_state_updates=NewQueue}};
handle_event({'end_wrapup'}=Event, 'wrapup', #state{agent_state_updates=Queue}=State) ->
    NewQueue = [Event | Queue],
    apply_state_updates(State#state{agent_state_updates=NewQueue});
handle_event({'end_wrapup'}, StateName, State) ->
    {'next_state', StateName, State};
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
    acdc_agent_listener:refresh_config(AgentListener
                                      ,AgentJObj
                                      ,StateName),
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

%%------------------------------------------------------------------------------
%% @private
%% @doc Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_sync_event(any(), {pid(),any()}, atom(), state()) -> kz_types:handle_sync_event_ret(state()).
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync event in state ~s: ~p", [StateName, _Event]),
    {'reply', 'ok', StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), atom(), state()) -> kz_types:handle_fsm_ret(state()).
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
handle_info(?NEW_CHANNEL_FROM(_CallId,_,_,_)=Evt, StateName, State) ->
    gen_fsm:send_event(self(), Evt),
    {'next_state', StateName, State};
handle_info(?NEW_CHANNEL_TO(_CallId,_,_)=Evt, StateName, State) ->
    gen_fsm:send_event(self(), Evt),
    {'next_state', StateName, State};
handle_info(?DESTROYED_CHANNEL(_, _)=Evt, StateName, State) ->
    gen_fsm:send_event(self(), Evt),
    {'next_state', StateName, State};
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled message in state ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), atom(), state()) -> 'ok'.
terminate(_Reason, _StateName, #state{agent_listener=AgentListener}) ->
    lager:debug("acdc agent fsm terminating while in ~s: ~p", [_StateName, _Reason]),
    acdc_agent_listener:stop(AgentListener),
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_SOLID).

%%------------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), atom(), state(), any()) -> {'ok', atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cancel_if_failed_originate(kz_term:ne_binary(), kz_term:ne_binary(), atom(), state()) ->
          {'next_state', atom(), state()}.
cancel_if_failed_originate(CallId, MemberCallId, StateName, #state{agent_listener=AgentListener
                                                                  ,member_call_id=MemberCallId1
                                                                  }=State) when MemberCallId =/= MemberCallId1 ->
    lager:debug("cancelling ~s (failed originate from queue call ~s"
               ,[CallId, MemberCallId]),
    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    {'next_state', StateName, State};
cancel_if_failed_originate(_, _, StateName, State) ->
    {'next_state', StateName, State}.

-spec start_wrapup_timer(integer()) -> reference().
start_wrapup_timer(Timeout) when Timeout =< 0 -> start_wrapup_timer(1); % send immediately
start_wrapup_timer(Timeout) -> gen_fsm:start_timer(Timeout*1000, ?WRAPUP_FINISHED).

-spec start_sync_timer() -> reference().
start_sync_timer() ->
    gen_fsm:start_timer(?SYNC_RESPONSE_TIMEOUT, ?SYNC_RESPONSE_MESSAGE).

-spec start_sync_timer(pid()) -> reference().
start_sync_timer(P) ->
    erlang:start_timer(?SYNC_RESPONSE_TIMEOUT, P, ?SYNC_RESPONSE_MESSAGE).

-spec start_resync_timer() -> reference().
start_resync_timer() ->
    gen_fsm:start_timer(?RESYNC_RESPONSE_TIMEOUT, ?RESYNC_RESPONSE_MESSAGE).

-spec start_pause_timer(pos_integer()) -> kz_term:api_reference().
start_pause_timer('undefined') -> start_pause_timer(1);
start_pause_timer(0) -> 'undefined';
start_pause_timer(Timeout) ->
    gen_fsm:start_timer(Timeout * 1000, ?PAUSE_MESSAGE).

-spec call_id(kz_json:object()) -> kz_term:api_binary().
call_id(JObj) ->
    case kz_json:get_value(<<"Call-ID">>, JObj) of
        'undefined' -> kz_json:get_value([<<"Call">>, <<"Call-ID">>], JObj);
        CallId -> CallId
    end.

%% returns time left in seconds
-spec time_left(reference() | 'false' | kz_term:api_integer() | 'infinity') -> kz_term:api_integer() | kz_term:ne_binary().
time_left(Ref) when is_reference(Ref) ->
    time_left(erlang:read_timer(Ref));
time_left('false') -> 'undefined';
time_left('undefined') -> 'undefined';
time_left('infinity') -> <<"infinity">>;
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
    lager:debug("agent has failed to connect ~b times(~p)", [Fails+1, _MaxFails]),
    clear_call(State#state{connect_failures=Fails+1}, 'ready');
clear_call(#state{fsm_call_id=FSMemberCallId
                 ,wrapup_ref=WRef
                 ,pause_ref=PRef
                 }=State, NextState)->
    kz_log:put_callid(FSMemberCallId),

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
               ,member_callback_candidates = []
               ,member_original_call = 'undefined'
               ,member_original_call_id = 'undefined'
               ,member_call_start = 'undefined'
               ,member_call_queue_id = 'undefined'
               ,agent_call_id = 'undefined'
               ,agent_callback_call = 'undefined'
               ,caller_exit_key = <<"#">>
               ,monitoring = 'false'
               }.

-spec current_call(kapps_call:call() | 'undefined', atom(), kz_term:ne_binary(), 'undefined' | kz_term:kz_now()) ->
          kz_term:api_object().
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

-spec elapsed('undefined' | kz_term:kz_now()) -> kz_term:api_integer().
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
    _ = acdc_stats:call_processed(AccountId, QueueId, AgentId, original_call_id(State), Initiator),

    acdc_agent_listener:channel_hungup(AgentListener, CallId),
    maybe_notify(Ns, ?NOTIFY_HANGUP, State),
    wrapup_timer(State).

-spec maybe_stop_timer(kz_term:api_reference() | 'infinity') -> 'ok'.
maybe_stop_timer('undefined') -> 'ok';
maybe_stop_timer('infinity') -> 'ok';
maybe_stop_timer(ConnRef) when is_reference(ConnRef) ->
    _ = gen_fsm:cancel_timer(ConnRef),
    'ok'.

-spec maybe_stop_timer(kz_term:api_reference() | 'infinity', boolean()) -> 'ok'.
maybe_stop_timer(TimerRef, 'true') -> maybe_stop_timer(TimerRef);
maybe_stop_timer(_, 'false') -> 'ok'.

-spec start_outbound_call_handling(kz_term:ne_binary() | kapps_call:call(), state()) -> state().
start_outbound_call_handling(CallId, State) when is_binary(CallId) ->
    start_outbound_call_handling(CallId, <<"unknown">>, <<"unknown">>, State);
start_outbound_call_handling(Call, State) ->
    start_outbound_call_handling(kapps_call:call_id(Call), State).

start_outbound_call_handling(CallId, Number, Name, #state{agent_listener=AgentListener
                                                         ,account_id=AccountId
                                                         ,agent_id=AgentId
                                                         ,outbound_call_ids=OutboundCallIds
                                                         }=State) when is_binary(CallId) ->
    kz_log:put_callid(CallId),
    lager:debug("agent making outbound call, not receiving ACDc calls"),
    acdc_agent_listener:outbound_call(AgentListener, CallId, Number, Name),
    acdc_agent_stats:agent_outbound(AccountId, AgentId, CallId),
    State#state{outbound_call_ids=[CallId | lists:delete(CallId, OutboundCallIds)]}.


-spec start_inbound_call_handling(kz_term:ne_binary() | kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), state()) -> state().
start_inbound_call_handling(CallId, Number, Name, #state{agent_listener=AgentListener
                                                        ,account_id=AccountId
                                                        ,agent_id=AgentId
                                                        ,inbound_call_ids=InboundCallIds
                                                        }=State) when is_binary(CallId) ->
    kz_log:put_callid(CallId),
    lager:debug("agent receiving inbound call, not receiving ACDc calls"),
    acdc_agent_listener:inbound_call(AgentListener, CallId, Number, Name),
    acdc_agent_stats:agent_inbound(AccountId, AgentId, CallId),
    State#state{inbound_call_ids=[CallId | lists:delete(CallId, InboundCallIds)]}.

-spec outbound_hungup(state()) -> kz_term:handle_fsm_ret(state()).
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
                <<"infinity">> -> apply_state_updates(clear_call(State, 'paused'));
                _P ->
                    lager:debug("wrapup left: ~p pause left: ~p", [_W, _P]),
                    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
                    apply_state_updates(clear_call(State, 'ready'))
            end
    end;
outbound_hungup(State) ->
    lager:debug("agent still has some outbound calls active"),
    {'next_state', 'outbound', State}.

-spec inbound_hungup(state()) -> kz_term:handle_fsm_ret(state()).
inbound_hungup(#state{agent_listener=AgentListener
                     ,wrapup_ref=WRef
                     ,pause_ref=PRef
                     ,inbound_call_ids=[]
                     }=State) ->
    case time_left(WRef) of
        N when is_integer(N), N > 0 -> apply_state_updates(clear_call(State, 'wrapup'));
        _W ->
            case time_left(PRef) of
                N when is_integer(N), N > 0 -> apply_state_updates(clear_call(State, 'paused'));
                <<"infinity">> -> apply_state_updates(clear_call(State, 'paused'));
                _P ->
                    lager:debug("wrapup left: ~p pause left: ~p", [_W, _P]),
                    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
                    apply_state_updates(clear_call(State, 'ready'))
            end
    end;
inbound_hungup(State) ->
    lager:debug("agent still has some inbound calls active"),
    {'next_state', 'inbound', State}.

-spec missed_reason(kz_term:ne_binary()) -> kz_term:ne_binary().
missed_reason(<<"-ERR ", Reason/binary>>) ->
    missed_reason(binary:replace(Reason, <<"\n">>, <<>>, ['global']));
missed_reason(<<"ALLOTTED_TIMEOUT">>) -> <<"timeout">>;
missed_reason(<<"NO_USER_RESPONSE">>) -> <<"rejected">>;
missed_reason(<<"CALL_REJECTED">>) -> <<"rejected">>;
missed_reason(<<"USER_BUSY">>) -> <<"rejected">>;
missed_reason(Reason) -> Reason.

-spec find_username(kz_json:object()) -> kz_term:api_binary().
find_username(EP) ->
    find_sip_username(EP, kzd_devices:sip_username(EP)).

-spec find_sip_username(kz_json:object(), kz_term:api_binary()) -> kz_term:api_binary().
find_sip_username(EP, 'undefined') -> kz_json:get_value(<<"To-User">>, EP);
find_sip_username(_EP, Username) -> Username.


-spec find_endpoint_id(kz_json:object()) -> kz_term:api_binary().
find_endpoint_id(EP) ->
    find_endpoint_id(EP, kz_doc:id(EP)).

-spec find_endpoint_id(kz_json:object(), kz_term:api_binary()) -> kz_term:api_binary().
find_endpoint_id(EP, 'undefined') -> kz_json:get_value(<<"Endpoint-ID">>, EP);
find_endpoint_id(_EP, EPId) -> EPId.

-spec monitor_endpoint(kz_term:api_object(), kz_term:ne_binary(), kz_term:server_ref()) -> _.
monitor_endpoint('undefined', _, _) -> 'ok';
monitor_endpoint(EP, AccountId, AgentListener) ->
    Username = find_username(EP),

    %% Bind for outbound call requests
    acdc_agent_listener:add_endpoint_bindings(AgentListener
                                             ,kz_endpoint:get_sip_realm(EP, AccountId)
                                             ,Username
                                             ),
    %% Inform us of device changes
    catch gproc:reg(?ENDPOINT_UPDATE_REG(AccountId, find_endpoint_id(EP))),
    catch gproc:reg(?NEW_CHANNEL_REG(AccountId, Username)),
    catch gproc:reg(?DESTROYED_CHANNEL_REG(AccountId, Username)).

-spec unmonitor_endpoint(kz_json:object(), kz_term:ne_binary(), kz_term:server_ref()) -> any().
unmonitor_endpoint(EP, AccountId, AgentListener) ->
    Username = find_username(EP),

    %% Bind for outbound call requests
    acdc_agent_listener:remove_endpoint_bindings(AgentListener
                                                ,kz_endpoint:get_sip_realm(EP, AccountId)
                                                ,Username
                                                ),
    %% Inform us of device changes
    catch gproc:unreg(?ENDPOINT_UPDATE_REG(AccountId, find_endpoint_id(EP))),
    catch gproc:unreg(?NEW_CHANNEL_REG(AccountId, Username)),
    catch gproc:unreg(?DESTROYED_CHANNEL_REG(AccountId, Username)).

-spec maybe_add_endpoint(kz_term:ne_binary(), kz_json:object(), kz_json:objects(), kz_term:ne_binary(), kz_term:server_ref()) -> any().
maybe_add_endpoint(EPId, EP, EPs, AccountId, AgentListener) ->
    case lists:partition(fun(E) -> find_endpoint_id(E) =:= EPId end, EPs) of
        {[], _} ->
            lager:debug("endpoint ~s not in our list, adding it", [EPId]),
            [begin monitor_endpoint(convert_to_endpoint(EP), AccountId, AgentListener), EP end | EPs];
        {_, _} -> EPs
    end.

-spec maybe_remove_endpoint(kz_term:ne_binary(), kz_json:objects(), kz_term:ne_binary(), kz_term:server_ref()) -> kz_json:objects().
maybe_remove_endpoint(EPId, EPs, AccountId, AgentListener) ->
    case lists:partition(fun(EP) -> find_endpoint_id(EP) =:= EPId end, EPs) of
        {[], _} -> EPs; %% unknown endpoint
        {[RemoveEP], EPs1} ->
            lager:debug("endpoint ~s in our list, removing it", [EPId]),
            _ = unmonitor_endpoint(RemoveEP, AccountId, AgentListener),
            EPs1
    end.

-spec convert_to_endpoint(kz_json:object()) -> kz_term:api_object().
convert_to_endpoint(EPDoc) ->
    Setters = [{fun kapps_call:set_account_id/2, kz_doc:account_id(EPDoc)}
              ,{fun kapps_call:set_account_db/2, kz_doc:account_db(EPDoc)}
              ,{fun kapps_call:set_owner_id/2, kzd_devices:owner_id(EPDoc)}
              ,{fun kapps_call:set_resource_type/2, ?RESOURCE_TYPE_AUDIO}
              ],

    Call = kapps_call:exec(Setters, kapps_call:new()),
    case kz_endpoint:build(kz_doc:id(EPDoc), [], Call) of
        {'ok', EP} -> EP;
        {'error', _} -> 'undefined'
    end.

-spec get_endpoints(kz_json:objects(), kz_term:server_ref(), kapps_call:call(), kz_term:api_binary(), kz_term:api_binary()) ->
          {'ok', kz_json:objects()} |
          {'error', any()}.
get_endpoints(OrigEPs, AgentListener, Call, AgentId, QueueId) ->
    case catch acdc_util:get_endpoints(Call, AgentId) of
        [] ->
            %% Survive couch connection issue by using last list of valid endpoints
            case OrigEPs of
                [] -> {'error', 'no_endpoints'};
                _ -> {'ok', [kz_json:set_value([<<"Custom-Channel-Vars">>, <<"Queue-ID">>], QueueId, EP) || EP <- OrigEPs]}
            end;
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

-spec return_to_state(non_neg_integer(), pos_integer() | 'infinity') -> 'paused' | 'ready'.
return_to_state(_, 'infinity') ->
    'ready';
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
                    _ = kz_process:spawn(fun notify/4, [Url, get_method(Ns), Key, State]),
                    'ok'
            end;
        Url ->
            lager:debug("send update for ~s to ~s", [Key, Url]),
            _ = kz_process:spawn(fun notify/4, [Url, get_method(Ns), Key, State]),
            'ok'
    end.

-spec get_method(kz_json:object()) -> 'get' | 'post'.
get_method(Ns) ->
    case kz_json:get_value(<<"method">>, Ns) of
        'undefined' -> 'get';
        M -> standardize_method(kz_term:to_lower_binary(M))
    end.

-spec standardize_method(kz_term:ne_binary()) -> 'get' | 'post'.
standardize_method(<<"post">>) -> 'post';
standardize_method(_) -> 'get'.

-spec notify(kz_term:ne_binary(), 'get' | 'post', kz_term:ne_binary(), state()) -> 'ok'.
notify(Url, Method, Key, #state{account_id=AccountId
                               ,agent_id=AgentId
                               ,agent_name=AgentName
                               ,member_call=MemberCall
                               ,agent_call_id=AgentCallId
                               ,member_call_queue_id=QueueId
                               }) ->
    kz_log:put_callid(kapps_call:call_id(MemberCall)),
    {CIDNumber, CIDName} = acdc_util:caller_id(MemberCall),
    Params = props:filter_undefined(
               [{<<"account_id">>, AccountId}
               ,{<<"agent_id">>, AgentId}
               ,{<<"agent_call_id">>, AgentCallId}
               ,{<<"queue_id">>, QueueId}
               ,{<<"member_call_id">>, kapps_call:call_id(MemberCall)}
               ,{<<"caller_id_name">>, CIDName}
               ,{<<"caller_id_number">>, CIDNumber}
               ,{<<"call_state">>, Key}
               ,{<<"now">>, kz_time:current_tstamp()}
               ,{<<"agent_username">>, AgentName}
               ]),
    notify_method(Url, Method, kz_json:from_list(Params)).

-spec notify_method(kz_term:ne_binary(), 'get' | 'post', kz_json:object()) -> 'ok'.
notify_method(Url, 'post', Data) ->
    notify(Url, [{"Content-Type", "application/json"}]
          ,'post', kz_json:encode(Data), []
          );
notify_method(Url, 'get', Data) ->
    notify(uri(Url, kz_http_util:json_to_querystring(Data))
          ,[], 'get', <<>>, []
          ).

-spec notify(iolist(), kz_term:proplist(), 'get' | 'post', binary(), kz_term:proplist()) -> 'ok'.
notify(Uri, Headers, Method, Body, Opts) ->
    Options = [{'connect_timeout', 1000}
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

-spec cdr_url(kz_json:object()) -> kz_term:api_binary().
cdr_url(JObj) ->
    case kz_json:get_value([<<"Notifications">>, ?NOTIFY_CDR], JObj) of
        'undefined' -> kz_json:get_ne_value(<<"CDR-Url">>, JObj);
        Url -> Url
    end.

-spec recording_url(kz_json:object()) -> kz_term:api_binary().
recording_url(JObj) ->
    case kz_json:get_value([<<"Notifications">>, ?NOTIFY_RECORDING], JObj) of
        'undefined' -> kz_json:get_ne_value(<<"Recording-URL">>, JObj);
        Url -> Url
    end.

-spec uri(kz_term:ne_binary(), iolist()) -> iolist().
uri(URI, QueryString) ->
    case kz_http_util:urlsplit(URI) of
        {Scheme, Host, Path, <<>>, Fragment} ->
            kz_http_util:urlunsplit({Scheme, Host, Path, QueryString, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            kz_http_util:urlunsplit({Scheme, Host, Path, <<QS/binary, "&", (kz_term:to_binary(QueryString))/binary>>, Fragment})
    end.

-spec apply_state_updates(state()) -> kz_term:handle_fsm_ret(state()).
apply_state_updates(#state{agent_state_updates=Q
                          ,wrapup_ref=WRef
                          ,pause_ref=PRef
                          }=State) ->
    FoldDefaultState = case time_left(WRef) of
                           N when is_integer(N), N > 0 -> 'wrapup';
                           _W ->
                               case time_left(PRef) of
                                   N when is_integer(N), N > 0 -> 'paused';
                                   <<"infinity">> -> 'paused';
                                   _P -> 'ready'
                               end
                       end,
    lager:debug("default state for applying state updates ~s", [FoldDefaultState]),
    apply_state_updates_fold({'next_state', FoldDefaultState, State#state{agent_state_updates=[]}}, lists:reverse(Q)).

-spec apply_state_updates_fold({'next_state', atom(), state()}, list()) -> kz_term:handle_fsm_ret(state()).
apply_state_updates_fold({_, StateName, #state{account_id=AccountId
                                              ,agent_id=AgentId
                                              ,agent_listener=AgentListener
                                              ,wrapup_ref=WRef
                                              ,pause_ref=PRef
                                              ,pause_alias=Alias
                                              }}=Acc, []) ->
    lager:debug("resulting agent state ~s", [StateName]),
    case StateName of
        'ready' ->
            acdc_agent_listener:send_agent_available(AgentListener),
            acdc_agent_stats:agent_ready(AccountId, AgentId);
        'wrapup' -> acdc_agent_stats:agent_wrapup(AccountId, AgentId, time_left(WRef));
        'paused' ->
            acdc_agent_listener:send_agent_busy(AgentListener),
            acdc_agent_stats:agent_paused(AccountId, AgentId, time_left(PRef), Alias)
    end,
    Acc;
apply_state_updates_fold({_, _, State}, [{'pause', 0, Alias}|Updates]) ->
    apply_state_updates_fold(handle_pause('infinity', Alias, State), Updates);
apply_state_updates_fold({_, _, State}, [{'pause', Timeout, Alias}|Updates]) ->
    apply_state_updates_fold(handle_pause(Timeout, Alias, State), Updates);
apply_state_updates_fold({_, _, State}, [{'resume'}|Updates]) ->
    apply_state_updates_fold(handle_resume(State), Updates);
apply_state_updates_fold({_, 'wrapup', State}, [{'end_wrapup'}|Updates]) ->
    apply_state_updates_fold(handle_end_wrapup('ready', State), Updates);
apply_state_updates_fold({_, StateName, State}, [{'end_wrapup'}|Updates]) ->
    apply_state_updates_fold(handle_end_wrapup(StateName, State), Updates);
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

-spec handle_agent_logout(state()) -> kz_term:handle_fsm_ret(state()).
handle_agent_logout(#state{account_id = AccountId
                          ,agent_id = AgentId
                          }=State) ->
    acdc_agent_stats:agent_logged_out(AccountId, AgentId),
    {'stop', 'normal', State}.

-spec handle_presence_update(kz_term:ne_binary(), kz_term:ne_binary(), state()) -> 'ok'.
handle_presence_update(PresenceId, PresenceState, #state{agent_id = AgentId
                                                        ,account_id = AccountId
                                                        }) ->
    Super = acdc_agents_sup:find_agent_supervisor(AccountId, AgentId),
    Listener = acdc_agent_sup:listener(Super),
    acdc_agent_listener:maybe_update_presence_id(Listener, PresenceId),
    acdc_agent_listener:presence_update(Listener, PresenceState).

-spec handle_resume(state()) -> kz_term:handle_fsm_ret(state()).
handle_resume(#state{agent_listener=AgentListener
                    ,pause_ref=Ref
                    }=State) ->
    lager:debug("resume received, putting agent back into action"),
    maybe_stop_timer(Ref),

    acdc_agent_listener:update_agent_status(AgentListener, <<"resume">>),

    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_GREEN),
    {'next_state', 'ready', State#state{pause_ref='undefined'}}.

-spec handle_pause(integer() | 'infinity', kz_term:api_binary(), state()) -> kz_term:handle_fsm_ret(state()).
handle_pause(Timeout, Alias, #state{agent_listener=AgentListener}=State) ->
    acdc_agent_listener:presence_update(AgentListener, ?PRESENCE_RED_FLASH),
    State1 = case Timeout of
                 'infinity' ->
                     State#state{pause_ref='infinity'
                                ,pause_alias=Alias
                                };
                 _ ->
                     Ref = start_pause_timer(Timeout),
                     State#state{pause_ref=Ref
                                ,pause_alias=Alias
                                }
             end,
    {'next_state', 'paused', State1}.

-spec handle_end_wrapup(atom(), state()) -> kz_term:handle_fsm_ret(state()).
handle_end_wrapup(NextState, #state{wrapup_ref=Ref}=State) ->
    lager:debug("end_wrapup received, cancelling wrapup timers"),
    maybe_stop_timer(Ref),
    {'next_state', NextState, State#state{wrapup_ref='undefined'}}.

-spec original_call_id(state()) -> kz_term:ne_binary().
original_call_id(#state{member_call_id=MemberCallId
                       ,member_original_call_id='undefined'
                       }) ->
    MemberCallId;
original_call_id(#state{member_original_call_id=OriginalCallId}) ->
    OriginalCallId.
