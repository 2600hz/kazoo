%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_listener).
-behaviour(gen_listener).

%% API
-export([start_link/2, start_link/3, start_link/5
        ,member_connect_resp/2
        ,member_connect_retry/2
        ,member_connect_accepted/1, member_connect_accepted/2, member_connect_accepted/3
        ,monitor_connect_accepted/2
        ,member_callback_accepted/2
        ,agent_timeout/1
        ,bridge_to_member/6
        ,originate_callback_to_agent/7
        ,originate_callback_return/2
        ,hangup_call/1
        ,monitor_call/4
        ,channel_hungup/2
        ,rebind_events/3
        ,unbind_from_events/2
        ,originate_execute/2
        ,originate_uuid/3
        ,outbound_call/4
        ,inbound_call/4
        ,send_agent_available/1
        ,send_agent_busy/1
        ,send_sync_req/1
        ,send_sync_resp/3, send_sync_resp/4
        ,config/1, refresh_config/3
        ,send_status_resume/1
        ,add_acdc_queue/3
        ,rm_acdc_queue/2
        ,call_status_req/1, call_status_req/2
        ,stop/1
        ,fsm_started/2
        ,add_endpoint_bindings/3, remove_endpoint_bindings/3
        ,outbound_call_id/2
        ,remove_cdr_urls/2
        ,logout_agent/1
        ,agent_info/2
        ,maybe_update_presence_id/2
        ,maybe_update_presence_state/2
        ,presence_update/2
        ,update_agent_status/2
        ]).

%% Introspection
-export([presence_id/1
        ,queues/1
        ,id/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-type config() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}.
-export_type([config/0]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {call :: kapps_call:call()
               ,original_call :: kapps_call:call()
               ,acdc_queue_id :: api_kz_term:ne_binary() % the ACDc Queue ID
               ,msg_queue_id :: api_kz_term:ne_binary() % the AMQP Queue ID of the ACDc Queue process
               ,agent_id :: api_kz_term:ne_binary()
               ,agent_priority :: agent_priority()
               ,skills :: kz_term:ne_binaries() % skills this agent has
               ,acct_db :: api_kz_term:ne_binary()
               ,acct_id :: api_kz_term:ne_binary()
               ,fsm_pid :: kz_term:api_pid()
               ,agent_queues = [] :: kz_term:ne_binaries()
               ,last_connect :: kz_term:kz_now() | undefined % last connection
               ,last_attempt :: kz_term:kz_now() | undefined % last attempt to connect
               ,my_id :: kz_term:ne_binary()
               ,my_q :: kz_term:api_binary() % AMQP queue name
               ,timer_ref :: kz_term:api_reference()
               ,sync_resp :: kz_json:object() % furthest along resp
               ,supervisor :: pid()
               ,record_calls = 'false' :: boolean()
               ,recording_url :: kz_term:api_binary() %% where to send recordings after the call
               ,is_thief = 'false' :: boolean()
               ,agent :: agent()
               ,agent_call_ids = [] :: kz_term:api_binaries() | kz_term:proplist()
               ,cdr_urls = dict:new() :: dict:dict() %% {CallId, Url}
               ,agent_presence_id :: kz_term:api_binary()
               }).
-type state() :: #state{}.

-type agent() :: kapps_call:call() | kz_json:object().

%%%=============================================================================
%%% Defines for different functionality
%%%=============================================================================

%% On init, an aget process sends a sync_req and waits SYNC_TIMER_TIMEOUT ms
%% The agent process checks its list of received
-define(SYNC_TIMER_MESSAGE, 'sync_timeout').
-define(SYNC_TIMER_TIMEOUT, 5000).

%% After receiving sync_resp, if the resp status requires waiting, SYNC_WAIT_TIMER_TIMEOUT
%% pauses the agent process, then restarts the sync process (send sync_req, start
%% SYNC_TIMER_TIMEOUT, collect sync_resp(s), etc
-define(SYNC_WAIT_TIMER_MESSAGE, 'sync_wait_timeout').
-define(SYNC_WAIT_TIMER_TIMEOUT, 5000).

%% When in the wrapup status, how long does an agent wait before going back to ready
-define(WRAPUP_TIMER_MESSAGE, 'wrapup_timeout').
-define(WRAPUP_TIMER_TIMEOUT, 60000).

%% When an agent is paused (on break, logged out, etc)
-define(PAUSED_TIMER_MESSAGE, 'paused_timeout').

-define(BINDINGS(AccountId, AgentId), [{'self', []}
                                      ,{'acdc_agent', [{'account_id', AccountId}
                                                      ,{'agent_id', AgentId}
                                                      ,{'restrict_to', ['member_connect_win', 'member_connect_satisfied', 'sync', 'fsm_shared']}
                                                      ]}
                                      ,{'conf', [{'action', <<"*">>}
                                                ,{'db', kzs_util:format_account_db(AccountId)}
                                                ,{'id', AgentId}
                                                ,'federate'
                                                ]}
                                      ]).

-define(RESPONDERS, [{{'acdc_agent_handler', 'handle_sync_req'}
                     ,[{<<"agent">>, <<"sync_req">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_sync_resp'}
                     ,[{<<"agent">>, <<"sync_resp">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_stats_req'}
                     ,[{<<"agent">>, <<"stats_req">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_call_event'}
                     ,[{<<"call_event">>, <<"*">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_originate_resp'}
                     ,[{<<"resource">>, <<"*">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_call_event'}
                     ,[{<<"error">>, <<"*">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_member_message'}
                     ,[{<<"member">>, <<"*">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_agent_message'}
                     ,[{<<"agent">>, <<"*">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_config_change'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), kz_json:object()) -> kz_term:startlink_ret().
start_link(Supervisor, AgentJObj) ->
    AgentId = kz_doc:id(AgentJObj),
    AccountId = account_id(AgentJObj),
    Queues = kz_json:get_value(<<"queues">>, AgentJObj, []),
    start_link(Supervisor, AgentJObj, AccountId, AgentId, Queues).

-spec start_link(pid(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:startlink_ret().
start_link(Supervisor, AgentJObj, AccountId, AgentId, Queues) ->
    lager:debug("start bindings for ~s(~s) in ready", [AccountId, AgentId]),
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, AgentId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Supervisor, AgentJObj, Queues]
                           ).

-spec start_link(pid(), kapps_call:call(), kz_term:ne_binary()) -> kz_term:startlink_ret().
start_link(Supervisor, ThiefCall, QueueId) ->
    AgentId = kapps_call:owner_id(ThiefCall),
    AccountId = kapps_call:account_id(ThiefCall),

    lager:debug("starting thief agent ~s(~s)", [AgentId, AccountId]),
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, AgentId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Supervisor, ThiefCall, [QueueId]]
                           ).

-spec stop(pid()) -> 'ok'.
stop(Srv) -> gen_listener:cast(Srv, {'stop_agent', self()}).

-spec member_connect_resp(pid(), kz_json:object()) -> 'ok'.
member_connect_resp(Srv, ReqJObj) ->
    gen_listener:cast(Srv, {'member_connect_resp', ReqJObj}).

-spec member_connect_retry(pid(), kz_json:object()) -> 'ok'.
member_connect_retry(Srv, WinJObj) ->
    gen_listener:cast(Srv, {'member_connect_retry', WinJObj}).

-spec agent_timeout(pid()) -> 'ok'.
agent_timeout(Srv) -> gen_listener:cast(Srv, 'agent_timeout').

-spec member_connect_accepted(pid()) -> 'ok'.
member_connect_accepted(Srv) ->
    gen_listener:cast(Srv, {'member_connect_accepted'}).

-spec member_connect_accepted(pid(), kz_term:ne_binary()) -> 'ok'.
member_connect_accepted(Srv, ACallId) ->
    gen_listener:cast(Srv, {'member_connect_accepted', ACallId}).

-spec member_connect_accepted(pid(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
member_connect_accepted(Srv, ACallId, MemberCall) ->
    gen_listener:cast(Srv, {'member_connect_accepted', ACallId, MemberCall}).

-spec monitor_connect_accepted(pid(), kz_term:ne_binary()) -> 'ok'.
monitor_connect_accepted(Srv, ACallId) ->
    gen_listener:cast(Srv, {'monitor_connect_accepted', ACallId}).

-spec member_callback_accepted(pid(), kapps_call:call()) -> 'ok'.
member_callback_accepted(Srv, ACall) ->
    gen_listener:cast(Srv, {'member_callback_accepted', ACall}).

-spec hangup_call(pid()) -> 'ok'.
hangup_call(Srv) ->
    gen_listener:cast(Srv, {'hangup_call'}).

-spec monitor_call(pid(), kapps_call:call(), kz_json:object(), kz_term:api_binary()) ->
          'ok'.
monitor_call(Srv, Call, WinJObj, RecordingUrl) ->
    gen_listener:cast(Srv, {'monitor_call', Call, WinJObj, RecordingUrl}).

-spec bridge_to_member(pid(), kapps_call:call(), kz_json:object()
                      ,kz_json:objects(), kz_term:api_binary(), kz_term:api_binary()
                      ) -> 'ok'.
bridge_to_member(Srv, Call, WinJObj, EPs, CDRUrl, RecordingUrl) ->
    gen_listener:cast(Srv, {'bridge_to_member', Call, WinJObj, EPs, CDRUrl, RecordingUrl}).

-spec originate_callback_to_agent(pid(), kapps_call:call(), kz_json:object()
                                 ,kz_json:objects(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()
                                 ) -> 'ok'.
originate_callback_to_agent(Srv, Call, WinJObj, EPs, CDRUrl, RecordingUrl, Number) ->
    gen_listener:cast(Srv, {'originate_callback_to_agent', Call, WinJObj, EPs, CDRUrl, RecordingUrl, Number}).

-spec originate_callback_return(pid(), kapps_call:call()) -> kz_term:ne_binary().
originate_callback_return(Srv, Call) ->
    gen_listener:call(Srv, {'originate_callback_return', Call}).

-spec channel_hungup(pid(), kz_term:ne_binary()) -> 'ok'.
channel_hungup(Srv, CallId) ->
    gen_listener:cast(Srv, {'channel_hungup', CallId}).

-spec unbind_from_events(pid(), kz_term:ne_binary()) -> 'ok'.
unbind_from_events(Srv, CallId) ->
    gen_listener:cast(Srv, {'unbind_from_events', CallId}).

-spec rebind_events(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
rebind_events(Srv, OldCallId, NewCallId) ->
    gen_listener:cast(Srv, {'rebind_events', OldCallId, NewCallId}).

-spec originate_execute(pid(), kz_json:object()) -> 'ok'.
originate_execute(Srv, JObj) ->
    gen_listener:cast(Srv, {'originate_execute', JObj}).

-spec originate_uuid(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
originate_uuid(Srv, UUID, CtlQ) ->
    gen_listener:cast(Srv, {'originate_uuid', UUID, CtlQ}).

-spec outbound_call(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
outbound_call(Srv, CallId, Number, Name) ->
    gen_listener:cast(Srv, {'outbound_call', CallId, Number, Name}).

-spec inbound_call(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
inbound_call(Srv, CallId, Number, Name) ->
    gen_listener:cast(Srv, {'inbound_call', CallId, Number, Name}).

-spec send_agent_available(pid()) -> 'ok'.
send_agent_available(Srv) ->
    gen_listener:cast(Srv, 'send_agent_available').

-spec send_agent_busy(pid()) -> 'ok'.
send_agent_busy(Srv) ->
    gen_listener:cast(Srv, 'send_agent_busy').

-spec send_sync_req(pid()) -> 'ok'.
send_sync_req(Srv) -> gen_listener:cast(Srv, {'send_sync_req'}).

-spec send_sync_resp(pid(), kz_term:text(), kz_json:object()) -> 'ok'.
send_sync_resp(Srv, Status, ReqJObj) -> send_sync_resp(Srv, Status, ReqJObj, []).

-spec send_sync_resp(pid(), kz_term:text(), kz_json:object(), kz_term:proplist()) -> 'ok'.
send_sync_resp(Srv, Status, ReqJObj, Options) ->
    gen_listener:cast(Srv, {'send_sync_resp', Status, ReqJObj, Options}).

-spec config(pid()) -> config().
config(Srv) -> gen_listener:call(Srv, 'config').

-spec refresh_config(pid(), kz_json:object(), fsm_state_name()) -> 'ok'.
refresh_config(Srv, JObj, StateName) ->
    gen_listener:cast(Srv, {'refresh_config', JObj, StateName}).

-spec agent_info(pid(), kz_json:path()) -> kz_json:api_json_term().
agent_info(Srv, Field) -> gen_listener:call(Srv, {'agent_info', Field}).

-spec send_status_resume(pid()) -> 'ok'.
send_status_resume(Srv) ->
    gen_listener:cast(Srv, {'send_status_update', 'resume'}).

-spec add_acdc_queue(pid(), kz_term:ne_binary(), fsm_state_name()) -> 'ok'.
add_acdc_queue(Srv, Q, StateName) ->
    gen_listener:cast(Srv, {'add_acdc_queue', Q, StateName}).

-spec rm_acdc_queue(pid(), kz_term:ne_binary()) -> 'ok'.
rm_acdc_queue(Srv, Q) ->
    gen_listener:cast(Srv, {'rm_acdc_queue', Q}).

-spec call_status_req(pid()) -> 'ok'.
call_status_req(Srv) ->
    gen_listener:cast(Srv, 'call_status_req').

-spec call_status_req(pid(), kz_term:ne_binary()) -> 'ok'.
call_status_req(Srv, CallId) ->
    gen_listener:cast(Srv, {'call_status_req', CallId}).

-spec fsm_started(pid(), pid()) -> 'ok'.
fsm_started(Srv, FSM) ->
    gen_listener:cast(Srv, {'fsm_started', FSM}).

-spec add_endpoint_bindings(pid(), kz_term:ne_binary(), api_kz_term:ne_binary()) -> 'ok'.
add_endpoint_bindings(_Srv, _Realm, 'undefined') ->
    lager:debug("ignoring adding endpoint bindings for undefined user @ ~s", [_Realm]);
add_endpoint_bindings(Srv, Realm, User) ->
    lager:debug("adding route bindings to ~p for endpoint ~s@~s", [Srv, User, Realm]),
    gen_listener:add_binding(Srv, 'route', [{'realm', Realm}
                                           ,{'user', User}
                                           ]).

-spec remove_endpoint_bindings(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
remove_endpoint_bindings(Srv, Realm, User) ->
    lager:debug("removing route bindings to ~p for endpoint ~s@~s", [Srv, User, Realm]),
    gen_listener:rm_binding(Srv, 'route', [{'realm', Realm}
                                          ,{'user', User}
                                          ]).

-spec remove_cdr_urls(pid(), kz_term:ne_binary()) -> 'ok'.
remove_cdr_urls(Srv, CallId) -> gen_listener:cast(Srv, {'remove_cdr_urls', CallId}).

-spec logout_agent(pid()) -> 'ok'.
logout_agent(Srv) -> gen_listener:cast(Srv, 'logout_agent').

-spec maybe_update_presence_id(pid(), api_kz_term:ne_binary()) -> 'ok'.
maybe_update_presence_id(_Srv, 'undefined') -> 'ok';
maybe_update_presence_id(Srv, Id) ->
    gen_listener:cast(Srv, {'presence_id', Id}).

-spec maybe_update_presence_state(pid(), api_kz_term:ne_binary()) -> 'ok'.
maybe_update_presence_state(_Srv, 'undefined') -> 'ok';
maybe_update_presence_state(Srv, State) ->
    presence_update(Srv, State).

-spec presence_update(pid(), api_kz_term:ne_binary()) -> 'ok'.
presence_update(_, 'undefined') -> 'ok';
presence_update(Srv, PresenceState) ->
    gen_listener:cast(Srv, {'presence_update', PresenceState}).

-spec update_agent_status(pid(), kz_term:ne_binary()) -> 'ok'.
update_agent_status(Srv, Status) ->
    gen_listener:cast(Srv, {'update_status', Status}).

-spec presence_id(pid()) -> kz_term:api_binary().
presence_id(Srv) ->
    gen_listener:call(Srv, 'presence_id').

-spec queues(pid()) -> kz_term:ne_binaries().
queues(Srv) ->
    gen_listener:call(Srv, 'queues').

-spec id(pid()) -> kz_term:api_binary().
id(Srv) ->
    gen_listener:call(Srv, 'my_id').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | agent() | kz_term:ne_binaries()]) -> {'ok', state()}.
init([Supervisor, Agent, Queues]) ->
    AgentId = agent_id(Agent),
    kz_log:put_callid(AgentId),
    lager:debug("starting acdc agent listener"),

    {'ok', #state{agent_id=AgentId
                 ,agent_priority=acdc_agent_util:agent_priority(Agent)
                 ,skills=kz_json:get_list_value(<<"acdc_skills">>, Agent, [])
                 ,acct_id=account_id(Agent)
                 ,acct_db=account_db(Agent)
                 ,my_id=acdc_util:proc_id()
                 ,supervisor=Supervisor
                 ,record_calls=record_calls(Agent)
                 ,is_thief=is_thief(Agent)
                 ,agent=Agent
                 ,agent_queues=Queues
                 ,agent_presence_id=AgentId
                 }}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_term:handle_call_ret_state(state()).
handle_call({'originate_callback_return', Call}, _, #state{my_q=MyQ}=State) ->
    MemberCallId = do_originate_callback_return(MyQ, Call),
    {'reply', MemberCallId, State};
handle_call('last_connect', _, #state{last_connect=LastConnect}=State) ->
    {'reply', LastConnect, State, 'hibernate'};
handle_call('presence_id', _, #state{agent_presence_id=PresenceId}=State) ->
    {'reply', PresenceId, State, 'hibernate'};
handle_call('queues', _, #state{agent_queues=Queues}=State) ->
    {'reply', Queues, State, 'hibernate'};
handle_call('my_id', _, #state{agent_id=AgentId}=State) ->
    {'reply', AgentId, State, 'hibernate'};
handle_call({'agent_info', Field}, _, #state{agent=Agent}=State) ->
    {'reply', kz_json:get_value(Field, Agent), State};
handle_call('config', _From, #state{acct_id=AccountId
                                   ,agent_id=AgentId
                                   ,my_q=Q
                                   }=State) ->
    {'reply', {AccountId, AgentId, Q}, State};
handle_call(_Request, _From, #state{}=State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Request]),
    {'reply', {'error', 'unhandled_call'}, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_term:handle_cast_ret_state(state()).
handle_cast({'refresh_config', JObj, StateName}, #state{agent_priority=Priority0
                                                       ,skills=Skills0
                                                       ,agent_queues=Queues
                                                       }=State) ->
    Qs = kz_json:get_list_value(<<"queues">>, JObj, []),
    Priority = acdc_agent_util:agent_priority(JObj),
    Skills = kz_json:get_list_value(<<"acdc_skills">>, JObj, []),

    {Add, Rm} = case is_prio_or_skills_updated(Priority0, Skills0, Priority, Skills) of
                    'true' ->
                        {_, Rm1} = acdc_agent_util:changed(Queues, Qs),
                        %% If true, all agent's resulting queues must be updated
                        Add1 = lists:subtract(Queues, Rm1),
                        lists:foreach(fun(Queue) ->
                                              lager:debug("prio/skills update for queue ~s", [Queue])
                                      end, Queues),
                        {Add1, Rm1};
                    'false' -> acdc_agent_util:changed(Queues, Qs)
                end,

    Self = self(),
    _ = [gen_listener:cast(Self, {'add_acdc_queue', A, StateName}) || A <- Add],
    _ = [gen_listener:cast(Self, {'rm_acdc_queue', R}) || R <- Rm],
    {'noreply', State#state{agent_priority=Priority
                           ,skills=Skills
                           }};
handle_cast({'stop_agent', Req}, #state{supervisor=Supervisor}=State) ->
    lager:debug("stop agent requested by ~p", [Req]),
    _ = kz_process:spawn(fun acdc_agent_sup:stop/1, [Supervisor]),
    {'noreply', State};

handle_cast({'fsm_started', FSMPid}, State) ->
    lager:debug("fsm started: ~p", [FSMPid]),
    handle_fsm_started(FSMPid),
    {'noreply', State#state{fsm_pid=FSMPid
                           ,my_id=acdc_util:proc_id(FSMPid)
                           }};

handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{my_q=Q}, 'hibernate'};

handle_cast({'add_acdc_queue', Q, StateName}, #state{agent_queues=Qs}=State) when is_binary(Q) ->
    case lists:member(Q, Qs) of
        'true' ->
            lager:debug("queue ~s already added", [Q]),
            send_availability_update(Q, StateName, State),
            {'noreply', State};
        'false' ->
            add_queue_binding(Q, StateName, State),
            {'noreply', State#state{agent_queues=[Q|Qs]}}
    end;

handle_cast({'rm_acdc_queue', Q}, #state{agent_queues=[Q]
                                        ,acct_id=AccountId
                                        ,agent_id=AgentId
                                        ,fsm_pid=FSM
                                        }=State) ->
    lager:debug("agent logged out of last known queue ~s, logging out", [Q]),
    rm_queue_binding(AccountId, AgentId, Q),
    acdc_agent_fsm:agent_logout(FSM),
    {'noreply', State#state{agent_queues=[]}};
handle_cast({'rm_acdc_queue', Q}, #state{agent_queues=Qs
                                        ,acct_id=AccountId
                                        ,agent_id=AgentId
                                        }=State) ->
    case lists:member(Q, Qs) of
        'true' ->
            rm_queue_binding(AccountId, AgentId, Q),
            {'noreply', State#state{agent_queues=lists:delete(Q, Qs)}, 'hibernate'};
        'false' ->
            lager:debug("not logged into queue ~s", [Q]),
            {'noreply', State}
    end;

handle_cast('bind_to_member_reqs', #state{agent_queues=Qs}=State) ->
    _ = [add_queue_binding(Q, 'ready', State) || Q <- Qs],
    {'noreply', State};

handle_cast({'rebind_events', OldCallId, NewCallId}, State) ->
    acdc_util:unbind_from_call_events(OldCallId),
    acdc_util:bind_to_call_events(NewCallId),
    {'noreply', State};

handle_cast({'unbind_from_events', CallId}, State) ->
    acdc_util:unbind_from_call_events(CallId),
    {'noreply', State};

handle_cast({'channel_hungup', CallId}, #state{call=Call
                                              ,is_thief=IsThief
                                              ,agent_call_ids=ACallIds
                                              ,agent_id=AgentId
                                              }=State) ->
    CCallId = call_id(Call),
    case CallId of
        CCallId ->
            lager:debug("member channel hungup, done with this call"),
            acdc_util:unbind_from_call_events(Call),

            ACallIds1 = filter_agent_calls(ACallIds, CallId),

            kz_log:put_callid(AgentId),
            case IsThief of
                'false' ->
                    {'noreply', State#state{call='undefined'
                                           ,original_call='undefined'
                                           ,msg_queue_id='undefined'
                                           ,acdc_queue_id='undefined'
                                           ,agent_call_ids=ACallIds1
                                           ,recording_url='undefined'
                                           ,last_connect=os:timestamp()
                                           }
                    ,'hibernate'};
                'true' ->
                    lager:debug("thief is done, going down"),
                    stop(self()),
                    {'noreply', State}
            end;
        _ ->
            case props:get_value(CallId, ACallIds) of
                'true' ->
                    lager:debug("agent channel ~s hungup/needs hanging up", [CallId]),
                    acdc_util:unbind_from_call_events(CallId),
                    {'noreply', State#state{agent_call_ids=lists:delete(CallId, ACallIds)}, 'hibernate'};
                'undefined' ->
                    lager:debug("unknown call id ~s for channel_hungup, ignoring", [CallId]),
                    lager:debug("listening for call id(~s) and agents (~p)", [CCallId, ACallIds]),
                    {'noreply', State};
                CtrlQ ->
                    lager:debug("agent channel ~s hungup, stop call on ctlq ~s", [CallId, CtrlQ]),
                    acdc_util:unbind_from_call_events(CallId),
                    stop_agent_leg(CallId, CtrlQ),
                    {'noreply', State#state{agent_call_ids=props:delete(CallId, ACallIds)}}
            end
    end;

handle_cast('agent_timeout', #state{agent_call_ids=ACallIds
                                   ,agent_id=AgentId
                                   }=State) ->
    lager:debug("agent timeout recv, stopping agent call"),

    ACallIds1 = filter_agent_calls(ACallIds, AgentId),

    kz_log:put_callid(AgentId),
    {'noreply', State#state{msg_queue_id='undefined'
                           ,acdc_queue_id='undefined'
                           ,agent_call_ids=ACallIds1
                           ,call='undefined'
                           }
    ,'hibernate'};
handle_cast({'member_connect_retry', CallId}, #state{my_id=MyId
                                                    ,msg_queue_id=Server
                                                    ,agent_call_ids=ACallIds
                                                    ,call=Call
                                                    ,agent_id=AgentId
                                                    }=State) when is_binary(CallId) ->
    case catch kapps_call:call_id(Call) of
        CallId ->
            lager:debug("need to retry member connect, agent isn't able to take it"),
            send_member_connect_retry(Server, CallId, MyId, AgentId),

            ACallIds1 = filter_agent_calls(ACallIds, AgentId),
            acdc_util:unbind_from_call_events(CallId),

            kz_log:put_callid(AgentId),

            {'noreply', State#state{original_call='undefined'
                                   ,msg_queue_id='undefined'
                                   ,acdc_queue_id='undefined'
                                   ,agent_call_ids=ACallIds1
                                   ,call='undefined'
                                   }
            ,'hibernate'
            };
        _MCallId ->
            lager:debug("retry call id(~s) is not our member call id ~p, ignoring", [CallId, _MCallId]),
            {'noreply', State}
    end;
handle_cast({'member_connect_retry', WinJObj}, #state{my_id=MyId
                                                     ,agent_id=AgentId
                                                     }=State) ->
    lager:debug("cannot process this win, sending a retry: ~s", [call_id(WinJObj)]),
    send_member_connect_retry(WinJObj, MyId, AgentId),
    {'noreply', State};

handle_cast({'bridge_to_member', Call, WinJObj, EPs, CDRUrl, RecordingUrl}, #state{is_thief='false'
                                                                                  ,agent_queues=Qs
                                                                                  ,acct_id=AccountId
                                                                                  ,agent_id=AgentId
                                                                                  ,my_q=MyQ
                                                                                  ,agent_call_ids=ACallIds
                                                                                  ,cdr_urls=Urls
                                                                                  ,agent=Agent
                                                                                  }=State) ->
    _ = kapps_call:put_callid(Call),
    lager:debug("bridging to agent endpoints"),

    RingTimeout = kz_json:get_value(<<"Ring-Timeout">>, WinJObj),
    lager:debug("ring agent for ~ps", [RingTimeout]),

    ShouldRecord = should_record_endpoints(EPs, record_calls(Agent)
                                          ,kz_json:is_true(<<"Record-Caller">>, WinJObj, 'false')
                                          ),

    AgentCallIds = lists:append(maybe_connect_to_agent(MyQ, EPs, Call, RingTimeout, AgentId, CDRUrl)
                               ,ACallIds),

    gen_listener:add_binding(self(), 'acdc_agent', [{'callid', call_id(Call)}
                                                   ,{'restrict_to', ['stats_req']}
                                                   ]),

    lager:debug("originate sent, waiting on successful bridge now"),
    update_my_queues_of_change(AccountId, AgentId, Call, Qs),
    {'noreply', State#state{call=Call
                           ,acdc_queue_id=kz_json:get_value(<<"Queue-ID">>, WinJObj)
                           ,record_calls=ShouldRecord
                           ,msg_queue_id=kz_json:get_value(<<"Server-ID">>, WinJObj)
                           ,agent_call_ids=AgentCallIds
                           ,cdr_urls=dict:store(kapps_call:call_id(Call)
                                               ,CDRUrl
                                               ,dict:store(AgentCallIds, CDRUrl, Urls)
                                               )
                           ,recording_url=RecordingUrl
                           }
    ,'hibernate'};

handle_cast({'bridge_to_member', Call, WinJObj, _, CDRUrl, RecordingUrl}, #state{is_thief='true'
                                                                                ,agent=Agent
                                                                                ,agent_id=AgentId
                                                                                ,agent_call_ids=ACallIds
                                                                                ,cdr_urls=Urls
                                                                                }=State) ->
    _ = kapps_call:put_callid(Call),
    lager:debug("connecting to thief at ~s", [kapps_call:call_id(Agent)]),
    acdc_util:bind_to_call_events(Call),

    AgentCallId = outbound_call_id(Call, AgentId),
    acdc_util:bind_to_call_events(AgentCallId),

    ShouldRecord = record_calls(Agent)
        orelse kz_json:is_true(<<"Record-Caller">>, WinJObj, 'false'),

    kapps_call_command:pickup(kapps_call:call_id(Agent), <<"now">>, Call),

    {'noreply', State#state{call=Call
                           ,acdc_queue_id=kz_json:get_value(<<"Queue-ID">>, WinJObj)
                           ,msg_queue_id=kz_json:get_value(<<"Server-ID">>, WinJObj)
                           ,agent_call_ids=[AgentCallId | ACallIds]
                           ,cdr_urls=dict:store(kapps_call:call_id(Call)
                                               ,CDRUrl
                                               ,dict:store(AgentCallId, CDRUrl, Urls)
                                               )
                           ,record_calls=ShouldRecord
                           ,recording_url=RecordingUrl
                           }
    ,'hibernate'};

handle_cast({'monitor_call', Call, WinJObj, RecordingUrl}, State) ->
    _ = kapps_call:put_callid(Call),

    lager:debug("monitoring member call ~s", [kapps_call:call_id(Call)]),

    {'noreply', State#state{call=Call
                           ,acdc_queue_id=kz_json:get_value(<<"Queue-ID">>, WinJObj)
                           ,msg_queue_id=kz_json:get_value(<<"Server-ID">>, WinJObj)
                           ,recording_url=RecordingUrl
                           }
    ,'hibernate'};

handle_cast({'originate_callback_to_agent', Call, WinJObj, EPs, CDRUrl, RecordingUrl, Number}, #state{agent_queues=Qs
                                                                                                     ,acct_id=AccountId
                                                                                                     ,agent_id=AgentId
                                                                                                     ,my_q=MyQ
                                                                                                     ,agent_call_ids=ACallIds
                                                                                                     ,cdr_urls=Urls
                                                                                                     ,agent=Agent
                                                                                                     }=State) ->
    _ = kapps_call:put_callid(Call),
    lager:debug("calling agent to begin callback"),

    RingTimeout = kz_json:get_value(<<"Ring-Timeout">>, WinJObj),
    lager:debug("ring agent for ~ps", [RingTimeout]),

    ShouldRecord = should_record_endpoints(EPs, record_calls(Agent)
                                          ,kz_json:is_true(<<"Record-Caller">>, WinJObj, 'false')
                                          ),

    AgentCallIds = lists:append(maybe_originate_callback(MyQ, EPs, Call, RingTimeout, AgentId, CDRUrl, Number)
                               ,ACallIds),

    lager:debug("originate sent, waiting on bridge of agent and callback call"),
    update_my_queues_of_change(AccountId, AgentId, Call, Qs),
    {'noreply', State#state{call=Call
                           ,record_calls=ShouldRecord
                           ,acdc_queue_id=kz_json:get_value(<<"Queue-ID">>, WinJObj)
                           ,msg_queue_id=kz_json:get_value(<<"Server-ID">>, WinJObj)
                           ,agent_call_ids=AgentCallIds
                           ,cdr_urls=dict:store(kapps_call:call_id(Call)
                                               ,CDRUrl
                                               ,dict:store(AgentCallIds, CDRUrl, Urls)
                                               )
                           ,recording_url=RecordingUrl
                           }
    ,'hibernate'};

handle_cast({'member_connect_accepted'}, #state{msg_queue_id=AmqpQueue
                                               ,call=Call
                                               ,acct_id=AccountId
                                               ,agent_id=AgentId
                                               ,agent_queues=Qs
                                               ,my_id=MyId
                                               ,record_calls=ShouldRecord
                                               ,recording_url=RecordingUrl
                                               }=State) ->
    lager:debug("member bridged to agent! waiting on agent call id though"),
    maybe_start_recording(Call, ShouldRecord, RecordingUrl),

    send_member_connect_accepted(AmqpQueue, call_id(Call), AccountId, AgentId, MyId),
    _ = [send_agent_busy(AccountId, AgentId, QueueId, Call) || QueueId <- Qs],
    {'noreply', State};

handle_cast({'member_connect_accepted', ACallId}, #state{msg_queue_id=AmqpQueue
                                                        ,call=Call
                                                        ,acct_id=AccountId
                                                        ,agent_id=AgentId
                                                        ,agent_queues=Qs
                                                        ,my_id=MyId
                                                        ,record_calls=ShouldRecord
                                                        ,recording_url=RecordingUrl
                                                        ,agent_call_ids=ACallIds
                                                        }=State) ->
    lager:debug("member bridged to agent!"),
    maybe_start_recording(Call, ShouldRecord, RecordingUrl),

    ACallIds1 = filter_agent_calls(ACallIds, ACallId),

    lager:debug("new agent call ids: ~p", [ACallIds1]),

    send_member_connect_accepted(AmqpQueue, call_id(Call), AccountId, AgentId, MyId),
    _ = [send_agent_busy(AccountId, AgentId, QueueId, Call) || QueueId <- Qs],
    {'noreply', State#state{agent_call_ids=ACallIds1}, 'hibernate'};

handle_cast({'member_connect_accepted', ACallId, NewCall}, #state{msg_queue_id=AmqpQueue
                                                                 ,call=Call
                                                                 ,acct_id=AccountId
                                                                 ,agent_id=AgentId
                                                                 ,agent_queues=Qs
                                                                 ,my_id=MyId
                                                                 ,record_calls=ShouldRecord
                                                                 ,recording_url=RecordingUrl
                                                                 ,agent_call_ids=ACallIds
                                                                 }=State) ->
    lager:debug("member's new call bridged to agent!"),
    maybe_start_recording(NewCall, ShouldRecord, RecordingUrl),

    ACallIds1 = filter_agent_calls(ACallIds, ACallId),

    lager:debug("new agent call ids: ~p", [ACallIds1]),

    send_member_connect_accepted(AmqpQueue, call_id(Call), call_id(NewCall), AccountId, AgentId, MyId),
    _ = [send_agent_busy(AccountId, AgentId, QueueId, Call) || QueueId <- Qs],
    {'noreply', State#state{call=NewCall
                           ,original_call=Call
                           ,agent_call_ids=ACallIds1
                           }, 'hibernate'};

handle_cast({'monitor_connect_accepted', ACallId}, #state{agent_call_ids=ACallIds}=State) ->
    lager:debug("monitoring ~s", [ACallId]),
    {'noreply', State#state{agent_call_ids=[ACallId | ACallIds]}, 'hibernate'};

handle_cast({'member_callback_accepted', ACall}, #state{msg_queue_id=AmqpQueue
                                                       ,call=Call
                                                       ,acct_id=AccountId
                                                       ,agent_id=AgentId
                                                       ,my_id=MyId
                                                       ,agent_call_ids=ACallIds
                                                       }=State) ->
    lager:debug("agent answered callback, mark call as accepted"),

    ACallId = kapps_call:call_id(ACall),
    %%    ACallIds1 = filter_agent_calls(ACallIds, ACallId),
    ACallIds1 = [ACallId],

    lager:debug("new agent call ids: ~p", [ACallIds1]),

    send_member_callback_accepted(AmqpQueue, call_id(Call), AccountId, AgentId, MyId),

    ACall1 = kapps_call:set_control_queue(props:get_value(ACallId, ACallIds), ACall),
    kapps_call_command:prompt(<<"queue-now_calling_back">>, ACall1),

    {'noreply', State#state{agent_call_ids=ACallIds1}, 'hibernate'};

handle_cast({'member_connect_resp', ReqJObj}, #state{agent_id=AgentId
                                                    ,last_connect=LastConn
                                                    ,agent_queues=Qs
                                                    ,my_id=MyId
                                                    ,my_q=MyQ
                                                    }=State) ->
    ACDcQueue = kz_json:get_value(<<"Queue-ID">>, ReqJObj),
    case is_valid_queue(ACDcQueue, Qs) of
        'false' ->
            lager:debug("queue ~s isn't one of ours", [ACDcQueue]),
            {'noreply', State};
        'true' ->
            lager:debug("responding to member_connect_req"),

            send_member_connect_resp(ReqJObj, MyQ, AgentId, MyId, LastConn),
            {'noreply', State#state{msg_queue_id = kz_json:get_value(<<"Server-ID">>, ReqJObj)}
            ,'hibernate'}
    end;

handle_cast({'hangup_call'}, #state{my_id=MyId
                                   ,msg_queue_id=Server
                                   ,agent_call_ids=ACallIds
                                   ,call=Call
                                   ,agent_id=AgentId
                                   }=State) ->
    %% Hangup this agent's calls
    lager:debug("agent FSM requested a hangup of the agent call, sending retry"),
    ACallIds1 = filter_agent_calls(ACallIds, AgentId),

    %% Pass the call on to another agent
    CallId = kapps_call:call_id(Call),
    send_member_connect_retry(Server, CallId, MyId, AgentId),
    acdc_util:unbind_from_call_events(CallId),

    put('callid', AgentId),
    {'noreply', State#state{call='undefined'
                           ,msg_queue_id='undefined'
                           ,acdc_queue_id='undefined'
                           ,agent_call_ids=ACallIds1
                           ,recording_url='undefined'
                           }
    ,'hibernate'};

handle_cast({'originate_execute', JObj}, #state{my_q=Q}=State) ->
    lager:debug("execute the originate for agent: ~p", [JObj]),
    send_originate_execute(JObj, Q),
    {'noreply', State, 'hibernate'};

handle_cast({'originate_uuid', UUID, CtlQ}, #state{agent_call_ids=ACallIds}=State) ->
    lager:debug("updating ~s with ~s in ~p", [UUID, CtlQ, ACallIds]),
    {'noreply', State#state{agent_call_ids=props:set_value(UUID, CtlQ, ACallIds)}};

handle_cast({'outbound_call', CallId, Number, Name}, #state{agent_id=AgentId
                                                           ,acct_id=AccountId
                                                           ,agent_queues=Qs
                                                           }=State) ->
    _ = kz_log:put_callid(CallId),
    acdc_util:bind_to_call_events(CallId),
    Call = kapps_call:set_call_id(CallId, kapps_call:new()),
    _ = [send_agent_busy(AccountId, AgentId, QueueId, Call, 'outbound', Number, Name) || QueueId <- Qs],

    lager:debug("bound to agent's outbound call ~s", [CallId]),
    {'noreply', State#state{call=Call}, 'hibernate'};

handle_cast({'inbound_call', CallId, Number, Name}, #state{agent_id=AgentId
                                                          ,acct_id=AccountId
                                                          ,agent_queues=Qs
                                                          }=State) ->
    _ = kz_log:put_callid(CallId),
    acdc_util:bind_to_call_events(CallId),
    Call = kapps_call:set_call_id(CallId, kapps_call:new()),
    _ = [send_agent_busy(AccountId, AgentId, QueueId, Call, 'inbound', Number, Name) || QueueId <- Qs],

    lager:debug("bound to agent's outbound call ~s", [CallId]),
    {'noreply', State#state{call=Call}, 'hibernate'};

handle_cast('send_agent_available', #state{agent_id=AgentId
                                          ,agent_priority=Priority
                                          ,skills=Skills
                                          ,acct_id=AccountId
                                          ,agent_queues=Qs
                                          }=State) ->
    [send_agent_available(AccountId, AgentId, QueueId, Priority, Skills) || QueueId <- Qs],
    {'noreply', State};

handle_cast('send_agent_busy', #state{agent_id=AgentId
                                     ,acct_id=AccountId
                                     ,agent_queues=Qs
                                     }=State) ->
    [send_agent_busy(AccountId, AgentId, QueueId) || QueueId <- Qs],
    {'noreply', State};

handle_cast({'send_sync_req'}, #state{my_id=MyId
                                     ,my_q=MyQ
                                     ,acct_id=AccountId
                                     ,agent_id=AgentId
                                     }=State) ->
    _ = case MyQ of
            'undefined' ->
                lager:debug("queue not ready yet, waiting for sync request"),
                timer:apply_after(100 , 'gen_listener', 'cast', [self(), {'send_sync_req'}]);
            _ ->
                lager:debug("queue retrieved: ~p , sending sync request", [MyQ]),
                send_sync_request(AccountId, AgentId, MyId, MyQ)
        end,
    {'noreply', State};

handle_cast({'send_sync_resp', Status, ReqJObj, Options}, #state{my_id=MyId
                                                                ,acct_id=AccountId
                                                                ,agent_id=AgentId
                                                                ,my_q=MyQ
                                                                }=State) ->
    send_sync_response(ReqJObj, AccountId, AgentId, MyId, MyQ, Status, Options),
    {'noreply', State};

handle_cast({'send_status_update', Status}, #state{acct_id=AccountId
                                                  ,agent_id=AgentId
                                                  }=State) ->
    send_status_update(AccountId, AgentId, Status),
    {'noreply', State};

handle_cast('call_status_req', #state{call=Call, my_q=Q}=State) ->
    CallId = kapps_call:call_id(Call),

    Command = [{<<"Call-ID">>, CallId}
              ,{<<"Server-ID">>, Q}
               | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],

    kapi_call:publish_channel_status_req(CallId, Command),
    {'noreply', State};

handle_cast({'call_status_req', CallId}, #state{my_q=Q}=State) when is_binary(CallId) ->
    Command = [{<<"Call-ID">>, CallId}
              ,{<<"Server-ID">>, Q}
               | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    kapi_call:publish_channel_status_req(CallId, Command),
    {'noreply', State};
handle_cast({'call_status_req', Call}, State) ->
    handle_cast({'call_status_req', kapps_call:call_id(Call)}, State);

handle_cast({'remove_cdr_urls', CallId}, #state{cdr_urls=Urls}=State) ->
    {'noreply', State#state{cdr_urls=dict:erase(CallId, Urls)}, 'hibernate'};

handle_cast('logout_agent', #state{acct_id=AccountId
                                  ,agent_id=AgentId
                                  }=State) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AccountId}
               ,{<<"Agent-ID">>, AgentId}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),

    kapi_acdc_agent:publish_logout(Update),
    lager:debug("published agent logout message"),
    {'noreply', State};

handle_cast({'presence_id', _Id}, #state{agent_presence_id=_Id}=State) ->
    {'noreply', State};
handle_cast({'presence_id', PresenceId}, #state{agent_presence_id=_Id}=State) ->
    lager:debug("updating presence id from ~s to ~s", [_Id, PresenceId]),
    {'noreply', State#state{agent_presence_id=PresenceId}};

handle_cast({'presence_update', PresenceState}, #state{acct_id=AccountId
                                                      ,agent_presence_id='undefined'
                                                      ,agent_id=AgentId
                                                      }=State) ->
    lager:debug("no custom presence id, using ~s for ~s", [AgentId, PresenceState]),
    acdc_util:presence_update(AccountId, AgentId, PresenceState),
    {'noreply', State};
handle_cast({'presence_update', PresenceState}, #state{acct_id=AccountId
                                                      ,agent_presence_id=PresenceId
                                                      }=State) ->
    lager:debug("custom presence id, using ~s for ~s", [PresenceId, PresenceState]),
    acdc_util:presence_update(AccountId, PresenceId, PresenceState),
    {'noreply', State};

handle_cast({'update_status', Status}, #state{agent_id=AgentId
                                             ,acct_id=AccountId
                                             }=State) ->
    catch acdc_agent_util:update_status(AccountId, AgentId, Status),
    {'noreply', State};

handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, 'hibernate'}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_term:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all messages from the message bus
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{fsm_pid='undefined'}) -> 'ignore';
handle_event(_JObj, #state{fsm_pid=FSM
                          ,agent_id=AgentId
                          ,acct_id=AccountId
                          ,cdr_urls=Urls
                          ,agent_call_ids=AgentCallIds
                          }) ->
    {'reply', [{'fsm_pid', FSM}
              ,{'agent_id', AgentId}
              ,{'acct_id', AccountId}
              ,{'cdr_urls', Urls}
              ,{'agent_call_ids', AgentCallIds}
              ]}.

%%------------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(Reason, #state{agent_queues=Queues
                        ,acct_id=AccountId
                        ,agent_id=AgentId
                        }
         ) when Reason == 'normal'; Reason == 'shutdown' ->
    _ = [rm_queue_binding(AccountId, AgentId, QueueId) || QueueId <- Queues],
    lager:debug("agent process going down: ~p", [Reason]);
terminate(_Reason, _State) ->
    lager:debug("agent process going down: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_valid_queue(kz_term:ne_binary(), kz_term:ne_binaries()) -> boolean().
is_valid_queue(Q, Qs) -> lists:member(Q, Qs).

-spec send_member_connect_resp(kz_json:object(), kz_term:ne_binary()
                              ,kz_term:ne_binary(), kz_term:ne_binary()
                              , kz_term:kz_now() | 'undefined'
                              ) -> 'ok'.
send_member_connect_resp(JObj, MyQ, AgentId, MyId, LastConn) ->
    Queue = kz_json:get_value(<<"Server-ID">>, JObj),
    IdleTime = idle_time(LastConn),
    Resp = props:filter_undefined(
             [{<<"Agent-ID">>, AgentId}
             ,{<<"Idle-Time">>, IdleTime}
             ,{<<"Process-ID">>, MyId}
             ,{<<"Server-ID">>, MyQ}
              | kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending connect_resp to ~s for ~s: ~s", [Queue, call_id(JObj), MyId]),
    kapi_acdc_queue:publish_member_connect_resp(Queue, Resp).

-spec send_member_connect_retry(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_member_connect_retry(JObj, MyId, AgentId) ->
    send_member_connect_retry(kz_json:get_value(<<"Server-ID">>, JObj)
                             ,call_id(JObj)
                             ,MyId
                             ,AgentId
                             ).

-spec send_member_connect_retry(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_member_connect_retry('undefined', _, _, _) ->
    lager:debug("no queue to send the retry to, seems bad");
send_member_connect_retry(Queue, CallId, MyId, AgentId) ->
    Resp = props:filter_undefined(
             [{<<"Process-ID">>, MyId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Agent-ID">>, AgentId}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    %% Delay the retry by 0.5 secs to allow agents to settle after RING timeout
    timer:apply_after(2000, kapi_acdc_queue, publish_member_connect_retry, [Queue, Resp]).

-spec send_member_connect_accepted(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_member_connect_accepted(Queue, CallId, AccountId, AgentId, MyId) ->
    Resp = props:filter_undefined([{<<"Call-ID">>, CallId}
                                  ,{<<"Account-ID">>, AccountId}
                                  ,{<<"Agent-ID">>, AgentId}
                                  ,{<<"Process-ID">>, MyId}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    kapi_acdc_queue:publish_member_connect_accepted(Queue, Resp).

-spec send_member_connect_accepted(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_member_connect_accepted(Queue, CallId, NewCallId, AccountId, AgentId, MyId) ->
    Resp = props:filter_undefined([{<<"Call-ID">>, NewCallId}
                                  ,{<<"Account-ID">>, AccountId}
                                  ,{<<"Agent-ID">>, AgentId}
                                  ,{<<"Process-ID">>, MyId}
                                  ,{<<"Old-Call-ID">>, CallId}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    kapi_acdc_queue:publish_member_connect_accepted(Queue, Resp).

-spec send_member_callback_accepted(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_member_callback_accepted(Queue, CallId, AccountId, AgentId, MyId) ->
    Resp = props:filter_undefined([{<<"Call-ID">>, CallId}
                                  ,{<<"Account-ID">>, AccountId}
                                  ,{<<"Agent-ID">>, AgentId}
                                  ,{<<"Process-ID">>, MyId}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    kapi_acdc_queue:publish_member_callback_accepted(Queue, Resp).

-spec send_originate_execute(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, kz_json:get_value(<<"Call-ID">>, JObj)}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_dialplan:publish_originate_execute(kz_json:get_value(<<"Server-ID">>, JObj), Prop).

-spec send_sync_request(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_sync_request(AccountId, AgentId, MyId, MyQ) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Process-ID">>, MyId}
            | kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_agent:publish_sync_req(Prop).

send_sync_response(ReqJObj, AccountId, AgentId, MyId, MyQ, Status, Options) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Process-ID">>, MyId}
           ,{<<"Status">>, kz_term:to_binary(Status)}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, ReqJObj)}
            | Options ++ kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
           ],
    Q = kz_json:get_value(<<"Server-ID">>, ReqJObj),
    lager:debug("sending sync resp to ~s", [Q]),
    kapi_acdc_agent:publish_sync_resp(Q, Prop).

send_status_update(AccountId, AgentId, 'resume') ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AccountId}
               ,{<<"Agent-ID">>, AgentId}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    kapi_acdc_agent:publish_resume(Update).


-spec idle_time('undefined' | kz_term:kz_now()) -> kz_term:api_integer().
idle_time('undefined') -> 'undefined';
idle_time(T) -> kz_time:elapsed_s(T).

-spec call_id(kapps_call:call() | kz_term:api_object()) ->
          kz_term:api_binary().
call_id('undefined') -> 'undefined';
call_id(Call) ->
    case kapps_call:is_call(Call) of
        'true' -> kapps_call:call_id(Call);
        'false' ->
            Keys = [[<<"Call">>, <<"Call-ID">>]
                   ,[<<"Call">>, <<"call_id">>]
                   ,<<"Call-ID">>
                   ],
            lists:foldl(fun(K, 'undefined') -> kz_json:get_value(K, Call);
                           (_, CallId) -> CallId
                        end, 'undefined', Keys)
    end.

-spec maybe_connect_to_agent(kz_term:ne_binary(), kz_json:objects(), kapps_call:call(), kz_term:api_integer(), kz_term:ne_binary(), kz_term:api_binary()) ->
          kz_term:proplist().
maybe_connect_to_agent(MyQ, EPs, Call, Timeout, AgentId, _CdrUrl) ->
    MCallId = kapps_call:call_id(Call),
    kz_log:put_callid(MCallId),

    ReqId = kz_binary:rand_hex(6),
    AccountId = kapps_call:account_id(Call),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AccountId}
                                  ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
                                  ,{<<"Request-ID">>, ReqId}
                                  ,{<<"Retain-CID">>, <<"true">>}
                                  ,{<<"Agent-ID">>, AgentId}
                                  ,{<<"Member-Call-ID">>, MCallId}
                                  ]),

    {ACallIds, Endpoints} = lists:foldl(fun(EP, {Cs, Es}) ->
                                                ACallId = outbound_call_id(Call, AgentId),
                                                acdc_util:bind_to_call_events(ACallId),

                                                {[ACallId | Cs]
                                                ,[kz_json:set_values([{<<"Endpoint-Timeout">>, Timeout}
                                                                     ,{<<"Outbound-Call-ID">>, ACallId}
                                                                     ,{<<"Existing-Call-ID">>, kapps_call:call_id(Call)}
                                                                     ], EP)
                                                  | Es
                                                 ]}
                                        end, {[], []}, EPs),

    {CIDNumber, CIDName} = acdc_util:caller_id(Call),

    Prop = props:filter_undefined(
             [{<<"Msg-ID">>, kz_binary:rand_hex(6)}
             ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
             ,{<<"Timeout">>, Timeout}
             ,{<<"Endpoints">>, Endpoints}
             ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                 ,<<"Retain-CID">>
                                                 ,<<"Authorizing-ID">>
                                                 ,<<"Authorizing-Type">>
                                                 ]}
             ,{<<"Account-ID">>, AccountId}
             ,{<<"Resource-Type">>, <<"originate">>}
             ,{<<"Application-Name">>, <<"bridge">>}
             ,{<<"Caller-ID-Name">>, CIDName}
             ,{<<"Caller-ID-Number">>, CIDNumber}
             ,{<<"Outbound-Caller-ID-Name">>, CIDName}
             ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
             ,{<<"Existing-Call-ID">>, kapps_call:call_id(Call)}
             ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
             ,{<<"Ignore-Early-Media">>, <<"true">>}
              | kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),

    lager:debug("sending originate request with agent call-ids ~p", [ACallIds]),

    kapi_resource:publish_originate_req(Prop),
    lists:map(fun(ACallId) -> {ACallId, 'undefined'} end, ACallIds).

-spec maybe_originate_callback(kz_term:ne_binary(), kz_json:objects(), kapps_call:call(), kz_term:api_integer(), kz_term:ne_binary(), kz_term:api_binary()
                              ,kz_json:object()) ->
          kz_term:proplist().
maybe_originate_callback(MyQ, EPs, Call, Timeout, AgentId, _CdrUrl, Details) ->
    MCallId = kapps_call:call_id(Call),
    put('callid', MCallId),

    ReqId = kz_binary:rand_hex(6),
    AccountId = kapps_call:account_id(Call),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AccountId}
                                  ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
                                  ,{<<"Authorizing-Type">>, <<"user">>}
                                  ,{<<"Request-ID">>, ReqId}
                                  ,{<<"Retain-CID">>, <<"true">>}
                                  ,{<<"Agent-ID">>, AgentId}
                                  ,{<<"Member-Call-ID">>, MCallId}
                                  ,{<<"Callback-Number">>, kz_json:get_value(<<"Callback-Number">>, Details)}
                                  ]),

    {ACallIds, Endpoints} = lists:foldl(fun(EP, {Cs, Es}) ->
                                                ACallId = outbound_call_id(Call, AgentId),
                                                acdc_util:bind_to_call_events(ACallId),

                                                {[ACallId | Cs]
                                                ,[kz_json:set_values([{<<"Endpoint-Timeout">>, Timeout}
                                                                     ,{<<"Outbound-Call-ID">>, ACallId}
                                                                     ], EP)
                                                  | Es
                                                 ]}
                                        end, {[], []}, EPs),

    {CIDNumber, CIDName} = acdc_util:caller_id(Call),

    Prop = props:filter_undefined([{<<"Application-Name">>, <<"park">>}
                                  ,{<<"Resource-Type">>, <<"originate">>}
                                  ,{<<"Account-ID">>, AccountId}
                                  ,{<<"Endpoints">>, Endpoints}
                                  ,{<<"Msg-ID">>, kz_binary:rand_hex(6)}
                                  ,{<<"Timeout">>, Timeout}
                                  ,{<<"Ignore-Display-Updates">>, <<"true">>}
                                  ,{<<"Ignore-Early-Media">>, <<"true">>}
                                  ,{<<"Caller-ID-Name">>, CIDName}
                                  ,{<<"Caller-ID-Number">>, CIDNumber}
                                  ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                                  ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
                                  ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
                                  ,{<<"Continue-On-Fail">>, 'false'}
                                  ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                                  ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                                      ,<<"Retain-CID">>
                                                                      ,<<"Authorizing-ID">>
                                                                      ,<<"Authorizing-Type">>
                                                                      ,<<"Callback-Number">>
                                                                      ]}
                                  ,{<<"Originate-Immediate">>, <<"true">>}
                                   | kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
                                  ]),

    lager:debug("sending originate request with agent call-ids ~p", [ACallIds]),

    kapi_resource:publish_originate_req(Prop),
    lists:map(fun(ACallId) -> {ACallId, 'undefined'} end, ACallIds).

-spec outbound_call_id(kapps_call:call() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
outbound_call_id(CallId, AgentId) when is_binary(CallId) ->
    Hash = kz_term:to_hex_binary(erlang:md5(CallId)),
    Rnd = kz_binary:rand_hex(4),
    <<Hash/binary, "-", AgentId/binary, "-", Rnd/binary>>;
outbound_call_id(Call, AgentId) ->
    outbound_call_id(kapps_call:call_id(Call), AgentId).

%%------------------------------------------------------------------------------
%% @doc Complete a callback to the callback number (in CCV)
%% Returns a target call id that has been hooked for events
%% @end
%%------------------------------------------------------------------------------
-spec do_originate_callback_return(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
do_originate_callback_return(MyQ, Call) ->
    MsgId = kz_binary:rand_hex(4),

    Extension = kapps_call:custom_channel_var(<<"Callback-Number">>, Call),
    TransferorLeg = kapps_call:call_id(Call),
    FromUser = kapps_call:to_user(Call),

    CCVs = props:filter_undefined(
             [{<<"Account-ID">>, kapps_call:account_id(Call)}
             ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
             ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
             ,{<<"Channel-Authorized">>, 'true'}
             ,{<<"From-URI">>, <<FromUser/binary, "@", (kapps_call:account_realm(Call))/binary>>}
             ,{<<"Retain-CID">>, 'true'}
             ]),

    TargetCallId = create_call_id(),
    acdc_util:bind_to_call_events(TargetCallId),

    Endpoint = kz_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                   ,{<<"Route">>,  Extension}
                   ,{<<"To-DID">>, Extension}
                   ,{<<"To-Realm">>, kapps_call:account_realm(Call)}
                   ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                   ,{<<"Outbound-Call-ID">>, TargetCallId}
                   ,{<<"Existing-Call-ID">>, TransferorLeg}
                   ])),

    Request = props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                ,{<<"Outbound-Call-ID">>, TargetCallId}
                ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                ,{<<"Msg-ID">>, MsgId}
                ,{<<"Continue-On-Fail">>, 'true'}
                ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>
                                                    ,<<"Authorizing-Type">>, <<"Authorizing-ID">>
                                                    ,<<"Channel-Authorized">>
                                                    ]}
                ,{<<"Application-Name">>, <<"bridge">>}
                ,{<<"Timeout">>, 60}

                ,{<<"Outbound-Caller-ID-Name">>, kapps_call:callee_id_number(Call)}
                ,{<<"Outbound-Caller-ID-Number">>, kapps_call:callee_id_number(Call)}
                ,{<<"Caller-ID-Name">>, kapps_call:callee_id_number(Call)}
                ,{<<"Caller-ID-Number">>, kapps_call:callee_id_number(Call)}

                ,{<<"Existing-Call-ID">>, TransferorLeg}
                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Originate-Immediate">>, 'true'}
                 | kz_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
                ]),

    kapi_resource:publish_originate_req(Request),
    TargetCallId.

-spec create_call_id() -> kz_term:ne_binary().
create_call_id() ->
    <<"callback-", (kz_binary:rand_hex(4))/binary>>.

-spec add_queue_binding(kz_term:ne_binary(), fsm_state_name(), state()) -> 'ok'.
add_queue_binding(QueueId, StateName, #state{acct_id=AccountId}=State) ->
    lager:debug("adding queue binding for ~s", [QueueId]),
    gen_listener:add_binding(self()
                            ,'acdc_queue'
                            ,[{'restrict_to', ['member_connect_req']}
                             ,{'queue_id', QueueId}
                             ,{'account_id', AccountId}
                             ]),
    send_availability_update(QueueId, StateName, State).

-spec rm_queue_binding(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
rm_queue_binding(AccountId, AgentId, QueueId) ->
    lager:debug("removing queue binding for ~s", [QueueId]),
    gen_listener:rm_binding(self()
                           ,'acdc_queue'
                           ,[{'restrict_to', ['member_connect_req']}
                            ,{'queue_id', QueueId}
                            ,{'account_id', AccountId}
                            ]),
    send_agent_unavailable(AccountId, AgentId, QueueId).

-spec send_availability_update(kz_term:ne_binary(), fsm_state_name(), state()) -> 'ok'.
send_availability_update(QueueId, 'ready', #state{agent_id=AgentId
                                                 ,agent_priority=Priority
                                                 ,skills=Skills
                                                 ,acct_id=AccountId
                                                 }) ->
    send_agent_available(AccountId, AgentId, QueueId, Priority, Skills);
send_availability_update(QueueId, _, #state{agent_id=AgentId
                                           ,acct_id=AccountId
                                           }) ->
    send_agent_busy(AccountId, AgentId, QueueId).

-spec send_agent_available(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), agent_priority(), kz_term:ne_binaries()) -> 'ok'.
send_agent_available(AccountId, AgentId, QueueId, Priority, Skills) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Priority">>, Priority}
           ,{<<"Skills">>, Skills}
           ,{<<"Change">>, <<"available">>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_agent_change(Prop).

-spec send_agent_busy(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_agent_busy(AccountId, AgentId, QueueId) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Change">>, <<"busy">>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_agent_change(Prop).

send_agent_busy(AccountId, AgentId, QueueId, Call) ->
    Direction = kapps_call:direction(Call),
    {CIDNumber, CIDName} = acdc_util:caller_id(Call),
    send_agent_busy(AccountId, AgentId, QueueId, Call, Direction, CIDNumber, CIDName).

send_agent_busy(AccountId, AgentId, QueueId, _Call, <<"outbound">>, Number, Name) ->
    send_agent_busy(AccountId, AgentId, QueueId, _Call, 'outbound', Number, Name);
send_agent_busy(AccountId, AgentId, QueueId, _Call, 'outbound', Number, Name) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Change">>, <<"busy">>}
           ,{<<"Call-Direction">>, <<"outbound">>}
           ,{<<"Callee-ID-Number">>, Number}
           ,{<<"Callee-ID-Name">>, Name}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_agent_change(Prop);
send_agent_busy(AccountId, AgentId, QueueId, _Call, <<"inbound">>, Number, Name) ->
    send_agent_busy(AccountId, AgentId, QueueId, _Call, 'inbound', Number, Name);
send_agent_busy(AccountId, AgentId, QueueId, _Call, 'inbound', Number, Name) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Change">>, <<"busy">>}
           ,{<<"Call-Direction">>, <<"inbound">>}
           ,{<<"Caller-ID-Number">>, Number}
           ,{<<"Caller-ID-Name">>, Name}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_agent_change(Prop).


-spec send_agent_unavailable(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_agent_unavailable(AccountId, AgentId, QueueId) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Agent-ID">>, AgentId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Change">>, <<"unavailable">>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_agent_change(Prop).

update_my_queues_of_change(AccountId, AgentId, Call, Qs) ->
    {CIDNumber, CIDName} = acdc_util:caller_id(Call),
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Caller-ID-Number">>, CIDNumber}
            ,{<<"Caller-ID-Name">>, CIDName}
            ,{<<"Change">>, <<"ringing">>}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = [kapi_acdc_queue:publish_agent_change([{<<"Queue-ID">>, QueueId} | Props])
         || QueueId <- Qs
        ],
    'ok'.

-spec is_prio_or_skills_updated(agent_priority(), kz_term:ne_binaries(), agent_priority(), kz_term:ne_binaries()) ->
          boolean().
is_prio_or_skills_updated(Priority0, Skills0, Priority, Skills) ->
    Priority0 =/= Priority
        orelse Skills0 =/= Skills.

-spec should_record_endpoints(kz_json:objects(), boolean(), kz_term:api_boolean()) -> boolean().
should_record_endpoints(_EPs, 'true', _) -> 'true';
should_record_endpoints(_EPs, 'false', 'true') -> 'true';
should_record_endpoints(EPs, _, _) ->
    lists:any(fun(EP) ->
                      kz_json:is_true(<<"record_calls">>, EP, 'false')
              end, EPs).

-spec maybe_start_recording(kapps_call:call(), boolean(), kz_term:ne_binary()) -> 'ok'.
maybe_start_recording(_Call, 'false', _) ->
    lager:debug("not recording this call");
maybe_start_recording(Call, 'true', Url) ->
    RecordingJObj =
        kz_json:from_list(
          [{<<"format">>, recording_format()}
          ,{<<"url">>, Url}
          ]),
    lager:debug("starting recording listener for ~s", [Url]),
    case acdc_recordings_sup:new(Call, RecordingJObj) of
        {'ok', _P} ->
            lager:debug("recording tracked in ~p", [_P]);
        _E -> lager:debug("failed to start recording: ~p", [_E])
    end.

recording_format() ->
    kapps_config:get_ne_binary(<<"callflow">>, [<<"call_recording">>, <<"extension">>], <<"mp3">>).

-spec agent_id(agent()) -> kz_term:api_binary().
agent_id(Agent) ->
    case kz_json:is_json_object(Agent) of
        'true' -> kz_doc:id(Agent);
        'false' -> kapps_call:owner_id(Agent)
    end.

-spec account_id(agent()) -> kz_term:api_binary().
account_id(Agent) ->
    case kz_json:is_json_object(Agent) of
        'true' -> find_account_id(Agent);
        'false' -> kapps_call:account_id(Agent)
    end.

-spec account_db(agent()) -> kz_term:api_binary().
account_db(Agent) ->
    case kz_json:is_json_object(Agent) of
        'true' -> kz_doc:account_db(Agent);
        'false' -> kapps_call:account_db(Agent)
    end.

-spec record_calls(agent()) -> boolean().
record_calls(Agent) ->
    kz_json:is_json_object(Agent)
        andalso kz_json:is_true(<<"record_calls">>, Agent, 'false').

-spec is_thief(agent()) -> boolean().
is_thief(Agent) -> not kz_json:is_json_object(Agent).

handle_fsm_started(_FSMPid) -> gen_listener:cast(self(), 'bind_to_member_reqs').

stop_agent_leg('undefined', _) -> lager:debug("agent call id not defined");
stop_agent_leg(_, 'undefined') -> lager:debug("agent ctrl queue not defined");
stop_agent_leg(ACallId, ACtrlQ) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
              ,{<<"Insert-At">>, <<"now">>}
              ,{<<"Call-ID">>, ACallId}
               | kz_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("sending hangup to ~s: ~s", [ACallId, ACtrlQ]),
    kapi_dialplan:publish_command(ACtrlQ, Command).

find_account_id(JObj) ->
    case kz_doc:account_id(JObj) of
        'undefined' -> kzs_util:format_account_id(kz_doc:account_db(JObj));
        AccountId -> AccountId
    end.

-spec filter_agent_calls(kz_term:proplist(), kz_term:ne_binary()) -> kz_term:proplist().
filter_agent_calls(ACallIds, ACallId) ->
    %% These calls should be cancelled, but need to wait for CtrlQ
    lists:filter(fun({ACancelId, 'undefined'}) when ACancelId =/= ACallId ->
                         lager:debug("~s will have to be cancelled when ctrl queue arrives"
                                    ,[ACancelId]),
                         'true';
                    %% Cancel all calls =/= ACallId that have CtrlQs
                    ({ACancelId, ACtrlQ}) when ACancelId =/= ACallId ->
                         lager:debug("cancelling and stopping leg ~s", [ACancelId]),
                         acdc_util:unbind_from_call_events(ACancelId),
                         stop_agent_leg(ACancelId, ACtrlQ),
                         'false';
                    %% Keep ACallId
                    ({_, _}) -> 'true';
                    (ACancelId) when ACancelId =/= ACallId ->
                         lager:debug("cancelling leg ~s", [ACancelId]),
                         acdc_util:unbind_from_call_events(ACancelId),
                         'false';
                    (_A) ->
                         lager:debug("ignoring ~p", [_A]),
                         'true'
                 end, ACallIds).
