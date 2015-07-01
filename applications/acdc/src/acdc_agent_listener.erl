%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent_listener).

-behaviour(gen_listener).

%% API
-export([start_link/2, start_link/3, start_link/5
         ,member_connect_resp/2
         ,member_connect_retry/2
         ,member_connect_accepted/1, member_connect_accepted/2
         ,agent_timeout/1
         ,bridge_to_member/6
         ,monitor_call/4
         ,channel_hungup/2
         ,rebind_events/3
         ,unbind_from_events/2
         ,originate_execute/2
         ,originate_uuid/3
         ,outbound_call/2
         ,send_sync_req/1
         ,send_sync_resp/3, send_sync_resp/4
         ,config/1, refresh_config/2
         ,send_status_resume/1
         ,add_acdc_queue/2
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

-type config() :: {ne_binary(), ne_binary(), ne_binary()}.
-export_type([config/0]).

-include("acdc.hrl").

-record(state, {
         call :: whapps_call:call()
         ,acdc_queue_id :: ne_binary() % the ACDc Queue ID
         ,msg_queue_id :: ne_binary() % the AMQP Queue ID of the ACDc Queue process
         ,agent_id :: ne_binary()
         ,acct_db :: ne_binary()
         ,acct_id :: ne_binary()
         ,fsm_pid :: pid()
         ,agent_queues = [] :: ne_binaries()
         ,last_connect :: wh_now() % last connection
         ,last_attempt :: wh_now() % last attempt to connect
         ,my_id :: ne_binary()
         ,my_q :: api_binary() % AMQP queue name
         ,timer_ref :: reference()
         ,sync_resp :: wh_json:object() % furthest along resp
         ,supervisor :: pid()
         ,record_calls = 'false' :: boolean()
         ,recording_url :: api_binary() %% where to send recordings after the call
         ,is_thief = 'false' :: boolean()
         ,agent :: agent()
         ,agent_call_ids = [] :: api_binaries() | wh_proplist()
         ,cdr_urls = dict:new() :: dict() %% {CallId, Url}
         ,agent_presence_id :: api_binary()
         }).

-type agent() :: whapps_call:call() | wh_json:object().

%%%===================================================================
%%% Defines for different functionality
%%%===================================================================

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

-define(BINDINGS(AcctId, AgentId), [{'self', []}
                                    ,{'acdc_agent', [{'account_id', AcctId}
                                                     ,{'agent_id', AgentId}
                                                     ,{'restrict_to', ['sync', 'stats_req']}
                                                    ]}
                                    ,{'conf', [{'action', <<"*">>}
                                               ,{'db', wh_util:format_account_id(AcctId, 'encoded')}
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
                     ,{{'acdc_agent_handler', 'handle_destroy'}
                       ,[{<<"channel">>, <<"destroy">>}]
                      }
                     ,{{'acdc_agent_handler', 'handle_config_change'}
                       ,[{<<"configuration">>, <<"*">>}]
                      }
                    ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Supervisor, AgentJObj) ->
    AgentId = wh_doc:id(AgentJObj),
    AcctId = account_id(AgentJObj),

    Queues = case wh_json:get_value(<<"queues">>, AgentJObj) of
                 'undefined' -> [];
                 Qs -> Qs
             end,
    start_link(Supervisor, AgentJObj, AcctId, AgentId, Queues).
start_link(Supervisor, _, _AcctId, _AgentId, []) ->
    lager:debug("agent ~s has no queues, not starting", [_AgentId]),
    _ = wh_util:spawn('acdc_agent_sup', 'stop', [Supervisor]),
    'ignore';
start_link(Supervisor, AgentJObj, AcctId, AgentId, Queues) ->
    case acdc_agent_util:most_recent_status(AcctId, AgentId) of
        {'ok', <<"logged_out">>} ->
            lager:debug("agent ~s in ~s is logged out, not starting", [AgentId, AcctId]),
            _ = wh_util:spawn('acdc_agent_sup', 'stop', [Supervisor]),
            'ignore';
        {'ok', _S} ->
            lager:debug("start bindings for ~s(~s) in ~s", [AcctId, AgentId, _S]),
            gen_listener:start_link(?MODULE
                                    ,[{'bindings', ?BINDINGS(AcctId, AgentId)}
                                      ,{'responders', ?RESPONDERS}
                                     ]
                                    ,[Supervisor, AgentJObj, Queues]
                                   )
    end.

start_link(Supervisor, ThiefCall, QueueId) ->
    AgentId = whapps_call:owner_id(ThiefCall),
    AcctId = whapps_call:account_id(ThiefCall),

    lager:debug("starting thief agent ~s(~s)", [AgentId, AcctId]),
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS(AcctId, AgentId)}
                              ,{'responders', ?RESPONDERS}
                             ]
                            ,[Supervisor, ThiefCall, [QueueId]]
                           ).

stop(Srv) -> gen_listener:cast(Srv, {'stop_agent', self()}).

-spec member_connect_resp(pid(), wh_json:object()) -> 'ok'.
member_connect_resp(Srv, ReqJObj) ->
    gen_listener:cast(Srv, {'member_connect_resp', ReqJObj}).

member_connect_retry(Srv, WinJObj) ->
    gen_listener:cast(Srv, {'member_connect_retry', WinJObj}).

-spec agent_timeout(pid()) -> 'ok'.
agent_timeout(Srv) -> gen_listener:cast(Srv, 'agent_timeout').

member_connect_accepted(Srv) ->
    gen_listener:cast(Srv, {'member_connect_accepted'}).
member_connect_accepted(Srv, ACallId) ->
    gen_listener:cast(Srv, {'member_connect_accepted', ACallId}).

-spec bridge_to_member(pid(), whapps_call:call(), wh_json:object()
                       ,wh_json:objects(), api_binary(), api_binary()
                      ) -> 'ok'.
bridge_to_member(Srv, Call, WinJObj, EPs, CDRUrl, RecordingUrl) ->
    gen_listener:cast(Srv, {'bridge_to_member', Call, WinJObj, EPs, CDRUrl, RecordingUrl}).

monitor_call(Srv, Call, CDRUrl, RecordingUrl) ->
    gen_listener:cast(Srv, {'monitor_call', Call, CDRUrl, RecordingUrl}).

-spec channel_hungup(pid(), ne_binary()) -> 'ok'.
channel_hungup(Srv, CallId) ->
    gen_listener:cast(Srv, {'channel_hungup', CallId}).

-spec unbind_from_events(pid(), ne_binary()) -> 'ok'.
unbind_from_events(Srv, CallId) ->
    gen_listener:cast(Srv, {'unbind_from_events', CallId}).

-spec rebind_events(pid(), ne_binary(), ne_binary()) -> 'ok'.
rebind_events(Srv, OldCallId, NewCallId) ->
    gen_listener:cast(Srv, {'rebind_events', OldCallId, NewCallId}).

originate_execute(Srv, JObj) ->
    gen_listener:cast(Srv, {'originate_execute', JObj}).

originate_uuid(Srv, UUID, CtlQ) ->
    gen_listener:cast(Srv, {'originate_uuid', UUID, CtlQ}).

outbound_call(Srv, CallId) ->
    gen_listener:cast(Srv, {'outbound_call', CallId}).

send_sync_req(Srv) -> gen_listener:cast(Srv, {'send_sync_req'}).

send_sync_resp(Srv, Status, ReqJObj) -> send_sync_resp(Srv, Status, ReqJObj, []).
send_sync_resp(Srv, Status, ReqJObj, Options) ->
    gen_listener:cast(Srv, {'send_sync_resp', Status, ReqJObj, Options}).

-spec config(pid()) -> config().
config(Srv) -> gen_listener:call(Srv, 'config').

refresh_config(_, 'undefined') -> 'ok';
refresh_config(Srv, Qs) -> gen_listener:cast(Srv, {'refresh_config', Qs}).

-spec agent_info(pid(), wh_json:key()) -> wh_json:json_term() | 'undefined'.
agent_info(Srv, Field) -> gen_listener:call(Srv, {'agent_info', Field}).

send_status_resume(Srv) ->
    gen_listener:cast(Srv, {'send_status_update', 'resume'}).

add_acdc_queue(Srv, Q) ->
    gen_listener:cast(Srv, {'queue_login', Q}).

rm_acdc_queue(Srv, Q) ->
    gen_listener:cast(Srv, {'queue_logout', Q}).

call_status_req(Srv) ->
    gen_listener:cast(Srv, 'call_status_req').
call_status_req(Srv, CallId) ->
    gen_listener:cast(Srv, {'call_status_req', CallId}).

fsm_started(Srv, FSM) ->
    gen_listener:cast(Srv, {'fsm_started', FSM}).

add_endpoint_bindings(_Srv, _Realm, 'undefined') ->
    lager:debug("ignoring adding endpoint bindings for undefined user @ ~s", [_Realm]);
add_endpoint_bindings(Srv, Realm, User) ->
    lager:debug("adding route bindings to ~p for endpoint ~s@~s", [Srv, User, Realm]),
    gen_listener:add_binding(Srv, 'route', [{'realm', Realm}
                                            ,{'user', User}
                                           ]).
remove_endpoint_bindings(Srv, Realm, User) ->
    lager:debug("removing route bindings to ~p for endpoint ~s@~s", [Srv, User, Realm]),
    gen_listener:rm_binding(Srv, 'route', [{'realm', Realm}
                                           ,{'user', User}
                                          ]).

remove_cdr_urls(Srv, CallId) -> gen_listener:cast(Srv, {'remove_cdr_urls', CallId}).

logout_agent(Srv) -> gen_listener:cast(Srv, 'logout_agent').

maybe_update_presence_id(_Srv, 'undefined') -> 'ok';
maybe_update_presence_id(Srv, Id) ->
    gen_listener:cast(Srv, {'presence_id', Id}).

maybe_update_presence_state(_Srv, 'undefined') -> 'ok';
maybe_update_presence_state(Srv, State) ->
    presence_update(Srv, State).

presence_update(_, 'undefined') -> 'ok';
presence_update(Srv, PresenceState) ->
    gen_listener:cast(Srv, {'presence_update', PresenceState}).

-spec update_agent_status(pid(), ne_binary()) -> 'ok'.
update_agent_status(Srv, Status) ->
    gen_listener:cast(Srv, {'update_status', Status}).

-spec presence_id(pid()) -> api_binary().
presence_id(Srv) ->
    gen_listener:call(Srv, 'presence_id').

-spec queues(pid()) -> ne_binaries().
queues(Srv) ->
    gen_listener:call(Srv, 'queues').

-spec id(pid()) -> api_binary().
id(Srv) ->
    gen_listener:call(Srv, 'my_id').

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Supervisor, Agent, Queues]) ->
    AgentId = agent_id(Agent),
    wh_util:put_callid(AgentId),
    lager:debug("starting acdc agent listener"),

    {'ok', #state{agent_id=AgentId
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call('presence_id', _, #state{agent_presence_id=PresenceId}=State) ->
    {'reply', PresenceId, State, 'hibernate'};
handle_call('queues', _, #state{agent_queues=Queues}=State) ->
    {'reply', Queues, State, 'hibernate'};
handle_call('my_id', _, #state{agent_id=AgentId}=State) ->
    {'reply', AgentId, State, 'hibernate'};
handle_call({'agent_info', Field}, _, #state{agent=Agent}=State) ->
    {'reply', wh_json:get_value(Field, Agent), State};
handle_call('config', _From, #state{acct_id=AcctId
                                    ,agent_id=AgentId
                                    ,my_q=Q
                                   }=State) ->
    {'reply', {AcctId, AgentId, Q}, State};
handle_call(_Request, _From, #state{}=State) ->
    lager:debug("unhandled call from ~p: ~p", [_From, _Request]),
    {'reply', {'error', 'unhandled_call'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'refresh_config', Qs}, #state{agent_queues=Queues}=State) ->
    {Add, Rm} = acdc_agent_util:changed(Queues, Qs),

    Self = self(),
    _ = [gen_listener:cast(Self, {'queue_login', A}) || A <- Add],
    _ = [gen_listener:cast(Self, {'queue_logout', R}) || R <- Rm],
    {'noreply', State};
handle_cast({'stop_agent', Req}, #state{supervisor=Supervisor}=State) ->
    lager:debug("stop agent requested by ~p", [Req]),
    _ = wh_util:spawn('acdc_agent_sup', 'stop', [Supervisor]),
    {'noreply', State};

handle_cast({'fsm_started', FSMPid}, State) ->
    lager:debug("fsm started: ~p", [FSMPid]),
    handle_fsm_started(FSMPid),
    {'noreply', State#state{fsm_pid=FSMPid
                            ,my_id=acdc_util:proc_id(FSMPid)
                           }};

handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{my_q=Q}, 'hibernate'};

handle_cast({'queue_login', Q}, #state{agent_queues=Qs
                                       ,acct_id=AcctId
                                       ,agent_id=AgentId
                                      }=State) when is_binary(Q) ->
    case lists:member(Q, Qs) of
        'true' ->
            lager:debug("already logged into queue ~s", [Q]),
            {'noreply', State};
        'false' ->
            lager:debug("adding binding (logging in) to queue ~s", [Q]),
            login_to_queue(AcctId, AgentId, Q),
            {'noreply', State#state{agent_queues=[Q|Qs]}}
    end;
handle_cast({'queue_login', QJObj}, State) ->
    lager:debug("queue jobj: ~p", [QJObj]),
    handle_cast({'queue_login', wh_doc:id(QJObj)}, State);

handle_cast({'queue_logout', Q}, #state{agent_queues=[Q]
                                        ,acct_id=AcctId
                                        ,agent_id=AgentId
                                       }=State) ->
    lager:debug("agent logged out of last known queue ~s, logging out", [Q]),
    logout_from_queue(AcctId, AgentId, Q),
    acdc_agent_listener:logout_agent(self()),
    {'noreply', State#state{agent_queues=[]}};
handle_cast({'queue_logout', Q}, #state{agent_queues=Qs
                                        ,acct_id=AcctId
                                        ,agent_id=AgentId
                                       }=State) ->
    case lists:member(Q, Qs) of
        'true' ->
            lager:debug("removing binding (logging out) from queue ~s", [Q]),
            logout_from_queue(AcctId, AgentId, Q),
            {'noreply', State#state{agent_queues=lists:delete(Q, Qs)}, 'hibernate'};
        'false' ->
            lager:debug("not logged into queue ~s", [Q]),
            {'noreply', State}
    end;

handle_cast('bind_to_member_reqs', #state{agent_queues=Qs
                                          ,acct_id=AcctId
                                          ,agent_id=AgentId
                                         }=State) ->
    lager:debug("binding to queues: ~p", [Qs]),
    _ = [login_to_queue(AcctId, AgentId, Q) || Q <- Qs],
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

            _ = filter_agent_calls(ACallIds, CallId),

            wh_util:put_callid(AgentId),
            case IsThief of
                'false' ->
                    {'noreply', State#state{call='undefined'
                                            ,msg_queue_id='undefined'
                                            ,acdc_queue_id='undefined'
                                            ,agent_call_ids=[]
                                            ,recording_url='undefined'
                                           }
                     ,'hibernate'};
                'true' ->
                    lager:debug("thief is done, going down"),
                    acdc_agent_listener:stop(self()),
                    {'noreply', State}
            end;
        _ ->
            case props:get_value(CallId, ACallIds) of
                'true' ->
                    lager:debug("agent channel ~s hungup/needs hanging up", [CallId]),
                    acdc_util:unbind_from_call_events(CallId),
                    {'noreply', State#state{agent_call_ids=lists:delete(CallId, ACallIds)}, 'hibernate'};
                {ACallId, ACtrlQ} ->
                    lager:debug("agent channel ~s hungup, stop call on ctlq ~s", [ACallId, ACtrlQ]),
                    acdc_util:unbind_from_call_events(ACallId),
                    stop_agent_leg(ACallId, ACtrlQ),
                    {'noreply', State#state{agent_call_ids=props:delete(ACallId, ACallIds)}};
                'undefined' ->
                    lager:debug("unknown call id ~s for channel_hungup, ignoring", [CallId]),
                    lager:debug("listening for call id(~s) and agents (~p)", [CCallId, ACallIds]),
                    {'noreply', State}
            end
    end;

handle_cast('agent_timeout', #state{agent_call_ids=ACallIds
                                    ,agent_id=AgentId
                                   }=State) ->
    lager:debug("agent timeout recv, stopping agent call"),

    _ = filter_agent_calls(ACallIds, AgentId),

    wh_util:put_callid(AgentId),
    {'noreply', State#state{msg_queue_id='undefined'
                            ,acdc_queue_id='undefined'
                            ,agent_call_ids=[]
                            ,call='undefined'
                           }
     ,'hibernate'};
handle_cast({'member_connect_retry', CallId}, #state{my_id=MyId
                                                     ,msg_queue_id=Server
                                                     ,agent_call_ids=ACallIds
                                                     ,call=Call
                                                     ,agent_id=AgentId
                                                    }=State) when is_binary(CallId) ->
    case catch whapps_call:call_id(Call) of
        CallId ->
            lager:debug("need to retry member connect, agent isn't able to take it"),
            send_member_connect_retry(Server, CallId, MyId, AgentId),

            _ = [acdc_util:unbind_from_call_events(ACallId) || ACallId <- ACallIds],
            acdc_util:unbind_from_call_events(CallId),

            wh_util:put_callid(AgentId),

            {'noreply', State#state{msg_queue_id='undefined'
                                    ,acdc_queue_id='undefined'
                                    ,agent_call_ids=[]
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
                                                                                   ,acct_id=AcctId
                                                                                   ,agent_id=AgentId
                                                                                   ,my_q=MyQ
                                                                                   ,cdr_urls=Urls
                                                                                   ,agent=Agent
                                                                                  }=State) ->
    _ = whapps_call:put_callid(Call),
    lager:debug("bridging to agent endpoints"),

    RingTimeout = wh_json:get_value(<<"Ring-Timeout">>, WinJObj),
    lager:debug("ring agent for ~ps", [RingTimeout]),

    ShouldRecord = should_record_endpoints(EPs, record_calls(Agent)
                                           ,wh_json:is_true(<<"Record-Caller">>, WinJObj, 'false')
                                          ),

    acdc_util:bind_to_call_events(Call),

    AgentCallIds = maybe_connect_to_agent(MyQ, EPs, Call, RingTimeout, AgentId, CDRUrl),

    lager:debug("originate sent, waiting on successful bridge now"),
    update_my_queues_of_change(AcctId, AgentId, Qs),
    {'noreply', State#state{call=Call
                            ,record_calls=ShouldRecord
                            ,msg_queue_id=wh_json:get_value(<<"Server-ID">>, WinJObj)
                            ,agent_call_ids=AgentCallIds
                            ,cdr_urls=dict:store(whapps_call:call_id(Call), CDRUrl,
                                                 dict:store(AgentCallIds, CDRUrl, Urls)
                                                )
                            ,recording_url=RecordingUrl
                           }
     ,'hibernate'};

handle_cast({'bridge_to_member', Call, WinJObj, _, CDRUrl, RecordingUrl}, #state{is_thief='true'
                                                                                 ,agent=Agent
                                                                                 ,agent_id=AgentId
                                                                                 ,cdr_urls=Urls
                                                                                }=State) ->
    _ = whapps_call:put_callid(Call),
    lager:debug("connecting to thief at ~s", [whapps_call:call_id(Agent)]),
    acdc_util:bind_to_call_events(Call),

    AgentCallId = outbound_call_id(Call, AgentId),
    acdc_util:bind_to_call_events(AgentCallId),

    ShouldRecord = record_calls(Agent) orelse wh_json:is_true(<<"Record-Caller">>, WinJObj, 'false'),

    whapps_call_command:pickup(whapps_call:call_id(Agent), <<"now">>, Call),

    {'noreply', State#state{call=Call
                            ,msg_queue_id=wh_json:get_value(<<"Server-ID">>, WinJObj)
                            ,agent_call_ids=[AgentCallId]
                            ,cdr_urls=dict:store(whapps_call:call_id(Call), CDRUrl,
                                                 dict:store(AgentCallId, CDRUrl, Urls)
                                                )
                            ,record_calls=ShouldRecord
                            ,recording_url=RecordingUrl
                           }
     ,'hibernate'};

handle_cast({'member_connect_accepted'}, #state{msg_queue_id=AmqpQueue
                                                ,call=Call
                                                ,acct_id=AcctId
                                                ,agent_id=AgentId
                                                ,my_id=MyId
                                                ,record_calls=ShouldRecord
                                                ,recording_url=RecordingUrl
                                               }=State) ->
    lager:debug("member bridged to agent! waiting on agent call id though"),
    maybe_start_recording(Call, ShouldRecord, RecordingUrl),

    send_member_connect_accepted(AmqpQueue, call_id(Call), AcctId, AgentId, MyId),
    {'noreply', State};

handle_cast({'member_connect_accepted', ACallId}, #state{msg_queue_id=AmqpQueue
                                                         ,call=Call
                                                         ,acct_id=AcctId
                                                         ,agent_id=AgentId
                                                         ,my_id=MyId
                                                         ,record_calls=ShouldRecord
                                                         ,recording_url=RecordingUrl
                                                         ,agent_call_ids=ACallIds
                                                        }=State) ->
    lager:debug("member bridged to agent!"),
    maybe_start_recording(Call, ShouldRecord, RecordingUrl),

    ACallIds1 = filter_agent_calls(ACallIds, ACallId),

    lager:debug("new agent call ids: ~p", [ACallIds1]),

    send_member_connect_accepted(AmqpQueue, call_id(Call), AcctId, AgentId, MyId),
    {'noreply', State#state{agent_call_ids=ACallIds1}, 'hibernate'};

handle_cast({'member_connect_resp', ReqJObj}, #state{agent_id=AgentId
                                                     ,last_connect=LastConn
                                                     ,agent_queues=Qs
                                                     ,my_id=MyId
                                                     ,my_q=MyQ
                                                    }=State) ->
    ACDcQueue = wh_json:get_value(<<"Queue-ID">>, ReqJObj),
    case is_valid_queue(ACDcQueue, Qs) of
        'false' ->
            lager:debug("queue ~s isn't one of ours", [ACDcQueue]),
            {'noreply', State};
        'true' ->
            lager:debug("responding to member_connect_req"),

            send_member_connect_resp(ReqJObj, MyQ, AgentId, MyId, LastConn),
            {'noreply', State#state{acdc_queue_id = ACDcQueue
                                    ,msg_queue_id = wh_json:get_value(<<"Server-ID">>, ReqJObj)
                                   }
             ,'hibernate'}
    end;

handle_cast({'monitor_call', Call, _CDRUrl, RecordingUrl}, State) ->
    _ = whapps_call:put_callid(Call),

    acdc_util:bind_to_call_events(Call),

    lager:debug("monitoring member call ~s", [whapps_call:call_id(Call)]),

    {'noreply', State#state{call=Call
                            ,agent_call_ids=[]
                            ,recording_url=RecordingUrl
                           }
     ,'hibernate'};

handle_cast({'originate_execute', JObj}, #state{my_q=Q}=State) ->
    lager:debug("execute the originate for agent: ~p", [JObj]),
    send_originate_execute(JObj, Q),
    {'noreply', State, 'hibernate'};

handle_cast({'originate_uuid', UUID, CtlQ}, #state{agent_call_ids=ACallIds}=State) ->
    lager:debug("updating ~s with ~s in ~p", [UUID, CtlQ, ACallIds]),
    {'noreply', State#state{agent_call_ids=[{UUID, CtlQ} | props:delete(UUID, ACallIds)]}};

handle_cast({'outbound_call', CallId}, State) ->
    _ = wh_util:put_callid(CallId),
    acdc_util:bind_to_call_events(CallId),

    lager:debug("bound to agent's outbound call ~s", [CallId]),
    {'noreply', State#state{call=whapps_call:set_call_id(CallId, whapps_call:new())}, 'hibernate'};

handle_cast({'send_sync_req'}, #state{my_id=MyId
                                      ,my_q=MyQ
                                      ,acct_id=AcctId
                                      ,agent_id=AgentId
                                     }=State) ->
    lager:debug("sending sync request"),
    send_sync_request(AcctId, AgentId, MyId, MyQ),
    {'noreply', State};

handle_cast({'send_sync_resp', Status, ReqJObj, Options}, #state{my_id=MyId
                                                                 ,acct_id=AcctId
                                                                 ,agent_id=AgentId
                                                                }=State) ->
    send_sync_response(ReqJObj, AcctId, AgentId, MyId, Status, Options),
    {'noreply', State};

handle_cast({'send_status_update', Status}, #state{acct_id=AcctId
                                                   ,agent_id=AgentId
                                                  }=State) ->
    send_status_update(AcctId, AgentId, Status),
    {'noreply', State};

handle_cast('call_status_req', #state{call=Call, my_q=Q}=State) ->
    CallId = whapps_call:call_id(Call),

    Command = [{<<"Call-ID">>, CallId}
               ,{<<"Server-ID">>, Q}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],

    wapi_call:publish_channel_status_req(CallId, Command),
    {'noreply', State};

handle_cast({'call_status_req', CallId}, #state{my_q=Q}=State) when is_binary(CallId) ->
    Command = [{<<"Call-ID">>, CallId}
               ,{<<"Server-ID">>, Q}
               | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_call:publish_channel_status_req(CallId, Command),
    {'noreply', State};
handle_cast({'call_status_req', Call}, State) ->
    handle_cast({'call_status_req', whapps_call:call_id(Call)}, State);

handle_cast({'remove_cdr_urls', CallId}, #state{cdr_urls=Urls}=State) ->
    {'noreply', State#state{cdr_urls=dict:erase(CallId, Urls)}, 'hibernate'};

handle_cast('logout_agent', #state{acct_id=AcctId
                                   ,agent_id=AgentId
                                  }=State) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
                ,{<<"Agent-ID">>, AgentId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),

    wapi_acdc_agent:publish_logout(Update),
    lager:debug("published agent logout message"),
    {'noreply', State};

handle_cast({'presence_id', _Id}, #state{agent_presence_id=_Id}=State) ->
    {'noreply', State};
handle_cast({'presence_id', PresenceId}, #state{agent_presence_id=_Id}=State) ->
    lager:debug("updating presence id from ~s to ~s", [_Id, PresenceId]),
    {'noreply', State#state{agent_presence_id=PresenceId}};

handle_cast({'presence_update', PresenceState}, #state{acct_id=AcctId
                                                       ,agent_presence_id='undefined'
                                                       ,agent_id=AgentId
                                                      }=State) ->
    lager:debug("no custom presence id, using ~s for ~s", [AgentId, PresenceState]),
    acdc_util:presence_update(AcctId, AgentId, PresenceState
                              ,wh_util:to_hex_binary(crypto:hash(md5, AgentId))
                             ),
    {'noreply', State};
handle_cast({'presence_update', PresenceState}, #state{acct_id=AcctId
                                                       ,agent_presence_id=PresenceId
                                                      }=State) ->
    lager:debug("custom presence id, using ~s for ~s", [PresenceId, PresenceState]),
    acdc_util:presence_update(AcctId, PresenceId, PresenceState
                              ,wh_util:to_hex_binary(crypto:hash(md5, PresenceId))
                             ),
    {'noreply', State};

handle_cast({'update_status', Status}, #state{agent_id=AgentId
                                              ,acct_id=AcctId
                                             }=State) ->
    catch acdc_agent_util:update_status(AcctId, AgentId, Status),
    {'noreply', State};

handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, 'hibernate'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all messages from the message bus
%%
%% @spec handle_info(JObj, State) -> {'reply', Proplist} |
%%                                   ignore
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{fsm_pid='undefined'}) -> 'ignore';
handle_event(_JObj, #state{fsm_pid=FSM
                           ,agent_id=AgentId
                           ,acct_id=AcctId
                           ,cdr_urls=Urls
                          }) ->
    {'reply', [{'fsm_pid', FSM}
               ,{'agent_id', AgentId}
               ,{'acct_id', AcctId}
               ,{'cdr_urls', Urls}
              ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{agent_queues=Queues
                         ,acct_id=AcctId
                         ,agent_id=AgentId
                        }
         ) when Reason == 'normal'; Reason == 'shutdown' ->
    _ = [logout_from_queue(AcctId, AgentId, QueueId) || QueueId <- Queues],
    lager:debug("agent process going down: ~p", [Reason]);
terminate(_Reason, _State) ->
    lager:debug("agent process going down: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec is_valid_queue(ne_binary(), ne_binaries()) -> boolean().
is_valid_queue(Q, Qs) -> lists:member(Q, Qs).

-spec send_member_connect_resp(wh_json:object(), ne_binary()
                               ,ne_binary(), ne_binary()
                               , wh_now() | 'undefined'
                              ) -> 'ok'.
send_member_connect_resp(JObj, MyQ, AgentId, MyId, LastConn) ->
    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
    IdleTime = idle_time(LastConn),
    Resp = props:filter_undefined(
             [{<<"Agent-ID">>, AgentId}
              ,{<<"Idle-Time">>, IdleTime}
              ,{<<"Process-ID">>, MyId}
              ,{<<"Server-ID">>, MyQ}
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending connect_resp to ~s for ~s: ~s", [Queue, call_id(JObj), MyId]),
    wapi_acdc_queue:publish_member_connect_resp(Queue, Resp).

-spec send_member_connect_retry(wh_json:object(), ne_binary(), ne_binary()) -> 'ok'.
-spec send_member_connect_retry(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_retry(JObj, MyId, AgentId) ->
    send_member_connect_retry(wh_json:get_value(<<"Server-ID">>, JObj)
                              ,call_id(JObj)
                              ,MyId
                              ,AgentId
                             ).

send_member_connect_retry('undefined', _, _, _) ->
    lager:debug("no queue to send the retry to, seems bad");
send_member_connect_retry(Queue, CallId, MyId, AgentId) ->
    Resp = props:filter_undefined(
             [{<<"Process-ID">>, MyId}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Agent-ID">>, AgentId}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_acdc_queue:publish_member_connect_retry(Queue, Resp).

-spec send_member_connect_accepted(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_accepted(Queue, CallId, AcctId, AgentId, MyId) ->
    Resp = props:filter_undefined([{<<"Call-ID">>, CallId}
                                   ,{<<"Account-ID">>, AcctId}
                                   ,{<<"Agent-ID">>, AgentId}
                                   ,{<<"Process-ID">>, MyId}
                                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    wapi_acdc_queue:publish_member_connect_accepted(Queue, Resp).

-spec send_originate_execute(wh_json:object(), ne_binary()) -> 'ok'.
send_originate_execute(JObj, Q) ->
    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_dialplan:publish_originate_execute(wh_json:get_value(<<"Server-ID">>, JObj), Prop).

-spec send_sync_request(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_sync_request(AcctId, AgentId, MyId, MyQ) ->
    Prop = [{<<"Account-ID">>, AcctId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Process-ID">>, MyId}
            | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_acdc_agent:publish_sync_req(Prop).

send_sync_response(ReqJObj, AcctId, AgentId, MyId, Status, Options) ->
    Prop = [{<<"Account-ID">>, AcctId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Process-ID">>, MyId}
            ,{<<"Status">>, wh_util:to_binary(Status)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ReqJObj)}
            | Options ++ wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = wh_json:get_value(<<"Server-ID">>, ReqJObj),
    lager:debug("sending sync resp to ~s", [Q]),
    wapi_acdc_agent:publish_sync_resp(Q, Prop).

send_status_update(AcctId, AgentId, 'resume') ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, AcctId}
                ,{<<"Agent-ID">>, AgentId}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    wapi_acdc_agent:publish_resume(Update).


-spec idle_time('undefined' | wh_now()) -> api_integer().
idle_time('undefined') -> 'undefined';
idle_time(T) -> wh_util:elapsed_s(T).

-spec call_id(whapps_call:call() | api_object()) ->
                     api_binary().
call_id('undefined') -> 'undefined';
call_id(Call) ->
    case whapps_call:is_call(Call) of
        'true' -> whapps_call:call_id(Call);
        'false' ->
            Keys = [[<<"Call">>, <<"Call-ID">>]
                    ,[<<"Call">>, <<"call_id">>]
                    ,<<"Call-ID">>
                   ],
            lists:foldl(fun(K, 'undefined') -> wh_json:get_value(K, Call);
                           (_, CallId) -> CallId
                        end, 'undefined', Keys)
    end.

-spec maybe_connect_to_agent(ne_binary(), wh_json:objects(), whapps_call:call(), api_integer(), ne_binary(), api_binary()) ->
                                    ne_binaries().
maybe_connect_to_agent(MyQ, EPs, Call, Timeout, AgentId, _CdrUrl) ->
    MCallId = whapps_call:call_id(Call),
    wh_util:put_callid(MCallId),

    ReqId = wh_util:rand_hex_binary(6),
    AcctId = whapps_call:account_id(Call),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AcctId}
                                   ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
                                   ,{<<"Request-ID">>, ReqId}
                                   ,{<<"Retain-CID">>, <<"true">>}
                                   ,{<<"Agent-ID">>, AgentId}
                                  ]),

    {ACallIds, Endpoints} = lists:foldl(fun(EP, {Cs, Es}) ->
                                                ACallId = outbound_call_id(Call, AgentId),
                                                acdc_util:bind_to_call_events(ACallId),

                                                {[ACallId | Cs]
                                                 ,[wh_json:set_values([{<<"Endpoint-Timeout">>, Timeout}
                                                                       ,{<<"Outbound-Call-ID">>, ACallId}
                                                                       ,{<<"Existing-Call-ID">>, whapps_call:call_id(Call)}
                                                                      ], EP)
                                                   | Es
                                                  ]}
                                        end, {[], []}, EPs),

    Prop = props:filter_undefined(
             [{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
              ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
              ,{<<"Timeout">>, Timeout}
              ,{<<"Endpoints">>, Endpoints}
              ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                   ,<<"Retain-CID">>
                                                   ,<<"Authorizing-ID">>
                                                   ,<<"Authorizing-Type">>
                                                  ]}
              ,{<<"Account-ID">>, AcctId}
              ,{<<"Resource-Type">>, <<"originate">>}
              ,{<<"Application-Name">>, <<"bridge">>}
              ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
              ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
              ,{<<"Outbound-Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
              ,{<<"Outbound-Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
              ,{<<"Existing-Call-ID">>, whapps_call:call_id(Call)}
              ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),

    lager:debug("sending originate request with agent call-ids ~p", [ACallIds]),

    wapi_resource:publish_originate_req(Prop),
    ACallIds.

outbound_call_id(CallId, AgentId) when is_binary(CallId) ->
    Rnd = wh_util:rand_hex_binary(4),
    <<(wh_util:to_hex_binary(erlang:md5(CallId)))/binary, "-", AgentId/binary, "-", Rnd/binary>>;
outbound_call_id(Call, AgentId) -> outbound_call_id(whapps_call:call_id(Call), AgentId).

-spec login_to_queue(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
login_to_queue(AcctId, AgentId, QueueId) ->
    gen_listener:add_binding(self()
                             ,'acdc_queue'
                             ,[{'restrict_to', ['member_connect_req']}
                               ,{'queue_id', QueueId}
                               ,{'account_id', AcctId}
                              ]),
    Prop = [{<<"Account-ID">>, AcctId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Change">>, <<"available">>}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_acdc_queue:publish_agent_change(Prop).

-spec logout_from_queue(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
logout_from_queue(AcctId, AgentId, QueueId) ->
    gen_listener:rm_binding(self()
                            ,'acdc_queue'
                            ,[{'restrict_to', ['member_connect_req']}
                              ,{'queue_id', QueueId}
                              ,{'account_id', AcctId}
                             ]),
    Prop = [{<<"Account-ID">>, AcctId}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Change">>, <<"unavailable">>}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_acdc_queue:publish_agent_change(Prop).

update_my_queues_of_change(AcctId, AgentId, Qs) ->
    Props = [{<<"Account-ID">>, AcctId}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Change">>, <<"ringing">>}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    _ = [wapi_acdc_queue:publish_agent_change([{<<"Queue-ID">>, QueueId} | Props])
         || QueueId <- Qs
        ],
    'ok'.

-spec should_record_endpoints(wh_json:objects(), boolean(), api_boolean()) -> boolean().
should_record_endpoints(_EPs, 'true', _) -> 'true';
should_record_endpoints(_EPs, 'false', 'true') -> 'true';
should_record_endpoints(EPs, _, _) ->
    lists:any(fun(EP) ->
                      wh_json:is_true(<<"record_calls">>, EP, 'false')
              end, EPs).

-spec maybe_start_recording(whapps_call:call(), boolean(), ne_binary()) -> 'ok'.
maybe_start_recording(_Call, 'false', _) ->
    lager:debug("not recording this call");
maybe_start_recording(Call, 'true', Url) ->
    RecordingJObj =
        wh_json:from_list(
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
    whapps_config:get(<<"callflow">>, [<<"call_recording">>, <<"extension">>], <<"mp3">>).

-spec agent_id(agent()) -> api_binary().
agent_id(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> wh_doc:id(Agent);
        'false' -> whapps_call:owner_id(Agent)
    end.

-spec account_id(agent()) -> api_binary().
account_id(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> find_account_id(Agent);
        'false' -> whapps_call:account_id(Agent)
    end.

-spec account_db(agent()) -> api_binary().
account_db(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> wh_doc:account_db(Agent);
        'false' -> whapps_call:account_db(Agent)
    end.

-spec record_calls(agent()) -> boolean().
record_calls(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> wh_json:is_true(<<"record_calls">>, Agent, 'false');
        'false' -> 'false'
    end.

-spec is_thief(agent()) -> boolean().
is_thief(Agent) -> not wh_json:is_json_object(Agent).

handle_fsm_started(_FSMPid) -> gen_listener:cast(self(), 'bind_to_member_reqs').

stop_agent_leg('undefined', _) -> lager:debug("agent call id not defined");
stop_agent_leg(_, 'undefined') -> lager:debug("agent ctrl queue not defined");
stop_agent_leg(ACallId, ACtrlQ) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, ACallId}
               | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    lager:debug("sending hangup to ~s: ~s", [ACallId, ACtrlQ]),
    wapi_dialplan:publish_command(ACtrlQ, Command).

find_account_id(JObj) ->
    case wh_doc:account_id(JObj) of
        'undefined' -> wh_util:format_account_id(wh_doc:account_db(JObj), 'raw');
        AcctId -> AcctId
    end.

-spec filter_agent_calls(wh_proplist(), ne_binary()) -> wh_proplist().
filter_agent_calls(ACallIds, ACallId) ->
    lists:filter(fun({ACancelId, ACtrlQ}) when ACancelId =/= ACallId ->
                         lager:debug("cancelling and stopping leg ~s", [ACancelId]),
                         acdc_util:unbind_from_call_events(ACancelId),
                         stop_agent_leg(ACancelId, ACtrlQ),
                         'false';
                    ({_, _}) -> 'true';
                    (ACancelId) when ACancelId =/= ACallId ->
                         lager:debug("cancelling leg ~s", [ACancelId]),
                         acdc_util:unbind_from_call_events(ACancelId),
                         'false';
                    (_A) ->
                         lager:debug("ignoring ~p", [_A]),
                         'true'
                 end, ACallIds).
