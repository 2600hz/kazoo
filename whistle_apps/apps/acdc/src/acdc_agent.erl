%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent).

-behaviour(gen_listener).

%% API
-export([start_link/2, start_link/3
         ,member_connect_resp/2
         ,member_connect_retry/2
         ,bridge_to_member/6
         ,monitor_call/4
         ,member_connect_accepted/1
         ,channel_hungup/2
         ,originate_execute/2
         ,outbound_call/2
         ,send_sync_req/1
         ,send_sync_resp/3, send_sync_resp/4
         ,config/1
         ,send_status_resume/1
         ,add_acdc_queue/2
         ,rm_acdc_queue/2
         ,get_recording_doc_id/1
         ,call_status_req/1, call_status_req/2
         ,stop/1
         ,fsm_started/2
         ,add_endpoint_bindings/3
         ,agent_call_id/3, outbound_call_id/1
         ,unbind_from_cdr/2
         ,logout_agent/1
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
         ,agent_call_id :: api_binary()
         ,agent_call_queue :: api_binary()
         ,cdr_urls = dict:new() :: dict() %% {CallId, Url}
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
                     ,{{'acdc_agent_handler', 'handle_cdr'}
                       ,[{<<"call_detail">>, <<"cdr">>}]
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
                     ,{{'acdc_agent_handler', 'handle_route_req'}
                       ,[{<<"dialplan">>, <<"route_req">>}]
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
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    AcctId = wh_json:get_value(<<"pvt_account_id">>, AgentJObj),

    case wh_json:get_value(<<"queues">>, AgentJObj) of
        'undefined' ->
            lager:debug("agent ~s has no queues, ignoring", [AgentId]),
            {'error', 'no_queues'};
        [] ->
            lager:debug("agent ~s in ~s has no queues, ignoring", [AgentId, AcctId]),
            {'error', 'no_queues'};
        Queues ->
            case acdc_util:agent_status(AcctId, AgentId) of
                <<"logout">> -> {'error', 'logged_out'};
                _S ->
                    lager:debug("start bindings for ~s(~s) in ~s", [AcctId, AgentId, _S]),
                    gen_listener:start_link(?MODULE
                                            ,[{'bindings', ?BINDINGS(AcctId, AgentId)}
                                              ,{'responders', ?RESPONDERS}
                                             ]
                                            ,[Supervisor, AgentJObj, Queues]
                                           )
            end
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

member_connect_accepted(Srv) ->
    gen_listener:cast(Srv, 'member_connect_accepted').

bridge_to_member(Srv, Call, WinJObj, EPs, CDRUrl, RecordingUrl) ->
    gen_listener:cast(Srv, {'bridge_to_member', Call, WinJObj, EPs, CDRUrl, RecordingUrl}).

monitor_call(Srv, Call, CDRUrl, RecordingUrl) ->
    gen_listener:cast(Srv, {'monitor_call', Call, CDRUrl, RecordingUrl}).

-spec channel_hungup(pid(), ne_binary()) -> 'ok'.
channel_hungup(Srv, CallId) ->
    gen_listener:cast(Srv, {'channel_hungup', CallId}).

agent_call_id(Srv, ACallId, ACtrlQ) ->
    gen_listener:cast(Srv, {'agent_call_id', ACallId, ACtrlQ}).

originate_execute(Srv, JObj) ->
    gen_listener:cast(Srv, {'originate_execute', JObj}).

outbound_call(Srv, Call) ->
    gen_listener:cast(Srv, {'outbound_call', Call}).

send_sync_req(Srv) -> gen_listener:cast(Srv, {'send_sync_req'}).

send_sync_resp(Srv, Status, ReqJObj) -> send_sync_resp(Srv, Status, ReqJObj, []).
send_sync_resp(Srv, Status, ReqJObj, Options) ->
    gen_listener:cast(Srv, {'send_sync_resp', Status, ReqJObj, Options}).

-spec config(pid()) -> {ne_binary(), ne_binary(), ne_binary()}.
config(Srv) -> gen_listener:call(Srv, 'config').

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

add_endpoint_bindings(Srv, Realm, User) ->
    lager:debug("adding route bindings to ~p for endpoint ~s@~s", [Srv, User, Realm]),
    gen_listener:add_binding(Srv, 'route', [{'realm', Realm}
                                            ,{'user', User}
                                           ]).

unbind_from_cdr(Srv, CallId) -> gen_listener:cast(Srv, {'unbind_from_cdr', CallId}).

logout_agent(Srv) -> gen_listener:cast(Srv, 'logout_agent').

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
    put('callid', AgentId),
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
handle_call('config', _From, #state{acct_id=AcctId
                                    ,agent_id=AgentId
                                    ,my_q=Q
                                   }=State) ->
    {'reply', {AcctId, AgentId, Q}, State};
handle_call(_Request, _From, State) ->
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
handle_cast({'stop_agent', Req}, #state{supervisor=Supervisor}=State) ->
    lager:debug("stop agent requested by ~p", [Req]),
    _ = spawn('acdc_agent_sup', 'stop', [Supervisor]),
    {'noreply', State};

handle_cast({'fsm_started', FSMPid}, State) ->
    lager:debug("fsm started: ~p", [FSMPid]),
    handle_fsm_started(FSMPid),
    {'noreply', State#state{fsm_pid=FSMPid
                            ,my_id=acdc_util:proc_id(FSMPid)
                           }};

handle_cast({'created_queue', Q}, State) ->
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
    handle_cast({'queue_login', wh_json:get_value(<<"_id">>, QJObj)}, State);

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

handle_cast({'channel_hungup', CallId}, #state{call=Call
                                               ,record_calls=ShouldRecord
                                               ,recording_url=RecordingUrl
                                               ,is_thief=IsThief
                                               ,agent_call_id=ACallId
                                               ,agent_call_queue=ACtrlQ
                                               ,agent_id=AgentId
                                               ,my_q=MyQ
                                              }=State) ->
    CCallId = call_id(Call),
    case CallId of
        CCallId ->
            lager:debug("member channel hungup, done with this call"),
            acdc_util:unbind_from_call_events(Call),
            acdc_util:unbind_from_call_events(ACallId),

            maybe_stop_recording(Call, ShouldRecord, RecordingUrl),
            stop_agent_leg(MyQ, ACallId, ACtrlQ),

            put('callid', AgentId),
            case IsThief of
                'false' ->
                    {'noreply', State#state{call='undefined'
                                            ,msg_queue_id='undefined'
                                            ,acdc_queue_id='undefined'
                                            ,agent_call_id='undefined'
                                            ,agent_call_queue='undefined'
                                            ,recording_url='undefined'
                                           }
                     ,'hibernate'};
                'true' ->
                    lager:debug("thief is done, going down"),
                    acdc_agent:stop(self()),
                    {'noreply', State}
            end;
        ACallId ->
            lager:debug("agent channel ~s hungup/needs hanging up", [ACallId]),
            acdc_util:unbind_from_call_events(ACallId),
            stop_agent_leg(MyQ, ACallId, ACtrlQ),
            {'noreply', State#state{agent_call_id='undefined'}, 'hibernate'};
        _CallId ->
            lager:debug("unknown call id ~s for channel_hungup, ignoring", [_CallId]),
            lager:debug("listening for agent(~s) and caller(~s)", [ACallId, CCallId]),
            {'noreply', State}
    end;

handle_cast({'member_connect_retry', CallId}, #state{my_id=MyId
                                                     ,msg_queue_id=Server
                                                     ,agent_call_id=ACallId
                                                     ,call=Call
                                                     ,agent_id=AgentId
                                                    }=State) when is_binary(CallId) ->
    case catch whapps_call:call_id(Call) of
        CallId ->
            lager:debug("need to retry member connect, agent isn't able to take it"),
            send_member_connect_retry(Server, CallId, MyId),

            acdc_util:unbind_from_call_events(ACallId),
            acdc_util:unbind_from_call_events(CallId),

            put('callid', AgentId),

            {'noreply', State#state{msg_queue_id='undefined'
                                    ,acdc_queue_id='undefined'
                                    ,agent_call_id='undefined'
                                    ,call='undefined'
                                   }
             ,'hibernate'
            };
        _MCallId ->
            lager:debug("retry call id(~s) is not our member call id ~p, ignoring", [CallId, _MCallId]),
            {'noreply', State}
    end;
handle_cast({'member_connect_retry', WinJObj}, #state{my_id=MyId}=State) ->
    lager:debug("cannot process this win, sending a retry: ~s", [call_id(WinJObj)]),
    send_member_connect_retry(WinJObj, MyId),
    {'noreply', State};

handle_cast('member_connect_accepted', #state{msg_queue_id=AmqpQueue
                                              ,call=Call
                                              ,acct_id=AcctId
                                              ,agent_id=AgentId
                                              ,my_id=MyId
                                              ,record_caller=ShouldRecord
                                             }=State) ->
    lager:debug("member bridged to agent!"),
    maybe_start_recording(Call, ShouldRecord),
    send_member_connect_accepted(AmqpQueue, call_id(Call), AcctId, AgentId, MyId),
    {'noreply', State};

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

    AgentCallId = outbound_call_id(Call),
    acdc_util:bind_to_call_events(Call, CDRUrl),
    acdc_util:bind_to_call_events(AgentCallId, CDRUrl),

    maybe_connect_to_agent(MyQ, EPs, Call, RingTimeout),

    lager:debug("originate sent, waiting on successful bridge now"),
    update_my_queues_of_change(AcctId, AgentId, Qs),
    {'noreply', State#state{call=Call
                            ,record_calls=ShouldRecord
                            ,msg_queue_id=wh_json:get_value(<<"Server-ID">>, WinJObj)
                            ,agent_call_id=AgentCallId
                            ,cdr_urls=dict:store(whapps_call:call_id(Call), CDRUrl,
                                                 dict:store(AgentCallId, CDRUrl, Urls)
                                                )
                            ,recording_url=RecordingUrl
                           }
     ,'hibernate'};

handle_cast({'bridge_to_member', Call, WinJObj, _, CDRUrl, RecordingUrl}, #state{is_thief='true'
                                                                                 ,agent=Agent
                                                                                 ,cdr_urls=Urls
                                                                                }=State) ->
    _ = whapps_call:put_callid(Call),
    lager:debug("connecting to thief at ~s", [whapps_call:call_id(Agent)]),
    acdc_util:bind_to_call_events(Call, CDRUrl),

    AgentCallId = outbound_call_id(Call),
    acdc_util:bind_to_call_events(AgentCallId, CDRUrl),

    ShouldRecord = record_calls(Agent) orelse wh_json:is_true(<<"Record-Caller">>, WinJObj, 'false'),

    whapps_call_command:pickup(whapps_call:call_id(Agent), <<"now">>, Call),

    {'noreply', State#state{call=Call
                            ,msg_queue_id=wh_json:get_value(<<"Server-ID">>, WinJObj)
                            ,agent_call_id=AgentCallId
                            ,cdr_urls=dict:store(whapps_call:call_id(Call), CDRUrl,
                                                 dict:store(AgentCallId, CDRUrl, Urls)
                                                )
                            ,record_calls=ShouldRecord
                            ,recording_url=RecordingUrl
                           }
     ,'hibernate'};

handle_cast({'monitor_call', Call, CDRUrl, RecordingUrl}, #state{cdr_urls=Urls}=State) ->
    _ = whapps_call:put_callid(Call),

    AgentCallId = outbound_call_id(Call),

    acdc_util:bind_to_call_events(Call, CDRUrl),
    acdc_util:bind_to_call_events(AgentCallId, CDRUrl),

    lager:debug("monitoring member call ~s, agent call on ~s", [whapps_call:call_id(Call), AgentCallId]),

    {'noreply', State#state{call=Call
                            ,agent_call_id=AgentCallId
                            ,cdr_urls=dict:store(whapps_call:call_id(Call), CDRUrl,
                                                 dict:store(AgentCallId, CDRUrl, Urls)
                                                )
                            ,recording_url=RecordingUrl
                           }, 'hibernate'};

handle_cast({'originate_execute', JObj}, #state{my_q=Q}=State) ->
    ACallId = wh_json:get_value(<<"Call-ID">>, JObj),
    acdc_util:bind_to_call_events(ACallId),

    lager:debug("execute the originate for agent call-id ~s", [ACallId]),

    send_originate_execute(JObj, Q),
    {'noreply', State#state{agent_call_id=ACallId}, 'hibernate'};

handle_cast({'outbound_call', Call}, State) ->
    _ = whapps_call:put_callid(Call),
    acdc_util:bind_to_call_events(Call),

    lager:debug("bound to agent's outbound call"),
    {'noreply', State#state{call=Call}, 'hibernate'};

handle_cast({'agent_call_id', ACallId, ACtrlQ}, #state{call=Call
                                                       ,record_calls=ShouldRecord
                                                      }=State) ->
    lager:debug("agent call id set: ~s using ctrl ~s", [ACallId, ACtrlQ]),

    acdc_util:bind_to_call_events(ACallId),

    {'noreply', State#state{agent_call_id=ACallId
                            ,agent_call_queue=ACtrlQ
                           }, 'hibernate'};

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

handle_cast({'unbind_from_cdr', CallId}, #state{cdr_urls=Urls}=State) ->
    acdc_util:unbind_from_cdr(CallId),
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
                           ,cdr_urls=Urls
                          }) ->
    {'reply', [{'fsm_pid', FSM}
               ,{'agent_id', AgentId}
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
                                     ,ne_binary(), ne_binary(), wh_now() | 'undefined'
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

-spec send_member_connect_retry(wh_json:object(), ne_binary()) -> 'ok'.
-spec send_member_connect_retry(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_member_connect_retry(JObj, MyId) ->
    send_member_connect_retry(wh_json:get_value(<<"Server-ID">>, JObj)
                              ,call_id(JObj)
                              ,MyId).

send_member_connect_retry('undefined', _, _) ->
    lager:debug("no queue to send the retry to, seems bad");
send_member_connect_retry(Queue, CallId, MyId) ->
    Resp = props:filter_undefined(
             [{<<"Process-ID">>, MyId}
              ,{<<"Call-ID">>, CallId}
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


-spec idle_time('undefined' | wh_now()) -> 'undefined' | integer().
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

-spec maybe_connect_to_agent(ne_binary(), list(), whapps_call:call(), integer() | 'undefined') -> 'ok'.
maybe_connect_to_agent(MyQ, EPs, Call, Timeout) ->
    put('callid', whapps_call:call_id(Call)),

    ReqId = wh_util:rand_hex_binary(6),
    AcctId = whapps_call:account_id(Call),

    CCVs = props:filter_undefined([{<<"Account-ID">>, AcctId}
                                   ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
                                   ,{<<"Request-ID">>, ReqId}
                                   ,{<<"Retain-CID">>, <<"true">>}
                                  ]),

    Endpoints = [wh_json:set_values([{<<"Endpoint-Timeout">>, Timeout}
                                     ,{<<"Outgoing-Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
                                     ,{<<"Outgoing-Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
                                    ], EP)
                 || EP <- EPs
                ],

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
              ,{<<"Outgoing-Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
              ,{<<"Outgoing-Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
              ,{<<"Outbound-Call-ID">>, outbound_call_id(Call)}
              ,{<<"Existing-Call-ID">>, whapps_call:call_id(Call)}
              | wh_api:default_headers(MyQ, ?APP_NAME, ?APP_VERSION)
             ]),

    lager:debug("sending originate request"),

    wapi_resource:publish_originate_req(Prop).

outbound_call_id(CallId) when is_binary(CallId) -> wh_util:to_hex_binary(erlang:md5(CallId));
outbound_call_id(Call) -> outbound_call_id(whapps_call:call_id(Call)).

-spec login_to_queue(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
login_to_queue(AcctId, AgentId, QueueId) ->
    gen_listener:add_binding(self(), 'acdc_queue', [{'restrict_to', ['member_connect_req']}
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
    gen_listener:rm_binding(self(), 'acdc_queue', [{'restrict_to', ['member_connect_req']}
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

-spec should_record_endpoints(wh_json:objects(), boolean(), boolean() | 'undefined') -> boolean().
should_record_endpoints(_EPs, 'true', _) -> 'true';
should_record_endpoints(_EPs, 'false', 'true') -> 'true';
should_record_endpoints(EPs, _, _) ->
    lists:any(fun(EP) ->
                      wh_json:is_true(<<"record_calls">>, EP, 'false')
              end, EPs).

maybe_stop_recording(_Call, 'false', _) -> 'ok';
maybe_stop_recording(Call, 'true', Url) ->
    Format = recording_format(),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),

    _ = whapps_call_command:record_call(MediaName, <<"stop">>, Call),
    lager:debug("recording of ~s stopped", [MediaName]),

    save_recording(Call, MediaName, Format, Url).

maybe_start_recording(_Call, 'false') -> lager:debug("not recording this call");
maybe_start_recording(Call, 'true') ->
    Format = recording_format(),
    MediaName = get_media_name(whapps_call:call_id(Call), Format),
    lager:debug("recording of ~s started", [MediaName]),

    whapps_call_command:record_call(MediaName, <<"start">>, Call).

recording_format() ->
    whapps_config:get(<<"callflow">>, [<<"call_recording">>, <<"extension">>], <<"mp3">>).

save_recording(Call, MediaName, Format, Url) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"store_recordings">>, 'false') of
        'true' ->
            {'ok', MediaJObj} = store_recording_meta(Call, MediaName, Format),
            lager:debug("stored meta: ~p", [MediaJObj]),

            StoreUrl = store_url(Call, MediaJObj),
            lager:debug("store url: ~s", [StoreUrl]),

            store_recording(MediaName, StoreUrl, Call);
        'false' when is_binary(Url) ->
            StoreUrl = iolist_to_binary([wh_util:strip_right_binary(Url, <<"/">>), "/", MediaName]),
            lager:debug("using ~s to maybe store recording", [StoreUrl]),

            store_recording(MediaName, StoreUrl, Call);
        'false' ->
            lager:debug("no external url to use, not saving recording")
    end.

-spec store_recording(ne_binary(), api_binary(), whapps_call:call()) -> 'ok'.
store_recording(_, 'undefined', _) -> lager:debug("no url to store");
store_recording(MediaName, StoreUrl, Call) ->
    'ok' = whapps_call_command:store(MediaName, StoreUrl, Call),
    whapps_call_command:set('undefined', wh_json:from_list([{<<"Recording-URL">>, StoreUrl}]), Call).

-spec store_recording_meta(whapps_call:call(), ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', any()}.
store_recording_meta(Call, MediaName, Ext) ->
    AcctDb = whapps_call:account_db(Call),
    CallId = whapps_call:call_id(Call),

    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list(
                   [{<<"name">>, MediaName}
                    ,{<<"description">>, <<"acdc recording ", MediaName/binary>>}
                    ,{<<"content_type">>, ext_to_mime(Ext)}
                    ,{<<"media_type">>, Ext}
                    ,{<<"media_source">>, <<"recorded">>}
                    ,{<<"source_type">>, wh_util:to_binary(?MODULE)}
                    ,{<<"pvt_type">>, <<"private_media">>}
                    ,{<<"from">>, whapps_call:from(Call)}
                    ,{<<"to">>, whapps_call:to(Call)}
                    ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                    ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                    ,{<<"call_id">>, CallId}
                    ,{<<"_id">>, get_recording_doc_id(CallId)}
                   ])
                 ,AcctDb
                ),
    couch_mgr:save_doc(AcctDb, MediaDoc).

ext_to_mime(<<"wav">>) -> <<"audio/x-wav">>;
ext_to_mime(_) -> <<"audio/mp3">>.

get_recording_doc_id(CallId) -> <<"call_recording_", CallId/binary>>.

-spec get_media_name(ne_binary(), ne_binary()) -> ne_binary().
get_media_name(CallId, Ext) ->
    <<(get_recording_doc_id(CallId))/binary, ".", Ext/binary>>.

-spec store_url(whapps_call:call(), wh_json:object()) -> ne_binary().
store_url(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),
    MediaName = wh_json:get_value(<<"name">>, JObj),

    Rev = wh_json:get_value(<<"_rev">>, JObj),
    list_to_binary([wh_couch_connections:get_url(), AccountDb
                    ,"/", MediaId
                    ,"/", MediaName
                    ,"?rev=", Rev
                   ]).

-spec agent_id(agent()) -> api_binary().
agent_id(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> wh_json:get_value(<<"_id">>, Agent);
        'false' -> whapps_call:owner_id(Agent)
    end.

-spec account_id(agent()) -> api_binary().
account_id(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> wh_json:get_value(<<"pvt_account_id">>, Agent);
        'false' -> whapps_call:account_id(Agent)
    end.

-spec account_db(agent()) -> api_binary().
account_db(Agent) ->
    case wh_json:is_json_object(Agent) of
        'true' -> wh_json:get_value(<<"pvt_account_db">>, Agent);
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

stop_agent_leg('undefined', _, _) -> 'ok';
stop_agent_leg(_, 'undefined', _) -> 'ok';
stop_agent_leg(_, _, 'undefined') -> 'ok';
stop_agent_leg(MyQ, ACallId, ACtrlQ) ->
    Command = [{<<"Application-Name">>, <<"hangup">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Call-ID">>, ACallId}
               | wh_api:default_headers(MyQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_dialplan:publish_command(ACtrlQ, Command).
