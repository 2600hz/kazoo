%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc Manages queue processes:
%%%   starting when a queue is created
%%%   stopping when a queue is deleted
%%%   collecting stats from queues
%%%   and more!!!
%%%
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_manager).
-behaviour(gen_listener).

%% API
-export([start_link/2, start_link/3
        ,handle_member_call/2
        ,handle_member_call_success/2
        ,handle_member_call_cancel/2
        ,handle_agent_change/2
        ,handle_queue_member_add/2
        ,handle_queue_member_remove/2
        ,has_agents/1
        ,handle_config_change/2
        ,should_ignore_member_call/3, should_ignore_member_call/4
        ,up_next/2
        ,config/1
        ,agents/1
        ,refresh/2
        ,add_diagnostics_receiver/2
        ,remove_diagnostics_receiver/2
        ]).

%% FSM helpers
-export([pick_winner/2]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-ifdef(TEST).
-export([update_strategy_with_agent/3

        ,assignable_agent_count/1
        ,agent_count/1
        ]).
-endif.

-include("acdc.hrl").
-include("acdc_queue_manager.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS(A, Q), [{'conf', [{'type', <<"queue">>}
                                  ,{'db', kz_util:format_account_id(A, 'encoded')}
                                  ,{'id', Q}
                                  ,'federate'
                                  ]}
                        ,{'acdc_queue', [{'restrict_to', ['stats_req', 'agent_change'
                                                         ,'member_addremove', 'member_call_result'
                                                         ]}
                                        ,{'account_id', A}
                                        ,{'queue_id', Q}
                                        ]}
                        ,{'presence', [{'restrict_to', ['probe']}]}
                        ]).

-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_config_change'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_stats_req'}
                     ,[{<<"queue">>, <<"stats_req">>}]
                     }
                    ,{{'acdc_queue_handler', 'handle_presence_probe'}
                     ,[{<<"presence">>, <<"probe">>}]
                     }
                    ,{{?MODULE, 'handle_member_call'}
                     ,[{<<"member">>, <<"call">>}]
                     }
                    ,{{?MODULE, 'handle_member_call_success'}
                     ,[{<<"member">>, <<"call_success">>}]
                     }
                    ,{{?MODULE, 'handle_member_call_cancel'}
                     ,[{<<"member">>, <<"call_cancel">>}]
                     }
                    ,{{?MODULE, 'handle_agent_change'}
                     ,[{<<"queue">>, <<"agent_change">>}]
                     }
                    ,{{?MODULE, 'handle_queue_member_add'}
                     ,[{<<"queue">>, <<"member_add">>}]
                     }
                    ,{{?MODULE, 'handle_queue_member_remove'}
                     ,[{<<"queue">>, <<"member_remove">>}]
                     }
                    ]).

-define(SECONDARY_BINDINGS(AccountId, QueueId)
       ,[{'acdc_queue', [{'restrict_to', ['member_call']}
                        ,{'account_id', AccountId}
                        ,{'queue_id', QueueId}
                        ]}
        ]).
-define(SECONDARY_QUEUE_NAME(QueueId), <<"acdc.queue.manager.", QueueId/binary>>).
-define(SECONDARY_QUEUE_OPTIONS(MaxPriority), [{'exclusive', 'false'}
                                              ,{'arguments',[{<<"x-max-priority">>, MaxPriority}]}
                                              ]).
-define(SECONDARY_CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), kz_json:object()) -> kz_types:startlink_ret().
start_link(Super, QueueJObj) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Super, QueueJObj]
                           ).

-spec start_link(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(Super, AccountId, QueueId) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Super, AccountId, QueueId]
                           ).

-spec handle_member_call(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_call(JObj, Props) ->
    'true' = kapi_acdc_queue:member_call_v(JObj),
    _ = kz_util:put_callid(JObj),

    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    Srv = props:get_value('server', Props),

    case has_agents(Srv, props:get_value('enter_when_empty', Props)) of
        'false' ->
            lager:info("no agents are available to take the call, cancel queueing"),
            gen_listener:cast(Srv, {'reject_member_call', Call, JObj});
        'true' ->
            start_queue_call(JObj, Props, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Return true if the queue operated by `Srv' has at least 1 agent or
%% should behave as if it does (`enter_when_empty' is enabled).
%% @end
%%------------------------------------------------------------------------------
-spec has_agents(kz_types:server_ref()) -> boolean().
has_agents(Srv) ->
    has_agents(Srv, gen_listener:call(Srv, 'enter_when_empty')).

%%------------------------------------------------------------------------------
%% @doc Return true if `EnterWhenEmpty' is true, otherwise return true if the
%% queue operated by `Srv' has at least 1 agent.
%% @end
%%------------------------------------------------------------------------------
-spec has_agents(kz_types:server_ref(), boolean()) -> boolean().
has_agents(Srv, EnterWhenEmpty) ->
    EnterWhenEmpty
        orelse gen_listener:call(Srv, 'get_has_agents').

start_queue_call(JObj, Props, Call) ->
    _ = kapps_call:put_callid(Call),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    lager:info("member call for queue ~s recv", [QueueId]),
    lager:debug("answering call"),
    kapps_call_command:answer_now(Call),

    case kz_media_util:media_path(props:get_value('moh', Props)
                                 ,kapps_call:account_id(Call)
                                 )
    of
        'undefined' ->
            lager:debug("using default moh"),
            kapps_call_command:hold(Call);
        MOH ->
            lager:debug("using MOH ~s (~p)", [MOH, Props]),
            kapps_call_command:hold(MOH, Call)
    end,

    JObj2 = kz_json:set_value([<<"Call">>, <<"Custom-Channel-Vars">>, <<"Queue-ID">>], QueueId, JObj),

    _ = kapps_call_command:set('undefined'
                              ,kz_json:from_list([{<<"Eavesdrop-Group-ID">>, QueueId}
                                                 ,{<<"Queue-ID">>, QueueId}
                                                 ])
                              ,Call
                              ),

    %% Add member to queue for tracking position
    gen_listener:cast(props:get_value('server', Props), {'add_queue_member', JObj2}).

-spec handle_member_call_success(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_call_success(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_remove', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec handle_member_call_cancel(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_call_cancel(JObj, Props) ->
    'true' = kapi_acdc_queue:member_call_cancel_v(JObj),
    _ = kz_util:put_callid(JObj),
    K = make_ignore_key(kz_json:get_value(<<"Account-ID">>, JObj)
                       ,kz_json:get_value(<<"Queue-ID">>, JObj)
                       ,kz_json:get_value(<<"Call-ID">>, JObj)
                       ),
    gen_listener:cast(props:get_value('server', Props), {'member_call_cancel', K, JObj}).

-spec handle_agent_change(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_agent_change(JObj, Prop) ->
    'true' = kapi_acdc_queue:agent_change_v(JObj),
    Server = props:get_value('server', Prop),
    case kz_json:get_value(<<"Change">>, JObj) of
        <<"available">> ->
            gen_listener:cast(Server, {'agent_available', JObj});
        <<"ringing">> ->
            gen_listener:cast(Server, {'agent_ringing', JObj});
        <<"busy">> ->
            gen_listener:cast(Server, {'agent_busy', JObj});
        <<"unavailable">> ->
            gen_listener:cast(Server, {'agent_unavailable', JObj})
    end.

-spec handle_queue_member_add(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_queue_member_add(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_add', JObj}).

-spec handle_queue_member_remove(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_queue_member_remove(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_remove', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec handle_config_change(kz_types:server_ref(), kz_json:object()) -> 'ok'.
handle_config_change(Srv, JObj) ->
    gen_listener:cast(Srv, {'update_queue_config', JObj}).

-spec should_ignore_member_call(kz_types:server_ref(), kapps_call:call(), kz_json:object()) -> boolean().
should_ignore_member_call(Srv, Call, CallJObj) ->
    should_ignore_member_call(Srv
                             ,Call
                             ,kz_json:get_value(<<"Account-ID">>, CallJObj)
                             ,kz_json:get_value(<<"Queue-ID">>, CallJObj)
                             ).

-spec should_ignore_member_call(kz_types:server_ref(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
should_ignore_member_call(Srv, Call, AccountId, QueueId) ->
    K = make_ignore_key(AccountId, QueueId, kapps_call:call_id(Call)),
    gen_listener:call(Srv, {'should_ignore_member_call', K}).

-spec up_next(pid(), kz_term:ne_binary()) -> boolean().
up_next(Srv, CallId) ->
    gen_listener:call(Srv, {'up_next', CallId}).

-spec config(pid()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
config(Srv) -> gen_listener:call(Srv, 'config').

%%------------------------------------------------------------------------------
%% @doc Return the IDs of the agents in the queue operated by `Srv'.
%% @end
%%------------------------------------------------------------------------------
-spec agents(kz_types:server_ref()) -> kz_term:ne_binaries().
agents(Srv) -> gen_listener:call(Srv, 'get_agents').

-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(Mgr, QueueJObj) -> gen_listener:cast(Mgr, {'refresh', QueueJObj}).

strategy(Srv) -> gen_listener:call(Srv, 'strategy').
next_winner(Srv) -> gen_listener:call(Srv, 'next_winner').

-spec pick_winner(pid(), kz_json:objects()) ->
          'undefined' |
          {kz_json:objects(), kz_json:objects()}.
pick_winner(Srv, Resps) -> pick_winner(Srv, Resps, strategy(Srv), next_winner(Srv)).

%%------------------------------------------------------------------------------
%% @doc Add a diagnostics receiver to the queue manager as a recipient of
%% strategy state diagnostic messages.
%% @end
%%------------------------------------------------------------------------------
-spec add_diagnostics_receiver(kz_types:server_ref(), pid()) -> any().
add_diagnostics_receiver(Srv, Receiver) ->
    gen_listener:call(Srv, {'add_diagnostics_receiver', Receiver}).

%%------------------------------------------------------------------------------
%% @doc Remove a previously-added diagnostics receiver from the queue manager.
%% @end
%%------------------------------------------------------------------------------
-spec remove_diagnostics_receiver(kz_types:server_ref(), pid()) -> any().
remove_diagnostics_receiver(Srv, Receiver) ->
    gen_listener:call(Srv, {'remove_diagnostics_receiver', Receiver}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([pid() | kz_json:object() | kz_term:ne_binary()]) -> {'ok', mgr_state()}.
init([Super, QueueJObj]) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    kz_util:put_callid(<<"mgr_", QueueId/binary>>),

    init(Super, AccountId, QueueId, QueueJObj);

init([Super, AccountId, QueueId]) ->
    kz_util:put_callid(<<"mgr_", QueueId/binary>>),
    put(?KEY_DIAGNOSTICS_PIDS, []),

    AcctDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(AcctDb, QueueId),

    init(Super, AccountId, QueueId, QueueJObj).

init(Super, AccountId, QueueId, QueueJObj) ->
    process_flag('trap_exit', 'false'),

    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    _ = kz_datamgr:add_to_doc_cache(AccountDb, QueueId, QueueJObj),

    _ = start_secondary_queue(AccountId, QueueId),

    gen_listener:cast(self(), {'start_workers'}),
    Strategy = get_strategy(kz_json:get_value(<<"strategy">>, QueueJObj)),
    StrategyState = create_strategy_state(Strategy),

    lager:debug("queue mgr started for ~s", [QueueId]),
    {'ok', update_properties(QueueJObj, #state{account_id=AccountId
                                              ,queue_id=QueueId
                                              ,supervisor=Super
                                              ,strategy=Strategy
                                              ,strategy_state=StrategyState
                                              })}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), mgr_state()) -> kz_types:handle_call_ret_state(mgr_state()).
handle_call({'should_ignore_member_call', {AccountId, QueueId, CallId}=K}, _, #state{ignored_member_calls=Dict
                                                                                    ,account_id=AccountId
                                                                                    ,queue_id=QueueId
                                                                                    }=State) ->
    case catch dict:fetch(K, Dict) of
        {'EXIT', _} -> {'reply', 'false', State};
        _Res ->
            publish_queue_member_remove(AccountId, QueueId, CallId),
            {'reply', 'true', State#state{ignored_member_calls=dict:erase(K, Dict)}}
    end;

handle_call({'up_next', CallId}, _, #state{current_member_calls=Calls}=State) ->
    Position = queue_member_position(CallId, lists:reverse(Calls)),
    {'reply', assignable_agent_count(State) >= Position, State};

handle_call('config', _, #state{account_id=AccountId
                               ,queue_id=QueueId
                               }=State) ->
    {'reply', {AccountId, QueueId}, State};

handle_call('strategy', _, #state{strategy=Strategy}=State) ->
    {'reply', Strategy, State, 'hibernate'};

handle_call('enter_when_empty', _, #state{enter_when_empty=EnterWhenEmpty}=State) ->
    {'reply', EnterWhenEmpty, State};

handle_call('next_winner', _, #state{strategy='mi'}=State) ->
    {'reply', 'undefined', State};
handle_call('next_winner', _, #state{strategy='rr'
                                    ,strategy_state=#strategy_state{agents=Agents}=SS
                                    }=State) ->
    case queue:out(Agents) of
        {{'value', Winner}, Agents1} ->
            ?DIAG("got next winner from ~p", [queue:to_list(Agents)]),
            {'reply', Winner, State#state{strategy_state=SS#strategy_state{agents=queue:in(Winner, Agents1)}}, 'hibernate'};
        {'empty', _} ->
            {'reply', 'undefined', State}
    end;

handle_call('get_agents', _, State) ->
    {'reply', agents_(State), State};

handle_call('get_has_agents', _, State) ->
    {'reply', agent_count(State) > 0, State};

handle_call({'queue_position', CallId}, _, #state{current_member_calls=Calls}=State) ->
    Position = queue_member_position(CallId, lists:reverse(Calls)),
    {'reply', Position, State};

handle_call({'add_diagnostics_receiver', Receiver}, _, State) ->
    DiagnosticsPids = get(?KEY_DIAGNOSTICS_PIDS),
    put(?KEY_DIAGNOSTICS_PIDS, [Receiver | DiagnosticsPids]),
    lager:debug("added ~p to diagnostics receivers", [Receiver]),
    {'reply', 'ok', State};

handle_call({'remove_diagnostics_receiver', Receiver}, _, State) ->
    DiagnosticsPids = get(?KEY_DIAGNOSTICS_PIDS),
    put(?KEY_DIAGNOSTICS_PIDS, lists:delete(Receiver, DiagnosticsPids)),
    lager:debug("removed ~p from diagnostics receivers", [Receiver]),
    {'reply', 'ok', State};

handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), mgr_state()) -> kz_types:handle_cast_ret_state(mgr_state()).
handle_cast({'update_strategy', StrategyState}, State) ->
    {'noreply', State#state{strategy_state=StrategyState}, 'hibernate'};

handle_cast({'update_queue_config', JObj}, #state{enter_when_empty=_EnterWhenEmpty}=State) ->
    EWE = kz_json:is_true([<<"Doc">>, <<"enter_when_empty">>], JObj, 'true'),
    lager:debug("maybe changing ewe from ~s to ~s", [_EnterWhenEmpty, EWE]),
    {'noreply', State#state{enter_when_empty=EWE}, 'hibernate'};

handle_cast({'member_call_cancel', K, JObj}, #state{ignored_member_calls=Dict
                                                   ,current_member_calls=Calls
                                                   }=State) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    Reason = kz_json:get_value(<<"Reason">>, JObj),

    'ok' = acdc_stats:call_abandoned(AccountId, QueueId, CallId, Reason),

    %% For cancels triggered outside of cf_acdc_member, inform cf_acdc_member
    %% proc to continue
    case queue_member(CallId, Calls) of
        'undefined' -> 'ok';
        Call ->
            Q = kapps_call:controller_queue(Call),
            publish_member_call_failure(Q, AccountId, QueueId, CallId, Reason)
    end,

    {'noreply', State#state{ignored_member_calls=dict:store(K, 'true', Dict)}};

handle_cast({'monitor_call', Call}, State) ->
    CallId = kapps_call:call_id(Call),
    gen_listener:add_binding(self(), 'call', [{'callid', CallId}
                                             ,{'restrict_to', [<<"CHANNEL_DESTROY">>]}
                                             ]),
    lager:debug("bound for call events for ~s", [CallId]),
    {'noreply', State};
handle_cast({'start_workers'}, #state{account_id=AccountId
                                     ,queue_id=QueueId
                                     ,supervisor=QueueSup
                                     }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    case kz_datamgr:get_results(kz_util:format_account_id(AccountId, 'encoded')
                               ,<<"queues/agents_listing">>
                               ,[{'startkey', [QueueId]}
                                ,{'endkey', [QueueId, kz_json:new()]}
                                ,{'group', 'true'}
                                ,{'group_level', 1}
                                ])
    of
        {'ok', []} ->
            lager:debug("no agents yet, but create a worker anyway"),
            acdc_queue_workers_sup:new_worker(WorkersSup, AccountId, QueueId);
        {'ok', [Result]} ->
            QWC = kz_json:get_integer_value(<<"value">>, Result),
            acdc_queue_workers_sup:new_workers(WorkersSup, AccountId, QueueId, QWC),
            'ok';
        {'error', _E} ->
            lager:debug("failed to find agent count: ~p", [_E]),
            QWC = kapps_config:get_integer(?CONFIG_CAT, <<"queue_worker_count">>, 5),
            acdc_queue_workers_sup:new_workers(WorkersSup, AccountId, QueueId, QWC)
    end,
    {'noreply', State};

handle_cast({'start_worker'}, State) ->
    handle_cast({'start_worker', 1}, State);
handle_cast({'start_worker', N}, #state{account_id=AccountId
                                       ,queue_id=QueueId
                                       ,supervisor=QueueSup
                                       }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    acdc_queue_workers_sup:new_workers(WorkersSup, AccountId, QueueId, N),
    {'noreply', State};

handle_cast({'agent_available', AgentId}, #state{supervisor=QueueSup}=State) when is_binary(AgentId) ->
    Data = #{},
    StrategyState1 = update_strategy_with_agent(State, AgentId, 'available', Data),
    State1 = State#state{strategy_state=StrategyState1},
    maybe_start_queue_workers(QueueSup, agent_count(State1)),
    {'noreply', State1, 'hibernate'};
handle_cast({'agent_available', JObj}, State) ->
    handle_cast({'agent_available', kz_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_ringing', AgentId}, #state{strategy=Strategy}=State) when is_binary(AgentId) ->
    lager:info("agent ~s ringing, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(State, AgentId, 'ringing'),
    State1 = State#state{strategy_state=StrategyState1},
    {'noreply', State1, 'hibernate'};
handle_cast({'agent_ringing', JObj}, State) ->
    handle_cast({'agent_ringing', kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_busy', AgentId}, #state{strategy=Strategy}=State) when is_binary(AgentId) ->
    lager:info("agent ~s busy, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(State, AgentId, 'busy'),
    State1 = State#state{strategy_state=StrategyState1},
    {'noreply', State1, 'hibernate'};
handle_cast({'agent_busy', JObj}, State) ->
    handle_cast({'agent_busy', kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_unavailable', AgentId}, #state{strategy=Strategy}=State) when is_binary(AgentId) ->
    lager:info("agent ~s unavailable, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(State, AgentId, 'unavailable'),
    State1 = State#state{strategy_state=StrategyState1},
    {'noreply', State1, 'hibernate'};
handle_cast({'agent_unavailable', JObj}, State) ->
    handle_cast({'agent_unavailable', kz_json:get_ne_binary_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'reject_member_call', Call, JObj}, #state{account_id=AccountId
                                                      ,queue_id=QueueId
                                                      }=State) ->
    Q = kz_json:get_value(<<"Server-ID">>, JObj),
    publish_member_call_failure(Q, AccountId, QueueId, kapps_call:call_id(Call), <<"no agents">>),
    {'noreply', State};

handle_cast({'gen_listener', {'created_queue', ?SECONDARY_QUEUE_NAME(QueueId)}}, #state{queue_id=QueueId}=State) ->
    {'noreply', State};

handle_cast({'gen_listener', {'created_queue', _}}, #state{account_id=AccountId
                                                          ,queue_id=QueueId
                                                          }=State) ->
    kapi_acdc_queue:publish_started_notif(
      kz_json:from_list([{<<"Account-ID">>, AccountId}
                        ,{<<"Queue-ID">>, QueueId}
                         | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                        ])
     ),
    {'noreply', State};

handle_cast({'refresh', QueueJObj}, State) ->
    lager:debug("refreshing queue configs"),
    {'noreply', update_properties(QueueJObj, State), 'hibernate'};

handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};

handle_cast({'add_queue_member', JObj}, #state{account_id=AccountId
                                              ,queue_id=QueueId
                                              ,current_member_calls=Calls
                                              ,announcements_config=AnnouncementsConfig
                                              ,announcements_pids=AnnouncementsPids
                                              }=State) ->
    Position = length(Calls)+1,
    Call = kapps_call:set_custom_channel_var(<<"Queue-Position">>
                                            ,Position
                                            ,kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj))),

    {CIDNumber, CIDName} = acdc_util:caller_id(Call),
    'ok' = acdc_stats:call_waiting(AccountId, QueueId
                                  ,kapps_call:call_id(Call)
                                  ,CIDName
                                  ,CIDNumber
                                  ,kz_json:get_integer_value(<<"Member-Priority">>, JObj)
                                  ),

    publish_queue_member_add(AccountId, QueueId, Call),

    %% Add call to shared queue
    kapi_acdc_queue:publish_shared_member_call(AccountId, QueueId, JObj),
    lager:debug("put call into shared messaging queue"),

    gen_listener:cast(self(), {'monitor_call', Call}),

    acdc_util:presence_update(AccountId, QueueId, ?PRESENCE_RED_FLASH),

    %% Schedule position/wait time announcements
    AnnouncementsPids1 = case acdc_announcements_sup:maybe_start_announcements(self(), Call, AnnouncementsConfig) of
                             'false' -> AnnouncementsPids;
                             {'ok', Pid} ->
                                 CallId = kapps_call:call_id(Call),
                                 AnnouncementsPids#{CallId => Pid}
                         end,

    {'noreply', State#state{current_member_calls=[Call | Calls]
                           ,announcements_pids=AnnouncementsPids1
                           }};

handle_cast({'handle_queue_member_add', JObj}, #state{current_member_calls=CurrentCalls}=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    CallId = kapps_call:call_id(Call),
    lager:debug("received notification of new queue member ~s", [CallId]),

    {'noreply', State#state{current_member_calls = [Call | lists:keydelete(CallId, 2, CurrentCalls)]}};

handle_cast({'handle_queue_member_remove', CallId}, State) ->
    State1 = remove_queue_member(CallId, State),
    {'noreply', State1};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), mgr_state()) -> kz_types:handle_info_ret_state(mgr_state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), mgr_state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{enter_when_empty=EnterWhenEmpty
                          ,moh=MOH
                          }) ->
    {'reply', [{'enter_when_empty', EnterWhenEmpty}
              ,{'moh', MOH}
              ]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), mgr_state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("queue manager terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), mgr_state(), any()) -> {'ok', mgr_state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
start_secondary_queue(AccountId, QueueId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Priority = acdc_util:max_priority(AccountDb, QueueId),
    kz_util:spawn(fun gen_listener:add_queue/4
                 ,[self()
                  ,?SECONDARY_QUEUE_NAME(QueueId)
                  ,[{'queue_options', ?SECONDARY_QUEUE_OPTIONS(Priority)}
                   ,{'consume_options', ?SECONDARY_CONSUME_OPTIONS}
                   ]
                  ,?SECONDARY_BINDINGS(AccountId, QueueId)
                  ]).

make_ignore_key(AccountId, QueueId, CallId) ->
    {AccountId, QueueId, CallId}.

-spec queue_member(kz_term:ne_binary(), [kapps_call:call()]) -> kapps_call:call() | 'undefined'.
queue_member(CallId, Calls) ->
    case queue_member_lookup(CallId, Calls) of
        'undefined' -> 'undefined';
        {Call, _} -> Call
    end.

-spec queue_member_position(kz_term:ne_binary(), [kapps_call:call()]) -> kz_term:api_pos_integer().
queue_member_position(CallId, Calls) ->
    case queue_member_lookup(CallId, Calls) of
        'undefined' -> 'undefined';
        {_, Position} -> Position
    end.

-spec queue_member_lookup(kz_term:ne_binary(), [kapps_call:call()]) ->
          {kapps_call:call(), pos_integer()} | 'undefined'.
queue_member_lookup(CallId, Calls) ->
    queue_member_lookup(CallId, Calls, 1).

-spec queue_member_lookup(kz_term:ne_binary(), [kapps_call:call()], pos_integer()) ->
          {kapps_call:call(), pos_integer()} | 'undefined'.
queue_member_lookup(_, [], _) -> 'undefined';
queue_member_lookup(CallId, [Call|Calls], Position) ->
    case kapps_call:call_id(Call) of
        CallId -> {Call, Position};
        _ -> queue_member_lookup(CallId, Calls, Position + 1)
    end.

-spec publish_queue_member_add(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
publish_queue_member_add(AccountId, QueueId, Call) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call">>, kapps_call:to_json(Call)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_queue_member_add(Prop).

-spec publish_queue_member_remove(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_queue_member_remove(AccountId, QueueId, CallId) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call-ID">>, CallId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_queue_member_remove(Prop).

-spec publish_member_call_failure(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_member_call_failure(Q, AccountId, QueueId, CallId, Reason) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Failure-Reason">>, Reason}
           ,{<<"Queue-ID">>, QueueId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    catch kapi_acdc_queue:publish_member_call_failure(Q, Prop).

%% Really sophisticated selection algorithm
-spec pick_winner(pid(), kz_json:objects(), queue_strategy(), kz_term:api_binary()) ->
          'undefined' |
          {kz_json:objects(), kz_json:objects()}.
pick_winner(_, [], _, _) ->
    lager:debug("no agent responses are left to choose from"),
    'undefined';
pick_winner(Mgr, CRs, 'rr', AgentId) ->
    case split_agents(AgentId, CRs) of
        {[], _O} ->
            lager:debug("oops, agent ~s appears to have not responded; try again", [AgentId]),
            pick_winner(Mgr, remove_unknown_agents(Mgr, CRs), 'rr', next_winner(Mgr));
        {Winners, OtherAgents} ->
            lager:debug("found winning responders for agent: ~s", [AgentId]),
            {Winners, OtherAgents}
    end;
pick_winner(_Mgr, CRs, 'mi', _) ->
    [MostIdle | Rest] = lists:usort(fun sort_agent/2, CRs),
    AgentId = kz_json:get_value(<<"Agent-ID">>, MostIdle),
    {Same, Other} = split_agents(AgentId, Rest),

    {[MostIdle|Same], Other}.

%%------------------------------------------------------------------------------
%% @doc Return the IDs of the agents that are ready to take calls.
%% @end
%%------------------------------------------------------------------------------
-spec ready_agents(mgr_state()) -> kz_term:ne_binaries().
ready_agents(#state{strategy=Strategy
                   ,strategy_state=#strategy_state{agents=Agents}
                   }) ->
    ready_agents(Strategy, Agents).

%%------------------------------------------------------------------------------
%% @doc Return the IDs of the agents that are ready to take calls, based on the
%% queue strategy.
%% @end
%%------------------------------------------------------------------------------
-spec ready_agents(queue_strategy(), queue_strategy_state()) -> kz_term:ne_binaries().
ready_agents('rr', AgentQueue) -> queue:to_list(AgentQueue);
ready_agents('mi', AgentL) -> AgentL.

%%------------------------------------------------------------------------------
%% @doc Return the count of agents that are ready to take calls.
%% @end
%%------------------------------------------------------------------------------
-spec ready_agent_count(mgr_state()) -> non_neg_integer().
ready_agent_count(#state{strategy=Strategy
                        ,strategy_state=#strategy_state{agents=Agents}
                        }) ->
    ready_agent_count(Strategy, Agents).

%%------------------------------------------------------------------------------
%% @doc Return the count of agents that are ready to take calls, based on the
%% queue strategy.
%% @end
%%------------------------------------------------------------------------------
-spec ready_agent_count(queue_strategy(), queue_strategy_state()) -> non_neg_integer().
ready_agent_count('rr', AgentQueue) -> queue:len(AgentQueue);
ready_agent_count('mi', AgentL) -> length(AgentL).

%%------------------------------------------------------------------------------
%% @doc Return the count of agents that are eligible to be assigned to a waiting
%% call.
%% @end
%%------------------------------------------------------------------------------
-spec assignable_agent_count(mgr_state()) -> non_neg_integer().
assignable_agent_count(#state{strategy=Strategy
                             ,strategy_state=#strategy_state{agents=Agents
                                                            ,ringing_agents=RingingAgents
                                                            }
                             }) ->
    assignable_agent_count(Strategy, Agents, RingingAgents).

%%------------------------------------------------------------------------------
%% @doc Return the count of agents that are eligible to be assigned to a waiting
%% call, based on the queue strategy. Round Robin and Most Idle strategies must
%% include ringing agents in the count to ensure maximal simultaneous ringing.
%% @end
%%------------------------------------------------------------------------------
-spec assignable_agent_count(queue_strategy(), queue_strategy_state(), kz_term:ne_binaries()) ->
          non_neg_integer().
assignable_agent_count('rr', AgentQueue, RingingAgents) ->
    ready_agent_count('rr', AgentQueue) + length(RingingAgents);
assignable_agent_count('mi', AgentL, RingingAgents) ->
    ready_agent_count('mi', AgentL) + length(RingingAgents).

%%------------------------------------------------------------------------------
%% @doc Return the IDs of all agents.
%% @end
%%------------------------------------------------------------------------------
-spec agents_(mgr_state()) -> kz_term:ne_binaries().
agents_(#state{strategy_state=#strategy_state{ringing_agents=RingingAgents
                                             ,busy_agents=BusyAgents
                                             }
              }=State) ->
    ready_agents(State) ++ RingingAgents ++ BusyAgents.

%%------------------------------------------------------------------------------
%% @doc Return the count of all agents.
%% @end
%%------------------------------------------------------------------------------
-spec agent_count(mgr_state()) -> non_neg_integer().
agent_count(#state{strategy_state=#strategy_state{ringing_agents=RingingAgents
                                                 ,busy_agents=BusyAgents
                                                 }
                  }=State) ->
    ready_agent_count(State) + length(RingingAgents) + length(BusyAgents).

%%------------------------------------------------------------------------------
%% @doc Update the strategy state with the change of availability for `AgentId'.
%% @end
%%------------------------------------------------------------------------------
-spec update_strategy_with_agent(mgr_state(), kz_term:ne_binary(), agent_change()) -> strategy_state().
update_strategy_with_agent(State, AgentId, Change) ->
    update_strategy_with_agent(State, AgentId, Change, #{}).

%%------------------------------------------------------------------------------
%% @doc Update the strategy state with the change of availability for `AgentId'.
%% If diagnostics are enabled, the strategy state changes will be forwarded to
%% the diagnostics receivers.
%% @end
%%------------------------------------------------------------------------------
-spec update_strategy_with_agent(mgr_state(), kz_term:ne_binary(), agent_change(), agent_change_data()) ->
          strategy_state().
update_strategy_with_agent(#state{strategy=Strategy
                                 ,strategy_state=SS
                                 }=State, AgentId, Change, Data) ->
    SS1 = set_flag(AgentId, Change, SS),
    State1 = State#state{strategy_state=SS1},
    SS2 = do_update_strategy_with_agent(State1, AgentId, Change, Data),
    #strategy_state{agents=Agents
                   ,ringing_agents=RingingAgents
                   ,busy_agents=BusyAgents
                   } = SS2,
    maybe_send_diagnostics_for_strategy_update(Strategy, Agents, AgentId, RingingAgents, BusyAgents),
    SS2.

%%------------------------------------------------------------------------------
%% @doc Update the strategy state with the change of availability for `AgentId',
%% based on the queue strategy.
%% @end
%%------------------------------------------------------------------------------
-spec do_update_strategy_with_agent(mgr_state(), kz_term:ne_binary(), agent_change(), agent_change_data()) ->
          strategy_state().
do_update_strategy_with_agent(#state{strategy='rr'
                                    ,strategy_state=SS
                                    }, AgentId, Change, Data) ->
    update_rr_strategy_with_agent(SS, AgentId, Change, Data);
do_update_strategy_with_agent(#state{strategy='mi'
                                    ,strategy_state=SS
                                    }, AgentId, Change, _) ->
    update_mi_strategy_with_agent(SS, AgentId, Change).

%%------------------------------------------------------------------------------
%% @doc Update the Round Robin strategy state with the change of availability
%% for `AgentId'.
%% @end
%%------------------------------------------------------------------------------
-spec update_rr_strategy_with_agent(strategy_state(), kz_term:ne_binary(), agent_change(), agent_change_data()) ->
          strategy_state().
update_rr_strategy_with_agent(#strategy_state{agents=AgentQueue}=SS
                             ,AgentId, 'available', _
                             ) ->
    Msg = io_lib:format("adding agent ~s to strategy rr", [AgentId]),
    {IsReAdd, AgentQueue1} = acdc_util:queue_remove(AgentId, AgentQueue),
    Msg1 = case IsReAdd of
               'true' -> ["re-", Msg];
               'false' -> Msg
           end,
    lager:info(Msg1),
    SS#strategy_state{agents=queue:in(AgentId, AgentQueue1)};
update_rr_strategy_with_agent(#strategy_state{agents=AgentQueue}=SS, AgentId, _, _) ->
    {Removed, AgentQueue1} = acdc_util:queue_remove(AgentId, AgentQueue),
    Removed
        andalso lager:info("removing agent ~s from strategy rr", [AgentId]),
    SS#strategy_state{agents=AgentQueue1}.

%%------------------------------------------------------------------------------
%% @doc Update the Most Idle strategy state with the change of availability for
%% `AgentId'.
%% @end
%%------------------------------------------------------------------------------
-spec update_mi_strategy_with_agent(strategy_state(), kz_term:ne_binary(), agent_change()) ->
          strategy_state().
update_mi_strategy_with_agent(#strategy_state{agents=AgentL}=SS, AgentId, 'available') ->
    lists:member(AgentId, AgentL)
        orelse lager:info("adding agent ~s to strategy mi", [AgentId]),
    AgentL1 = [AgentId | lists:delete(AgentId, AgentL)],
    SS#strategy_state{agents=AgentL1};
update_mi_strategy_with_agent(#strategy_state{agents=AgentL}=SS, AgentId, _) ->
    lists:member(AgentId, AgentL)
        andalso lager:info("removing agent ~s from strategy mi", [AgentId]),
    AgentL1 = lists:delete(AgentId, AgentL),
    SS#strategy_state{agents=AgentL1}.

%%------------------------------------------------------------------------------
%% @doc Apply strategy state changes based on the agent change flag (ringing/
%% busy).
%% @end
%%------------------------------------------------------------------------------
-spec set_flag(kz_term:ne_binary(), agent_change(), strategy_state()) -> strategy_state().
set_flag(AgentId, Flag, #strategy_state{ringing_agents=RingingAgents
                                       ,busy_agents=BusyAgents
                                       }=SS) ->
    RingingAgents1 = lists:delete(AgentId, RingingAgents),
    BusyAgents1 = lists:delete(AgentId, BusyAgents),
    SS1 = SS#strategy_state{ringing_agents=RingingAgents1
                           ,busy_agents=BusyAgents1
                           },

    case Flag of
        'ringing' -> SS1#strategy_state{ringing_agents=[AgentId | RingingAgents1]};
        'busy' -> SS1#strategy_state{busy_agents=[AgentId | BusyAgents1]};
        _ -> SS1
    end.

%% If A's idle time is greater, it should come before B
-spec sort_agent(kz_json:object(), kz_json:object()) -> boolean().
sort_agent(A, B) ->
    kz_json:get_integer_value(<<"Idle-Time">>, A, 0) >
        kz_json:get_integer_value(<<"Idle-Time">>, B, 0).

%% Handle when an agent process has responded to the connect_req
%% but then the agent logs out of their phone (removing the agent
%% from the list in the queue manager).
%% Otherwise CRs will never be empty
-spec remove_unknown_agents(pid(), kz_json:objects()) -> kz_json:objects().
remove_unknown_agents(Mgr, CRs) ->
    case gen_listener:call(Mgr, 'current_agents') of
        [] -> [];
        Agents ->
            [CR || CR <- CRs,
                   lists:member(kz_json:get_value(<<"Agent-ID">>, CR), Agents)
            ]
    end.

-spec split_agents(kz_term:ne_binary(), kz_json:objects()) ->
          {kz_json:objects(), kz_json:objects()}.
split_agents(AgentId, Rest) ->
    lists:partition(fun(R) ->
                            AgentId =:= kz_json:get_value(<<"Agent-ID">>, R)
                    end, Rest).

-spec get_strategy(kz_term:api_binary()) -> queue_strategy().
get_strategy(<<"round_robin">>) -> 'rr';
get_strategy(<<"most_idle">>) -> 'mi';
get_strategy(_) -> 'rr'.

-spec create_strategy_state(queue_strategy()) -> strategy_state().
create_strategy_state(Strategy) ->
    Agents = create_ss_agents(Strategy),
    #strategy_state{agents=Agents}.

-spec create_ss_agents(queue_strategy()) -> queue_strategy_state().
create_ss_agents('rr') -> queue:new();
create_ss_agents('mi') -> [].

maybe_start_queue_workers(QueueSup, Count) ->
    WSup = acdc_queue_sup:workers_sup(QueueSup),
    case acdc_queue_workers_sup:worker_count(WSup) of
        N when N >= Count -> 'ok';
        N when N < Count -> gen_listener:cast(self(), {'start_worker', Count - N})
    end.

-spec update_properties(kz_json:object(), mgr_state()) -> mgr_state().
update_properties(QueueJObj, State) ->
    State#state{enter_when_empty=kz_json:is_true(<<"enter_when_empty">>, QueueJObj, 'true')
               ,moh=kz_json:get_ne_value(<<"moh">>, QueueJObj)
               ,announcements_config=announcements_config(QueueJObj)
               }.

-spec announcements_config(kz_json:object()) -> kz_term:proplist().
announcements_config(Config) ->
    kz_json:recursive_to_proplist(
      kz_json:get_json_value(<<"announcements">>, Config, kz_json:new())).

-spec cancel_position_announcements(kapps_call:call() | 'false', map()) ->
          map().
cancel_position_announcements('false', Pids) -> Pids;
cancel_position_announcements(Call, Pids) ->
    CallId = kapps_call:call_id(Call),
    case catch maps:get(CallId, Pids) of
        {'badkey', _} ->
            lager:debug("did not have the announcements for call ~s", [CallId]),
            Pids;
        Pid ->
            lager:debug("cancelling announcements for ~s", [CallId]),
            Pids1 = maps:remove(CallId, Pids),
            _ = acdc_announcements_sup:stop_announcements(Pid),

            %% Attempt to skip remaining announcement media, but don't flush hangups
            NoopId = kz_datamgr:get_uuid(),
            Command = [{<<"Application-Name">>, <<"noop">>}
                      ,{<<"Msg-ID">>, NoopId}
                      ,{<<"Insert-At">>, <<"now">>}
                      ,{<<"Filter-Applications">>, [<<"play">>, <<"say">>, <<"play">>]}
                      ],
            kapps_call_command:send_command(Command, Call),
            Pids1
    end.

-spec remove_queue_member(kz_term:api_binary(), mgr_state()) -> mgr_state().
remove_queue_member(CallId, #state{current_member_calls=CurrentCalls
                                  ,announcements_pids=AnnouncementsPids
                                  }=State) ->
    lager:debug("removing call id ~s", [CallId]),

    AnnouncementsPids1 = cancel_position_announcements(lists:keyfind(CallId, 2, CurrentCalls), AnnouncementsPids),

    State#state{current_member_calls=lists:keydelete(CallId, 2, CurrentCalls)
               ,announcements_pids=AnnouncementsPids1
               }.

%%------------------------------------------------------------------------------
%% @doc If there is at least one diagnostics receiver attached to the queue
%% manager, send the diagnostics message returned by `GetDiagnosticsMessage' to
%% all the diagnostics receivers. `GetDiagnosticsMessage' is a factory function
%% so that the diagnostics message generation is a noop when diagnostics are
%% disabled for the queue.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_diagnostics(fun(() -> iolist())) -> 'ok'.
maybe_send_diagnostics(GetDiagnosticsMessage) ->
    case get(?KEY_DIAGNOSTICS_PIDS) of
        'undefined' -> 'ok';
        [] -> 'ok';
        Pids ->
            Message = GetDiagnosticsMessage(),
            acdc_queue_manager_diag_sup:send_diagnostics(Pids, Message)
    end.

%%------------------------------------------------------------------------------
%% @doc Prepare a diagnostics payload for an update of the strategy state due to
%% an agent state change (e.g. ringing/busy). The diagnostics will only be
%% prepared if there is at least one diagnostics receiver attached to the queue
%% manager.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_diagnostics_for_strategy_update(queue_strategy()
                                                ,queue_strategy_state()
                                                ,kz_term:ne_binary()
                                                ,kz_term:ne_binaries()
                                                ,kz_term:ne_binaries()
                                                ) -> 'ok'.
maybe_send_diagnostics_for_strategy_update('rr', AgentQueue, AgentId, RingingAgents, BusyAgents) ->
    Message = "agent ~s updated in SS~n~n"
        ++ "ringing agents: ~p~n~n"
        ++ "busy agents: ~p~n~n"
        ++ "agent queue: ~p",
    ?DIAG(Message
         ,[AgentId
          ,RingingAgents
          ,BusyAgents
          ,queue:to_list(AgentQueue)
          ]);
maybe_send_diagnostics_for_strategy_update('mi', AgentL, AgentId, RingingAgents, BusyAgents) ->
    Message = "agent ~s updated in SS~n~n"
        ++ "ringing agents: ~p~n~n"
        ++ "busy agents: ~p~n~n"
        ++ "agent list: ~p",
    ?DIAG(Message
         ,[AgentId, RingingAgents, BusyAgents, AgentL]
         ).
