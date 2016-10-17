%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%% Manages queue processes:
%%%   starting when a queue is created
%%%   stopping when a queue is deleted
%%%   collecting stats from queues
%%%   and more!!!
%%% @end
%%% @contributors
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%   Daniel Finke
%%%-------------------------------------------------------------------
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
        ,are_agents_available/1
        ,handle_config_change/2
        ,should_ignore_member_call/3, should_ignore_member_call/4
        ,config/1
        ,status/1
        ,current_agents/1
        ,refresh/2
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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(pid(), kz_json:object()) -> startlink_ret().
start_link(Super, QueueJObj) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Super, QueueJObj]
                           ).

-spec start_link(pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(Super, AccountId, QueueId) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Super, AccountId, QueueId]
                           ).

handle_member_call(JObj, Props) ->
    'true' = kapi_acdc_queue:member_call_v(JObj),
    _ = kz_util:put_callid(JObj),

    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),

    case are_agents_available(props:get_value('server', Props)
                             ,props:get_value('enter_when_empty', Props)
                             )
    of
        'false' ->
            lager:info("no agents are available to take the call, cancel queueing"),
            gen_listener:cast(props:get_value('server', Props)
                             ,{'reject_member_call', Call, JObj}
                             );
        'true' ->
            start_queue_call(JObj, Props, Call)
    end.

-spec are_agents_available(server_ref()) -> boolean().
are_agents_available(Srv) ->
    are_agents_available(Srv, gen_listener:call(Srv, 'enter_when_empty')).

are_agents_available(Srv, EnterWhenEmpty) ->
    agents_available(Srv) > 0
        orelse EnterWhenEmpty.

start_queue_call(JObj, Props, Call) ->
    _ = kapps_call:put_callid(Call),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),

    lager:info("member call for queue ~s recv", [QueueId]),
    lager:debug("answering call"),
    kapps_call_command:answer_now(Call),

    case kz_media_util:media_path(props:get_value('moh', Props), Call) of
        'undefined' ->
            lager:debug("using default moh"),
            kapps_call_command:hold(Call);
        MOH ->
            lager:debug("using MOH ~s (~p)", [MOH, Props]),
            kapps_call_command:hold(MOH, Call)
    end,

    _ = kapps_call_command:set('undefined'
                              ,kz_json:from_list([{<<"Eavesdrop-Group-ID">>, QueueId}
                                                 ,{<<"Queue-ID">>, QueueId}
                                                 ])
                              ,Call
                              ),

    %% Add member to queue for tracking position
    gen_listener:cast(props:get_value('server', Props), {'add_queue_member', JObj}).

-spec handle_member_call_success(kz_json:object(), kz_proplist()) -> 'ok'.
handle_member_call_success(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_remove', kz_json:get_value(<<"Call-ID">>, JObj)}).

handle_member_call_cancel(JObj, Props) ->
    kz_util:put_callid(JObj),
    lager:debug("cancel call ~p", [JObj]),
    'true' = kapi_acdc_queue:member_call_cancel_v(JObj),
    K = make_ignore_key(kz_json:get_value(<<"Account-ID">>, JObj)
                       ,kz_json:get_value(<<"Queue-ID">>, JObj)
                       ,kz_json:get_value(<<"Call-ID">>, JObj)
                       ),
    gen_listener:cast(props:get_value('server', Props), {'member_call_cancel', K, JObj}).

handle_agent_change(JObj, Prop) ->
    'true' = kapi_acdc_queue:agent_change_v(JObj),
    Server = props:get_value('server', Prop),
    case kz_json:get_value(<<"Change">>, JObj) of
        <<"available">> ->
            gen_listener:cast(Server, {'agent_available', JObj});
        <<"ringing">> ->
            gen_listener:cast(Server, {'agent_ringing', JObj});
        <<"unavailable">> ->
            gen_listener:cast(Server, {'agent_unavailable', JObj})
    end.

-spec handle_queue_member_add(kz_json:object(), kz_proplist()) -> 'ok'.
handle_queue_member_add(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_add', JObj}).

-spec handle_queue_member_remove(kz_json:object(), kz_proplist()) -> 'ok'.
handle_queue_member_remove(JObj, Prop) ->
    gen_listener:cast(props:get_value('server', Prop), {'handle_queue_member_remove', kz_json:get_value(<<"Call-ID">>, JObj)}).

-spec handle_config_change(server_ref(), kz_json:object()) -> 'ok'.
handle_config_change(Srv, JObj) ->
    gen_listener:cast(Srv, {'update_queue_config', JObj}).

-spec should_ignore_member_call(server_ref(), kapps_call:call(), kz_json:object()) -> boolean().
-spec should_ignore_member_call(server_ref(), kapps_call:call(), ne_binary(), ne_binary()) -> boolean().
should_ignore_member_call(Srv, Call, CallJObj) ->
    should_ignore_member_call(Srv
                             ,Call
                             ,kz_json:get_value(<<"Account-ID">>, CallJObj)
                             ,kz_json:get_value(<<"Queue-ID">>, CallJObj)
                             ).
should_ignore_member_call(Srv, Call, AccountId, QueueId) ->
    K = make_ignore_key(AccountId, QueueId, kapps_call:call_id(Call)),
    gen_listener:call(Srv, {'should_ignore_member_call', K}).

-spec config(pid()) -> {ne_binary(), ne_binary()}.
config(Srv) -> gen_listener:call(Srv, 'config').

-spec current_agents(server_ref()) -> ne_binaries().
current_agents(Srv) -> gen_listener:call(Srv, 'current_agents').

status(Srv) -> gen_listener:call(Srv, 'status').

refresh(Mgr, QueueJObj) -> gen_listener:cast(Mgr, {'refresh', QueueJObj}).

strategy(Srv) -> gen_listener:call(Srv, 'strategy').
next_winner(Srv) -> gen_listener:call(Srv, 'next_winner').

agents_available(Srv) -> gen_listener:call(Srv, 'agents_available').

pick_winner(Srv, Resps) -> pick_winner(Srv, Resps, strategy(Srv), next_winner(Srv)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
-spec init([pid() | kz_json:object() | ne_binary()]) -> {'ok', mgr_state()}.
init([Super, QueueJObj]) ->
    AccountId = kz_doc:account_id(QueueJObj),
    QueueId = kz_doc:id(QueueJObj),

    kz_util:put_callid(<<"mgr_", QueueId/binary>>),

    init(Super, AccountId, QueueId, QueueJObj);

init([Super, AccountId, QueueId]) ->
    kz_util:put_callid(<<"mgr_", QueueId/binary>>),

    AcctDb = kz_util:format_account_id(AccountId, 'encoded'),
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(AcctDb, QueueId),

    init(Super, AccountId, QueueId, QueueJObj).

init(Super, AccountId, QueueId, QueueJObj) ->
    process_flag('trap_exit', 'false'),

    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    kz_datamgr:add_to_doc_cache(AccountDb, QueueId, QueueJObj),

    _ = start_secondary_queue(AccountId, QueueId),

    gen_listener:cast(self(), {'start_workers'}),
    Strategy = get_strategy(kz_json:get_value(<<"strategy">>, QueueJObj)),
    StrategyState = create_strategy_state(Strategy, AccountDb, QueueId),

    _ = update_strategy_state(self(), Strategy, StrategyState),

    lager:debug("queue mgr started for ~s", [QueueId]),
    {'ok', update_properties(QueueJObj, #state{account_id=AccountId
                                              ,queue_id=QueueId
                                              ,supervisor=Super
                                              ,strategy=Strategy
                                              ,strategy_state=StrategyState
                                              })}.

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
-spec handle_call(any(), pid_ref(), mgr_state()) -> handle_call_ret_state(mgr_state()).
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

handle_call('config', _, #state{account_id=AccountId
                               ,queue_id=QueueId
                               }=State) ->
    {'reply', {AccountId, QueueId}, State};

handle_call('status', _, #state{known_agents=As}=State) ->
    Known = [A || {A, N} <- dict:to_list(As), N > 0],
    {'reply', Known, State};

handle_call('strategy', _, #state{strategy=Strategy}=State) ->
    {'reply', Strategy, State, 'hibernate'};

handle_call('agents_available', _, #state{strategy_state='undefined'}=State) ->
    {'reply', 0, State};
handle_call('agents_available', _, #state{strategy_state=[]}=State) ->
    {'reply', 0, State};
handle_call('agents_available', _, #state{strategy_state=[_|_]}=State) ->
    {'reply', 1, State};
handle_call('agents_available', _, #state{strategy_state=SS}=State) ->
    {'reply', queue:len(SS), State};

handle_call('enter_when_empty', _, #state{enter_when_empty=EnterWhenEmpty}=State) ->
    {'reply', EnterWhenEmpty, State};

handle_call('next_winner', _, #state{strategy='mi'}=State) ->
    {'reply', 'undefined', State};
handle_call('next_winner', _, #state{strategy='rr'
                                    ,strategy_state=SS
                                    }=State) ->
    case queue:out(SS) of
        {{'value', Winner}, SS1} ->
            {'reply', Winner, State#state{strategy_state=queue:in(Winner, SS1)}, 'hibernate'};
        {'empty', _} ->
            {'reply', 'undefined', State}
    end;

handle_call('current_agents', _, #state{strategy='rr'
                                       ,strategy_state=Q
                                       }=State) ->
    {'reply', queue:to_list(Q), State};
handle_call('current_agents', _, #state{strategy='mi'
                                       ,strategy_state=L
                                       }=State) ->
    {'reply', L, State};

handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

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
-spec handle_cast(any(), mgr_state()) -> handle_cast_ret_state(mgr_state()).
handle_cast({'update_strategy', StrategyState}, State) ->
    {'noreply', State#state{strategy_state=StrategyState}, 'hibernate'};

handle_cast({'update_queue_config', JObj}, #state{enter_when_empty=_EnterWhenEmpty}=State) ->
    EWE = kz_json:is_true([<<"Doc">>, <<"enter_when_empty">>], JObj, 'true'),
    lager:debug("maybe changing ewe from ~s to ~s", [_EnterWhenEmpty, EWE]),
    {'noreply', State#state{enter_when_empty=EWE}, 'hibernate'};

handle_cast({'member_call_cancel', K, JObj}, #state{ignored_member_calls=Dict}=State) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    Reason = kz_json:get_value(<<"Reason">>, JObj),

    acdc_stats:call_abandoned(AccountId, QueueId, CallId, Reason),
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
                               ,[{'key', QueueId}
                                ,'include_docs'
                                ])
    of
        {'ok', Agents} ->
            _ = [start_agent_and_worker(WorkersSup, AccountId, QueueId
                                       ,kz_json:get_value(<<"doc">>, A)
                                       )
                 || A <- Agents
                ],
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

handle_cast({'agent_available', AgentId}, #state{strategy=Strategy
                                                ,strategy_state=StrategyState
                                                ,known_agents=As
                                                }=State) when is_binary(AgentId) ->
    lager:info("adding agent ~s to strategy ~s", [AgentId, Strategy]),
    {StrategyState1, As1} = update_strategy_with_agent(Strategy, StrategyState, As, AgentId, 'add'),
    {'noreply', State#state{strategy_state=StrategyState1
                           ,known_agents=As1
                           }
    ,'hibernate'};
handle_cast({'agent_available', JObj}, State) ->
    handle_cast({'agent_available', kz_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_ringing', AgentId}, #state{strategy=Strategy
                                              ,strategy_state=StrategyState
                                              }=State) when is_binary(AgentId) ->
    lager:info("agent ~s ringing, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = maybe_update_strategy(Strategy, StrategyState, AgentId),
    {'noreply', State#state{strategy_state=StrategyState1}, 'hibernate'};
handle_cast({'agent_ringing', JObj}, State) ->
    handle_cast({'agent_ringing', kz_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_unavailable', AgentId}, #state{strategy=Strategy
                                                  ,strategy_state=StrategyState
                                                  ,known_agents=As
                                                  ,supervisor=QueueSup
                                                  }=State) when is_binary(AgentId) ->
    lager:info("agent ~s unavailable, maybe updating strategy ~s", [AgentId, Strategy]),

    {StrategyState1, As1} = update_strategy_with_agent(Strategy, StrategyState, As, AgentId, 'remove'),

    maybe_start_queue_workers(QueueSup, dict:size(As1)),

    {'noreply', State#state{strategy_state=StrategyState1
                           ,known_agents=As1
                           }
    ,'hibernate'};
handle_cast({'agent_unavailable', JObj}, State) ->
    handle_cast({'agent_unavailable', kz_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'reject_member_call', Call, JObj}, #state{account_id=AccountId
                                                      ,queue_id=QueueId
                                                      }=State) ->
    Prop = [{<<"Call-ID">>, kapps_call:call_id(Call)}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Failure-Reason">>, <<"no agents">>}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = kz_json:get_value(<<"Server-ID">>, JObj),
    catch kapi_acdc_queue:publish_member_call_failure(Q, Prop),
    {'noreply', State};

handle_cast({'sync_with_agent', A}, #state{account_id=AccountId}=State) ->
    case acdc_agent_util:most_recent_status(AccountId, A) of
        {'ok', <<"logout">>} -> gen_listener:cast(self(), {'agent_unavailable', A});
        _ -> gen_listener:cast(self(), {'agent_available', A})
    end,
    {'noreply', State};

handle_cast({'gen_listener', {'created_queue', _}}, State) ->
    {'noreply', State};

handle_cast({'refresh', QueueJObj}, State) ->
    lager:debug("refreshing queue configs"),
    {'noreply', update_properties(QueueJObj, State), 'hibernate'};

handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};

handle_cast({'add_queue_member', JObj}, #state{account_id=AccountId
                                              ,queue_id=QueueId
                                              ,current_member_calls=CurrentCalls
                                              }=State) ->
    Position = length(CurrentCalls)+1,
    Call = kapps_call:set_custom_channel_var(<<"Queue-Position">>
                                            ,Position
                                            ,kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj))),

    acdc_stats:call_waiting(AccountId, QueueId
                           ,kapps_call:call_id(Call)
                           ,kapps_call:caller_id_name(Call)
                           ,kapps_call:caller_id_number(Call)
                           ,kz_json:get_integer_value(<<"Member-Priority">>, JObj)
                           ),

    publish_queue_member_add(AccountId, QueueId, Call),

    %% Add call to shared queue
    kapi_acdc_queue:publish_shared_member_call(AccountId, QueueId, JObj),
    lager:debug("put call into shared messaging queue"),

    gen_listener:cast(self(), {'monitor_call', Call}),

    acdc_util:presence_update(AccountId, QueueId, ?PRESENCE_RED_FLASH),

    {'noreply', State#state{current_member_calls=[Call | CurrentCalls]
                           }};

handle_cast({'handle_queue_member_add', JObj}, #state{current_member_calls=CurrentCalls}=State) ->
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    CallId = kapps_call:call_id(Call),
    lager:debug("received notification of new queue member ~s", [CallId]),

    {'noreply', State#state{current_member_calls = [Call | lists:keydelete(CallId, 2, CurrentCalls)]}};

handle_cast({'handle_queue_member_remove', CallId}, #state{current_member_calls=CurrentCalls}=State) ->
    lager:debug("removing call id ~s", [CallId]),

    {'noreply', State#state{current_member_calls=lists:keydelete(CallId, 2, CurrentCalls)}};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

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
-spec handle_info(any(), mgr_state()) -> handle_info_ret_state(mgr_state()).
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
-spec terminate(any(), mgr_state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("queue manager terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), mgr_state(), any()) -> {'ok', mgr_state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_secondary_queue(AccountId, QueueId) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Priority = lookup_priority_levels(AccountDb, QueueId),
    kz_util:spawn(fun gen_listener:add_queue/4
                 ,[self()
                  ,?SECONDARY_QUEUE_NAME(QueueId)
                  ,[{'queue_options', ?SECONDARY_QUEUE_OPTIONS(Priority)}
                   ,{'consume_options', ?SECONDARY_CONSUME_OPTIONS}
                   ]
                  ,?SECONDARY_BINDINGS(AccountId, QueueId)
                  ]).

-spec lookup_priority_levels(ne_binary(), ne_binary()) -> api_integer().
lookup_priority_levels(AccountDB, QueueId) ->
    case kz_datamgr:open_cache_doc(AccountDB, QueueId) of
        {'ok', JObj} -> kz_json:get_value(<<"max_priority">>, JObj);
        _ -> 'undefined'
    end.

make_ignore_key(AccountId, QueueId, CallId) ->
    {AccountId, QueueId, CallId}.

-spec publish_queue_member_add(ne_binary(), ne_binary(), kapps_call:call()) -> 'ok'.
publish_queue_member_add(AccountId, QueueId, Call) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call">>, kapps_call:to_json(Call)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_queue_member_add(Prop).

-spec publish_queue_member_remove(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
publish_queue_member_remove(AccountId, QueueId, CallId) ->
    Prop = [{<<"Account-ID">>, AccountId}
           ,{<<"Queue-ID">>, QueueId}
           ,{<<"Call-ID">>, CallId}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_acdc_queue:publish_queue_member_remove(Prop).

-spec start_agent_and_worker(pid(), ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
start_agent_and_worker(WorkersSup, AccountId, QueueId, AgentJObj) ->
    acdc_queue_workers_sup:new_worker(WorkersSup, AccountId, QueueId),
    AgentId = kz_doc:id(AgentJObj),
    case acdc_agent_util:most_recent_status(AccountId, AgentId) of
        {'ok', <<"logout">>} -> 'ok';
        {'ok', <<"logged_out">>} -> 'ok';
        {'ok', _Status} ->
            lager:debug("maybe starting agent ~s(~s) for queue ~s", [AgentId, _Status, QueueId]),

            case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
                'undefined' -> acdc_agents_sup:new(AgentJObj);
                P when is_pid(P) -> 'ok'
            end
    end.

%% Really sophisticated selection algorithm
-spec pick_winner(pid(), kz_json:objects(), queue_strategy(), api_binary()) ->
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

-spec update_strategy_with_agent(queue_strategy(), queue_strategy_state(), dict:dict(), ne_binary(), 'add' | 'remove') ->
                                        {queue_strategy_state(), dict:dict()}.
update_strategy_with_agent('rr', 'undefined', As, AgentId, 'add') ->
    {queue:in(AgentId, queue:new()), dict:update_counter(AgentId, 1, As)};
update_strategy_with_agent('rr', AgentQueue, As, AgentId, 'add') ->
    case queue:member(AgentId, AgentQueue) of
        'true' -> {AgentQueue, dict:update_counter(AgentId, 1, As)};
        'false' -> {queue:in(AgentId, AgentQueue), dict:update_counter(AgentId, 1, As)}
    end;
update_strategy_with_agent('rr', AgentQueue, As, AgentId, 'remove') ->
    case queue:member(AgentId, AgentQueue) of
        'false' -> {AgentQueue, As};
        'true' ->
            case catch dict:fetch(AgentId, As) of
                N when is_integer(N), N > 1 -> {AgentQueue, dict:update_counter(AgentId, -1, As)};
                _ -> {queue:from_list(
                        [A || A <- queue:to_list(AgentQueue),
                              A =/= AgentId
                        ])
                     ,dict:erase(AgentId, As)
                     }
            end
    end;
update_strategy_with_agent('mi', AgentL, As, AgentId, 'add') ->
    case lists:member(AgentId, AgentL) of
        'true' -> {AgentL, As};
        'false' -> {[AgentId | AgentL], dict:update_counter(AgentId, 1, As)}
    end;
update_strategy_with_agent('mi', AgentL, As, AgentId, 'remove') ->
    case lists:member(AgentId, AgentL) of
        'false' -> {AgentL, As};
        'true' ->
            case catch dict:fetch(AgentId, As) of
                N when is_integer(N), N > 1 ->
                    {AgentL, dict:update_counter(AgentId, -1, As)};
                _ ->
                    {[A || A <- AgentL, A =/= AgentId], dict:erase(AgentId, As)}
            end
    end;
update_strategy_with_agent('mi', _, As, _, _) ->
    {'undefined', As}.

maybe_update_strategy('mi', StrategyState, _AgentId) -> StrategyState;
maybe_update_strategy('rr', StrategyState, AgentId) ->
    case queue:out(StrategyState) of
        {{'value', AgentId}, StrategyState1} ->
            lager:debug("agent ~s was front of queue, moving", [AgentId]),
            queue:in(AgentId, StrategyState1);
        _ -> StrategyState
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

-spec split_agents(ne_binary(), kz_json:objects()) ->
                          {kz_json:objects(), kz_json:objects()}.
split_agents(AgentId, Rest) ->
    lists:partition(fun(R) ->
                            AgentId =:= kz_json:get_value(<<"Agent-ID">>, R)
                    end, Rest).

-spec get_strategy(api_binary()) -> queue_strategy().
get_strategy(<<"round_robin">>) -> 'rr';
get_strategy(<<"most_idle">>) -> 'mi';
get_strategy(_) -> 'rr'.

-spec create_strategy_state(queue_strategy()
                           ,queue_strategy_state() | 'undefined'
                           ,ne_binary(), ne_binary()
                           ) -> queue_strategy_state().
create_strategy_state(Strategy, AcctDb, QueueId) ->
    create_strategy_state(Strategy, 'undefined', AcctDb, QueueId).

create_strategy_state('rr', 'undefined', AcctDb, QueueId) ->
    create_strategy_state('rr', queue:new(), AcctDb, QueueId);
create_strategy_state('rr', AgentQ, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>, [{'key', QueueId}]) of
        {'ok', []} -> lager:debug("no agents around"), AgentQ;
        {'ok', JObjs} ->
            Q = queue:from_list([Id || JObj <- JObjs,
                                       not queue:member((Id = kz_doc:id(JObj)), AgentQ)
                                ]),
            queue:join(AgentQ, Q);
        {'error', _E} -> lager:debug("error creating strategy rr: ~p", [_E]), AgentQ
    end;
create_strategy_state('mi', 'undefined', AcctDb, QueueId) ->
    create_strategy_state('mi', [], AcctDb, QueueId);
create_strategy_state('mi', AgentL, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {'ok', []} -> lager:debug("no agents around"), AgentL;
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, Acc) ->
                                Id = kz_doc:id(JObj),
                                case lists:member(Id, Acc) of
                                    'true' -> Acc;
                                    'false' -> [Id | Acc]
                                end
                        end, AgentL, JObjs);
        {'error', _E} -> lager:debug("error creating strategy mi: ~p", [_E]), AgentL
    end.

update_strategy_state(Srv, 'rr', StrategyState) ->
    L = queue:to_list(StrategyState),
    update_strategy_state(Srv, L);
update_strategy_state(Srv, 'mi', StrategyState) ->
    update_strategy_state(Srv, StrategyState).
update_strategy_state(Srv, L) ->
    [gen_listener:cast(Srv, {'sync_with_agent', A}) || A <- L].

maybe_start_queue_workers(QueueSup, AgentCount) ->
    WSup = acdc_queue_sup:workers_sup(QueueSup),
    case acdc_queue_workers_sup:worker_count(WSup) of
        N when N >= AgentCount -> 'ok';
        N when N < AgentCount -> gen_listener:cast(self(), {'start_worker', AgentCount-N})
    end.

-spec update_properties(kz_json:object(), mgr_state()) -> mgr_state().
update_properties(QueueJObj, State) ->
    State#state{
      enter_when_empty=kz_json:is_true(<<"enter_when_empty">>, QueueJObj, 'true')
               ,moh=kz_json:get_ne_value(<<"moh">>, QueueJObj)
     }.
