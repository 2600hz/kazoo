%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Manages queue processes:
%%%   starting when a queue is created
%%%   stopping when a queue is deleted
%%%   collecting stats from queues
%%%   and more!!!
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(acdc_queue_manager).

-behaviour(gen_listener).

%% API
-export([start_link/2, start_link/3
         ,handle_member_call/2
         ,handle_member_call_cancel/2
         ,handle_agent_change/2
         ,handle_config_change/2
         ,should_ignore_member_call/3
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

-define(SERVER, ?MODULE).

%% rr :: Round Robin
%% mi :: Most Idle
-type queue_strategy() :: 'rr' | 'mi'.
-type queue_strategy_state() :: queue() | ne_binaries().

-record(state, {
          ignored_member_calls = dict:new() :: dict()
          ,acct_id :: api_binary()
          ,queue_id :: api_binary()
          ,supervisor :: pid()
          ,strategy = 'rr' :: queue_strategy() % round-robin | most-idle
          ,strategy_state :: queue_strategy_state() % based on the strategy
          ,known_agents = dict:new() :: dict() % how many agent processes are available {AgentId, Count}
          ,enter_when_empty = 'true' :: boolean() % allow caller into queue if no agents are logged in
          ,moh :: api_binary()
         }).
-type mgr_state() :: #state{}.

-define(BINDINGS(A, Q), [{'conf', [{'type', <<"queue">>}
                                   ,{'db', wh_util:format_account_id(A, 'encoded')}
                                   ,{'id', Q}
                                  ]}
                         ,{'acdc_queue', [{'restrict_to', ['stats_req', 'agent_change']}
                                        ,{'account_id', A}
                                        ,{'queue_id', Q}
                                       ]}
                         ,{'notifications', [{'restrict_to', ['presence_probe']}]}
                        ]).

-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_config_change'}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                     ,{{'acdc_queue_handler', 'handle_stats_req'}
                       ,[{<<"queue">>, <<"stats_req">>}]
                      }
                     ,{{'acdc_queue_handler', 'handle_presence_probe'}
                       ,[{<<"notification">>, <<"presence_probe">>}]
                      }
                     ,{{'acdc_queue_manager', 'handle_member_call'}
                       ,[{<<"member">>, <<"call">>}]
                      }
                     ,{{'acdc_queue_manager', 'handle_member_call_cancel'}
                       ,[{<<"member">>, <<"call_cancel">>}]
                      }
                     ,{{'acdc_queue_manager', 'handle_agent_change'}
                       ,[{<<"queue">>, <<"agent_change">>}]
                      }
                    ]).

-define(SECONDARY_BINDINGS(AcctId, QueueId)
        ,[{'acdc_queue', [{'restrict_to', ['member_call']}
                          ,{'account_id', AcctId}
                          ,{'queue_id', QueueId}
                         ]}
         ]).
-define(SECONDARY_QUEUE_NAME(QueueId), <<"acdc.queue.manager.", QueueId/binary>>).
-define(SECONDARY_QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(SECONDARY_CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
-spec start_link(pid(), wh_json:object()) -> startlink_ret().
start_link(Super, QueueJObj) ->
    AcctId = wh_json:get_value(<<"pvt_account_id">>, QueueJObj),
    QueueId = wh_json:get_value(<<"_id">>, QueueJObj),

    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS(AcctId, QueueId)}
                              ,{'responders', ?RESPONDERS}
                             ]
                            ,[Super, QueueJObj]
                           ).

-spec start_link(pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(Super, AcctId, QueueId) ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS(AcctId, QueueId)}
                              ,{'responders', ?RESPONDERS}
                             ]
                            ,[Super, AcctId, QueueId]
                           ).

handle_member_call(JObj, Props) ->
    'true' = wapi_acdc_queue:member_call_v(JObj),
    _ = wh_util:put_callid(JObj),

    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),

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

are_agents_available(Srv, EnterWhenEmpty) ->
    agents_available(Srv) > 0 orelse EnterWhenEmpty.

start_queue_call(JObj, Props, Call) ->
    _ = whapps_call:put_callid(Call),
    AcctId = whapps_call:account_id(Call),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    lager:info("member call for queue ~s recv", [QueueId]),

    acdc_stats:call_waiting(AcctId, QueueId
                            ,whapps_call:call_id(Call)
                            ,whapps_call:caller_id_name(Call)
                            ,whapps_call:caller_id_number(Call)
                           ),

    lager:debug("answering call"),
    whapps_call_command:answer_now(Call),

    case cf_util:correct_media_path(props:get_value('moh', Props), Call) of
        'undefined' ->
            lager:debug("using default moh"),
            whapps_call_command:hold(Call);
        MOH ->
            lager:debug("using MOH ~s", [MOH]),
            whapps_call_command:hold(MOH, Call)
    end,

    wapi_acdc_queue:publish_shared_member_call(AcctId, QueueId, JObj),
    lager:debug("put call into shared messaging queue"),

    _ = whapps_call_command:set('undefined'
                                ,wh_json:from_list([{<<"Eavesdrop-Group-ID">>, QueueId}
                                                    ,{<<"Queue-ID">>, QueueId}
                                                   ])
                                ,Call
                               ),

    gen_listener:cast(props:get_value('server', Props), {'monitor_call', Call}),

    acdc_util:presence_update(AcctId, QueueId, ?PRESENCE_RED_FLASH).

handle_member_call_cancel(JObj, Props) ->
    'true' = wapi_acdc_queue:member_call_cancel_v(JObj),
    K = make_ignore_key(wh_json:get_value(<<"Account-ID">>, JObj)
                        ,wh_json:get_value(<<"Queue-ID">>, JObj)
                        ,wh_json:get_value(<<"Call-ID">>, JObj)
                       ),
    gen_listener:cast(props:get_value('server', Props), {'member_call_cancel', K, JObj}).

handle_agent_change(JObj, Prop) ->
    'true' = wapi_acdc_queue:agent_change_v(JObj),
    case wh_json:get_value(<<"Change">>, JObj) of
        <<"available">> ->
            gen_listener:cast(props:get_value('server', Prop), {'agent_available', JObj});
        <<"ringing">> ->
            gen_listener:cast(props:get_value('server', Prop), {'agent_ringing', JObj});
        <<"unavailable">> ->
            gen_listener:cast(props:get_value('server', Prop), {'agent_unavailable', JObj})
    end.

handle_config_change(Srv, JObj) ->
    gen_listener:cast(Srv, {'update_queue_config', JObj}).

should_ignore_member_call(Srv, Call, CallJObj) ->
    K = make_ignore_key(wh_json:get_value(<<"Account-ID">>, CallJObj)
                        ,wh_json:get_value(<<"Queue-ID">>, CallJObj)
                        ,whapps_call:call_id(Call)
                       ),
    gen_listener:call(Srv, {'should_ignore_member_call', K}).

config(Srv) -> gen_listener:call(Srv, 'config').
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
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Super, QueueJObj]) ->
    AcctId = wh_json:get_value(<<"pvt_account_id">>, QueueJObj),
    QueueId = wh_json:get_value(<<"_id">>, QueueJObj),

    put('callid', <<"mgr_", QueueId/binary>>),

    init(Super, AcctId, QueueId, QueueJObj);
    
init([Super, AcctId, QueueId]) ->
    put('callid', <<"mgr_", QueueId/binary>>),

    AcctDb = wh_util:format_account_id(AcctId, 'encoded'),
    {'ok', QueueJObj} = couch_mgr:open_cache_doc(AcctDb, QueueId),

    init(Super, AcctId, QueueId, QueueJObj).

init(Super, AcctId, QueueId, QueueJObj) ->
    process_flag('trap_exit', 'false'),

    AcctDb = wh_util:format_account_id(AcctId, 'encoded'),
    couch_mgr:cache_db_doc(AcctDb, QueueId, QueueJObj),

    _ = start_secondary_queue(AcctId, QueueId),

    gen_listener:cast(self(), {'start_workers'}),
    Strategy = get_strategy(wh_json:get_value(<<"strategy">>, QueueJObj)),
    StrategyState = create_strategy_state(Strategy, AcctDb, QueueId),

    _ = update_strategy_state(self(), Strategy, StrategyState),

    lager:debug("queue mgr started for ~s", [QueueId]),
    {'ok', update_properties(QueueJObj, #state{acct_id=AcctId
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
handle_call({'should_ignore_member_call', K}, _, #state{ignored_member_calls=Dict}=State) ->
    case catch dict:fetch(K, Dict) of
        {'EXIT', _} -> {'reply', 'false', State};
        _Res -> {'reply', 'true', State#state{ignored_member_calls=dict:erase(K, Dict)}}
    end;

handle_call('config', _, #state{acct_id=AcctId
                                ,queue_id=QueueId
                               }=State) ->
    {'reply', {AcctId, QueueId}, State};

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
handle_cast({'update_strategy', StrategyState}, State) ->
    {'noreply', State#state{strategy_state=StrategyState}, 'hibernate'};

handle_cast({'update_queue_config', JObj}, #state{enter_when_empty=_EnterWhenEmpty}=State) ->
    EWE = wh_json:is_true([<<"Doc">>, <<"enter_when_empty">>], JObj, 'true'),
    lager:debug("maybe changing ewe from ~s to ~s", [_EnterWhenEmpty, EWE]),
    {'noreply', State#state{enter_when_empty=EWE}, 'hibernate'};

handle_cast({'member_call_cancel', K, JObj}, #state{ignored_member_calls=Dict}=State) ->
    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Reason = wh_json:get_value(<<"Reason">>, JObj),

    acdc_stats:call_abandoned(AcctId, QueueId, CallId, Reason),
    {'noreply', State#state{
                  ignored_member_calls=dict:store(K, 'true', Dict)
                 }};
handle_cast({'monitor_call', Call}, State) ->
    gen_listener:add_binding(self(), 'call', [{'callid', whapps_call:call_id(Call)}
                                              ,{'restrict_to', ['events']}
                                             ]),
    {'noreply', State};
handle_cast({'start_workers'}, #state{acct_id=AcctId
                                      ,queue_id=QueueId
                                      ,supervisor=QueueSup
                                     }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    case couch_mgr:get_results(wh_util:format_account_id(AcctId, 'encoded')
                               ,<<"agents/agents_listing">>
                                   ,[{'key', QueueId}
                                     ,'include_docs'
                                    ]
                              )
    of
        {'ok', Agents} ->
            _ = [start_agent_and_worker(WorkersSup, AcctId, QueueId
                                        ,wh_json:get_value(<<"doc">>, A)
                                       )
                 || A <- Agents
                ], 'ok';
        {'error', _E} ->
            lager:debug("failed to find agent count: ~p", [_E]),
            QWC = whapps_config:get_integer(<<"acdc">>, <<"queue_worker_count">>, 5),
            acdc_queue_workers_sup:new_workers(WorkersSup, AcctId, QueueId, QWC)
    end,
    {'noreply', State};

handle_cast({'start_worker'}, State) ->
    handle_cast({'start_worker', 1}, State);
handle_cast({'start_worker', N}, #state{acct_id=AcctId
                                        ,queue_id=QueueId
                                        ,supervisor=QueueSup
                                       }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    acdc_queue_workers_sup:new_workers(WorkersSup, AcctId, QueueId, N),
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
    handle_cast({'agent_available', wh_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_ringing', AgentId}, #state{strategy=Strategy
                                               ,strategy_state=StrategyState
                                              }=State) when is_binary(AgentId) ->
    lager:info("agent ~s ringing, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = maybe_update_strategy(Strategy, StrategyState, AgentId),
    {'noreply', State#state{strategy_state=StrategyState1}, 'hibernate'};
handle_cast({'agent_ringing', JObj}, State) ->
    handle_cast({'agent_ringing', wh_json:get_value(<<"Agent-ID">>, JObj)}, State);

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
    handle_cast({'agent_unavailable', wh_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'reject_member_call', Call, JObj}, #state{acct_id=AcctId
                                                       ,queue_id=QueueId
                                                      }=State) ->
    Prop = [{<<"Call-ID">>, whapps_call:call_id(Call)}
            ,{<<"Account-ID">>, AcctId}
            ,{<<"Queue-ID">>, QueueId}
            ,{<<"Failure-Reason">>, <<"no agents">>}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = wh_json:get_value(<<"Server-ID">>, JObj),
    catch wapi_acdc_queue:publish_member_call_failure(Q, Prop),
    {'noreply', State};

handle_cast({'sync_with_agent', A}, #state{acct_id=AcctId}=State) ->
    case acdc_agent_util:most_recent_status(AcctId, A) of
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
handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_secondary_queue(AcctId, QueueId) ->
    Self = self(),
    _ = spawn(fun() -> gen_listener:add_queue(Self
                                              ,?SECONDARY_QUEUE_NAME(QueueId)
                                              ,[{'queue_options', ?SECONDARY_QUEUE_OPTIONS}
                                                ,{'consume_options', ?SECONDARY_CONSUME_OPTIONS}
                                               ]
                                              ,?SECONDARY_BINDINGS(AcctId, QueueId)
                                             )
              end).

make_ignore_key(AcctId, QueueId, CallId) ->
    {AcctId, QueueId, CallId}.

-spec start_agent_and_worker(pid(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
start_agent_and_worker(WorkersSup, AcctId, QueueId, AgentJObj) ->
    acdc_queue_workers_sup:new_worker(WorkersSup, AcctId, QueueId),

    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),

    case acdc_agent_util:most_recent_status(AcctId, AgentId) of
        {'ok', <<"logout">>} -> 'ok';
        {'ok', <<"logged_out">>} -> 'ok';
        {'ok', _Status} ->
            lager:debug("maybe starting agent ~s(~s) for queue ~s", [AgentId, _Status, QueueId]),

            case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
                'undefined' -> acdc_agents_sup:new(AgentJObj);
                P when is_pid(P) -> 'ok'
            end
    end.

%% Really sophisticated selection algorithm
-spec pick_winner(pid(), wh_json:objects(), queue_strategy(), api_binary()) ->
                         'undefined' |
                         {wh_json:objects(), wh_json:objects()}.
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
    AgentId = wh_json:get_value(<<"Agent-ID">>, MostIdle),
    {Same, Other} = split_agents(AgentId, Rest),

    {[MostIdle|Same], Other}.

-spec update_strategy_with_agent(queue_strategy(), queue_strategy_state(), dict(), ne_binary(), 'add' | 'remove') ->
                                        {queue_strategy_state(), dict()}.
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
        {{value, AgentId}, StrategyState1} ->
            lager:debug("agent ~s was front of queue, moving", [AgentId]),
            queue:in(AgentId, StrategyState1);
        _ -> StrategyState
    end.

%% If A's idle time is greater, it should come before B
-spec sort_agent(wh_json:object(), wh_json:object()) -> boolean().
sort_agent(A, B) ->
    wh_json:get_integer_value(<<"Idle-Time">>, A, 0) >
        wh_json:get_integer_value(<<"Idle-Time">>, B, 0).

%% Handle when an agent process has responded to the connect_req
%% but then the agent logs out of their phone (removing the agent
%% from the list in the queue manager).
%% Otherwise CRs will never be empty
-spec remove_unknown_agents(pid(), wh_json:objects()) -> wh_json:objects().
remove_unknown_agents(Mgr, CRs) ->
    case gen_listener:call(Mgr, 'current_agents') of
        [] -> [];
        Agents ->
            [CR || CR <- CRs,
                   lists:member(wh_json:get_value(<<"Agent-ID">>, CR), Agents)
            ]
    end.

-spec split_agents(ne_binary(), wh_json:objects()) ->
                          {wh_json:objects(), wh_json:objects()}.
split_agents(AgentId, Rest) ->
    lists:partition(fun(R) ->
                            AgentId =:= wh_json:get_value(<<"Agent-ID">>, R)
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
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{'key', QueueId}]) of
        {'ok', []} -> lager:debug("no agents around"), AgentQ;
        {'ok', JObjs} ->
            Q = queue:from_list([Id || JObj <- JObjs,
                                       not queue:member((Id = wh_json:get_value(<<"id">>, JObj)), AgentQ)
                                ]),
            queue:join(AgentQ, Q);
        {'error', _E} -> lager:debug("error creating strategy rr: ~p", [_E]), AgentQ
    end;
create_strategy_state('mi', 'undefined', AcctDb, QueueId) ->
    create_strategy_state('mi', [], AcctDb, QueueId);
create_strategy_state('mi', AgentL, AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {'ok', []} -> lager:debug("no agents around"), AgentL;
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, Acc) ->
                                Id = wh_json:get_value(<<"id">>, JObj),
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

-spec update_properties(wh_json:object(), mgr_state()) -> mgr_state().
update_properties(QueueJObj, State) ->
    State#state{
      enter_when_empty=wh_json:is_true(<<"enter_when_empty">>, QueueJObj, 'true')
      ,moh=wh_json:get_value(<<"moh">>, QueueJObj)
     }.
