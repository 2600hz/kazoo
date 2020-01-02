%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Manages queue processes:
%%%   starting when a queue is created
%%%   stopping when a queue is deleted
%%%   collecting stats from queues
%%%   and more!!!
%%%
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
        ,are_agents_available/1
        ,handle_config_change/2
        ,queue_size/1
        ,should_ignore_member_call/3, should_ignore_member_call/4
        ,up_next/2
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

-ifdef(TEST).
-export([ss_size/2
        ,update_strategy_with_agent/5
        ]).
-endif.

-include("acdc.hrl").
-include("acdc_queue_manager.hrl").

-define(SERVER, ?MODULE).

-define(BINDINGS(A, Q), [{'conf', [{'type', <<"queue">>}
                                  ,{'db', kzs_util:format_account_db(A)}
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
    _ = kz_log:put_callid(JObj),

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

-spec are_agents_available(kz_types:server_ref()) -> boolean().
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
    kz_log:put_callid(JObj),
    lager:debug("cancel call ~p", [JObj]),
    'true' = kapi_acdc_queue:member_call_cancel_v(JObj),
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

-spec queue_size(kz_types:server_ref()) -> kz_term:non_neg_integer().
queue_size(Srv) ->
    gen_listener:call(Srv, 'queue_size').

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

-spec current_agents(kz_types:server_ref()) -> kz_term:ne_binaries().
current_agents(Srv) -> gen_listener:call(Srv, 'current_agents').

-spec status(pid()) -> kz_term:ne_binaries().
status(Srv) -> gen_listener:call(Srv, 'status').

-spec refresh(pid(), kz_json:object()) -> 'ok'.
refresh(Mgr, QueueJObj) -> gen_listener:cast(Mgr, {'refresh', QueueJObj}).

strategy(Srv) -> gen_listener:call(Srv, 'strategy').
next_winner(Srv) -> gen_listener:call(Srv, 'next_winner').

agents_available(Srv) -> gen_listener:call(Srv, 'agents_available').

-spec pick_winner(pid(), kz_json:objects()) ->
          'undefined' |
          {kz_json:objects(), kz_json:objects()}.
pick_winner(Srv, Resps) -> pick_winner(Srv, Resps, strategy(Srv), next_winner(Srv)).

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

    kz_log:put_callid(<<"mgr_", QueueId/binary>>),

    init(Super, AccountId, QueueId, QueueJObj);

init([Super, AccountId, QueueId]) ->
    kz_log:put_callid(<<"mgr_", QueueId/binary>>),

    AcctDb = kzs_util:format_account_db(AccountId),
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(AcctDb, QueueId),

    init(Super, AccountId, QueueId, QueueJObj).

init(Super, AccountId, QueueId, QueueJObj) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    _ = kz_datamgr:add_to_doc_cache(AccountDb, QueueId, QueueJObj),

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

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), mgr_state()) -> kz_types:handle_call_ret_state(mgr_state()).
handle_call('queue_size', _, #state{current_member_calls=Calls}=State) ->
    {'reply', length(Calls), State};
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

handle_call({'up_next', CallId}, _, #state{strategy_state=SS
                                          ,current_member_calls=CurrentCalls
                                          }=State) ->
    FreeAgents = ss_size(SS, 'free'),
    Position = call_position(CallId, lists:reverse(CurrentCalls)),
    {'reply', FreeAgents >= Position, State};

handle_call('config', _, #state{account_id=AccountId
                               ,queue_id=QueueId
                               }=State) ->
    {'reply', {AccountId, QueueId}, State};

handle_call('status', _, #state{strategy_state=#strategy_state{details=Details}}=State) ->
    Known = [A || {A, {N, _}} <- dict:to_list(Details), N > 0],
    {'reply', Known, State};

handle_call('strategy', _, #state{strategy=Strategy}=State) ->
    {'reply', Strategy, State, 'hibernate'};

handle_call('agents_available', _, #state{strategy_state=SS}=State) ->
    {'reply', ss_size(SS, 'logged_in'), State};

handle_call('enter_when_empty', _, #state{enter_when_empty=EnterWhenEmpty}=State) ->
    {'reply', EnterWhenEmpty, State};

handle_call('next_winner', _, #state{strategy='mi'}=State) ->
    {'reply', 'undefined', State};
handle_call('next_winner', _, #state{strategy='rr'
                                    ,strategy_state=#strategy_state{agents=Agents}=SS
                                    }=State) ->
    case queue:out(Agents) of
        {{'value', Winner}, Agents1} ->
            {'reply', Winner, State#state{strategy_state=SS#strategy_state{agents=queue:in(Winner, Agents1)}}, 'hibernate'};
        {'empty', _} ->
            {'reply', 'undefined', State}
    end;
handle_call('next_winner', _, #state{strategy='all'}=State) ->
    {'reply', 'undefined', State};

handle_call('current_agents', _, #state{strategy='rr'
                                       ,strategy_state=#strategy_state{agents=Q}
                                       }=State) ->
    {'reply', queue:to_list(Q), State};
handle_call('current_agents', _, #state{strategy='mi'
                                       ,strategy_state=#strategy_state{agents=L}
                                       }=State) ->
    {'reply', L, State};
handle_call('current_agents', _, #state{strategy='all'
                                       ,strategy_state=#strategy_state{agents=Q}
                                       }=State) ->
    {'reply', queue:to_list(Q), State};

handle_call({'queue_position', CallId}, _, #state{current_member_calls=CurrentCalls}=State) ->
    Position = call_position(CallId, lists:reverse(CurrentCalls)),
    {'reply', Position, State};

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

handle_cast({'member_call_cancel', K, JObj}, #state{ignored_member_calls=Dict}=State) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    QueueId = kz_json:get_value(<<"Queue-ID">>, JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    Reason = kz_json:get_value(<<"Reason">>, JObj),

    'ok' = acdc_stats:call_abandoned(AccountId, QueueId, CallId, Reason),
    {'noreply', State#state{ignored_member_calls=dict:store(K, 'true', Dict)}};
handle_cast({'start_workers'}, #state{account_id=AccountId
                                     ,queue_id=QueueId
                                     ,supervisor=QueueSup
                                     }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
    case kz_datamgr:get_results(kzs_util:format_account_db(AccountId)
                               ,<<"queues/agents_listing">>
                               ,[{'key', QueueId}
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

handle_cast({'agent_available', AgentId}, #state{strategy=Strategy
                                                ,strategy_state=StrategyState
                                                ,supervisor=QueueSup
                                                }=State) when is_binary(AgentId) ->
    lager:info("adding agent ~s to strategy ~s", [AgentId, Strategy]),
    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, 'add', 'undefined'),
    maybe_start_queue_workers(QueueSup, ss_size(StrategyState1, 'logged_in')),
    {'noreply', State#state{strategy_state=StrategyState1}
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

handle_cast({'agent_busy', AgentId}, #state{strategy=Strategy
                                           ,strategy_state=StrategyState
                                           }=State) when is_binary(AgentId) ->
    lager:info("agent ~s busy, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, 'remove', 'busy'),
    {'noreply', State#state{strategy_state=StrategyState1}
    ,'hibernate'};
handle_cast({'agent_busy', JObj}, State) ->
    handle_cast({'agent_busy', kz_json:get_value(<<"Agent-ID">>, JObj)}, State);

handle_cast({'agent_unavailable', AgentId}, #state{strategy=Strategy
                                                  ,strategy_state=StrategyState
                                                  }=State) when is_binary(AgentId) ->
    lager:info("agent ~s unavailable, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId, 'remove', 'undefined'),
    {'noreply', State#state{strategy_state=StrategyState1}
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
    {'ok', Status} = acdc_agent_util:most_recent_status(AccountId, A),
    case acdc_agent_util:status_should_auto_start(Status) of
        'true' -> 'ok';
        'false' -> gen_listener:cast(self(), {'agent_unavailable', A})
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
                                              ,announcements_config=AnnouncementsConfig
                                              ,announcements_pids=AnnouncementsPids
                                              }=State) ->
    Position = length(CurrentCalls)+1,
    Call = kapps_call:set_custom_channel_var(<<"Queue-Position">>
                                            ,Position
                                            ,kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj))),

    'ok' = acdc_stats:call_waiting(AccountId, QueueId
                                  ,kapps_call:call_id(Call)
                                  ,kapps_call:caller_id_name(Call)
                                  ,kapps_call:caller_id_number(Call)
                                  ,kz_json:get_integer_value(<<"Member-Priority">>, JObj)
                                  ),

    publish_queue_member_add(AccountId, QueueId, Call),

    %% Add call to shared queue
    kapi_acdc_queue:publish_shared_member_call(AccountId, QueueId, JObj),
    lager:debug("put call into shared messaging queue"),

    acdc_util:presence_update(AccountId, QueueId, ?PRESENCE_RED_FLASH),

    %% Schedule position/wait time announcements
    AnnouncementsPids1 = case acdc_announcements_sup:maybe_start_announcements(self(), Call, AnnouncementsConfig) of
                             'false' -> AnnouncementsPids;
                             {'ok', Pid} ->
                                 CallId = kapps_call:call_id(Call),
                                 AnnouncementsPids#{CallId => Pid}
                         end,

    {'noreply', State#state{current_member_calls=[Call | CurrentCalls]
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
    AccountDb = kzs_util:format_account_db(AccountId),
    Priority = lookup_priority_levels(AccountDb, QueueId),
    kz_process:spawn(fun gen_listener:add_queue/4
                    ,[self()
                     ,?SECONDARY_QUEUE_NAME(QueueId)
                     ,[{'queue_options', ?SECONDARY_QUEUE_OPTIONS(Priority)}
                      ,{'consume_options', ?SECONDARY_CONSUME_OPTIONS}
                      ]
                     ,?SECONDARY_BINDINGS(AccountId, QueueId)
                     ]).

-spec lookup_priority_levels(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_integer().
lookup_priority_levels(AccountDB, QueueId) ->
    case kz_datamgr:open_cache_doc(AccountDB, QueueId) of
        {'ok', JObj} -> kz_json:get_value(<<"max_priority">>, JObj);
        _ -> 'undefined'
    end.

make_ignore_key(AccountId, QueueId, CallId) ->
    {AccountId, QueueId, CallId}.

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

    {[MostIdle|Same], Other};
pick_winner(_Mgr, CRs, 'all', _AgentId) ->
    {CRs, []}.

-spec update_strategy_with_agent(queue_strategy(), strategy_state(), kz_term:ne_binary(), 'add' | 'remove', 'busy' | 'undefined') ->
          strategy_state().
update_strategy_with_agent('rr', #strategy_state{agents=AgentQueue}=SS, AgentId, 'add', Busy) ->
    case queue:member(AgentId, AgentQueue) of
        'true' -> set_busy(AgentId, Busy, SS);
        'false' -> set_busy(AgentId, Busy, add_agent('rr', AgentId, SS))
    end;
update_strategy_with_agent('rr', SS, AgentId, 'remove', 'busy') ->
    set_busy(AgentId, 'busy', SS);
update_strategy_with_agent('rr', #strategy_state{agents=AgentQueue}=SS, AgentId, 'remove', Busy) ->
    case queue:member(AgentId, AgentQueue) of
        'false' -> set_busy(AgentId, Busy, SS);
        'true' -> set_busy(AgentId, Busy, remove_agent('rr', AgentId, SS))
    end;
update_strategy_with_agent('mi', #strategy_state{agents=AgentL}=SS, AgentId, 'add', Busy) ->
    case lists:member(AgentId, AgentL) of
        'true' -> set_busy(AgentId, Busy, SS);
        'false' -> set_busy(AgentId, Busy, add_agent('mi', AgentId, SS))
    end;
update_strategy_with_agent('mi', SS, AgentId, 'remove', 'busy') ->
    set_busy(AgentId, 'busy', SS);
update_strategy_with_agent('mi', #strategy_state{agents=AgentL}=SS, AgentId, 'remove', Busy) ->
    case lists:member(AgentId, AgentL) of
        'false' -> set_busy(AgentId, Busy, SS);
        'true' -> set_busy(AgentId, Busy, remove_agent('mi', AgentId, SS))
    end;
update_strategy_with_agent('all', #strategy_state{agents=AgentQueue}=SS, AgentId, 'add', Busy) ->
    case queue:member(AgentId, AgentQueue) of
        'true' -> set_busy(AgentId, Busy, SS);
        'false' -> set_busy(AgentId, Busy, add_agent('all', AgentId, SS))
    end;
update_strategy_with_agent('all', SS, AgentId, 'remove', 'busy') ->
    set_busy(AgentId, 'busy', SS);
update_strategy_with_agent('all', #strategy_state{agents=AgentQueue}=SS, AgentId, 'remove', Busy) ->
    case queue:member(AgentId, AgentQueue) of
        'false' -> set_busy(AgentId, Busy, SS);
        'true' -> set_busy(AgentId, Busy, remove_agent('all', AgentId, SS))
    end.

-spec add_agent(queue_strategy(), kz_term:ne_binary(), strategy_state()) -> strategy_state().
add_agent('rr', AgentId, #strategy_state{agents=AgentQueue
                                        ,details=Details
                                        }=SS) ->
    SS#strategy_state{agents=queue:in(AgentId, AgentQueue)
                     ,details=incr_agent(AgentId, Details)
                     };
add_agent('mi', AgentId, #strategy_state{agents=AgentL
                                        ,details=Details
                                        }=SS) ->
    SS#strategy_state{agents=[AgentId | AgentL]
                     ,details=incr_agent(AgentId, Details)
                     };
add_agent('all', AgentId, #strategy_state{agents=AgentQueue
                                         ,details=Details
                                         }=SS) ->
    SS#strategy_state{agents=queue:in(AgentId, AgentQueue)
                     ,details=incr_agent(AgentId, Details)
                     }.

-spec remove_agent(queue_strategy(), kz_term:ne_binary(), strategy_state()) -> strategy_state().
remove_agent('rr', AgentId, #strategy_state{agents=AgentQueue
                                           ,details=Details
                                           }=SS) ->
    case dict:find(AgentId, Details) of
        {'ok', {Count, _}} when Count > 1 ->
            SS#strategy_state{details=decr_agent(AgentId, Details)};
        _ ->
            SS#strategy_state{agents=queue:filter(fun(AgentId1) when AgentId =:= AgentId1 -> 'false';
                                                     (_) -> 'true' end
                                                 ,AgentQueue
                                                 )
                             ,details=decr_agent(AgentId, Details)
                             }
    end;
remove_agent('mi', AgentId, #strategy_state{agents=AgentL
                                           ,details=Details
                                           }=SS) ->
    case dict:find(AgentId, Details) of
        {'ok', {Count, _}} when Count > 1 ->
            SS#strategy_state{details=decr_agent(AgentId, Details)};
        _ ->
            SS#strategy_state{agents=[A || A <- AgentL, A =/= AgentId]
                             ,details=decr_agent(AgentId, Details)
                             }
    end;
remove_agent('all', AgentId, #strategy_state{agents=AgentQueue
                                            ,details=Details
                                            }=SS) ->
    case dict:find(AgentId, Details) of
        {'ok', {Count, _}} when Count > 1 ->
            SS#strategy_state{details=decr_agent(AgentId, Details)};
        _ ->
            SS#strategy_state{agents=queue:filter(fun(AgentId1) when AgentId =:= AgentId1 -> 'false';
                                                     (_) -> 'true' end
                                                 ,AgentQueue
                                                 )
                             ,details=decr_agent(AgentId, Details)
                             }
    end.

-spec incr_agent(kz_term:ne_binary(), dict:dict(kz_term:ne_binary(), ss_details())) ->
          dict:dict(kz_term:ne_binary(), ss_details()).
incr_agent(AgentId, Details) ->
    dict:update(AgentId, fun({Count, Busy}) -> {Count + 1, Busy} end, {1, 'undefined'}, Details).

-spec decr_agent(kz_term:ne_binary(), dict:dict(kz_term:ne_binary(), ss_details())) ->
          dict:dict(kz_term:ne_binary(), ss_details()).
decr_agent(AgentId, Details) ->
    dict:update(AgentId, fun({Count, Busy}) when Count > 1 -> {Count - 1, Busy};
                            ({_, Busy}) -> {0, Busy} end
               ,{0, 'undefined'}, Details).

-spec set_busy(kz_term:ne_binary(), 'busy' | 'undefined', strategy_state()) -> strategy_state().
set_busy(AgentId, Busy, #strategy_state{details=Details}=SS) ->
    SS#strategy_state{details=dict:update(AgentId, fun({Count, _}) -> {Count, Busy} end, {0, Busy}, Details)}.

maybe_update_strategy('mi', StrategyState, _AgentId) -> StrategyState;
maybe_update_strategy('rr', #strategy_state{agents=AgentQueue}=SS, AgentId) ->
    case queue:out(AgentQueue) of
        {{'value', AgentId}, AgentQueue1} ->
            lager:debug("agent ~s was front of queue, moving", [AgentId]),
            SS#strategy_state{agents=queue:in(AgentId, AgentQueue1)};
        _ -> SS
    end;
maybe_update_strategy('all', #strategy_state{agents=AgentQueue}=SS, AgentId) ->
    case queue:out(AgentQueue) of
        {{'value', AgentId}, AgentQueue1} ->
            lager:debug("agent ~s was front of queue, moving", [AgentId]),
            SS#strategy_state{agents=queue:in(AgentId, AgentQueue1)};
        _ -> SS
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
get_strategy(<<"ring_all">>) -> 'all';
get_strategy(_) -> 'rr'.

-spec create_strategy_state(queue_strategy(), kz_term:ne_binary(), kz_term:ne_binary()) -> strategy_state().
create_strategy_state(Strategy, AcctDb, QueueId) ->
    create_strategy_state(Strategy, #strategy_state{}, AcctDb, QueueId).

-spec create_strategy_state(queue_strategy(), strategy_state(), kz_term:ne_binary(), kz_term:ne_binary()) -> strategy_state().
create_strategy_state('rr', #strategy_state{agents='undefined'}=SS, AcctDb, QueueId) ->
    create_strategy_state('rr', SS#strategy_state{agents=queue:new()}, AcctDb, QueueId);
create_strategy_state('rr', #strategy_state{agents=AgentQ}=SS, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>
                               ,[{'key', QueueId}, {'reduce', 'false'}])
    of
        {'ok', []} -> lager:debug("no agents around"), SS;
        {'ok', JObjs} ->
            Q = queue:from_list([Id || JObj <- JObjs,
                                       not queue:member((Id = kz_doc:id(JObj)), AgentQ)
                                ]),
            Details = lists:foldl(fun(JObj, Acc) ->
                                          dict:store(kz_doc:id(JObj), {1, 'undefined'}, Acc)
                                  end, dict:new(), JObjs),
            SS#strategy_state{agents=queue:join(AgentQ, Q)
                             ,details=Details
                             };
        {'error', _E} -> lager:debug("error creating strategy rr: ~p", [_E]), SS
    end;
create_strategy_state('mi', #strategy_state{agents='undefined'}=SS, AcctDb, QueueId) ->
    create_strategy_state('mi', SS#strategy_state{agents=[]}, AcctDb, QueueId);
create_strategy_state('mi', #strategy_state{agents=AgentL}=SS, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>
                               ,[{'key', QueueId}, {'reduce', 'false'}])
    of
        {'ok', []} -> lager:debug("no agents around"), SS;
        {'ok', JObjs} ->
            AgentL1 = lists:foldl(fun(JObj, Acc) ->
                                          Id = kz_doc:id(JObj),
                                          case lists:member(Id, Acc) of
                                              'true' -> Acc;
                                              'false' -> [Id | Acc]
                                          end
                                  end, AgentL, JObjs),
            Details = lists:foldl(fun(JObj, Acc) ->
                                          dict:store(kz_doc:id(JObj), {1, 'undefined'}, Acc)
                                  end, dict:new(), JObjs),
            SS#strategy_state{agents=AgentL1
                             ,details=Details
                             };
        {'error', _E} -> lager:debug("error creating strategy mi: ~p", [_E]), SS
    end;
create_strategy_state('all', #strategy_state{agents='undefined'}=SS, AcctDb, QueueId) ->
    create_strategy_state('all', SS#strategy_state{agents=queue:new()}, AcctDb, QueueId);
create_strategy_state('all', #strategy_state{agents=AgentQ}=SS, AcctDb, QueueId) ->
    case kz_datamgr:get_results(AcctDb, <<"queues/agents_listing">>
                               ,[{'key', QueueId}, {'reduce', 'false'}])
    of
        {'ok', []} -> lager:debug("no agents around"), SS;
        {'ok', JObjs} ->
            Q = queue:from_list([Id || JObj <- JObjs,
                                       not queue:member((Id = kz_doc:id(JObj)), AgentQ)
                                ]),
            Details = lists:foldl(fun(JObj, Acc) ->
                                          dict:store(kz_doc:id(JObj), {1, 'undefined'}, Acc)
                                  end, dict:new(), JObjs),
            SS#strategy_state{agents=queue:join(AgentQ, Q)
                             ,details=Details
                             };
        {'error', _E} -> lager:debug("error creating strategy all: ~p", [_E]), SS
    end.

update_strategy_state(Srv, 'rr', #strategy_state{agents=AgentQueue}) ->
    L = queue:to_list(AgentQueue),
    update_strategy_state(Srv, L);
update_strategy_state(Srv, 'mi', #strategy_state{agents=AgentL}) ->
    update_strategy_state(Srv, AgentL);
update_strategy_state(Srv, 'all', #strategy_state{agents=AgentQueue}) ->
    L = queue:to_list(AgentQueue),
    update_strategy_state(Srv, L).
update_strategy_state(Srv, L) ->
    [gen_listener:cast(Srv, {'sync_with_agent', A}) || A <- L].

-spec call_position(kz_term:ne_binary(), [kapps_call:call()]) -> kz_term:api_integer().
call_position(CallId, Calls) ->
    call_position(CallId, Calls, 1).

-spec call_position(kz_term:ne_binary(), [kapps_call:call()], pos_integer()) -> pos_integer().
call_position(_, [], _) ->
    'undefined';
call_position(CallId, [Call|Calls], Position) ->
    case kapps_call:call_id(Call) of
        CallId -> Position;
        _ -> call_position(CallId, Calls, Position + 1)
    end.

-spec ss_size(strategy_state(), 'free' | 'logged_in') -> integer().
ss_size(#strategy_state{agents=Agents}, 'logged_in') ->
    case queue:is_queue(Agents) of
        'true' -> queue:len(Agents);
        'false' -> length(Agents)
    end;
ss_size(#strategy_state{agents=Agents
                       ,details=Details
                       }, 'free') when is_list(Agents) ->
    lists:foldl(fun(AgentId, Count) ->
                        case dict:find(AgentId, Details) of
                            {'ok', {ProcCount, 'undefined'}} when ProcCount > 0 -> Count + 1;
                            _ -> Count
                        end
                end, 0, Agents);
ss_size(#strategy_state{agents=Agents}=SS, 'free') ->
    ss_size(SS#strategy_state{agents=queue:to_list(Agents)}, 'free').

maybe_start_queue_workers(QueueSup, AgentCount) ->
    WSup = acdc_queue_sup:workers_sup(QueueSup),
    case acdc_queue_workers_sup:worker_count(WSup) of
        N when N >= AgentCount -> 'ok';
        N when N < AgentCount -> gen_listener:cast(self(), {'start_worker', AgentCount-N})
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
