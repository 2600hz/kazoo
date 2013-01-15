%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-export([start_link/3
         ,handle_member_call/2
         ,handle_member_call_cancel/2
         ,handle_agent_change/2
         ,should_ignore_member_call/3
         ,config/1
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
          ,enter_when_empty = 'true' :: boolean() % allow caller into queue if no agents are logged in
         }).

-define(BINDINGS(A, Q), [{conf, [{doc_type, <<"queue">>}
                                 ,{db, wh_util:format_account_id(A, encoded)}
                                 ,{doc_id, Q}
                                ]}
                         ,{acdc_queue, [{restrict_to, [stats_req, agent_change]}
                                        ,{account_id, A}
                                        ,{queue_id, Q}
                                       ]}
                         ,{notifications, [{restrict_to, [presence_probe]}]}
                        ]).

-define(RESPONDERS, [{{acdc_queue_handler, handle_config_change}
                      ,[{<<"configuration">>, <<"*">>}]
                     }
                     ,{{acdc_queue_handler, handle_stats_req}
                       ,[{<<"queue">>, <<"stats_req">>}]
                      }
                     ,{{acdc_queue_handler, handle_presence_probe}
                       ,[{<<"notification">>, <<"presence_probe">>}]
                      }
                     ,{{acdc_queue_manager, handle_member_call}
                       ,[{<<"member">>, <<"call">>}]
                      }
                     ,{{acdc_queue_manager, handle_member_call_cancel}
                       ,[{<<"member">>, <<"call_cancel">>}]
                      }
                     ,{{acdc_queue_manager, handle_agent_change}
                       ,[{<<"queue">>, <<"agent_change">>}]
                      }
                    ]).

-define(SECONDARY_BINDINGS(AcctId, QueueId), [{acdc_queue, [{restrict_to, [member_call]}
                                                            ,{account_id, AcctId}
                                                            ,{queue_id, QueueId}
                                                           ]}
                                             ]).
-define(SECONDARY_QUEUE_NAME, <<"acdc.queue.manager">>).
-define(SECONDARY_QUEUE_OPTIONS, [{exclusive, false}]).
-define(SECONDARY_CONSUME_OPTIONS, [{exclusive, false}]).

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
-spec start_link/3 :: (pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(Super, AcctId, QueueId) ->
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS(AcctId, QueueId)}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[Super, AcctId, QueueId]
                           ).

handle_member_call(JObj, Props) ->
    true = wapi_acdc_queue:member_call_v(JObj),
    _ = wh_util:put_callid(JObj),

    case are_agents_available(props:get_value(server, Props)
                              ,props:get_value(enter_when_empty, Props)
                             )
    of
        false ->
            lager:info("no agents are available to take the call, cancel queueing"),
            gen_listener:cast(props:get_value(server, Props), {reject_member_call, JObj});
        true ->
            start_queue_call(JObj, Props)
    end.

are_agents_available(Srv, EnterWhenEmpty) ->
    agents_available(Srv) > 0 orelse EnterWhenEmpty.

start_queue_call(JObj, Props) ->
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),

    AcctId = wh_json:get_value(<<"Account-ID">>, JObj),
    QueueId = wh_json:get_value(<<"Queue-ID">>, JObj),

    lager:debug("member call for ~s: ~s", [QueueId, whapps_call:call_id(Call)]),

    acdc_stats:call_waiting(AcctId, QueueId
                            ,whapps_call:call_id(Call)
                            ,whapps_call:caller_id_name(Call)
                            ,whapps_call:caller_id_number(Call)
                           ),

    whapps_call_command:answer(Call),
    whapps_call_command:hold(Call),

    wapi_acdc_queue:publish_shared_member_call(AcctId, QueueId, JObj),

    _ = whapps_call_command:set(undefined
                                ,wh_json:from_list([{<<"Eavesdrop-Group-ID">>, QueueId}])
                                ,Call
                               ),

    gen_listener:cast(props:get_value(server, Props), {monitor_call, Call}),

    acdc_util:presence_update(AcctId, QueueId, ?PRESENCE_RED_FLASH).

handle_member_call_cancel(JObj, Props) ->
    true = wapi_acdc_queue:member_call_cancel_v(JObj),
    K = make_ignore_key(wh_json:get_value(<<"Account-ID">>, JObj)
                        ,wh_json:get_value(<<"Queue-ID">>, JObj)
                        ,wh_json:get_value(<<"Call-ID">>, JObj)
                       ),
    gen_listener:cast(props:get_value(server, Props), {member_call_cancel, K}).


handle_agent_change(JObj, Prop) ->
    true = wapi_acdc_queue:agent_change_v(JObj),
    case wh_json:get_value(<<"Change">>, JObj) of
        <<"available">> ->
            gen_listener:cast(props:get_value(server, Prop), {agent_available, JObj});
        <<"ringing">> ->
            gen_listener:cast(props:get_value(server, Prop), {agent_ringing, JObj});
        <<"unavailable">> ->
            gen_listener:cast(props:get_value(server, Prop), {agent_unavailable, JObj})
    end.

should_ignore_member_call(Srv, Call, CallJObj) ->
    K = make_ignore_key(wh_json:get_value(<<"Account-ID">>, CallJObj)
                        ,wh_json:get_value(<<"Queue-ID">>, CallJObj)
                        ,whapps_call:call_id(Call)
                       ),
    gen_listener:call(Srv, {should_ignore_member_call, K}).

config(Srv) ->
    gen_listener:call(Srv, config).

strategy(Srv) ->
    gen_listener:call(Srv, strategy).
next_winner(Srv) ->
    gen_listener:call(Srv, next_winner).

agents_available(Srv) ->
    gen_listener:call(Srv, agents_available).

pick_winner(Srv, Resps) ->
    pick_winner(Srv, Resps, strategy(Srv), next_winner(Srv)).

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
init([Super, AcctId, QueueId]) ->
    put(callid, <<"mgr_", QueueId/binary>>),

    _ = start_secondary_queue(AcctId, QueueId),

    AcctDb = wh_util:format_account_id(AcctId, encoded),
    {ok, QueueJObj} = couch_mgr:open_doc(AcctDb, QueueId),

    gen_listener:cast(self(), {start_workers}),
    Strategy = get_strategy(wh_json:get_value(<<"strategy">>, QueueJObj)),

    lager:debug("queue mgr started"),
    {ok, #state{
       acct_id=AcctId
       ,queue_id=QueueId
       ,supervisor=Super
       ,strategy = Strategy
       ,strategy_state = create_strategy_state(Strategy, AcctDb, QueueId)
       ,enter_when_empty = wh_json:get_boolean_value(<<"enter_when_empty">>, QueueJObj, true)
      }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({should_ignore_member_call, K}, _, #state{ignored_member_calls=Dict}=State) ->
    case catch dict:fetch(K, Dict) of
        {'EXIT', _} -> {reply, false, State};
        _Res -> {reply, true, State#state{ignored_member_calls=dict:erase(K, Dict)}, hibernate}
    end;

handle_call(config, _, #state{acct_id=AcctId
                              ,queue_id=QueueId
                             }=State) ->
    {reply, {AcctId, QueueId}, State};

handle_call(strategy, _, #state{strategy=Strategy}=State) ->
    {reply, Strategy, State, hibernate};

handle_call(agents_available, _, #state{strategy_state=undefined}=State) ->
    {reply, 0, State};
handle_call(agents_available, _, #state{strategy_state=[]}=State) ->
    {reply, 0, State};
handle_call(agents_available, _, #state{strategy_state=[_|_]}=State) ->
    {reply, 1, State};
handle_call(agents_available, _, #state{strategy_state=SS}=State) ->
    {reply, queue:len(SS), State};

handle_call(next_winner, _, #state{strategy='mi'}=State) ->
    {reply, undefined, State};
handle_call(next_winner, _, #state{strategy='rr'
                                   ,strategy_state=SS
                                  }=State) ->
    case queue:out(SS) of
        {{value, Winner}, SS1} ->
            lager:debug("agent queue: ~p ~s", [queue:to_list(SS1), Winner]),
            {reply, Winner, State#state{strategy_state=queue:in(Winner, SS1)}, hibernate};
        {empty, _} ->
            {reply, undefined, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({update_strategy, StrategyState}, State) ->
    {noreply, State#state{strategy_state=StrategyState}, hibernate};
handle_cast({member_call_cancel, K}, #state{ignored_member_calls=Dict}=State) ->
    {noreply, State#state{
                ignored_member_calls=dict:store(K, true, Dict)
               }};
handle_cast({monitor_call, Call}, State) ->
    gen_listener:add_binding(self(), call, [{callid, whapps_call:call_id(Call)}
                                            ,{restrict_to, [events]}
                                           ]),
    {noreply, State};
handle_cast({start_workers}, #state{acct_id=AcctId
                                    ,queue_id=QueueId
                                    ,supervisor=QueueSup
                                   }=State) ->
    WorkersSup = acdc_queue_sup:workers_sup(QueueSup),

    lager:debug("q sup: ~p ws sup: ~p", [QueueSup, WorkersSup]),

    case couch_mgr:get_results(wh_util:format_account_id(AcctId, encoded)
                               ,<<"agents/agents_listing">>
                               ,[{key, QueueId}
                                 ,include_docs
                                ]
                              )
    of
        {ok, Agents} ->
            _ = [start_agent_and_worker(WorkersSup, AcctId, QueueId, wh_json:get_value(<<"doc">>, A))
                 || A <- Agents
                ], ok;
        {error, _E} ->
            lager:debug("failed to find agent count: ~p", [_E]),
            QWC = whapps_config:get_integer(<<"acdc">>, <<"queue_worker_count">>, 5),
            acdc_queue_workers_sup:new_workers(WorkersSup, AcctId, QueueId, QWC)
    end,
    {noreply, State};

handle_cast({agent_available, JObj}, #state{strategy=Strategy
                                            ,strategy_state=StrategyState
                                           }=State) ->
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    lager:debug("adding agent ~s to strategy ~s", [AgentId, Strategy]),
    StrategyState1 = update_strategy_with_agent(Strategy, StrategyState, AgentId),
    lager:debug("agent queue: ~p", [queue:to_list(StrategyState1)]),
    {noreply, State#state{strategy_state=StrategyState1}, hibernate};

handle_cast({agent_ringing, JObj}, #state{strategy=Strategy
                                          ,strategy_state=StrategyState
                                         }=State) ->
    AgentId = wh_json:get_value(<<"Agent-ID">>, JObj),
    lager:debug("agent ~s ringing, maybe updating strategy ~s", [AgentId, Strategy]),

    StrategyState1 = maybe_update_strategy(Strategy, StrategyState, AgentId),
    lager:debug("agent queue: ~p", [queue:to_list(StrategyState1)]),
    {noreply, State#state{strategy_state=StrategyState1}, hibernate};

handle_cast({reject_member_call, JObj}, State) ->
    Prop = [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj)}
            ,{<<"Queue-ID">>, wh_json:get_value(<<"Queue-ID">>, JObj)}
            ,{<<"Reason">>, <<"no agents">>}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    Q = wh_json:get_value(<<"Server-ID">>, JObj),
    catch wapi_acdc_queue:publish_member_call_cancel(Q, Prop),
    {noreply, State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, #state{enter_when_empty=EnterWhenEmpty}) ->
    {reply, [{enter_when_empty, EnterWhenEmpty}]}.

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
    spawn(fun() -> gen_listener:add_queue(Self
                                          ,?SECONDARY_QUEUE_NAME
                                          ,[{queue_options, ?SECONDARY_QUEUE_OPTIONS}
                                            ,{consume_options, ?SECONDARY_CONSUME_OPTIONS}
                                            ,{basic_qos, 1}
                                           ]
                                          ,?SECONDARY_BINDINGS(AcctId, QueueId)
                                         )
          end).

make_ignore_key(AcctId, QueueId, CallId) ->
    {AcctId, QueueId, CallId}.

-spec start_agent_and_worker/4 :: (pid(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
start_agent_and_worker(WorkersSup, AcctId, QueueId, AgentJObj) ->
    acdc_queue_workers_sup:new_worker(WorkersSup, AcctId, QueueId),

    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),

    case acdc_util:agent_status(AcctId, AgentId) of
        <<"logout">> -> ok;
        _Status ->
            lager:debug("maybe starting agent ~s(~s) for queue ~s", [AgentId, _Status, QueueId]),

            case acdc_agents_sup:find_agent_supervisor(AcctId, AgentId) of
                undefined -> acdc_agents_sup:new(AgentJObj);
                P when is_pid(P) -> ok
            end
    end.

%% Really sophisticated selection algorithm
-spec pick_winner/4 :: (pid(), wh_json:objects()
                        ,queue_strategy(), api_binary()
                       ) ->
                               'undefined' |
                               {wh_json:objects()
                                ,wh_json:objects()
                               }.
pick_winner(_, [], _, _) -> 'undefined';
pick_winner(Mgr, CRs, 'rr', AgentId) ->
    lager:debug("chose ~s to win", [AgentId]),

    case split_agents(AgentId, CRs) of
        {[], _O} ->
            lager:debug("oops, agent appears to have not responded; try again"),
            pick_winner(Mgr, CRs, 'rr', next_winner(Mgr));
        {Winners, OtherAgents} ->
            lager:debug("found winner(s) for the agent"),
            {Winners, OtherAgents}
    end;
pick_winner(_Mgr, CRs, 'mi', _) ->
    [MostIdle | Rest] = lists:usort(fun sort_agent/2, CRs),
    AgentId = wh_json:get_value(<<"Agent-ID">>, MostIdle),
    {Same, Other} = split_agents(AgentId, Rest),

    {[MostIdle|Same], Other}.

-spec update_strategy_with_agent/3 :: (queue_strategy(), queue_strategy_state(), ne_binary()) ->
                                              queue_strategy_state().
update_strategy_with_agent('rr', undefined, AgentId) ->
    queue:in(AgentId, queue:new());
update_strategy_with_agent('rr', AgentQueue, AgentId) ->
    case queue:member(AgentId, AgentQueue) of
        true -> AgentQueue;
        false -> queue:in(AgentId, AgentQueue)
    end;
update_strategy_with_agent('mi', _, _) ->
   undefined.

maybe_update_strategy('mi', StrategyState, _AgentId) -> StrategyState;
maybe_update_strategy('rr', StrategyState, AgentId) ->
    case queue:out(StrategyState) of
        {{value, AgentId}, StrategyState1} ->
            lager:debug("agent ~s was front of queue, moving", [AgentId]),
            queue:in(AgentId, StrategyState1);
        _ -> StrategyState
    end.

%% If A's idle time is greater, it should come before B
-spec sort_agent/2 :: (wh_json:object(), wh_json:object()) -> boolean().
sort_agent(A, B) ->
    wh_json:get_integer_value(<<"Idle-Time">>, A, 0) >
        wh_json:get_integer_value(<<"Idle-Time">>, B, 0).

-spec split_agents/2 :: (ne_binary(), wh_json:objects()) ->
                                {wh_json:objects(), wh_json:objects()}.
split_agents(AgentId, Rest) ->
    lists:partition(fun(R) ->
                            AgentId =:= wh_json:get_value(<<"Agent-ID">>, R)
                    end, Rest).

-spec get_strategy/1 :: (api_binary()) -> queue_strategy().
get_strategy(<<"round_robin">>) -> 'rr';
get_strategy(<<"most_idle">>) -> 'mi';
get_strategy(_) -> 'rr'.

-spec create_strategy_state/4 :: (queue_strategy(), queue_strategy_state(), ne_binary(), ne_binary()) -> queue_strategy_state().
create_strategy_state(Strategy, AcctDb, QueueId) ->
    create_strategy_state(Strategy, undefined, AcctDb, QueueId).

create_strategy_state('rr', undefined, AcctDb, QueueId) ->
    create_strategy_state('rr', queue:new(), AcctDb, QueueId);
create_strategy_state('rr', AgentQ, AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {ok, []} -> lager:debug("no agents around"), AgentQ;
        {ok, JObjs} ->
            Q = queue:from_list([Id
                                 || JObj <- JObjs,
                                    not queue:member((Id = wh_json:get_value(<<"id">>, JObj)), AgentQ)
                                ]),
            queue:join(AgentQ, Q);
        {error, _E} -> lager:debug("error: ~p", [_E]), AgentQ
    end;
create_strategy_state('mi', undefined, AcctDb, QueueId) ->
    create_strategy_state('mi', [], AcctDb, QueueId);
create_strategy_state('mi', AgentL, AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {ok, []} -> lager:debug("no agents around"), AgentL;
        {ok, JObjs} ->
            lists:foldl(fun(JObj, Acc) ->
                                Id = wh_json:get_value(<<"id">>, JObj),
                                case lists:member(Id, Acc) of
                                    true -> Acc;
                                    false -> [Id | Acc]
                                end
                        end, AgentL, JObjs);
        {error, _E} -> lager:debug("error: ~p", [_E]), AgentL
    end.
