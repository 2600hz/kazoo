%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CRUD for call queues
%%% /queues
%%%   GET: list all known queues
%%%   PUT: create a new queue
%%%
%%% /queues/stats
%%%   GET: retrieve stats across all queues
%%% /queues/stats/realtime
%%%   GET: retrieve stats across all queues
%%%
%%% /queues/QID
%%%   GET: queue details
%%%   POST: edit queue details
%%%   DELETE: delete a queue
%%%
%%% /queues/QID/stats
%%%   GET: retrieve stats for this queue
%%% /queues/QID/stats/realtime
%%%   GET: retrieve realtime stats for the queues
%%%
%%% /queues/QID/roster
%%%   GET: get list of agent_ids
%%%   POST: add a list of agent_ids
%%%   DELETE: rm a list of agent_ids
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_queues).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("include/crossbar.hrl").

-define(PVT_TYPE, <<"queue">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"queues/crossbar_listing">>).
-define(CB_AGENTS_LIST, <<"queues/agents_listing">>). %{agent_id, queue_id}

-define(STATS_PATH_TOKEN, <<"stats">>).
-define(REALTIME_PATH_TOKEN, <<"realtime">>).
-define(ROSTER_PATH_TOKEN, <<"roster">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.queues">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.queues">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.queues">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.queues">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.queues">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.queues">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
-spec allowed_methods/3 :: (path_token(), path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID, ?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID, ?ROSTER_PATH_TOKEN) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(_QID, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /queues => []
%%    /queues/foo => [<<"foo">>]
%%    /queues/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
-spec resource_exists/3 :: (path_token(), path_token(), path_token()) -> 'true'.
resource_exists() -> true.

resource_exists(_) -> true.

resource_exists(?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) -> true;
resource_exists(_, ?STATS_PATH_TOKEN) -> true;
resource_exists(_, ?ROSTER_PATH_TOKEN) -> true.

resource_exists(_, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /queues mights load a list of queue objects
%% /queues/123 might load the queue object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_queue_stats(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    fetch_all_queue_stats(Context, realtime);
validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?STATS_PATH_TOKEN) ->
    fetch_queue_stats(Id, Context);
validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    load_agent_roster(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    add_queue_to_agents(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    rm_queue_from_agents(Id, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    fetch_queue_stats(Id, Context, realtime).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    lager:debug("saving new queue"),
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    read(Id, crossbar_doc:save(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _) ->
    crossbar_doc:delete(Context).
delete(#cb_context{}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    read(Id, crossbar_doc:save(Context)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"queues">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    case crossbar_doc:load(Id, Context) of
        #cb_context{resp_status=success}=Context1 ->
            load_queue_agents(Id, Context1);
        Context1 -> Context1
    end.

load_queue_agents(Id, #cb_context{resp_data=Queue}=Context) ->
    case load_agent_roster(Id, Context) of
        #cb_context{resp_status=success, resp_data=Agents, doc=_D} ->
            Context#cb_context{resp_data=wh_json:set_value(<<"agents">>, Agents, Queue)};
        _ -> Context
    end.

load_agent_roster(Id, Context) ->
    crossbar_doc:load_view(?CB_AGENTS_LIST, [{key, Id}]
                           ,Context
                           ,fun normalize_agents_results/2
                          ).

add_queue_to_agents(Id, #cb_context{req_data=[]}=Context) ->
    lager:debug("no agents listed, removing all agents from ~s", [Id]),
    #cb_context{resp_data=CurrAgentIds} = load_agent_roster(Id, Context),
    rm_queue_from_agents(Id, Context#cb_context{req_data=CurrAgentIds});

add_queue_to_agents(Id, #cb_context{req_data=[_|_]=AgentIds}=Context) ->
    %% We need to figure out what agents are on the queue already, and remove those not
    %% in the AgentIds list
    #cb_context{resp_data=CurrAgentIds} = load_agent_roster(Id, Context),

    {InQueueAgents, RmAgentIds} = lists:partition(fun(A) -> lists:member(A, AgentIds) end, CurrAgentIds),
    AddAgentIds = [A || A <- AgentIds, (not lists:member(A, InQueueAgents))],

    _P = spawn(fun() ->
                       _ = cb_context:put_reqid(Context),
                       maybe_rm_agents(Id, Context, RmAgentIds)
               end),

    add_queue_to_agents(Id, Context, AddAgentIds).

add_queue_to_agents(_Id, Context, []) ->
    Context#cb_context{resp_status=success, doc=[]};
add_queue_to_agents(Id, Context, AgentIds) ->
    case crossbar_doc:load(AgentIds, Context) of
        #cb_context{resp_status=success
                    ,doc=Agents
                   }=Context1 ->
            lager:debug("fetched agents: ~p", [Agents]),
            Context1#cb_context{doc=[maybe_add_queue_to_agent(Id, A) || A <- Agents]};
        Context1 -> Context1
    end.

maybe_add_queue_to_agent(Id, A) ->
    Qs = case wh_json:get_value(<<"queues">>, A) of
             L when is_list(L) ->
                 case lists:member(Id, L) of
                     true -> L;
                     false -> [Id | L]
                 end;
             _ -> [Id]
         end,
    lager:debug("agent ~s queues: ~p", [wh_json:get_value(<<"_id">>, A), Qs]),
    wh_json:set_value(<<"queues">>, Qs, A).

-spec maybe_rm_agents/3 :: (ne_binary(), #cb_context{}, wh_json:json_strings()) -> #cb_context{}.
maybe_rm_agents(_Id, Context, []) ->
    lager:debug("no agents to remove from the queue ~s", [_Id]),
    Context#cb_context{resp_status=success};
maybe_rm_agents(Id, Context, AgentIds) ->
    #cb_context{}=RMContext = rm_queue_from_agents(Id, Context#cb_context{req_data=AgentIds}),
    #cb_context{resp_status=_S1}=RMContext1 = crossbar_doc:save(RMContext),
    lager:debug("rm resulted in ~s", [_S1]),
    RMContext1.

rm_queue_from_agents(_Id, #cb_context{req_data=[]}=Context) ->
    Context;
rm_queue_from_agents(Id, #cb_context{req_data=[_|_]=AgentIds}=Context) ->
    lager:debug("remove agents: ~p", [AgentIds]),
    case crossbar_doc:load(AgentIds, Context) of
        #cb_context{resp_status=success
                    ,doc=Agents
                   }=Context1 ->
            lager:debug("removed agents successfully"),
            Context1#cb_context{doc=[maybe_rm_queue_from_agent(Id, A) || A <- Agents]};
        Context1 -> Context1
    end;
rm_queue_from_agents(_, #cb_context{req_data=_Req}=Context) ->
    Context#cb_context{resp_status=success, doc=undefined}.

maybe_rm_queue_from_agent(Id, A) ->
    Qs = wh_json:get_value(<<"queues">>, A, []),
    wh_json:set_value(<<"queues">>, lists:delete(Id, Qs), A).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"queues">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            crossbar_doc:load_merge(Id, JObj1, Context)
    end.

fetch_all_queue_stats(Context) ->
    fetch_all_queue_stats(Context, history).
fetch_all_queue_stats(Context, history) ->
    {Today, _} = calendar:universal_time(),
    From = calendar:datetime_to_gregorian_seconds({Today, {0,0,0}}),

    crossbar_doc:load_view(<<"acdc_stats/stats_per_queue_by_time">>
                               ,[{startkey, [wh_util:current_tstamp(), <<"\ufff0">>]}
                                 ,{endkey, [From, <<>>]}
                                 ,descending
                                ]
                           ,Context
                           ,fun normalize_queue_results/2
                          );
fetch_all_queue_stats(#cb_context{account_id=AcctId}=Context, realtime) ->
    Req = [{<<"Account-ID">>, AcctId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_queue:publish_stats_req/1
                                       ,fun wapi_acdc_queue:stats_resp_v/1
                                       ,2000
                                      ) of
        {ok, Resp} ->
            lager:debug("fetched stats successfully"),
            Resp1 = strip_api_fields(wh_json:normalize(Resp)),
            Context#cb_context{resp_status=success
                               ,resp_data=total_up_stats(Resp1)
                               ,doc=Resp1
                              };
        {error, _E} ->
            lager:debug("failed to fetch stats: ~p", [_E]),
            Context
    end.

total_up_stats(Stats) ->
    QueuesJObj = wh_json:get_value(<<"current_stats">>, Stats, wh_json:new()),
    {TotalCalls, TotalWait, QueuesJObj1} = wh_json:foldl(fun total_up_stats_for_queue/3
                                                         ,{0, 0, QueuesJObj}
                                                         ,QueuesJObj
                                                        ),
    wh_json:set_values([{<<"current_stats">>, QueuesJObj1}
                        ,{<<"calls_this_hour">>, TotalCalls}
                        ,{<<"avg_wait_time_this_hour">>, avg_wait(TotalWait, TotalCalls)}
                       ], Stats).

total_up_stats_for_queue(QueueId, QueueStats, {TotCalls, TotWait, ByQueue}) ->
    Calls = wh_json:get_value(<<"calls">>, QueueStats, wh_json:new()),
    {Wait, L} = sum_and_count_wait_time(Calls),
    ByQueue1 = wh_json:set_values([{[QueueId, <<"calls_this_hour">>], L}
                                   ,{[QueueId, <<"avg_wait_time_this_hour">>], avg_wait(Wait, L)}
                                  ], ByQueue),

    {TotCalls + L, TotWait + Wait, ByQueue1}.

sum_and_count_wait_time(Calls) ->
    wh_json:foldl(fun(_CallId, CallData, {WaitAcc, Tot}) ->
                          case wh_json:get_integer_value(<<"wait_time">>, CallData) of
                              undefined -> find_wait_time(CallData, WaitAcc, Tot);
                              N -> {N + WaitAcc, Tot+1}
                          end
                  end, {0, 0}, Calls).

find_wait_time(CallData, WaitAcc, Tot) ->
    case wh_json:get_integer_value(<<"entered">>, CallData) of
        undefined -> {WaitAcc, Tot};
        EnteredTStamp ->
            try wh_json:get_integer_value(<<"timestamp">>, CallData) - EnteredTStamp of
                WaitTime -> {WaitAcc+WaitTime, Tot+1}
            catch
                error:badarith -> {WaitAcc, Tot}
            end
    end.

avg_wait(_, 0) -> 0;
avg_wait(W, C) -> W / C.

fetch_queue_stats(Id, Context) ->
    fetch_queue_stats(Id, Context, history).
fetch_queue_stats(Id, Context, history) ->
    lager:debug("fetching queue stats for ~s", [Id]),

    {Today, _} = calendar:universal_time(),
    From = calendar:datetime_to_gregorian_seconds({Today, {0,0,0}}),

    crossbar_doc:load_view(<<"acdc_stats/stats_per_queue">>
                               ,[{startkey, [Id, wh_util:current_tstamp()]}
                                 ,{endkey, [Id, From]}
                                 ,descending
                                ]
                           ,Context
                           ,fun normalize_queue_results/2
                          );
fetch_queue_stats(Id, #cb_context{account_id=AcctId}=Context, realtime) ->
    Req = [{<<"Account-ID">>, AcctId}
           ,{<<"Queue-ID">>, Id}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_queue:publish_stats_req/1
                                       ,fun wapi_acdc_queue:stats_resp_v/1
                                       ,2000
                                      ) of
        {ok, Resp} ->
            Resp1 = strip_api_fields(wh_json:normalize(Resp)),
            Context#cb_context{resp_status=success
                               ,resp_data=Resp1
                               ,doc=Resp1
                              };
        {error, _} -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_queue_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_queue_results(JObj, Acc) ->
    [begin
         [_, QID] = wh_json:get_value(<<"key">>, JObj),
         wh_json:set_value(<<"queue_id">>, QID, wh_json:get_value(<<"value">>, JObj))
     end
     | Acc].

normalize_agents_results(JObj, Acc) ->
    [wh_json:get_value(<<"id">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).

strip_api_fields(JObj) ->
    Strip = [<<"event_name">>, <<"event_category">>
                 ,<<"app_name">>, <<"app_version">>
                 ,<<"node">>, <<"msg_id">>, <<"server_id">>
            ],
    wh_json:filter(fun({K,_}) -> not lists:member(K, Strip) end, JObj).
