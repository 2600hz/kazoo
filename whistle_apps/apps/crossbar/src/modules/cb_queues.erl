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
%%% /queues/eavesdrop
%%%   PUT: ring a phone/user and eavesdrop on given call-id
%%% /queues/QID/eavesdrop
%%%   PUT: ring a phone/user and eavesdrop on the queue's calls
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
         ,put/1, put/2, put/3
         ,post/2, post/3
         ,delete/2, delete/3

         ,fold_stats/1
        ]).

-include("include/crossbar.hrl").

-define(CB_LIST, <<"queues/crossbar_listing">>).
-define(CB_AGENTS_LIST, <<"queues/agents_listing">>). %{agent_id, queue_id}

-define(STATS_PATH_TOKEN, <<"stats">>).
-define(REALTIME_PATH_TOKEN, <<"realtime">>).
-define(ROSTER_PATH_TOKEN, <<"roster">>).
-define(EAVESDROP_PATH_TOKEN, <<"eavesdrop">>).

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
allowed_methods(?EAVESDROP_PATH_TOKEN) ->
    ['PUT'];
allowed_methods(_QID) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID, ?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID, ?ROSTER_PATH_TOKEN) ->
    ['GET', 'POST', 'DELETE'];
allowed_methods(_QID, ?EAVESDROP_PATH_TOKEN) ->
    ['PUT'].

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
resource_exists(_, ?ROSTER_PATH_TOKEN) -> true;
resource_exists(_, ?EAVESDROP_PATH_TOKEN) -> true.

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
-spec validate/1 :: (cb_context:context()) -> cb_context:context().
-spec validate/2 :: (cb_context:context(), path_token()) -> cb_context:context().
-spec validate/3 :: (cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_queue_stats(Context);
validate(#cb_context{req_verb = <<"put">>}=Context, ?EAVESDROP_PATH_TOKEN) ->
    validate_eavesdrop_on_call(Context);
validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    validate_request(Id, Context);
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
    rm_queue_from_agents(Id, Context);
validate(#cb_context{req_verb = <<"put">>}=Context, Id, ?EAVESDROP_PATH_TOKEN) ->
    validate_eavesdrop_on_queue(Context, Id).

validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?STATS_PATH_TOKEN, ?REALTIME_PATH_TOKEN) ->
    fetch_queue_stats(Id, Context, realtime).

validate_eavesdrop_on_call(#cb_context{req_data=Data}=Context) ->
    case is_valid_endpoint(Context, Data)
        andalso is_valid_call(Data)
        andalso is_valid_mode(Data)
    of
        true -> Context#cb_context{resp_status=success
                                   ,resp_data=wh_json:new()
                                  };
        {error, E} -> cb_context:add_system_error(E, Context)
    end.

validate_eavesdrop_on_queue(#cb_context{req_data=Data}=Context, QueueId) ->
    case is_valid_endpoint(Context, Data)
        andalso is_valid_queue(Context, QueueId)
        andalso is_valid_mode(Data)
    of
        true ->
            Context#cb_context{resp_status=success
                               ,resp_data=wh_json:new()
                              };
        {error, E} ->
            cb_context:add_system_error(E, Context)
    end.

is_valid_mode(Data) ->
    case wapi_resource:is_valid_mode(wh_json:get_value(<<"mode">>, Data, <<"listen">>)) of
        true -> true;
        false -> {error, faulty_request}
    end.

is_valid_call(Data) ->
    case wh_json:get_value(<<"call_id">>, Data) of
        undefined -> {error, bad_identifier};
        CallId ->
            case whapps_call_command:b_call_status(CallId) of
                {error, _} -> {error, bad_identifier};
                {ok, _} -> true
            end
    end.

is_valid_queue(Context, QueueId) ->
    AcctDb = cb_context:account_db(Context),
    case couch_mgr:open_cache_doc(AcctDb, QueueId) of
        {ok, QueueJObj} -> is_valid_queue(QueueJObj);
        {error, _} -> {error, bad_identifier}
    end.
is_valid_queue(QueueJObj) ->
    case wh_json:get_value(<<"pvt_type">>, QueueJObj) of
        <<"queue">> -> true;
        _ -> {error, bad_identifier}
    end.

is_valid_endpoint(Context, DataJObj) ->
    AcctDb = cb_context:account_db(Context),
    Id = wh_json:get_value(<<"id">>, DataJObj),

    case couch_mgr:open_cache_doc(AcctDb, Id) of
        {ok, CallMeJObj} -> is_valid_endpoint(CallMeJObj);
        {error, _} -> {error, bad_identifier}
    end.
is_valid_endpoint(CallMeJObj) ->
    case wh_json:get_value(<<"pvt_type">>, CallMeJObj) of
        <<"device">> -> true;
        <<"user">> -> true;
        _ -> {error, bad_identifier}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put/1 :: (cb_context:context()) -> cb_context:context().
-spec put/2 :: (cb_context:context(), path_token()) -> cb_context:context().
-spec put/3 :: (cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    lager:debug("saving new queue"),
    crossbar_doc:save(Context).

put(#cb_context{req_data=Data}=Context, ?EAVESDROP_PATH_TOKEN) ->
    Prop = [{<<"Eavesdrop-Call-ID">>, wh_json:get_value(<<"call_id">>, Data)}
            | default_eavesdrop_req(Context)
           ],
    eavesdrop_req(Context, Prop).
put(Context, QID, ?EAVESDROP_PATH_TOKEN) ->
    Prop = [{<<"Eavesdrop-Group-ID">>, QID}
            | default_eavesdrop_req(Context)
           ],
    eavesdrop_req(Context, Prop).

default_eavesdrop_req(#cb_context{req_data=Data}=Context) ->
    [{<<"Eavesdrop-Mode">>, wh_json:get_value(<<"mode">>, Data, <<"listen">>)}
     ,{<<"Account-ID">>, cb_context:account_id(Context)}
     ,{<<"Endpoint-ID">>, wh_json:get_value(<<"id">>, Data)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

eavesdrop_req(Context, Prop) ->
    case whapps_util:amqp_pool_request(Prop
                                       ,fun wapi_resource:publish_eavesdrop_req/1
                                       ,fun wapi_resource:eavesdrop_resp_v/1
                                       ,2000
                                      )
    of
        {ok, Resp} -> Context#cb_context{resp_status=success, resp_data=Resp};
        {error, timeout} -> Context#cb_context{resp_status=error, resp_data = <<"request timed out">>};
        {error, E} -> Context#cb_context{resp_status=error, resp_data=E}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (cb_context:context(), path_token()) -> cb_context:context().
-spec post/3 :: (cb_context:context(), path_token(), path_token()) -> cb_context:context().
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
-spec delete/2 :: (cb_context:context(), path_token()) -> cb_context:context().
-spec delete/3 :: (cb_context:context(), path_token(), path_token()) -> cb_context:context().
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
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    case crossbar_doc:load(Id, Context) of
        #cb_context{resp_status=success}=Context1 ->
            load_queue_agents(Id, Context1);
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_request/2 :: (api_binary(), cb_context:context()) -> cb_context:context().
validate_request(QueueId, Context) ->
    check_queue_schema(QueueId, Context).

check_queue_schema(QueueId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(QueueId, C) end,
    cb_context:validate_request_data(<<"queues">>, Context, OnSuccess).

on_successful_validation(undefined, #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"queue">>}],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
on_successful_validation(QueueId, #cb_context{}=Context) -> 
    crossbar_doc:load_merge(QueueId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
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

-spec maybe_rm_agents/3 :: (ne_binary(), cb_context:context(), wh_json:json_strings()) -> cb_context:context().
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
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_all_queue_stats/1 :: (cb_context:context()) -> cb_context:context().
-spec fetch_all_queue_stats/2 :: (cb_context:context(), 'history' | 'realtime') -> cb_context:context().

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
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_acdc_queue:publish_stats_req/1
                                       ,1000
                                      ) of
        {ok, Resps0} ->
            case [strip_api_fields(wh_json:normalize(R)) || R <- Resps0, wapi_acdc_queue:stats_resp_v(R)] of
                [] ->
                    Context#cb_context{resp_status=success
                                       ,resp_data=default_stats()
                                       ,doc=default_stats()
                                      };
                Resps1 ->
                    Resp = fold_stats(Resps1),
                    Totaled = total_up_stats(Resp),

                    lager:debug("acdc queue stats: ~p", [Totaled]),

                    Context#cb_context{resp_status=success
                                       ,resp_data=Totaled
                                       ,doc=Resp
                                      }
            end;
        {error, _E} ->
            lager:debug("failed to fetch stats: ~p", [_E]),
            Context
    end.

default_stats() ->
    wh_json:from_list([{<<"current_stats">>, wh_json:new()}
                       ,{<<"current_calls">>, wh_json:new()}
                      ]).

fold_stats([]) -> default_stats();
fold_stats([R|Rs]) ->
    fold_stats(Rs, R).

fold_stats([R|Rs], Resp) ->
    fold_stats(Rs, lists:foldl(fun(K, Acc) -> fold_stat(R, Acc, K) end, Resp, wh_json:get_keys(R)));
fold_stats([], Resp) -> Resp.

fold_stat(R, Resp, <<"calls_this_hour">> = K) ->
    CTH = wh_json:get_integer_value(K, R, 0) + wh_json:get_integer_value(K, Resp, 0),
    wh_json:set_value(K, CTH, Resp);

fold_stat(R, Resp, <<"current_stats">> = K) ->
    wh_json:foldl(fun(QID, QV, Acc) ->
                          fold_queue(K, QID, QV, Acc)
                  end, Resp, wh_json:get_value(K, R, wh_json:new()));

fold_stat(R, Resp, <<"current_calls">> = K) ->
    wh_json:foldl(fun(CallK, CallV, Acc) ->
                          wh_json:set_value([K, CallK], CallV, Acc)
                  end, Resp, wh_json:get_value(K, R, wh_json:new()));

fold_stat(R, Resp, <<"current_statuses">> = K) ->
    Statuses = wh_json:get_value(K, R),
    wh_json:foldl(fun(StatusK, StatusV, Acc) ->
                          wh_json:set_value([K, StatusK], StatusV, Acc)
                  end, Resp, Statuses);
fold_stat(R, Resp, K) ->
    wh_json:set_value(K, wh_json:get_value(K, R), Resp).

-spec fold_queue/4 :: (wh_json:json_key(), ne_binary(), wh_json:json_object(), wh_json:json_object()) ->
                              wh_json:json_object().
fold_queue(K, QID, QV, Resp) ->
    wh_json:foldl(fun(<<"calls">> = CallK, CallV, Acc) ->
                          Key = [K, QID, CallK],
                          AccCallV = wh_json:get_value(Key, Acc, wh_json:new()),

                          wh_json:set_value(Key, wh_json:merge_recursive(CallV, AccCallV), Acc);
                     (QKey, QVal, Acc) ->
                          wh_json:set_value([K, QID, QKey], QVal, Acc)
                  end, Resp, QV).

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
avg_wait(W, C) -> ((W * 100) div C) / 100.

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
-spec summary/1 :: (cb_context:context()) -> cb_context:context().
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
%%
%% @end
%%--------------------------------------------------------------------
strip_api_fields(JObj) ->
    Strip = [<<"event_name">>, <<"event_category">>
                 ,<<"app_name">>, <<"app_version">>
                 ,<<"node">>, <<"msg_id">>, <<"server_id">>
            ],
    wh_json:filter(fun({K,_}) -> not lists:member(K, Strip) end, JObj).

%% {[{<<"current_stats">>
%%        ,{[{<<"55048d29e7fb29061f8c5ef0ae2dbbb9">>
%%                ,{[{<<"calls">>
%%                        ,{[{<<"76714966">>
%%                                ,{[{<<"wait_time">>
%%                                        ,7}
%%                                   ,{<<"timestamp">>
%%                                         ,63517892418}]}}
%%                           ,{<<"1619546012">>
%%                                 ,{[{<<"abandoned">>
%%                                         ,<<"member_hangup">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892617}]}}
%%                           ,{<<"948285573">>
%%                                 ,{[{<<"abandoned">>
%%                                         ,<<"member_hangup">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892633}]}}
%%                           ,{<<"1847732656">>
%%                                 ,{[{<<"wait_time">>
%%                                         ,8}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892814}
%%                                    ,{<<"duration">>
%%                                          ,169}
%%                                    ,{<<"agent_id">>
%%                                          ,<<"934a5d7bfc8a097d535297caab004b0e">>}]}}
%%                           ,{<<"2108908609">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892912}
%%                                    ,{<<"duration">>
%%                                          ,163}
%%                                    ,{<<"agent_id">>
%%                                          ,<<"934a5d7bfc8a097d535297caab004b0e">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517893079}]}}]}}]}}]}}
%%   ,{<<"account_id">>
%%         ,<<"934a5d7bfc8a097d535297caab003839">>}]}

%% {[{<<"current_stats">>
%%        ,{[{<<"55048d29e7fb29061f8c5ef0ae2dbbb9">>
%%                ,{[{<<"calls">>
%%                        ,{[{<<"76714966">>
%%                                ,{[{<<"duration">>
%%                                        ,6}
%%                                   ,{<<"agent_id">>
%%                                         ,<<"934a5d7bfc8a097d535297caab004b0e">>}
%%                                   ,{<<"timestamp">>
%%                                         ,63517892420}]}}
%%                           ,{<<"1619546012">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892612}
%%                                    ,{<<"abandoned">>
%%                                          ,<<"member_hangup">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892617}]}}
%%                           ,{<<"948285573">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892628}
%%                                    ,{<<"abandoned">>
%%                                          ,<<"member_hangup">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892633}]}}
%%                           ,{<<"1847732656">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892641}]}}
%%                           ,{<<"2108908609">>
%%                                 ,{[{<<"wait_time">>
%%                                         ,7}
%%                                    ,{<<"timestamp">>
%%                                          ,63517893079}
%%                                    ,{<<"duration">>
%%                                          ,163}
%%                                    ,{<<"agent_id">>
%%                                          ,<<"934a5d7bfc8a097d535297caab004b0e">>}]}}]}}]}}]}}
%%   ,{<<"account_id">>
%%         ,<<"934a5d7bfc8a097d535297caab003839">>}]}






%% {[{<<"current_stats">>
%%        ,{[{<<"55048d29e7fb29061f8c5ef0ae2dbbb9">>
%%                ,{[{<<"calls">>
%%                        ,{[{<<"76714966">>
%%                                ,{[{<<"duration">>
%%                                        ,6}
%%                                   ,{<<"agent_id">>
%%                                         ,<<"934a5d7bfc8a097d535297caab004b0e">>}
%%                                   ,{<<"timestamp">>
%%                                         ,63517892420}]}}
%%                           ,{<<"1619546012">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892612}
%%                                    ,{<<"abandoned">>
%%                                          ,<<"member_hangup">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892617}]}}
%%                           ,{<<"948285573">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892628}
%%                                    ,{<<"abandoned">>
%%                                          ,<<"member_hangup">>}
%%                                    ,{<<"timestamp">>
%%                                          ,63517892633}]}}
%%                           ,{<<"1847732656">>
%%                                 ,{[{<<"entered">>
%%                                         ,63517892641}]}}
%%                           ,{<<"2108908609">>
%%                                 ,{[{<<"wait_time">>
%%                                         ,7}
%%                                    ,{<<"timestamp">>
%%                                          ,63517893079}
%%                                    ,{<<"duration">>
%%                                          ,163}
%%                                    ,{<<"agent_id">>
%%                                          ,<<"934a5d7bfc8a097d535297caab004b0e">>}]}}]}}
%%                   ,{<<"avg_wait_time_this_hour">>
%%                         ,5.666666666666667}
%%                   ,{<<"calls_this_hour">>
%%                         ,3}]}}]}}
%%   ,{<<"account_id">>
%%         ,<<"934a5d7bfc8a097d535297caab003839">>}
%%   ,{<<"avg_wait_time_this_hour">>
%%         ,5.666666666666667}
%%   ,{<<"calls_this_hour">>
%%         ,3}]}
