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
%%%   GET: retrieve stats across all queues for the last hour
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2, put/3
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".queues">>).

-define(CB_LIST, <<"queues/crossbar_listing">>).
-define(CB_AGENTS_LIST, <<"queues/agents_listing">>). %{agent_id, queue_id}

-define(STATS_PATH_TOKEN, <<"stats">>).
-define(ROSTER_PATH_TOKEN, <<"roster">>).
-define(EAVESDROP_PATH_TOKEN, <<"eavesdrop">>).

-define(STAT_TIMESTAMP_PROCESSED, <<"finished_with_agent">>).
-define(STAT_TIMESTAMP_HANDLING, <<"connected_with_agent">>).
-define(STAT_TIMESTAMP_ABANDONED, <<"caller_abandoned_queue">>).
-define(STAT_TIMESTAMP_WAITING, <<"caller_entered_queue">>).
-define(STAT_AGENTS_MISSED, <<"missed">>).

-define(STAT_TIMESTAMP_KEYS, [?STAT_TIMESTAMP_PROCESSED
                              ,?STAT_TIMESTAMP_HANDLING
                              ,?STAT_TIMESTAMP_ABANDONED
                              ,?STAT_TIMESTAMP_WAITING
                             ]).

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

allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(?EAVESDROP_PATH_TOKEN) ->
    ['PUT'];
allowed_methods(_QID) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(_QID, ?ROSTER_PATH_TOKEN) ->
    ['GET', 'POST', 'DELETE'];
allowed_methods(_QID, ?EAVESDROP_PATH_TOKEN) ->
    ['PUT'].

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
resource_exists() -> true.

resource_exists(_) -> true.

resource_exists(_, ?ROSTER_PATH_TOKEN) -> true;
resource_exists(_, ?EAVESDROP_PATH_TOKEN) -> true.

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

validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    load_agent_roster(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    add_queue_to_agents(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    rm_queue_from_agents(Id, Context);
validate(#cb_context{req_verb = <<"put">>}=Context, Id, ?EAVESDROP_PATH_TOKEN) ->
    validate_eavesdrop_on_queue(Context, Id).

validate_eavesdrop_on_call(#cb_context{req_data=Data}=Context) ->
    Fs = [{fun is_valid_endpoint/2, [Context, Data]}
          ,{fun is_valid_call/2, [Context, Data]}
          ,{fun is_valid_mode/2, [Context, Data]}
         ],
    case all_true(Fs) of
        true -> Context#cb_context{resp_status=success};
        {false, Context1} -> Context1
    end.

validate_eavesdrop_on_queue(#cb_context{req_data=Data}=Context, QueueId) ->
    Fs = [{fun is_valid_endpoint/2, [Context, Data]}
          ,{fun is_valid_queue/2, [Context, QueueId]}
          ,{fun is_valid_mode/2, [Context, Data]}
         ],
    case all_true(Fs) of
        true ->
            Context#cb_context{resp_status=success};
        {false, Context1} -> Context1
    end.

-spec all_true/1 :: ([{fun(), list()},...]) ->
                            'true' |
                            {'false', cb_context:context()}.
all_true(Fs) ->
    lists:foldl(fun({F, Args}, true) -> apply(F, Args);
                   (_, Acc) -> Acc
                end, true, Fs).

is_valid_mode(Context, Data) ->
    case wapi_resource:is_valid_mode(wh_json:get_value(<<"mode">>, Data, <<"listen">>)) of
        true -> true;
        false -> {false
                  ,cb_context:add_validation_error(<<"mode">>, <<"enum">>
                                                   ,<<"enum:Value not found in enumerated list of values">>
                                                   ,Context
                                                  )
                 }
    end.

is_valid_call(Context, Data) ->
    case wh_json:get_value(<<"call_id">>, Data) of
        undefined ->
            {false
             ,cb_context:add_validation_error(<<"call_id">>, <<"required">>
                                              ,<<"required:Field is required but missing">>
                                              ,Context
                                             )
            };
        CallId ->
            case whapps_call_command:b_call_status(CallId) of
                {error, _E} ->
                    lager:debug("is not valid call: ~p", [_E]),
                    {false
                     ,cb_context:add_validation_error(<<"call_id">>, <<"not_found">>
                                                      ,<<"not_found:Call was not found">>
                                                      ,Context
                                                     )
                    };
                {ok, _} -> true
            end
    end.

is_valid_queue(Context, ?NE_BINARY = QueueId) ->
    AcctDb = cb_context:account_db(Context),
    case couch_mgr:open_cache_doc(AcctDb, QueueId) of
        {ok, QueueJObj} -> is_valid_queue(Context, QueueJObj);
        {error, _} ->
            {false
             ,cb_context:add_validation_error(<<"queue_id">>, <<"not_found">>
                                              ,<<"not_found:Queue was not found">>
                                              ,Context
                                             )
            }
    end;
is_valid_queue(Context, QueueJObj) ->
    case wh_json:get_value(<<"pvt_type">>, QueueJObj) of
        <<"queue">> -> true;
        _ ->
            {false
             ,cb_context:add_validation_error(<<"queue_id">>, <<"type">>
                                                  ,<<"type:Id did not represent a queue">>
                                              ,Context
                                             )
            }
    end.

is_valid_endpoint(Context, DataJObj) ->
    AcctDb = cb_context:account_db(Context),
    Id = wh_json:get_value(<<"id">>, DataJObj),

    case couch_mgr:open_cache_doc(AcctDb, Id) of
        {ok, CallMeJObj} -> is_valid_endpoint_type(Context, CallMeJObj);
        {error, _} ->
            {false
             ,cb_context:add_validation_error(<<"id">>, <<"not_found">>
                                              ,<<"not_found:Id was not found">>
                                              ,Context
                                             )
            }
    end.
is_valid_endpoint_type(Context, CallMeJObj) ->
    case wh_json:get_value(<<"pvt_type">>, CallMeJObj) of
        <<"device">> -> true;
        %%<<"user">> -> true;
        _ ->
            {false
             ,cb_context:add_validation_error(<<"id">>, <<"type">>
                                              ,<<"type:Id did not represent a valid endpoint">>
                                              ,Context
                                             )
            }
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

-spec default_eavesdrop_req/1 :: (cb_context:context()) -> wh_proplist().
default_eavesdrop_req(#cb_context{req_data=Data}=Context) ->
    [{<<"Eavesdrop-Mode">>, wh_json:get_value(<<"mode">>, Data, <<"listen">>)}
     ,{<<"Account-ID">>, cb_context:account_id(Context)}
     ,{<<"Endpoint-ID">>, wh_json:get_value(<<"id">>, Data)}
     ,{<<"Endpoint-Timeout">>, wh_json:get_integer_value(<<"timeout">>, Data, 20)}
     ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"caller_id_name">>, Data)}
     ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"caller_id_number">>, Data)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec eavesdrop_req/2 :: (cb_context:context(), wh_proplist()) -> cb_context:context().
eavesdrop_req(Context, Prop) ->
    case whapps_util:amqp_pool_request(props:filter_undefined(Prop)
                                       ,fun wapi_resource:publish_eavesdrop_req/1
                                       ,fun wapi_resource:eavesdrop_resp_v/1
                                       ,2000
                                      )
    of
        {ok, Resp} -> crossbar_util:response(filter_response_fields(Resp), Context);
        {error, timeout} ->
            cb_context:add_system_error(timeout
                                        ,[{details, <<"eavesdrop failed to start">>}]
                                        ,Context
                                       );
        {error, E} -> crossbar_util:response(error, <<"error">>, 500, E, Context)
    end.

-define(REMOVE_FIELDS, [<<"Server-ID">>
                        ,<<"Node">>
                        ,<<"Msg-ID">>
                        ,<<"App-Version">>
                        ,<<"App-Name">>
                        ,<<"Event-Name">>
                        ,<<"Event-Category">>
                       ]).
filter_response_fields(JObj) ->
    wh_json:set_value(<<"eavesdrop_request_id">>, wh_json:get_value(<<"Msg-ID">>, JObj)
                      ,wh_json:normalize(wh_json:delete_keys(?REMOVE_FIELDS, JObj))
                     ).

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
fetch_all_queue_stats(Context) ->
    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, 3600),

    To = wh_util:to_integer(cb_context:req_value(Context, <<"end_range">>, wh_util:current_tstamp())),
    MaxFrom = To - MaxRange,

    From = case wh_util:to_integer(cb_context:req_value(Context, <<"start_range">>, MaxFrom)) of
               F when F > To -> MaxFrom; % created_from comes after created_to
               F when F < MaxFrom -> MaxFrom; % range is too large
               F -> F
           end,

    AcctId = cb_context:account_id(Context),
    Opts = [{startkey, [To, AcctId]}
            ,{endkey, [From, AcctId]}
            ,include_docs
            ,descending
           ],

    case cb_context:req_value(Context, <<"format">>, <<"compressed">>) of
        <<"compressed">> ->
            Context1 = crossbar_doc:load_view(<<"call_stats/call_log">>
                                              ,Opts
                                              ,cb_context:set_account_db(Context, acdc_stats:db_name(AcctId))
                                              ,fun extract_doc/2
                                             ),
            compress_stats(Context1, From, To);
        <<"verbose">> ->
            crossbar_doc:load_view(<<"call_stats/call_log">>
                                   ,Opts
                                   ,cb_context:set_account_db(Context, acdc_stats:db_name(AcctId))
                                   ,fun normalize_queue_stats/2
                                  );
        _Format ->
            lager:debug("unrecognized stats format: ~s", [_Format]),
            cb_context:add_validation_error(<<"format">>, <<"enum">>, <<"enum:Value not found in enumerated list">>, Context)
    end.

-spec compress_stats/3 :: (cb_context:context(), integer(), integer()) -> cb_context:context().
compress_stats(Context, From, To) ->
    case cb_context:resp_status(Context) of
        success ->
            Compressed = compress_stats(cb_context:doc(Context), {wh_json:new(), wh_json:new(), wh_json:new()}),
            crossbar_util:response(wh_json:set_values([{<<"start_range">>, From}
                                                       ,{<<"end_range">>, To}
                                                      ], Compressed)
                                   ,Context);
        _S ->
            lager:debug("failed to load stats"),
            Context
    end.

-spec compress_stats/2 :: ('undefined' | wh_json:objects(), {wh_json:object(), wh_json:object(), wh_json:object()}) ->
                                  wh_json:object().
compress_stats(undefined, {Compressed, _, _}) -> Compressed;
compress_stats([], {Compressed, Global, PerQueue}) -> accumulate_stats(Compressed, Global, PerQueue);
compress_stats([Stat|Stats], {_Compressed, _Global, _PerQueue}=Res) ->
    compress_stats(Stats, add_stat(Stat, Res)).

-spec accumulate_stats/3 :: (wh_json:object(), wh_json:object(), wh_json:object()) ->
                                    wh_json:object().
accumulate_stats(Compressed, Global, PerQueue) ->
    AccCompressed = wh_json:map(fun accumulate_queue_stats/2, Compressed),
    AccGlobal = wh_json:foldl(fun fold_call_totals/3, wh_json:new(), Global),
    AccPerQueue = wh_json:foldl(fun fold_queue_totals/3, wh_json:new(), PerQueue),
    wh_json:set_value(<<"totals">>, AccGlobal, wh_json:merge_recursive(AccPerQueue, AccCompressed)).

fold_call_totals(_CallId, Stats, Acc) ->
    AccWaitTime = wh_json:get_value(<<"wait_time">>, Acc, 0),
    AccTalkTime = wh_json:get_value(<<"call_time">>, Acc, 0),
    AccTotalCalls = wh_json:get_value(<<"total_calls">>, Acc, 0),
    AccAbandonedCalls = wh_json:get_value(<<"abandoned_calls">>, Acc, 0),
    AccAgentsMissed = wh_json:get_value(<<"agents_missed">>, Acc, 0),

    Abandoned = case wh_json:get_value(?STAT_TIMESTAMP_ABANDONED, Stats) of undefined -> 0; _ -> 1 end,

    Set = [{<<"total_calls">>, AccTotalCalls + 1}
           ,{<<"agents_missed">>, AccAgentsMissed + wh_json:get_integer_value(?STAT_AGENTS_MISSED, Stats, 0)}
           ,{<<"abandoned_calls">>, AccAbandonedCalls + Abandoned}
          ],

    Set1 = case wait_time(Stats) of
               undefined -> Set;
               WaitTime ->
                   case call_time(Stats) of
                       undefined -> Set;
                       TalkTime -> [{<<"wait_time">>, AccWaitTime + WaitTime}
                                    ,{<<"call_time">>, AccTalkTime + TalkTime}
                                    | Set
                                   ]
                   end
           end,
    wh_json:set_values(Set1, Acc).

fold_queue_totals(QueueId, Calls, Acc) ->
    wh_json:set_value([QueueId, <<"totals">>], wh_json:foldl(fun fold_call_totals/3, wh_json:new(), Calls), Acc).

accumulate_queue_stats(QueueId, Calls) ->
    AccCalls = wh_json:map(fun accumulate_call_stats/2, Calls),
    {QueueId, wh_json:foldl(fun fold_calls_waiting/3, wh_json:set_value(<<"calls_waiting">>, [], AccCalls), Calls)}.
accumulate_call_stats(CallId, Stats) ->
    WaitTime = wait_time(Stats),
    TalkTime = call_time(Stats),
    {CurrentStatus, CurrentTstamp} = current_status(Stats),

    AccStats = wh_json:filter(fun({_, V}) -> V =/= undefined end
                            ,wh_json:set_values([{<<"wait_time">>, WaitTime}
                                                 ,{<<"call_time">>, TalkTime}
                                                 ,{<<"current_timestamp">>, CurrentTstamp}
                                                 ,{<<"current_status">>, CurrentStatus}
                                                ], Stats)
                             ),
    {CallId, AccStats}.

fold_calls_waiting(CallId, Stats, Acc) ->
    Waiting = wh_json:get_value(<<"calls_waiting">>, Acc, []),
    case current_status(Stats) of
        {<<"waiting">>, _} -> wh_json:set_value(<<"calls_waiting">>, [CallId | Waiting], Acc);
        _ -> Acc
    end.

wait_time(Stats) ->
    case wh_json:get_integer_value(?STAT_TIMESTAMP_WAITING, Stats) of
        undefined -> undefined;
        Entered -> wait_time(Stats, Entered)
    end.
wait_time(Stats, Entered) ->
    case wh_json:get_integer_value(?STAT_TIMESTAMP_HANDLING, Stats) of
        undefined -> wait_time_abandoned(Stats, Entered);
        Conn -> Conn - Entered
    end.
wait_time_abandoned(Stats, Entered) ->
    case wh_json:get_integer_value(?STAT_TIMESTAMP_ABANDONED, Stats) of
        undefined -> undefined;
        Abandoned -> Abandoned - Entered
    end.

call_time(Stats) ->
    case wh_json:get_integer_value(?STAT_TIMESTAMP_HANDLING, Stats) of
        undefined -> undefined;
        Conn -> call_time(Stats, Conn)
    end.
call_time(Stats, Conn) ->
    case wh_json:get_value(?STAT_TIMESTAMP_PROCESSED, Stats) of
        undefined -> undefined;
        Finished -> Finished - Conn
    end.

current_status(Stats) ->
    current_status(Stats, ?STAT_TIMESTAMP_KEYS).
current_status(_Stats, []) -> {undefined, undefined};
current_status(Stats, [K|Ks]) ->
    case wh_json:get_value(K, Stats) of
        undefined -> current_status(Stats, Ks);
        T -> {status(K), T}
    end.

status(?STAT_TIMESTAMP_PROCESSED) -> <<"processed">>;
status(?STAT_TIMESTAMP_HANDLING) -> <<"handling">>;
status(?STAT_TIMESTAMP_ABANDONED) -> <<"abandoned">>;
status(?STAT_TIMESTAMP_WAITING) -> <<"waiting">>.

add_stat(Stat, {_Compressed, _Global, _PerQueue}=Res) ->
    add_stat(Stat, Res, wh_json:get_value(<<"status">>, Stat)).

add_stat(Stat, {Compressed, Global, PerQueue}, <<"waiting">>) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    {wh_json:set_values([{[QID, CID, ?STAT_TIMESTAMP_WAITING], TStamp}
                        ,{[QID, CID, <<"start_timestamp">>], TStamp}
                        ], Compressed)
     ,wh_json:set_value([CID, ?STAT_TIMESTAMP_WAITING], TStamp, Global)
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_WAITING], TStamp, PerQueue)
    };
add_stat(Stat, {Compressed, Global, PerQueue}, <<"handling">>) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    {wh_json:set_values([{[QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp}
                         ,{[QID, CID, <<"agent_id">>], AID}
                        ], Compressed)
     ,wh_json:set_value([CID, ?STAT_TIMESTAMP_HANDLING], TStamp, Global)
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_HANDLING], TStamp, PerQueue)
    };
add_stat(Stat, {Compressed, Global, PerQueue}, <<"processed">>) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    {wh_json:set_values([{[QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp}
                         ,{[QID, CID, <<"agent_id">>], AID}
                        ], Compressed)
     ,wh_json:set_value([CID, ?STAT_TIMESTAMP_PROCESSED], TStamp, Global)
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_PROCESSED], TStamp, PerQueue)
    };
add_stat(Stat, {Compressed, Global, PerQueue}, ?STAT_AGENTS_MISSED) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),
    AID = wh_json:get_value(<<"agent_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),

    K = [QID, CID, <<"agents_tried">>],
    AgentsTried = wh_json:set_value(wh_util:to_binary(TStamp), AID, wh_json:get_value(K, Compressed, wh_json:new())),

    NumAgentsTried = length(wh_json:get_keys(AgentsTried)),

    {wh_json:set_values([{K, AgentsTried}
                        ], Compressed)
     ,case wh_json:get_value([CID, ?STAT_AGENTS_MISSED], Global, 0) of
          N when N < NumAgentsTried ->
              wh_json:set_value([CID, ?STAT_AGENTS_MISSED], NumAgentsTried, Global);
          _ -> Global
      end
     ,case wh_json:get_value([QID, CID, ?STAT_AGENTS_MISSED], PerQueue, 0) of
          N when N < NumAgentsTried ->
              wh_json:set_value([QID, CID, ?STAT_AGENTS_MISSED], NumAgentsTried, PerQueue);
          _ -> PerQueue
      end
    };
add_stat(Stat, {Compressed, Global, PerQueue}, <<"abandoned">>) ->
    QID = wh_json:get_value(<<"queue_id">>, Stat),
    CID = wh_json:get_value(<<"call_id">>, Stat),

    TStamp = wh_json:get_value(<<"timestamp">>, Stat),
    Reason = wh_json:get_value(<<"abandon_reason">>, Stat),

    {wh_json:set_values([{[QID, CID, ?STAT_TIMESTAMP_ABANDONED], TStamp}
                         ,{[QID, CID, <<"abandon_reason">>], Reason}
                        ], Compressed)
     ,wh_json:set_value([CID, ?STAT_TIMESTAMP_ABANDONED], TStamp, Global)
     ,wh_json:set_value([QID, CID, ?STAT_TIMESTAMP_ABANDONED], TStamp, PerQueue)
    }.

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
-spec normalize_view_results/2 :: (wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

normalize_queue_stats(JObj, Acc) ->
    [wh_doc:public_fields(wh_json:get_value(<<"doc">>, JObj)) | Acc].

normalize_agents_results(JObj, Acc) ->
    [wh_json:get_value(<<"id">>, JObj) | Acc].

extract_doc(JObj, Acc) ->
    [wh_json:get_value(<<"doc">>, JObj) | Acc].
