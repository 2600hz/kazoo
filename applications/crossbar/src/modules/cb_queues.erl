%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
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
         ,content_types_provided/1, content_types_provided/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2, put/3
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("../crossbar.hrl").

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

-define(STATUS_PROCESSED, <<"processed">>).
-define(STATUS_HANDLING, <<"handling">>).
-define(STATUS_ABANDONED, <<"abandoned">>).
-define(STATUS_WAITING, <<"waiting">>).

-define(STAT_TIMESTAMP_KEYS, [?STAT_TIMESTAMP_PROCESSED
                              ,?STAT_TIMESTAMP_HANDLING
                              ,?STAT_TIMESTAMP_ABANDONED
                              ,?STAT_TIMESTAMP_WAITING
                             ]).

-define(FORMAT_COMPRESSED, <<"compressed">>).
-define(FORMAT_VERBOSE, <<"verbose">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.queues">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.queues">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.queues">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.queues">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.queues">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.queues">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.queues">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(?STATS_PATH_TOKEN) ->
    [?HTTP_GET];
allowed_methods(?EAVESDROP_PATH_TOKEN) ->
    [?HTTP_PUT];
allowed_methods(_QID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_QID, ?ROSTER_PATH_TOKEN) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE];
allowed_methods(_QID, ?EAVESDROP_PATH_TOKEN) ->
    [?HTTP_PUT].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /queues => []
%%    /queues/foo => [<<"foo">>]
%%    /queues/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.

resource_exists(_) -> 'true'.

resource_exists(_, ?ROSTER_PATH_TOKEN) -> 'true';
resource_exists(_, ?EAVESDROP_PATH_TOKEN) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{}=Context) -> Context.
content_types_provided(#cb_context{}=Context, ?STATS_PATH_TOKEN) ->
    CTPs = [{'to_json', ?JSON_CONTENT_TYPES}
            ,{'to_csv', ?CSV_CONTENT_TYPES}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

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
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request('undefined', Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_queue_stats(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, ?EAVESDROP_PATH_TOKEN) ->
    validate_eavesdrop_on_call(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id) ->
    validate_request(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    load_agent_roster(Id, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    add_queue_to_agents(Id, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    rm_queue_from_agents(Id, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context, Id, ?EAVESDROP_PATH_TOKEN) ->
    validate_eavesdrop_on_queue(Context, Id).

validate_eavesdrop_on_call(#cb_context{req_data=Data}=Context) ->
    Fs = [{fun is_valid_endpoint/2, [Context, Data]}
          ,{fun is_valid_call/2, [Context, Data]}
          ,{fun is_valid_mode/2, [Context, Data]}
         ],
    case all_true(Fs) of
        'true' -> Context#cb_context{resp_status='success'};
        {'false', Context1} -> Context1
    end.

validate_eavesdrop_on_queue(#cb_context{req_data=Data}=Context, QueueId) ->
    Fs = [{fun is_valid_endpoint/2, [Context, Data]}
          ,{fun is_valid_queue/2, [Context, QueueId]}
          ,{fun is_valid_mode/2, [Context, Data]}
         ],
    case all_true(Fs) of
        'true' ->
            Context#cb_context{resp_status='success'};
        {'false', Context1} -> Context1
    end.

-spec all_true([{fun(), list()},...]) ->
                            'true' |
                            {'false', cb_context:context()}.
all_true(Fs) ->
    lists:foldl(fun({F, Args}, 'true') -> apply(F, Args);
                   (_, Acc) -> Acc
                end, 'true', Fs).

is_valid_mode(Context, Data) ->
    case wapi_resource:is_valid_mode(wh_json:get_value(<<"mode">>, Data, <<"listen">>)) of
        'true' -> 'true';
        'false' -> {'false'
                    ,cb_context:add_validation_error(<<"mode">>, <<"enum">>
                                                     ,<<"enum:Value not found in enumerated list of values">>
                                                     ,Context
                                                    )
                   }
    end.

is_valid_call(Context, Data) ->
    case wh_json:get_binary_value(<<"call_id">>, Data) of
        'undefined' ->
            {'false'
             ,cb_context:add_validation_error(<<"call_id">>, <<"required">>
                                              ,<<"required:Field is required but missing">>
                                              ,Context
                                             )
            };
        CallId ->
            case whapps_call_command:b_call_status(CallId) of
                {'error', _E} ->
                    lager:debug("is not valid call: ~p", [_E]),
                    {'false'
                     ,cb_context:add_validation_error(<<"call_id">>, <<"not_found">>
                                                      ,<<"not_found:Call was not found">>
                                                      ,Context
                                                     )
                    };
                {'ok', _} -> 'true'
            end
    end.

is_valid_queue(Context, ?NE_BINARY = QueueId) ->
    AcctDb = cb_context:account_db(Context),
    case couch_mgr:open_cache_doc(AcctDb, QueueId) of
        {'ok', QueueJObj} -> is_valid_queue(Context, QueueJObj);
        {'error', _} ->
            {'false'
             ,cb_context:add_validation_error(<<"queue_id">>, <<"not_found">>
                                              ,<<"not_found:Queue was not found">>
                                              ,Context
                                             )
            }
    end;
is_valid_queue(Context, QueueJObj) ->
    case wh_json:get_value(<<"pvt_type">>, QueueJObj) of
        <<"queue">> -> 'true';
        _ ->
            {'false'
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
        {'ok', CallMeJObj} -> is_valid_endpoint_type(Context, CallMeJObj);
        {'error', _} ->
            {'false'
             ,cb_context:add_validation_error(<<"id">>, <<"not_found">>
                                              ,<<"not_found:Id was not found">>
                                              ,Context
                                             )
            }
    end.
is_valid_endpoint_type(Context, CallMeJObj) ->
    case wh_json:get_value(<<"pvt_type">>, CallMeJObj) of
        <<"device">> -> 'true';
        _ ->
            {'false'
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
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context) ->
    lager:debug("saving new queue"),
    crossbar_doc:save(Context).

put(Context, ?EAVESDROP_PATH_TOKEN) ->
    Prop = [{<<"Eavesdrop-Call-ID">>, cb_context:req_value(Context, <<"call_id">>)}
            | default_eavesdrop_req(Context)
           ],
    eavesdrop_req(Context, Prop).
put(Context, QID, ?EAVESDROP_PATH_TOKEN) ->
    Prop = [{<<"Eavesdrop-Group-ID">>, QID}
            | default_eavesdrop_req(Context)
           ],
    eavesdrop_req(Context, Prop).

-spec default_eavesdrop_req(cb_context:context()) -> wh_proplist().
default_eavesdrop_req(Context) ->
    [{<<"Eavesdrop-Mode">>, cb_context:req_value(Context, <<"mode">>, <<"listen">>)}
     ,{<<"Account-ID">>, cb_context:account_id(Context)}
     ,{<<"Endpoint-ID">>, cb_context:req_value(Context, <<"id">>)}
     ,{<<"Endpoint-Timeout">>, wh_util:to_integer(cb_context:req_value(Context, <<"timeout">>, 20))}
     ,{<<"Outbound-Caller-ID-Name">>, cb_context:req_value(Context, <<"caller_id_name">>)}
     ,{<<"Outbound-Caller-ID-Number">>, cb_context:req_value(Context, <<"caller_id_number">>)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec eavesdrop_req(cb_context:context(), wh_proplist()) -> cb_context:context().
eavesdrop_req(Context, Prop) ->
    case whapps_util:amqp_pool_request(props:filter_undefined(Prop)
                                       ,fun wapi_resource:publish_eavesdrop_req/1
                                       ,fun wapi_resource:eavesdrop_resp_v/1
                                       ,2000
                                      )
    of
        {'ok', Resp} -> crossbar_util:response(filter_response_fields(Resp), Context);
        {'error', 'timeout'} ->
            cb_context:add_system_error('timeout'
                                        ,[{'details', <<"eavesdrop failed to start">>}]
                                        ,Context
                                       );
        {'error', E} -> crossbar_util:response('error', <<"error">>, 500, E, Context)
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
                      ,wh_json:normalize(wh_api:remove_defaults(JObj))
                     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
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
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
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
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    case crossbar_doc:load(Id, Context) of
        #cb_context{resp_status='success'}=Context1 ->
            load_queue_agents(Id, Context1);
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(QueueId, Context) ->
    check_queue_schema(QueueId, Context).

check_queue_schema(QueueId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(QueueId, C) end,
    cb_context:validate_request_data(<<"queues">>, Context, OnSuccess).

on_successful_validation('undefined', #cb_context{doc=Doc}=Context) ->
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
        #cb_context{resp_status='success', resp_data=Agents, doc=_D} ->
            Context#cb_context{resp_data=wh_json:set_value(<<"agents">>, Agents, Queue)};
        _ -> Context
    end.

load_agent_roster(Id, Context) ->
    crossbar_doc:load_view(?CB_AGENTS_LIST, [{'key', Id}]
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

    _ = maybe_rm_agents(Id, Context, RmAgentIds),
    add_queue_to_agents(Id, Context, AddAgentIds).

add_queue_to_agents(_Id, Context, []) ->
    Context#cb_context{resp_status='success', doc=[]};
add_queue_to_agents(Id, Context, AgentIds) ->
    case crossbar_doc:load(AgentIds, Context) of
        #cb_context{resp_status='success'
                    ,doc=Agents
                   }=Context1 ->
            Context1#cb_context{doc=[maybe_add_queue_to_agent(Id, A) || A <- Agents]};
        Context1 -> Context1
    end.

maybe_add_queue_to_agent(Id, A) ->
    Qs = case wh_json:get_value(<<"queues">>, A) of
             L when is_list(L) ->
                 case lists:member(Id, L) of
                     'true' -> L;
                     'false' -> [Id | L]
                 end;
             _ -> [Id]
         end,
    lager:debug("agent ~s queues: ~p", [wh_json:get_value(<<"_id">>, A), Qs]),
    wh_json:set_value(<<"queues">>, Qs, A).

-spec maybe_rm_agents(ne_binary(), cb_context:context(), wh_json:json_strings()) -> cb_context:context().
maybe_rm_agents(_Id, Context, []) ->
    lager:debug("no agents to remove from the queue ~s", [_Id]),
    Context#cb_context{resp_status='success'};
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
        #cb_context{resp_status='success'
                    ,doc=Agents
                   }=Context1 ->
            lager:debug("removed agents successfully"),
            Context1#cb_context{doc=[maybe_rm_queue_from_agent(Id, A) || A <- Agents]};
        Context1 -> Context1
    end;
rm_queue_from_agents(_, #cb_context{req_data=_Req}=Context) ->
    Context#cb_context{resp_status='success', doc='undefined'}.

maybe_rm_queue_from_agent(Id, A) ->
    Qs = wh_json:get_value(<<"queues">>, A, []),
    wh_json:set_value(<<"queues">>, lists:delete(Id, Qs), A).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_all_queue_stats(cb_context:context()) -> cb_context:context().
fetch_all_queue_stats(Context) ->
    case cb_context:req_value(Context, <<"start_range">>) of
        'undefined' -> fetch_all_current_queue_stats(Context);
        StartRange -> fetch_ranged_queue_stats(Context, StartRange)
    end.

-spec fetch_all_current_queue_stats(cb_context:context()) -> cb_context:context().
fetch_all_current_queue_stats(Context) ->
    lager:debug("querying for all recent stats"),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Status">>, cb_context:req_value(Context, <<"status">>)}
             ,{<<"Agent-ID">>, cb_context:req_value(Context, <<"agent_id">>)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_from_amqp(Context, Req).

format_stats(Context, Resp) ->
    Stats = wh_json:from_list([{<<"current_timestamp">>, wh_util:current_tstamp()}
                               ,{<<"stats">>,
                                 wh_doc:public_fields(
                                   wh_json:get_value(<<"Handled">>, Resp, []) ++
                                       wh_json:get_value(<<"Abandoned">>, Resp, []) ++
                                       wh_json:get_value(<<"Waiting">>, Resp, []) ++
                                       wh_json:get_value(<<"Processed">>, Resp, [])
                                  )}
                              ]),
    cb_context:set_resp_status(
      cb_context:set_resp_data(Context, Stats)
      ,'success'
     ).

fetch_ranged_queue_stats(Context, StartRange) ->
    MaxRange = whapps_config:get_integer(<<"acdc">>, <<"archive_window_s">>, 3600),

    Now = wh_util:current_tstamp(),
    Past = Now - MaxRange,

    To = wh_util:to_integer(cb_context:req_value(Context, <<"end_range">>, Now)),
    MaxFrom = To - MaxRange,

    case wh_util:to_integer(StartRange) of
        F when F > To ->
            %% start_range is larger than end_range
            cb_context:add_validation_error(<<"end_range">>, <<"maximum">>
                                            ,<<"value is greater than start_range">>, Context
                                           );
        F when F < MaxFrom ->
            %% Range is too big
            fetch_ranged_queue_stats(Context, MaxFrom, To, MaxFrom >= Past);
        F when F < Past, To > Past ->
            %% range overlaps archived/real data, use real
            fetch_ranged_queue_stats(Context, Past, To, Past >= Past);
        F ->
            fetch_ranged_queue_stats(Context, F, To, F >= Past)
    end.

fetch_ranged_queue_stats(Context, From, To, 'true') ->
    lager:debug("ranged query from ~b to ~b(~b) of current stats (now ~b)", [From, To, To-From, wh_util:current_tstamp()]),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Status">>, cb_context:req_value(Context, <<"status">>)}
             ,{<<"Agent-ID">>, cb_context:req_value(Context, <<"agent_id">>)}
             ,{<<"Start-Range">>, From}
             ,{<<"End-Range">>, To}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_from_amqp(Context, Req);
fetch_ranged_queue_stats(Context, From, To, 'false') ->
    lager:debug("ranged query from ~b to ~b of archived stats", [From, To]),
    Context.

-spec fetch_from_amqp(cb_context:context(), wh_proplist()) -> cb_context:context().
fetch_from_amqp(Context, Req) ->
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_stats:publish_current_calls_req/1
                                       ,fun wapi_acdc_stats:current_calls_resp_v/1
                                      )
    of
        {'error', _E} ->
            lager:debug("failed to recv resp from AMQP: ~p", [_E]),
            cb_context:add_system_error('datastore_unreachable', Context);
        {'ok', Resp} -> format_stats(Context, Resp)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

normalize_agents_results(JObj, Acc) ->
    [wh_json:get_value(<<"id">>, JObj) | Acc].
