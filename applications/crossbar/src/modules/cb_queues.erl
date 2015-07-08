%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
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
%%%   POST: replace queue details
%%%   PATCH: patch queue details
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
         ,patch/2
         ,delete/2, delete/3
         ,delete_account/2
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
    _ = couch_mgr:db_create(?KZ_ACDC_DB),
    _ = couch_mgr:revise_doc_from_file(?KZ_ACDC_DB, 'crossbar', <<"views/acdc.json">>),

    _ = crossbar_bindings:bind(<<"*.allowed_methods.queues">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.queues">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.queues">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.queues">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.queues">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.queues">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.queues">>, ?MODULE, 'patch'),

    _ = crossbar_bindings:bind(<<"*.execute.delete.accounts">>, ?MODULE, 'delete_account'),

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
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

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
content_types_provided(Context) -> Context.
content_types_provided(Context, ?STATS_PATH_TOKEN) ->
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
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
validate(Context) ->
    validate_queues(Context, cb_context:req_verb(Context)).

validate_queues(Context, ?HTTP_GET) -> summary(Context);
validate_queues(Context, ?HTTP_PUT) -> validate_request('undefined', Context).

validate(Context, PathToken) ->
    validate_queue(Context, PathToken, cb_context:req_verb(Context)).

validate_queue(Context, ?STATS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_all_queue_stats(Context);
validate_queue(Context, ?EAVESDROP_PATH_TOKEN, ?HTTP_PUT) ->
    validate_eavesdrop_on_call(Context);
validate_queue(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_queue(Context, Id, ?HTTP_POST) ->
    validate_request(Id, Context);
validate_queue(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_queue(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

validate(Context, Id, Token) ->
    validate_queue_operation(Context, Id, Token, cb_context:req_verb(Context)).

validate_queue_operation(Context, Id, ?ROSTER_PATH_TOKEN, ?HTTP_GET) ->
    load_agent_roster(Id, Context);
validate_queue_operation(Context, Id, ?ROSTER_PATH_TOKEN, ?HTTP_POST) ->
    add_queue_to_agents(Id, Context);
validate_queue_operation(Context, Id, ?ROSTER_PATH_TOKEN, ?HTTP_DELETE) ->
    rm_queue_from_agents(Id, Context);
validate_queue_operation(Context, Id, ?EAVESDROP_PATH_TOKEN, ?HTTP_PUT) ->
    validate_eavesdrop_on_queue(Context, Id).

validate_eavesdrop_on_call(Context) ->
    Data = cb_context:req_data(Context),
    Fs = [{fun is_valid_endpoint/2, [Context, Data]}
          ,{fun is_valid_call/2, [Context, Data]}
          ,{fun is_valid_mode/2, [Context, Data]}
         ],
    case all_true(Fs) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        {'false', Context1} -> Context1
    end.

validate_eavesdrop_on_queue(Context, QueueId) ->
    Data = cb_context:req_data(Context),
    Fs = [{fun is_valid_endpoint/2, [Context, Data]}
          ,{fun is_valid_queue/2, [Context, QueueId]}
          ,{fun is_valid_mode/2, [Context, Data]}
         ],
    case all_true(Fs) of
        'true' -> cb_context:set_resp_status(Context, 'success');
        {'false', Context1} -> Context1
    end.

-spec all_true([{fun(), list()},...]) ->
                      'true' |
                      {'false', cb_context:context()}.
all_true(Fs) ->
    lists:foldl(fun({F, Args}, 'true') -> apply(F, Args);
                   (_, Acc) -> Acc
                end, 'true', Fs).

-spec is_valid_mode(cb_context:context(), wh_json:object()) ->
                           'true' |
                           {'false', cb_context:context()}.
is_valid_mode(Context, Data) ->
    Mode = wh_json:get_value(<<"mode">>, Data, <<"listen">>),
    case wapi_resource:is_valid_mode(Mode) of
        'true' -> 'true';
        'false' ->
            {'false'
             ,cb_context:add_validation_error(
                <<"mode">>
                ,<<"enum">>
                ,wh_json:from_list(
                   [{<<"message">>, <<"Value not found in enumerated list of values">>}
                    ,{<<"cause">>, Mode}
                   ])
                ,Context
               )
            }
    end.

-spec is_valid_call(cb_context:context(), wh_json:object()) ->
                           'true' |
                           {'false', cb_context:context()}.
is_valid_call(Context, Data) ->
    case wh_json:get_binary_value(<<"call_id">>, Data) of
        'undefined' ->
            {'false'
             ,cb_context:add_validation_error(
                <<"call_id">>
                ,<<"required">>
                ,wh_json:from_list(
                   [{<<"message">>, <<"Field is required but missing">>}]
                  )
                ,Context
               )
            };
        CallId ->
            is_active_call(Context, CallId)
    end.

-spec is_active_call(cb_context:context(), ne_binary()) ->
                            'true' |
                            {'false', cb_context:context()}.
is_active_call(Context, CallId) ->
    case whapps_call_command:b_channel_status(CallId) of
        {'error', _E} ->
            lager:debug("is not valid call: ~p", [_E]),
            {'false'
             ,cb_context:add_validation_error(
                <<"call_id">>
                ,<<"not_found">>
                ,wh_json:from_list(
                   [{<<"message">>, <<"Call was not found">>}
                    ,{<<"cause">>, CallId}
                   ])
                ,Context
               )
            };
        {'ok', _} -> 'true'
    end.

is_valid_queue(Context, <<_/binary>> = QueueId) ->
    AcctDb = cb_context:account_db(Context),
    case couch_mgr:open_cache_doc(AcctDb, QueueId) of
        {'ok', QueueJObj} -> is_valid_queue(Context, QueueJObj);
        {'error', _} ->
            {'false'
             ,cb_context:add_validation_error(
                <<"queue_id">>
                ,<<"not_found">>
                ,wh_json:from_list(
                   [{<<"message">>, <<"Queue was not found">>}
                    ,{<<"cause">>, QueueId}
                   ])
                ,Context
               )
            }
    end;
is_valid_queue(Context, QueueJObj) ->
    case wh_doc:type(QueueJObj) of
        <<"queue">> -> 'true';
        _ ->
            {'false'
             ,cb_context:add_validation_error(
                <<"queue_id">>
                ,<<"type">>
                ,wh_json:from_list([{<<"message">>, <<"Id did not represent a queue">>}])
                ,Context
               )
            }
    end.

is_valid_endpoint(Context, DataJObj) ->
    AcctDb = cb_context:account_db(Context),
    Id = wh_doc:id(DataJObj),
    case couch_mgr:open_cache_doc(AcctDb, Id) of
        {'ok', CallMeJObj} -> is_valid_endpoint_type(Context, CallMeJObj);
        {'error', _} ->
            {'false'
             ,cb_context:add_validation_error(
                <<"id">>
                ,<<"not_found">>
                ,wh_json:from_list(
                   [{<<"message">>, <<"Id was not found">>}
                    ,{<<"cause">>, Id}
                   ])
                ,Context
               )
            }
    end.

is_valid_endpoint_type(Context, CallMeJObj) ->
    case wh_doc:type(CallMeJObj) of
        <<"device">> -> 'true';
        Type ->
            {'false'
             ,cb_context:add_validation_error(
                <<"id">>
                ,<<"type">>
                ,wh_json:from_list(
                   [{<<"message">>, <<"Id did not represent a valid endpoint">>}
                    ,{<<"cause">>, Type}
                   ])
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
-spec put(cb_context:context()) ->
                 cb_context:context().
-spec put(cb_context:context(), path_token()) ->
                 cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) ->
                 cb_context:context().
put(Context) ->
    activate_account_for_acdc(Context),
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
                                       ,2 * ?MILLISECONDS_IN_SECOND
                                      )
    of
        {'ok', Resp} -> crossbar_util:response(filter_response_fields(Resp), Context);
        {'error', 'timeout'} ->
            cb_context:add_system_error(
              'timeout'
              ,wh_json:from_list([{<<"cause">>, <<"eavesdrop failed to start">>}])
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
post(Context, _) ->
    activate_account_for_acdc(Context),
    crossbar_doc:save(Context).
post(Context, Id, ?ROSTER_PATH_TOKEN) ->
    activate_account_for_acdc(Context),
    read(Id, crossbar_doc:save(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _) ->
    activate_account_for_acdc(Context),
    crossbar_doc:delete(Context).
delete(Context, Id, ?ROSTER_PATH_TOKEN) ->
    activate_account_for_acdc(Context),
    read(Id, crossbar_doc:save(Context)).

-spec delete_account(cb_context:context(), path_token()) -> cb_context:context().
delete_account(Context, AccountId) ->
    lager:debug("account ~s is being deleted, cleaning up ~s", [AccountId, ?KZ_ACDC_DB]),
    deactivate_account_for_acdc(AccountId),
    Context.

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
    Context1 = crossbar_doc:load(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' -> load_queue_agents(Id, Context1);
        _Status -> Context1
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(api_binary(), cb_context:context()) -> cb_context:context().
validate_patch(QueueId, Context) ->
    crossbar_doc:patch_and_validate(QueueId, Context, fun validate_request/2).

check_queue_schema(QueueId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(QueueId, C) end,
    cb_context:validate_request_data(<<"queues">>, Context, OnSuccess).

on_successful_validation('undefined', Context) ->
    Props = [{<<"pvt_type">>, <<"queue">>}],
    cb_context:set_doc(Context, wh_json:set_values(Props, cb_context:doc(Context)));
on_successful_validation(QueueId, Context) ->
    crossbar_doc:load_merge(QueueId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
load_queue_agents(Id, Context) ->
    Context1 = load_agent_roster(Id, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_resp_data(Context
                                     ,wh_json:set_value(<<"agents">>
                                                        ,cb_context:resp_data(Context1)
                                                        ,cb_context:resp_data(Context)
                                                       )
                                    );
        _Status -> Context1
    end.

load_agent_roster(Id, Context) ->
    crossbar_doc:load_view(?CB_AGENTS_LIST, [{'key', Id}]
                           ,Context
                           ,fun normalize_agents_results/2
                          ).

add_queue_to_agents(Id, Context) ->
    add_queue_to_agents(Id, Context, cb_context:req_data(Context)).

add_queue_to_agents(Id, Context, []) ->
    lager:debug("no agents listed, removing all agents from ~s", [Id]),

    Context1 = load_agent_roster(Id, Context),
    CurrAgentIds = cb_context:resp_data(Context1),

    rm_queue_from_agents(Id, Context1, CurrAgentIds);
add_queue_to_agents(Id, Context, AgentIds) ->
    %% We need to figure out what agents are on the queue already, and remove those not
    %% in the AgentIds list
    Context1 = load_agent_roster(Id, Context),
    CurrAgentIds = cb_context:resp_data(Context1),

    {InQueueAgents, RmAgentIds} = lists:partition(fun(A) -> lists:member(A, AgentIds) end, CurrAgentIds),
    AddAgentIds = [A || A <- AgentIds, (not lists:member(A, InQueueAgents))],

    _ = maybe_rm_agents(Id, Context, RmAgentIds),
    add_queue_to_agents_diff(Id, Context, AddAgentIds).

add_queue_to_agents_diff(_Id, Context, []) ->
    lager:debug("no more agent ids to add to queue"),
    cb_context:set_doc(
      cb_context:set_resp_status(Context, 'success')
      ,[]
     );
add_queue_to_agents_diff(Id, Context, AgentIds) ->
    Context1 = crossbar_doc:load(AgentIds, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            cb_context:set_doc(Context1
                               ,[maybe_add_queue_to_agent(Id, A) || A <- cb_context:doc(Context1)]
                              );
        _Status -> Context1
    end.

-spec maybe_add_queue_to_agent(ne_binary(), wh_json:object()) -> wh_json:object().
maybe_add_queue_to_agent(Id, A) ->
    Qs = case wh_json:get_value(<<"queues">>, A) of
             L when is_list(L) ->
                 case lists:member(Id, L) of
                     'true' -> L;
                     'false' -> [Id | L]
                 end;
             _ -> [Id]
         end,
    lager:debug("agent ~s adding queues: ~p", [wh_doc:id(A), Qs]),
    wh_json:set_value(<<"queues">>, Qs, A).

-spec maybe_rm_agents(ne_binary(), cb_context:context(), wh_json:keys()) -> cb_context:context().
maybe_rm_agents(_Id, Context, []) ->
    lager:debug("no agents to remove from the queue ~s", [_Id]),
    cb_context:set_resp_status(Context, 'success');
maybe_rm_agents(Id, Context, AgentIds) ->
    RMContext = rm_queue_from_agents(Id, Context, AgentIds),
    RMContext1 = crossbar_doc:save(RMContext),
    lager:debug("rm resulted in ~s", [cb_context:resp_status(RMContext1)]),
    RMContext1.

-spec rm_queue_from_agents(ne_binary(), cb_context:context()) ->
                                  cb_context:context().
-spec rm_queue_from_agents(ne_binary(), cb_context:context(), wh_json:keys()) ->
                                  cb_context:context().
rm_queue_from_agents(Id, Context) ->
    Context1 = load_agent_roster(Id, Context),
    rm_queue_from_agents(Id, Context, cb_context:doc(Context1)).

rm_queue_from_agents(_Id, Context, []) ->
    cb_context:set_resp_status(Context, 'success');
rm_queue_from_agents(Id, Context, [_|_]=AgentIds) ->
    lager:debug("remove agents: ~p", [AgentIds]),
    Context1 = crossbar_doc:load(AgentIds, Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("removed agents successfully"),
            cb_context:set_doc(Context1
                               ,[maybe_rm_queue_from_agent(Id, A) || A <- cb_context:doc(Context1)]
                              );
        _Status -> Context1
    end;
rm_queue_from_agents(_Id, Context, _Data) ->
    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}
                         ,{fun cb_context:set_doc/2, 'undefined'}
                        ]).

-spec maybe_rm_queue_from_agent(ne_binary(), wh_json:object()) -> wh_json:object().
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
            cb_context:add_validation_error(
                <<"end_range">>
                ,<<"maximum">>
                ,wh_json:from_list([
                    {<<"message">>, <<"value is greater than start_range">>}
                    ,{<<"cause">>, StartRange}
                 ])
                ,Context
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
    crossbar_doc:load_view(?CB_LIST
                           ,[]
                           ,Context
                           ,fun normalize_view_results/2
                          ).

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
    [wh_doc:id(JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates an entry in the acdc db of the account's participation in acdc
%% @end
%%--------------------------------------------------------------------
-spec activate_account_for_acdc(cb_context:context()) -> 'ok'.
activate_account_for_acdc(Context) ->
    case couch_mgr:open_cache_doc(?KZ_ACDC_DB, cb_context:account_id(Context)) of
        {'ok', _} -> 'ok';
        {'error', 'not_found'} ->
            lager:debug("creating account doc ~s in acdc db", [cb_context:account_id(Context)]),
            Doc = wh_doc:update_pvt_parameters(wh_json:from_list([{<<"_id">>, cb_context:account_id(Context)}])
                                               ,?KZ_ACDC_DB
                                               ,[{'account_id', cb_context:account_id(Context)}
                                                 ,{'type', <<"acdc_activation">>}
                                                ]),
            {'ok', _} = couch_mgr:ensure_saved(?KZ_ACDC_DB, Doc),
            'ok';
        {'error', _E} ->
            lager:debug("failed to check acdc activation doc: ~p", [_E])
    end.

-spec deactivate_account_for_acdc(ne_binary()) -> 'ok'.
deactivate_account_for_acdc(AccountId) ->
    case couch_mgr:open_doc(?KZ_ACDC_DB, AccountId) of
        {'error', _} -> 'ok';
        {'ok', JObj} ->
            case couch_mgr:del_doc(?KZ_ACDC_DB, JObj) of
                {'ok', _} ->
                    lager:debug("removed ~s from ~s", [AccountId, ?KZ_ACDC_DB]);
                {'error', _E} ->
                    lager:debug("failed to remove ~s: ~p", [AccountId, _E])
            end
    end.
