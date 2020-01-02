%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc CRUD for call queues
%%% /agents
%%%   GET: list all known agents and their queues
%%%
%%% /agents/stats
%%%   GET: stats for all agents for the last hour
%%% /agents/statuses
%%%   GET: statuses for each agents
%%% /agents/AID
%%%   GET: agent details
%%% /agents/AID/queue_status
%%%   POST: login/logout agent to/from queue
%%%
%%% /agents/AID/status
%%%   GET: last 10 status updates
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_agents).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,post/3
        ]).

-include_lib("crossbar/src/crossbar.hrl").
-include("acdc_config.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".queues">>).

-define(FORMAT_COMPRESSED, <<"compressed">>).
-define(FORMAT_VERBOSE, <<"verbose">>).

-define(CB_LIST, <<"agents/crossbar_listing">>).
-define(STATS_PATH_TOKEN, <<"stats">>).
-define(STATUS_PATH_TOKEN, <<"status">>).
-define(QUEUE_STATUS_PATH_TOKEN, <<"queue_status">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kapi_acdc_agent:declare_exchanges(),
    _ = kapi_acdc_stats:declare_exchanges(),

    _ = crossbar_bindings:bind(<<"*.allowed_methods.agents">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.agents">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.agents">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.execute.post.agents">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.validate.agents">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?STATUS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(?STATS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(_UserId) -> [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?STATUS_PATH_TOKEN, _UserId) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(_UserId, ?STATUS_PATH_TOKEN) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(_UserId, ?QUEUE_STATUS_PATH_TOKEN) -> [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%% ```
%%    /agents => []
%%    /agents/foo => [<<"foo">>]
%%    /agents/foo/bar => [<<"foo">>, <<"bar">>]
%%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, ?STATUS_PATH_TOKEN) -> 'true';
resource_exists(?STATUS_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?QUEUE_STATUS_PATH_TOKEN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) -> Context.

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, ?STATUS_PATH_TOKEN) -> Context;
content_types_provided(Context, ?STATS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"format">>, ?FORMAT_COMPRESSED) of
        ?FORMAT_VERBOSE ->
            cb_context:add_content_types_provided(Context
                                                 ,[{'to_json', ?JSON_CONTENT_TYPES}
                                                  ,{'to_csv', ?CSV_CONTENT_TYPES}
                                                  ]);
        ?FORMAT_COMPRESSED ->
            cb_context:add_content_types_provided(Context
                                                 ,[{'to_json', ?JSON_CONTENT_TYPES}]
                                                 )
    end.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context, ?STATUS_PATH_TOKEN, _) -> Context;
content_types_provided(Context, _, ?STATUS_PATH_TOKEN) -> Context;
content_types_provided(Context, _, ?QUEUE_STATUS_PATH_TOKEN) -> Context.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /agents might load a list of agent objects
%% /agents/123 might load the agent object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) ->
          cb_context:context().
validate(Context) ->
    summary(Context).

-spec validate(cb_context:context(), path_token()) ->
          cb_context:context().
validate(Context, PathToken) ->
    validate_agent(Context, PathToken, cb_context:req_verb(Context)).

validate_agent(Context, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_all_agent_statuses(Context);
validate_agent(Context, ?STATS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_all_agent_stats(Context);
validate_agent(Context, Id, ?HTTP_GET) ->
    read(Id, Context).

-spec validate(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
validate(Context, AgentId, PathToken) ->
    validate_agent_action(Context, AgentId, PathToken, cb_context:req_verb(Context)).

validate_agent_action(Context, AgentId, ?STATUS_PATH_TOKEN, ?HTTP_POST) ->
    validate_status_change(read(AgentId, Context));
validate_agent_action(Context, AgentId, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_status(AgentId, Context);
validate_agent_action(Context, ?STATUS_PATH_TOKEN, AgentId, ?HTTP_GET) ->
    fetch_agent_status(AgentId, Context);
validate_agent_action(Context, AgentId, ?QUEUE_STATUS_PATH_TOKEN, ?HTTP_POST) ->
    OnSuccess = fun (C) -> maybe_queues_change(read(AgentId, C)) end,
    cb_context:validate_request_data(<<"queue_update">>, Context, OnSuccess);
validate_agent_action(Context, AgentId, ?QUEUE_STATUS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_queues(read(AgentId, Context)).

-spec maybe_queues_change(cb_context:context()) -> cb_context:context().
maybe_queues_change(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> handle_queue_update(Context);
        _ -> Context
    end.

-spec handle_queue_update(cb_context:context()) -> cb_context:context().
handle_queue_update(Context) ->
    QueueID = cb_context:req_value(Context, <<"queue_id">>),
    Updater = case cb_context:req_value(Context, <<"action">>) of
                  <<"login">> -> fun(Doc) -> kzd_agent:maybe_add_queue(Doc, QueueID) end;
                  <<"logout">> -> fun(Doc) -> kzd_agent:maybe_rm_queue(Doc, QueueID) end
              end,
    cb_context:update_doc(Context, Updater).

-spec fetch_agent_queues(cb_context:context()) -> cb_context:context().
fetch_agent_queues(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            Queues = kz_json:get_value(<<"queues">>, Doc),
            cb_context:set_resp_data(Context, Queues);
        _ ->
            Context
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, AgentId, ?STATUS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"status">>) of
        <<"login">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_login/1);
        <<"logout">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_logout/1);
        <<"pause">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_pause/1);
        <<"resume">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_resume/1);
        <<"end_wrapup">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_end_wrapup/1)
    end,
    crossbar_util:response(<<"status update sent">>, Context);
post(Context, AgentId, ?QUEUE_STATUS_PATH_TOKEN) ->
    publish_action(Context, AgentId),
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Queues = kz_json:get_value(<<"queues">>, cb_context:doc(Context), []),
            cb_context:set_resp_data(Context1, Queues);
        _Status ->
            Context1
    end.

-spec publish_action(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
publish_action(Context, AgentId) ->
    Publisher = case cb_context:req_value(Context, <<"action">>) of
                    <<"logout">> -> fun kapi_acdc_agent:publish_logout_queue/1;
                    <<"login">> -> fun kapi_acdc_agent:publish_login_queue/1
                end,

    Props = props:filter_undefined(
              [{<<"Account-ID">>, cb_context:account_id(Context)}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Queue-ID">>, cb_context:req_value(Context, <<"queue_id">>)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ]),

    kz_amqp_worker:cast(Props, Publisher).

-spec publish_update(cb_context:context(), kz_term:api_binary(), function()) -> 'ok'.
publish_update(Context, AgentId, PubFun) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, cb_context:account_id(Context)}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Time-Limit">>, cb_context:req_value(Context, <<"timeout">>)}
               ,{<<"Presence-ID">>, cb_context:req_value(Context, <<"presence_id">>)}
               ,{<<"Presence-State">>, cb_context:req_value(Context, <<"presence_state">>)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    kz_amqp_worker:cast(Update, PubFun).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(path_token(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_users:type())).

-define(CB_AGENTS_LIST, <<"users/crossbar_listing">>).
-spec fetch_all_agent_statuses(cb_context:context()) -> cb_context:context().
fetch_all_agent_statuses(Context) ->
    fetch_all_current_statuses(Context
                              ,'undefined'
                              ,cb_context:req_value(Context, <<"status">>)
                              ).

-spec fetch_agent_status(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
fetch_agent_status(AgentId, Context) ->
    case kz_term:is_true(cb_context:req_value(Context, <<"recent">>)) of
        'false' ->
            fetch_current_status(Context
                                ,AgentId
                                ,kz_term:is_true(cb_context:req_value(Context, <<"full">>))
                                );

        'true' ->
            fetch_all_current_statuses(Context
                                      ,AgentId
                                      ,cb_context:req_value(Context, <<"status">>)
                                      )
    end.

-spec fetch_all_agent_stats(cb_context:context()) -> cb_context:context().
fetch_all_agent_stats(Context) ->
    case cb_context:req_value(Context, <<"start_range">>) of
        'undefined' -> fetch_all_current_agent_stats(Context);
        StartRange -> fetch_ranged_agent_stats(Context, StartRange)
    end.

-spec fetch_all_current_agent_stats(cb_context:context()) -> cb_context:context().
fetch_all_current_agent_stats(Context) ->
    fetch_all_current_stats(Context
                           ,cb_context:req_value(Context, <<"agent_id">>)
                           ).

-spec fetch_all_current_stats(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
fetch_all_current_stats(Context, AgentId) ->
    Now = kz_time:now_s(),
    From = Now - min(?SECONDS_IN_DAY, ?ACDC_CLEANUP_WINDOW),

    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Start-Range">>, From}
            ,{<<"End-Range">>, Now}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_stats_from_amqp(Context, Req).

-spec fetch_current_status(cb_context:context(), kz_term:api_binary(), kz_term:api_boolean()) -> cb_context:context().
fetch_current_status(Context, AgentId, 'false') ->
    {'ok', Resp} = acdc_agent_util:most_recent_status(cb_context:account_id(Context), AgentId),
    crossbar_util:response(Resp, Context);
fetch_current_status(Context, AgentId, 'true') ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Agent-ID">>, AgentId}
            ,{<<"Limit">>, 1}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_acdc_stats:publish_status_req/1
                            ,fun kapi_acdc_stats:status_resp_v/1
                            )
    of
        {'error', E} ->
            crossbar_util:response('error'
                                  ,<<"status request contains errors">>
                                  ,400
                                  ,kz_json:get_value(<<"Error-Reason">>, E)
                                  ,Context
                                  );
        {'ok', Resp} ->
            Agents = kz_json:get_value(<<"Agents">>, Resp, kz_json:new()),
            Agents1 = kz_json:map(fun(K, AgentStats) -> {K, remove_timestamps(AgentStats)} end, Agents),
            crossbar_util:response(kz_json:get_json_value(AgentId, Agents1), Context)
    end.

remove_timestamps(AgentStats) ->
    [Key|_] = kz_json:get_keys(AgentStats),
    kz_json:get_json_value(Key, AgentStats).

-spec fetch_all_current_statuses(cb_context:context(), kz_term:api_binary(), kz_term:api_binary()) ->
          cb_context:context().
fetch_all_current_statuses(Context, AgentId, Status) ->
    Now = kz_time:now_s(),
    From = Now - min(?SECONDS_IN_DAY, ?ACDC_CLEANUP_WINDOW),

    Opts = props:filter_undefined(
             [{<<"Status">>, Status}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Start-Range">>, From}
             ,{<<"End-Range">>, Now}
             ,{<<"Limit">>, cb_context:req_value(Context, <<"limit">>)}
             ]),

    {'ok', Resp} = acdc_agent_util:most_recent_statuses(cb_context:account_id(Context), Opts),
    crossbar_util:response(Resp, Context).

-spec fetch_ranged_agent_stats(cb_context:context(), pos_integer()) -> cb_context:context().
fetch_ranged_agent_stats(Context, StartRange) ->
    MaxRange = ?ACDC_ARCHIVE_WINDOW,

    Now = kz_time:now_s(),
    Past = Now - MaxRange,

    To = kz_term:to_integer(cb_context:req_value(Context, <<"end_range">>, Now)),
    MaxFrom = To - MaxRange,

    case kz_term:to_integer(StartRange) of
        F when F > To ->
            %% start_range is larger than end_range
            Msg = kz_json:from_list([{<<"message">>, <<"value is greater than start_range">>}
                                    ,{<<"cause">>, To}
                                    ]),
            cb_context:add_validation_error(<<"end_range">>, <<"maximum">>, Msg, Context);
        F when F < MaxFrom ->
            %% Range is too big
            fetch_ranged_agent_stats(Context, MaxFrom, To, MaxFrom >= Past);
        F when F < Past, To > Past ->
            %% range overlaps archived/real data, use real
            fetch_ranged_agent_stats(Context, Past, To, Past >= Past);
        F ->
            fetch_ranged_agent_stats(Context, F, To, F >= Past)
    end.

-spec fetch_ranged_agent_stats(cb_context:context(), pos_integer(), pos_integer(), boolean()) ->
          cb_context:context().
fetch_ranged_agent_stats(Context, From, To, 'true') ->
    lager:debug("ranged query from ~b to ~b(~b) of current stats (now ~b)", [From, To, To-From, kz_time:now_s()]),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Status">>, cb_context:req_value(Context, <<"status">>)}
            ,{<<"Agent-ID">>, cb_context:req_value(Context, <<"agent_id">>)}
            ,{<<"Start-Range">>, From}
            ,{<<"End-Range">>, To}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_stats_from_amqp(Context, Req);
fetch_ranged_agent_stats(Context, From, To, 'false') ->
    lager:debug("ranged query from ~b to ~b of archived stats", [From, To]),
    Context.

-spec fetch_stats_from_amqp(cb_context:context(), kz_term:proplist()) -> cb_context:context().
fetch_stats_from_amqp(Context, Req) ->
    case kz_amqp_worker:call(Req
                            ,fun kapi_acdc_stats:publish_current_calls_req/1
                            ,fun kapi_acdc_stats:current_calls_resp_v/1
                            )
    of
        {'error', E} ->
            crossbar_util:response('error', <<"stat request had errors">>, 400
                                  ,kz_json:get_value(<<"Error-Reason">>, E)
                                  ,Context
                                  );
        {'ok', Resp} -> format_stats(Context, Resp)
    end.

-spec format_stats(cb_context:context(), kz_json:object()) ->
          cb_context:context().
format_stats(Context, Resp) ->
    Stats = kz_json:get_value(<<"Handled">>, Resp, [])
        ++ kz_json:get_value(<<"Abandoned">>, Resp, [])
        ++ kz_json:get_value(<<"Waiting">>, Resp, [])
        ++ kz_json:get_value(<<"Processed">>, Resp, []),

    FormattedStats = lists:foldl(fun format_stats_fold/2
                                ,kz_json:new()
                                ,Stats
                                ),
    crossbar_util:response(FormattedStats, Context).

-spec format_stats_fold(kz_json:object(), kz_json:object()) ->
          kz_json:object().
format_stats_fold(Stat, Acc) ->
    QueueId = kz_json:get_value(<<"queue_id">>, Stat),

    case kz_json:get_value(<<"agent_id">>, Stat) of
        'undefined' -> maybe_add_misses(Stat, Acc, QueueId);
        AgentId ->
            TotalsK = [AgentId, <<"total_calls">>],
            QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],

            Totals = kz_json:get_integer_value(TotalsK, Acc, 0),
            QTotals = kz_json:get_integer_value(QTotalsK, Acc, 0),

            AnsweredData = maybe_add_answered(Stat, Acc),

            maybe_add_misses(Stat
                            ,kz_json:set_values([{TotalsK, Totals + 1}
                                                ,{QTotalsK, QTotals + 1}
                                                 | AnsweredData
                                                ]
                                               ,Acc
                                               )
                            ,QueueId
                            )
    end.

-spec maybe_add_answered(kz_json:object(), kz_json:object()) ->
          [{kz_json:path(), non_neg_integer()}].
maybe_add_answered(Stat, Acc) ->
    maybe_add_answered(Stat, Acc, kz_json:get_value(<<"status">>, Stat)).

-spec maybe_add_answered(kz_json:object(), kz_json:object(), kz_term:api_binary()) ->
          [{kz_json:path(), non_neg_integer()}].
maybe_add_answered(Stat, Acc, <<"handled">>) ->
    add_answered(Stat, Acc);
maybe_add_answered(Stat, Acc, <<"processed">>) ->
    add_answered(Stat, Acc);
maybe_add_answered(_, _, _S) ->
    lager:debug("status ~s not indicative of an answered call", [_S]),
    [].

-spec add_answered(kz_json:object(), kz_json:object()) ->
          [{kz_json:path(), non_neg_integer()},...].
add_answered(Stat, Acc) ->
    AgentId = kz_json:get_value(<<"agent_id">>, Stat),
    QueueId = kz_json:get_value(<<"queue_id">>, Stat),

    AnsweredK = [AgentId, <<"answered_calls">>],
    QAnsweredK = [AgentId, <<"queues">>, QueueId, <<"answered_calls">>],

    Answered = kz_json:get_integer_value(AnsweredK, Acc, 0),
    QAnswered = kz_json:get_integer_value(QAnsweredK, Acc, 0),

    [{AnsweredK, Answered + 1}
    ,{QAnsweredK, QAnswered + 1}
    ].

-spec maybe_add_misses(kz_json:object(), kz_json:object(), kz_term:ne_binary()) ->
          kz_json:object().
maybe_add_misses(Stat, Acc, QueueId) ->
    case kz_json:get_value(<<"misses">>, Stat, []) of
        [] -> Acc;
        Misses ->
            lists:foldl(fun(Miss, AccJObj) ->
                                add_miss(Miss, AccJObj, QueueId)
                        end
                       ,Acc
                       ,Misses
                       )
    end.

-spec add_miss(kz_json:object(), kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
add_miss(Miss, Acc, QueueId) ->
    AgentId = kz_json:get_value(<<"agent_id">>, Miss),
    MissesK = [AgentId, <<"missed_calls">>],
    QMissesK = [AgentId, <<"queues">>, QueueId, <<"missed_calls">>],

    Misses = kz_json:get_integer_value(MissesK, Acc, 0),
    QMisses = kz_json:get_integer_value(QMissesK, Acc, 0),

    TotalsK = [AgentId, <<"total_calls">>],
    QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],

    Totals = kz_json:get_integer_value(TotalsK, Acc, 0),
    QTotals = kz_json:get_integer_value(QTotalsK, Acc, 0),

    kz_json:set_values([{MissesK, Misses + 1}
                       ,{QMissesK, QMisses + 1}
                       ,{TotalsK, Totals + 1}
                       ,{QTotalsK, QTotals + 1}
                       ]
                      ,Acc
                      ).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
          kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:set_value(<<"id">>
                      ,kz_doc:id(JObj)
                      ,kz_json:get_value(<<"value">>, JObj)
                      )
     | Acc
    ].

-spec validate_status_change(cb_context:context()) -> cb_context:context().
validate_status_change(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            lager:debug("read agent doc"),
            validate_status_change(Context, cb_context:req_value(Context, <<"status">>));
        _ ->
            lager:debug("failed to read agent doc"),
            check_for_status_error(Context, cb_context:req_value(Context, <<"status">>))
    end.

-define(STATUS_CHANGES, [<<"login">>, <<"logout">>, <<"pause">>, <<"resume">>, <<"end_wrapup">>]).
-spec validate_status_change(cb_context:context(), kz_term:api_binary()) ->
          cb_context:context().
validate_status_change(Context, S) ->
    case lists:member(S, ?STATUS_CHANGES) of
        'true' -> validate_status_change_params(Context, S);
        'false' ->
            lager:debug("status ~s not valid", [S]),
            cb_context:add_validation_error(<<"status">>
                                           ,<<"enum">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value is not a valid status">>}
                                              ,{<<"cause">>, S}
                                              ])
                                           ,Context
                                           )
    end.

-spec check_for_status_error(cb_context:context(), kz_term:api_binary()) ->
          cb_context:context().
check_for_status_error(Context, S) ->
    case lists:member(S, ?STATUS_CHANGES) of
        'true' -> Context;
        'false' ->
            lager:debug("status ~s not found", [S]),
            cb_context:add_validation_error(<<"status">>
                                           ,<<"enum">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value is not a valid status">>}
                                              ,{<<"cause">>, S}
                                              ])
                                           ,Context
                                           )
    end.

-spec validate_status_change_params(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
validate_status_change_params(Context, <<"pause">>) ->
    Value = cb_context:req_value(Context, <<"timeout">>),
    try kz_term:to_integer(Value) of
        N when N >= 0 -> cb_context:set_resp_status(Context, 'success');
        N ->
            lager:debug("bad int for pause: ~p", [N]),
            cb_context:add_validation_error(<<"timeout">>
                                           ,<<"minimum">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value must be at least greater than or equal to 0">>}
                                              ,{<<"cause">>, N}
                                              ])
                                           ,Context
                                           )
    catch
        _E:_R ->
            lager:debug("bad int for pause: ~s: ~p", [_E, _R]),
            cb_context:add_validation_error(<<"timeout">>
                                           ,<<"type">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value must be an integer greater than or equal to 0">>}
                                              ,{<<"cause">>, Value}
                                              ])
                                           ,Context
                                           )
    end;
validate_status_change_params(Context, _S) ->
    lager:debug("great success for ~s", [_S]),
    cb_context:set_resp_status(Context, 'success').
