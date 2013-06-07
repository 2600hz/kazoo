%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CRUD for call queues
%%% /agents
%%%   GET: list all known agents and their queues
%%%
%%% /agents/stats
%%%   GET: stats for all agents for the last hour
%%% /agents/statuses
%%%   GET: statuses for each agents
%%% /agents/AID
%%%   GET: agent details
%%%
%%% /agents/AID/status
%%%   GET: last 10 status updates
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_agents).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,content_types_provided/1, content_types_provided/2, content_types_provided/3
         ,validate/1, validate/2, validate/3
        ]).

-include("src/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".queues">>).

-define(FORMAT_COMPRESSED, <<"compressed">>).
-define(FORMAT_VERBOSE, <<"verbose">>).

-define(CB_LIST, <<"agents/crossbar_listing">>).
-define(STATS_PATH_TOKEN, <<"stats">>).
-define(STATUS_PATH_TOKEN, <<"status">>).

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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.agents">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.agents">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.agents">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.agents">>, ?MODULE, 'validate').

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
allowed_methods() -> [?HTTP_GET].
allowed_methods(_) -> [?HTTP_GET].
allowed_methods(?STATUS_PATH_TOKEN, _) -> [?HTTP_GET];
allowed_methods(_, ?STATUS_PATH_TOKEN) -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /agents => []
%%    /agents/foo => [<<"foo">>]
%%    /agents/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?STATUS_PATH_TOKEN) -> 'true';
resource_exists(?STATUS_PATH_TOKEN, _) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(#cb_context{}=Context) -> Context.
content_types_provided(#cb_context{}=Context, ?STATUS_PATH_TOKEN) -> Context;
content_types_provided(#cb_context{}=Context, ?STATS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"format">>, ?FORMAT_COMPRESSED) of
        ?FORMAT_VERBOSE ->
            CTPs = [{'to_json', ?JSON_CONTENT_TYPES}
                    ,{'to_csv', ?CSV_CONTENT_TYPES}
                   ],
            cb_context:add_content_types_provided(Context, CTPs);
        ?FORMAT_COMPRESSED ->
            CTPs = [{'to_json', [{<<"application">>, <<"json">>}]}],
            cb_context:add_content_types_provided(Context, CTPs)
    end.
content_types_provided(#cb_context{}=Context, ?STATUS_PATH_TOKEN, _) -> Context;
content_types_provided(#cb_context{}=Context, _, ?STATUS_PATH_TOKEN) -> Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /agents mights load a list of agent objects
%% /agents/123 might load the agent object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATUS_PATH_TOKEN) ->
    fetch_all_agent_statuses(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_agent_stats(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, AgentId, ?STATUS_PATH_TOKEN) ->
    fetch_agent_status(AgentId, Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATUS_PATH_TOKEN, AgentId) ->
    fetch_agent_status(AgentId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(path_token(), cb_context:context()) -> cb_context:context().
read(Id, Context) -> crossbar_doc:load(Id, Context).

-define(CB_AGENTS_LIST, <<"users/crossbar_listing">>).
fetch_all_agent_statuses(Context) ->
    fetch_all_current_statuses(Context
                               ,'undefined'
                               ,cb_context:req_value(Context, <<"status">>)
                              ).

fetch_agent_status(AgentId, Context) ->
    case wh_util:is_true(cb_context:req_value(Context, <<"recent">>)) of
        'false' ->
            crossbar_util:response(acdc_util:agent_status(cb_context:account_id(Context), AgentId)
                                   ,Context
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

fetch_all_current_agent_stats(Context) ->
    fetch_all_current_stats(Context
                            ,cb_context:req_value(Context, <<"agent_id">>)
                           ).

fetch_all_current_stats(Context, AgentId) ->
    Now = wh_util:current_tstamp(),
    Yday = Now - ?SECONDS_IN_DAY,

    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Start-Range">>, Yday}
             ,{<<"End-Range">>, Now}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_stats_from_amqp(Context, Req).

fetch_all_current_statuses(Context, AgentId, Status) ->
    Now = wh_util:current_tstamp(),
    Yday = Now - ?SECONDS_IN_DAY,

    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Status">>, Status}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Start-Range">>, Yday}
             ,{<<"End-Range">>, Now}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_statuses_from_amqp(Context, Req).

fetch_ranged_agent_stats(Context, StartRange) ->
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
            fetch_ranged_agent_stats(Context, MaxFrom, To, MaxFrom >= Past);
        F when F < Past, To > Past ->
            %% range overlaps archived/real data, use real
            fetch_ranged_agent_stats(Context, Past, To, Past >= Past);
        F ->
            fetch_ranged_agent_stats(Context, F, To, F >= Past)
    end.

fetch_ranged_agent_stats(Context, From, To, 'true') ->
    lager:debug("ranged query from ~b to ~b(~b) of current stats (now ~b)", [From, To, To-From, wh_util:current_tstamp()]),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Status">>, cb_context:req_value(Context, <<"status">>)}
             ,{<<"Agent-ID">>, cb_context:req_value(Context, <<"agent_id">>)}
             ,{<<"Start-Range">>, From}
             ,{<<"End-Range">>, To}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_stats_from_amqp(Context, Req);
fetch_ranged_agent_stats(Context, From, To, 'false') ->
    lager:debug("ranged query from ~b to ~b of archived stats", [From, To]),
    Context.

-spec fetch_statuses_from_amqp(cb_context:context(), wh_proplist()) ->
                                      cb_context:context().
fetch_statuses_from_amqp(Context, Req) ->
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_stats:publish_status_req/1
                                       ,fun wapi_acdc_stats:status_resp_v/1
                                      )
    of
        {'error', E} ->
            crossbar_util:response('error', <<"status request had errors">>, 400
                                   ,wh_json:get_value(<<"Error-Reason">>, E)
                                   ,Context
                                  );
        {'ok', Resp} -> format_statuses(Context, Resp)
    end.

fetch_stats_from_amqp(Context, Req) ->
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_stats:publish_current_calls_req/1
                                       ,fun wapi_acdc_stats:current_calls_resp_v/1
                                      )
    of
        {'error', E} ->
            crossbar_util:response('error', <<"stat request had errors">>, 400
                                   ,wh_json:get_value(<<"Error-Reason">>, E)
                                   ,Context
                                  );
        {'ok', Resp} -> format_stats(Context, Resp)
    end.

format_statuses(Context, Resp) ->
    As = wh_json:get_value(<<"Agents">>, Resp, wh_json:new()),
    crossbar_util:response(
      wh_json:foldl(fun format_statuses_fold/3, wh_json:new(), As)
      ,Context
     ).

format_statuses_fold(AgentId, Statuses, Acc) ->
    wh_json:set_value(AgentId
                      ,wh_json:map(fun format_status_fold_map/2, Statuses)
                      ,Acc
                     ).

format_status_fold_map(Time, Status) ->
    {Time, wh_doc:public_fields(Status)}.

-spec format_stats(cb_context:context(), wh_json:object()) -> cb_context:context().
format_stats(Context, Resp) ->
    Stats = wh_json:get_value(<<"Handled">>, Resp, []) ++
        wh_json:get_value(<<"Abandoned">>, Resp, []) ++
        wh_json:get_value(<<"Waiting">>, Resp, []) ++
        wh_json:get_value(<<"Processed">>, Resp, []),

    crossbar_util:response(lists:foldl(fun format_stats_fold/2, wh_json:new(), Stats), Context).

format_stats_fold(Stat, Acc) ->
    QueueId = wh_json:get_value(<<"queue_id">>, Stat),

    case wh_json:get_value(<<"agent_id">>, Stat) of
        'undefined' -> maybe_add_misses(Stat, Acc, QueueId);
        AgentId ->
            TotalsK = [AgentId, <<"total_calls">>],
            QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],

            Totals = wh_json:get_integer_value(TotalsK, Acc, 0),
            QTotals = wh_json:get_integer_value(QTotalsK, Acc, 0),

            maybe_add_misses(Stat
                             ,wh_json:set_values([{TotalsK, Totals + 1}
                                                  ,{QTotalsK, QTotals + 1}
                                                 ], Acc)
                             ,QueueId
                            )
    end.

maybe_add_misses(Stat, Acc, QueueId) ->
    case wh_json:get_value(<<"misses">>, Stat, []) of
        [] -> Acc;
        Misses ->
            lists:foldl(fun(Miss, AccJObj) ->
                                add_miss(Miss, AccJObj, QueueId)
                        end, Acc, Misses)
    end.

add_miss(Miss, Acc, QueueId) ->
    AgentId = wh_json:get_value(<<"agent_id">>, Miss),
    MissesK = [AgentId, <<"missed_calls">>],
    QMissesK = [AgentId, <<"queues">>, QueueId, <<"missed_calls">>],

    Misses = wh_json:get_integer_value(MissesK, Acc, 0),
    QMisses = wh_json:get_integer_value(QMissesK, Acc, 0),

    wh_json:set_values([{MissesK, Misses + 1}
                        ,{QMissesK, QMisses + 1}
                       ], Acc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) -> crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:set_value(<<"id">>
                       ,wh_json:get_value(<<"id">>, JObj)
                       ,wh_json:get_value(<<"value">>, JObj)
                      )
     | Acc
    ].
