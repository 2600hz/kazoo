%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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
         ,post/3
        ]).

-include("../crossbar.hrl").

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.agents">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.agents">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.agents">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.execute.post.agents">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.validate.agents">>, ?MODULE, 'validate').

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

allowed_methods(?STATUS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(?STATS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(_) -> [?HTTP_GET].

allowed_methods(?STATUS_PATH_TOKEN, _) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(_, ?STATUS_PATH_TOKEN) -> [?HTTP_GET, ?HTTP_POST].

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
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    summary(Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATUS_PATH_TOKEN) ->
    fetch_all_agent_statuses(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATS_PATH_TOKEN) ->
    fetch_all_agent_stats(Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = ?HTTP_POST}=Context, AgentId, ?STATUS_PATH_TOKEN) ->
    validate_status_change(read(AgentId, Context));

validate(#cb_context{req_verb = ?HTTP_GET}=Context, AgentId, ?STATUS_PATH_TOKEN) ->
    fetch_agent_status(AgentId, Context);
validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?STATUS_PATH_TOKEN, AgentId) ->
    fetch_agent_status(AgentId, Context).

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, AgentId, ?STATUS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"status">>) of
        <<"login">> -> publish_update(Context, AgentId, fun wapi_acdc_agent:publish_login/1);
        <<"logout">> -> publish_update(Context, AgentId, fun wapi_acdc_agent:publish_logout/1);
        <<"pause">> -> publish_update(Context, AgentId, fun wapi_acdc_agent:publish_pause/1);
        <<"resume">> -> publish_update(Context, AgentId, fun wapi_acdc_agent:publish_resume/1)
    end,
    crossbar_util:response(<<"status update sent">>, Context).

-spec publish_update(cb_context:context(), api_binary(), function()) -> 'ok'.
publish_update(Context, AgentId, PubFun) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, cb_context:account_id(Context)}
                ,{<<"Agent-ID">>, AgentId}
                ,{<<"Time-Limit">>, cb_context:req_value(Context, <<"timeout">>)}
                ,{<<"Presence-ID">>, cb_context:req_value(Context, <<"presence_id">>)}
                ,{<<"Presence-State">>, cb_context:req_value(Context, <<"presence_state">>)}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(path_token(), cb_context:context()) -> cb_context:context().
read(Id, Context) -> crossbar_doc:load(Id, Context).

-define(CB_AGENTS_LIST, <<"users/crossbar_listing">>).
-spec fetch_all_agent_statuses(cb_context:context()) -> cb_context:context().
fetch_all_agent_statuses(Context) ->
    fetch_all_current_statuses(Context
                               ,'undefined'
                               ,cb_context:req_value(Context, <<"status">>)
                              ).

-spec fetch_agent_status(api_binary(), cb_context:context()) -> cb_context:context().
fetch_agent_status(AgentId, Context) ->
    case wh_util:is_true(cb_context:req_value(Context, <<"recent">>)) of
        'false' ->
            {'ok', Resp} = acdc_agent_util:most_recent_status(cb_context:account_id(Context), AgentId),
            crossbar_util:response(Resp, Context);
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

-spec fetch_all_current_stats(cb_context:context(), api_binary()) -> cb_context:context().
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

-spec fetch_all_current_statuses(cb_context:context(), api_binary(), api_binary()) ->
                                        cb_context:context().
fetch_all_current_statuses(Context, AgentId, Status) ->
    Now = wh_util:current_tstamp(),
    Yday = Now - ?SECONDS_IN_DAY,

    Recent = cb_context:req_value(Context, <<"recent">>, 'false'),

    Opts = props:filter_undefined(
             [{<<"Status">>, Status}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Start-Range">>, Yday}
              ,{<<"End-Range">>, Now}
              ,{<<"Most-Recent">>, wh_util:is_false(Recent)}
             ]),

    {'ok', Resp} = acdc_agent_util:most_recent_statuses(cb_context:account_id(Context), Opts),
    crossbar_util:response(Resp, Context).

-spec fetch_ranged_agent_stats(cb_context:context(), pos_integer()) -> cb_context:context().
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

-spec fetch_ranged_agent_stats(cb_context:context(), pos_integer(), pos_integer(), boolean()) ->
                                      cb_context:context().
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

-spec fetch_stats_from_amqp(cb_context:context(), wh_proplist()) -> cb_context:context().
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

-spec format_stats(cb_context:context(), wh_json:object()) -> cb_context:context().
format_stats(Context, Resp) ->
    Stats = wh_json:get_value(<<"Handled">>, Resp, []) ++
        wh_json:get_value(<<"Abandoned">>, Resp, []) ++
        wh_json:get_value(<<"Waiting">>, Resp, []) ++
        wh_json:get_value(<<"Processed">>, Resp, []),

    crossbar_util:response(lists:foldl(fun format_stats_fold/2, wh_json:new(), Stats), Context).

-spec format_stats_fold(wh_json:object(), wh_json:object()) -> wh_json:object().
format_stats_fold(Stat, Acc) ->
    QueueId = wh_json:get_value(<<"queue_id">>, Stat),

    case wh_json:get_value(<<"agent_id">>, Stat) of
        'undefined' -> maybe_add_misses(Stat, Acc, QueueId);
        AgentId ->
            TotalsK = [AgentId, <<"total_calls">>],
            QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],

            Totals = wh_json:get_integer_value(TotalsK, Acc, 0),
            QTotals = wh_json:get_integer_value(QTotalsK, Acc, 0),

            AnsweredData = maybe_add_answered(Stat, Acc),

            maybe_add_misses(Stat
                             ,wh_json:set_values([{TotalsK, Totals + 1}
                                                  ,{QTotalsK, QTotals + 1}
                                                  | AnsweredData
                                                 ], Acc)
                             ,QueueId
                            )
    end.

-spec maybe_add_answered(wh_json:object(), wh_json:object()) ->
                                [{wh_json:key(), non_neg_integer()},...] | [].
-spec maybe_add_answered(wh_json:object(), wh_json:object(), api_binary()) ->
                                [{wh_json:key(), non_neg_integer()},...] | [].
maybe_add_answered(Stat, Acc) ->
    maybe_add_answered(Stat, Acc, wh_json:get_value(<<"status">>, Stat)).
maybe_add_answered(Stat, Acc, <<"handled">>) ->
    add_answered(Stat, Acc);
maybe_add_answered(Stat, Acc, <<"processed">>) ->
    add_answered(Stat, Acc);
maybe_add_answered(_, _, _S) ->
    lager:debug("status ~s not indicative of an answered call", [_S]),
    [].

-spec add_answered(wh_json:object(), wh_json:object()) ->
                          [{wh_json:key(), non_neg_integer()},...].
add_answered(Stat, Acc) ->
    AgentId = wh_json:get_value(<<"agent_id">>, Stat),
    QueueId = wh_json:get_value(<<"queue_id">>, Stat),

    AnsweredK = [AgentId, <<"answered_calls">>],
    QAnsweredK = [AgentId, <<"queues">>, QueueId, <<"answered_calls">>],

    Answered = wh_json:get_integer_value(AnsweredK, Acc, 0),
    QAnswered = wh_json:get_integer_value(QAnsweredK, Acc, 0),

    [{AnsweredK, Answered + 1}
     ,{QAnsweredK, QAnswered + 1}
    ].

-spec maybe_add_misses(wh_json:object(), wh_json:object(), ne_binary()) ->
                              wh_json:object().
maybe_add_misses(Stat, Acc, QueueId) ->
    case wh_json:get_value(<<"misses">>, Stat, []) of
        [] -> Acc;
        Misses ->
            lists:foldl(fun(Miss, AccJObj) ->
                                add_miss(Miss, AccJObj, QueueId)
                        end, Acc, Misses)
    end.

-spec add_miss(wh_json:object(), wh_json:object(), ne_binary()) -> wh_json:object().
add_miss(Miss, Acc, QueueId) ->
    AgentId = wh_json:get_value(<<"agent_id">>, Miss),
    MissesK = [AgentId, <<"missed_calls">>],
    QMissesK = [AgentId, <<"queues">>, QueueId, <<"missed_calls">>],

    Misses = wh_json:get_integer_value(MissesK, Acc, 0),
    QMisses = wh_json:get_integer_value(QMissesK, Acc, 0),

    TotalsK = [AgentId, <<"total_calls">>],
    QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],

    Totals = wh_json:get_integer_value(TotalsK, Acc, 0),
    QTotals = wh_json:get_integer_value(QTotalsK, Acc, 0),

    wh_json:set_values([{MissesK, Misses + 1}
                        ,{QMissesK, QMisses + 1}
                        ,{TotalsK, Totals + 1}
                        ,{QTotalsK, QTotals + 1}
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

-define(STATUS_CHANGES, [<<"login">>, <<"logout">>, <<"pause">>, <<"resume">>]).
-spec validate_status_change(cb_context:context(), api_binary()) -> cb_context:context().
validate_status_change(Context, S) ->
    case lists:member(S, ?STATUS_CHANGES) of
        'true' -> validate_status_change_params(Context, S);
        'false' ->
            lager:debug("status ~s not valid", [S]),
            cb_context:add_validation_error(<<"status">>, <<"enum">>, <<"value is not a valid status">>, Context)
    end.

-spec check_for_status_error(cb_context:context(), api_binary()) -> cb_context:context().
check_for_status_error(Context, S) ->
    case lists:member(S, ?STATUS_CHANGES) of
        'true' -> Context;
        'false' ->
            lager:debug("status ~s not found", [S]),
            cb_context:add_validation_error(<<"status">>, <<"enum">>, <<"value is not a valid status">>, Context)
    end.

-spec validate_status_change_params(cb_context:context(), ne_binary()) ->
                                           cb_context:context().
validate_status_change_params(Context, <<"pause">>) ->
    try wh_util:to_integer(cb_context:req_value(Context, <<"timeout">>)) of
        N when N >= 0 -> cb_context:set_resp_status(Context, 'success');
        _N ->
            lager:debug("bad int for pause: ~p", [_N]),
            cb_context:add_validation_error(<<"timeout">>, <<"minimum">>, <<"value must be at least greater than or equal to 0">>, Context)
    catch
        _:_ ->
            lager:debug("bad int for pause"),
            cb_context:add_validation_error(<<"timeout">>, <<"type">>, <<"value must be an integer greater than or equal to 0">>, Context)
    end;
validate_status_change_params(Context, _S) ->
    lager:debug("great success for ~s", [_S]),
    cb_context:set_resp_status(Context, 'success').
