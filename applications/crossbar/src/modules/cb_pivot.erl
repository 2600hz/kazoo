%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_pivot).

-export([init/0
        ,allowed_methods/1, allowed_methods/2
        ,resource_exists/1, resource_exists/2
        ,validate/2, validate/3
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"pivot/crossbar_listing">>).
-define(CB_DEBUG_LIST, <<"pivot/debug_listing">>).
-define(CB_FIRST_ITERATION, <<"pivot/first_iteration">>).

-define(DEBUG_PATH_TOKEN, <<"debug">>).

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
    _ = crossbar_bindings:bind(<<"*.allowed_methods.pivot">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.pivot">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.pivot">>, ?MODULE, 'validate').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?DEBUG_PATH_TOKEN) ->
    [?HTTP_GET].
allowed_methods(?DEBUG_PATH_TOKEN, _UUID) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /pivot => []
%%    /pivot/foo => [<<"foo">>]
%%    /pivot/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?DEBUG_PATH_TOKEN) -> 'true'.
resource_exists(?DEBUG_PATH_TOKEN, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /pivot mights load a list of pivot objects
%% /pivot/123 might load the pivot object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().

validate(Context, ?DEBUG_PATH_TOKEN) ->
    debug_summary(Context).

validate(Context, ?DEBUG_PATH_TOKEN, CallId) ->
    debug_read(Context, CallId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec debug_summary(cb_context:context()) -> cb_context:context().
debug_summary(Context) ->
    case modb_view_options(Context, 'undefined') of
        {'ok', ViewOptions} ->
            maybe_normalize_debug_results(
              crossbar_doc:load_view(?CB_FIRST_ITERATION
                                    ,ViewOptions
                                    ,fix_req_pagination(Context)
                                    ,fun normalize_view_results/2
                                    )
             );
        C -> C
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_req_pagination(cb_context:context()) -> cb_context:context().
fix_req_pagination(Context) ->
    QS = cb_context:query_string(Context),
    Size = crossbar_doc:pagination_page_size(Context),
    cb_context:set_query_string(Context, kz_json:set_value(<<"page_size">>, Size * 2 + 1, QS)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec debug_read(cb_context:context(), ne_binary()) -> cb_context:context().
debug_read(Context, ?MATCH_MODB_PREFIX(Year, Month, CallId)) ->
    AccountModb = kazoo_modb:get_modb(cb_context:account_id(Context), Year, Month),
    Context1 =
        crossbar_doc:load_view(?CB_DEBUG_LIST
                              ,[{'endkey', [CallId, kz_json:new()]}
                               ,{'startkey', [CallId]}
                               ,'include_docs'
                               ]
                              ,cb_context:set_account_db(Context, AccountModb)
                              ,fun normalize_debug_read/2
                              ),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = cb_context:resp_data(Context1),
            cb_context:set_resp_data(Context1, lists:reverse(RespData));
        _Status -> Context1
    end;
debug_read(Context, CallId) ->
    case modb_view_options(Context, CallId) of
        {'ok', ViewOptions} ->
            crossbar_doc:load_view(?CB_DEBUG_LIST, ViewOptions, Context, fun normalize_debug_read/2);
        C -> C
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec modb_view_options(cb_context:context(), api_ne_binary()) ->
                               {'ok', crossbar_doc:view_options()} |
                               cb_context:context().
modb_view_options(Context, CallId) ->
    AccountId = cb_context:account_id(Context),
    case cb_modules_util:range_view_options(Context) of
        {CreatedFrom, CreatedTo} ->
            MODBs = lists:reverse(kazoo_modb:get_range(AccountId, CreatedFrom, CreatedTo)),
            {'ok', maybe_add_start_end_key(CallId, CreatedTo, CreatedFrom, MODBs)};
        Ctx -> Ctx
    end.

-spec maybe_add_start_end_key(api_binary(), gregorian_seconds(), gregorian_seconds(), ne_binaries()) ->
                                     crossbar_doc:view_options().
maybe_add_start_end_key('undefined', _CreatedTo, _CreatedFrom, MODBs) ->
    [{'databases', MODBs}
    ,'descending'
    ];
maybe_add_start_end_key(CallId, CreatedTo, CreatedFrom, MODBs) ->
    [{'databases', MODBs}
    ,{'startkey', [CallId, CreatedTo]}
    ,{'endkey', [CallId, CreatedFrom]}
    ,'include_docs'
    ,'descending'
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the results of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_normalize_debug_results(cb_context:context()) -> cb_context:context().
maybe_normalize_debug_results(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> normalize_debug_results(Context);
        _ -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_debug_results(cb_context:context()) -> cb_context:context().
-spec normalize_debug_results(cb_context:context(), kz_proplist()) -> kz_json:objects().
normalize_debug_results(Context) ->
    Dict =
        lists:foldl(fun normalize_debug_results_fold/2
                   ,dict:new()
                   ,lists:reverse(cb_context:resp_data(Context))
                   ),
    RespData = normalize_debug_results(Context, dict:to_list(Dict)),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, RespData}
                       ,fun fix_page_size/1
                       ]
                      ).

normalize_debug_results(Context, List) ->
    Size = kz_term:to_integer((crossbar_doc:pagination_page_size(Context)-1)/2),
    FinalList =
        case erlang:length(List) > Size of
            'false' -> List;
            'true' ->
                {L2, _} = lists:split(Size, List),
                L2
        end,
    [Flow || {_CallId, Flow} <- FinalList].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_debug_results_fold(kz_json:object(), dict:dict()) -> dict:dict().
normalize_debug_results_fold(JObj, Dict) ->
    CallId = kz_json:get_value(<<"call_id">>, JObj),
    NewJObj = kz_json:set_value(<<"debug_id">>, debug_id(JObj), JObj),
    case dict:find(CallId, Dict) of
        'error' -> dict:store(CallId, NewJObj, Dict);
        {'ok', Value} ->
            dict:store(CallId, kz_json:merge_jobjs(Value, NewJObj), Dict)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_page_size(cb_context:context()) -> cb_context:context().
fix_page_size(Context) ->
    RespEnv = cb_context:resp_envelope(Context),
    RespData = cb_context:resp_data(Context),
    Size = erlang:length(RespData),
    cb_context:set_resp_envelope(Context
                                ,kz_json:set_value(<<"page_size">>, Size, RespEnv)
                                ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize_debug_read(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_debug_read(JObj, Acc) ->
    [leak_pvt_field(kz_json:get_value(<<"doc">>, JObj)) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec leak_pvt_field(kz_json:object()) -> kz_json:object().
leak_pvt_field(JObj) ->
    Routines = [fun leak_pvt_created/2
               ,fun leak_pvt_node/2
               ,fun set_debug_id/2
               ],
    lists:foldl(fun(F, Acc) -> F(JObj, Acc) end
               ,JObj
               ,Routines
               ).

-spec leak_pvt_created(kz_json:object(), kz_json:object()) -> kz_json:object().
leak_pvt_created(JObj, Acc) ->
    kz_json:set_value(<<"created">>, kz_doc:created(JObj), Acc).

-spec leak_pvt_node(kz_json:object(), kz_json:object()) -> kz_json:object().
leak_pvt_node(JObj, Acc) ->
    Node = kz_json:get_value([<<"pvt_node">>], JObj),
    kz_json:set_value(<<"node">>, Node, Acc).

-spec set_debug_id(kz_json:object(), kz_json:objects()) -> kz_json:object().
set_debug_id(JObj, Acc) ->
    kz_json:set_value(<<"debug_id">>, debug_id(JObj), Acc).

-spec debug_id(kz_json:object()) -> ne_binary().
debug_id(JObj) ->
    Created = kz_json:get_first_defined([<<"created">>, <<"pvt_created">>], JObj),
    CallId = kz_json:get_value(<<"call_id">>, JObj),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Created),
    ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_time:pad_month(Month), CallId).
