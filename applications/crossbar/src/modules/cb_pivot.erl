%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Crossbar API for pivot.
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.pivot">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.pivot">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.pivot">>, ?MODULE, 'validate').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?DEBUG_PATH_TOKEN) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?DEBUG_PATH_TOKEN, _UUID) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /pivot => []
%%    /pivot/foo => [<<"foo">>]
%%    /pivot/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?DEBUG_PATH_TOKEN) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?DEBUG_PATH_TOKEN, _) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /pivot might load a list of pivot objects
%% /pivot/123 might load the pivot object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?DEBUG_PATH_TOKEN) ->
    debug_summary(Context).

-spec validate(cb_context:context(), path_token(), path_token()) ->cb_context:context().
validate(Context, ?DEBUG_PATH_TOKEN, CallId) ->
    debug_read(Context, CallId).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% Proper pagination is merely impossible for this API since we debugs
%% are in two documents, setting limit to 2 * PageSize is a bad choice.
%% What if the response or even the request debug document is not exists?
%% @end
%%------------------------------------------------------------------------------
-spec debug_summary(cb_context:context()) -> cb_context:context().
debug_summary(Context) ->
    ViewOptions = [{'mapper', crossbar_view:get_value_fun()}
                  ,{'range_keymap', 'nil'}
                  ,{'limit', limit_by_page_size(Context)}
                  ],
    maybe_normalize_debug_results(crossbar_view:load_modb(Context, ?CB_FIRST_ITERATION, ViewOptions)).

%%------------------------------------------------------------------------------
%% @doc Pivot debugs are store in two documents, request debug and response
%% debug. If page_size is requested set limit accordingly to return
%% both documents.
%% @end
%%------------------------------------------------------------------------------
-spec limit_by_page_size(cb_context:context()) -> kz_term:api_pos_integer().
limit_by_page_size(Context) ->
    case cb_context:pagination_page_size(Context) of
        Size when is_integer(Size), Size > 0 ->
            Size * 2;
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec debug_read(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
debug_read(Context, ?MATCH_MODB_PREFIX(Year, Month, CallId)) ->
    AccountModb = kazoo_modb:get_modb(cb_context:account_id(Context), Year, Month),
    Options = [{'databases', [AccountModb]}
              ,{'endkey', [CallId, kz_json:new()]}
              ,{'mapper', fun normalize_debug_read/2}
              ,{'startkey', [CallId]}
              ,'include_docs'
              ],
    Context1 = crossbar_view:load(Context, ?CB_DEBUG_LIST, Options),
    case cb_context:resp_status(Context1) of
        'success' ->
            RespData = cb_context:resp_data(Context1),
            cb_context:set_resp_data(Context1, lists:reverse(RespData));
        _Status -> Context1
    end;
debug_read(Context, CallId) ->
    ViewOptions = [{'mapper', fun normalize_debug_read/2}
                  ,{'range_keymap', CallId}
                  ,{'unchunkable', 'true'}
                  ,'include_docs'
                  ],
    crossbar_view:load_modb(Context, ?CB_DEBUG_LIST, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_normalize_debug_results(cb_context:context()) -> cb_context:context().
maybe_normalize_debug_results(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> normalize_debug_results(Context);
        _ -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_debug_results(cb_context:context()) -> cb_context:context().
normalize_debug_results(Context) ->
    Dir = crossbar_view:direction(Context),
    {_, RespData} = lists:foldl(fun normalize_debug_results_fold/2, {sets:new(), []}, cb_context:resp_data(Context)),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, lists:sort(fun(A, B) -> sort_by_created(A, B, Dir) end, RespData)}
                       ,fun fix_page_size/1
                       ]
                      ).

%%------------------------------------------------------------------------------
%% @doc Since view key is Call-ID, we're sure debug docs for the same Call-ID
%% are adjacent to each other. If we previously visited the Call-ID
%% the next doc with the same Call-ID are merged with list's head.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_debug_results_fold(kz_json:object(), {sets:set(), kz_json:objects()}) -> {sets:set(), kz_json:objects()}.
normalize_debug_results_fold(JObj, {Set, []}) ->
    CallId = kz_json:get_value(<<"call_id">>, JObj),
    NewJObj = kz_json:set_value(<<"debug_id">>, debug_id(JObj), JObj),

    {sets:add_element(CallId, Set), [NewJObj]};
normalize_debug_results_fold(JObj, {Set, [H|Rest]=JObjAcc}) ->
    CallId = kz_json:get_value(<<"call_id">>, JObj),
    NewJObj = kz_json:set_value(<<"debug_id">>, debug_id(JObj), JObj),

    case sets:is_element(CallId, Set) of
        'false' -> {sets:add_element(CallId, Set), [NewJObj|JObjAcc]};
        'true' -> {Set, [kz_json:merge_jobjs(H, NewJObj)|Rest]}
    end.

-spec sort_by_created(kz_json:object(), kz_json:object(), crossbar_view:direction()) -> boolean().
sort_by_created(A, B, 'ascending') ->
    kz_json:get_integer_value(<<"created">>, A, 0) < kz_json:get_integer_value(<<"created">>, B, 1);
sort_by_created(A, B, 'descending') ->
    kz_json:get_integer_value(<<"created">>, A, 0) > kz_json:get_integer_value(<<"created">>, B, 1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_page_size(cb_context:context()) -> cb_context:context().
fix_page_size(Context) ->
    RespEnv = cb_context:resp_envelope(Context),
    RespData = cb_context:resp_data(Context),
    Size = erlang:length(RespData),
    cb_context:set_resp_envelope(Context
                                ,kz_json:set_value(<<"page_size">>, Size, RespEnv)
                                ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_debug_read(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_debug_read(JObj, Acc) ->
    [leak_pvt_field(kz_json:get_value(<<"doc">>, JObj)) | Acc].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

-spec debug_id(kz_json:object()) -> kz_term:ne_binary().
debug_id(JObj) ->
    Created = kz_json:get_first_defined([<<"created">>, <<"pvt_created">>], JObj),
    CallId = kz_json:get_value(<<"call_id">>, JObj),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Created),
    ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), CallId).
