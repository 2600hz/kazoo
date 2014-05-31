%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"pivot/crossbar_listing">>).
-define(CB_DEBUG_LIST, <<"pivot/debug_listing">>).

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
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(?DEBUG_PATH_TOKEN, _) ->
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
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
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
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().

validate(Context) -> summary(Context).

validate(Context, ?DEBUG_PATH_TOKEN) -> debug_summary(Context);
validate(Context, Id) -> read(Id, Context).

validate(Context, ?DEBUG_PATH_TOKEN, CallId) -> debug_read(CallId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

-spec debug_read(ne_binary(), cb_context:context()) -> cb_context:context().
debug_read(CallId, Context) ->
    AccountModb = wh_util:format_account_mod_id(cb_context:account_id(Context)),
    crossbar_doc:load_view(?CB_DEBUG_LIST
                           ,[{'endkey', [CallId]}
                             ,{'startkey', [CallId, wh_json:new()]}
                             ,'descending'
                             ,'include_docs'
                             ,{'reduce', 'false'}
                            ]
                           ,cb_context:set_account_db(Context, AccountModb)
                           ,fun normalize_debug_read/2
                          ).

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

-spec debug_summary(cb_context:context()) -> cb_context:context().
debug_summary(Context) ->
    AccountModb = wh_util:format_account_mod_id(cb_context:account_id(Context)),
    crossbar_doc:load_view(?CB_DEBUG_LIST
                           ,[{'group_level', 1}]
                           ,cb_context:set_account_db(Context, AccountModb)
                           ,fun normalize_debug_results/2
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_debug_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_debug_results(JObj, Acc) ->
    [wh_json:get_value([<<"key">>, 1], JObj)|Acc].

-spec normalize_debug_read(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_debug_read(JObj, Acc) ->
    [wh_json:get_value(<<"doc">>, JObj) | Acc].
