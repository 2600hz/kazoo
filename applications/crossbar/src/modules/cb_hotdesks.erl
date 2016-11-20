%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Hotdesks module
%%%
%%% Handle client requests for hotdesks management
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_hotdesks).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").

-define(VIEW_FILE, <<"views/hotdesks.json">>).
-define(CB_LIST, <<"hotdesks/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hotdesks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hotdesks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.hotdesks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.hotdesks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.hotdesks">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.hotdesks">>, ?MODULE, 'delete'),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_hotdesks(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_hotdesks(cb_context:context(), http_method(), kz_proplist()) -> cb_context:context().
validate_hotdesks(Context, ?HTTP_GET, [{<<"hotdesks">>, _}, {<<"users">>, [UserId]}|_]) ->
    fetch_device_hotdesks(UserId, Context);
validate_hotdesks(Context, ?HTTP_GET, [{<<"hotdesks">>, _}, {<<"devices">>, [DeviceId]}|_]) ->
    fetch_user_hotdesks(DeviceId, Context);
validate_hotdesks(Context, ?HTTP_GET, _) ->
    fetch_all_hotdesks(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_all_hotdesks(cb_context:context()) -> cb_context:context().
fetch_all_hotdesks(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

-spec fetch_user_hotdesks(ne_binary(), cb_context:context()) -> cb_context:context().
fetch_user_hotdesks(DeviceId, Context) ->
    Context1 = crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kz_device:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Users = kz_json:get_value([<<"hotdesk">>, <<"users">>], JObj, kz_json:new()),
            fetch_users(kz_json:get_keys(Users), Context1);
        _Else -> Context1
    end.

-spec fetch_users(ne_binaries(), cb_context:context()) -> cb_context:context().
fetch_users(UserIds, Context) ->
    ViewOptions = [{'keys', UserIds}],
    View = <<"users/list_by_id">>,
    crossbar_doc:load_view(View, ViewOptions, Context, fun normalize_view_results/2).

-spec fetch_device_hotdesks(ne_binary(), cb_context:context()) -> cb_context:context().
fetch_device_hotdesks(UserId, Context) ->
    ViewOptions = [{'key', UserId}],
    crossbar_doc:load_view(?CB_LIST, ViewOptions, Context, fun normalize_view_results/2).
