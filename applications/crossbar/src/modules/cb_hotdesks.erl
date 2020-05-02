%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Hotdesk module
%%% Handle client requests for hotdesk management
%%%
%%%
%%% @author Edouard Swiac
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_hotdesks).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.hotdesks">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.hotdesks">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.hotdesks">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.hotdesks">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.hotdesks">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.hotdesks">>, ?MODULE, 'delete'),
    ok.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_hotdesks(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_hotdesks(cb_context:context(), http_method(), kz_term:proplist()) -> cb_context:context().
validate_hotdesks(Context, ?HTTP_GET, [{<<"hotdesks">>, _}, {<<"users">>, [UserId]}|_]) ->
    fetch_device_hotdesks(UserId, Context);
validate_hotdesks(Context, ?HTTP_GET, [{<<"hotdesks">>, _}, {<<"devices">>, [DeviceId]}|_]) ->
    fetch_user_hotdesks(DeviceId, Context);
validate_hotdesks(Context, ?HTTP_GET, _) ->
    fetch_all_hotdesks(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_all_hotdesks(cb_context:context()) -> cb_context:context().
fetch_all_hotdesks(Context) ->
    Options = [{'startkey', [<<"hotdesk">>]}
              ,{'endkey', [<<"hotdesk">>, kz_datamgr:view_highest_value()]}
              ,{'mapper', crossbar_view:get_value_fun()}
              ],
    crossbar_view:load(Context, ?KZD_LIST_BY_TYPE_ID, Options).

-spec fetch_user_hotdesks(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
fetch_user_hotdesks(DeviceId, Context) ->
    Context1 = crossbar_doc:load(DeviceId, Context, ?TYPE_CHECK_OPTION(kzd_devices:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            Users = kz_json:get_value([<<"hotdesk">>, <<"users">>], JObj, kz_json:new()),
            fetch_users(kz_json:get_keys(Users), Context1);
        _Else -> Context1
    end.

-spec fetch_users(kz_term:ne_binaries(), cb_context:context()) -> cb_context:context().
fetch_users(UserIds, Context) ->
    ViewOptions = [{'keys', [[kzd_users:type(), UserId] || UserId <- UserIds]}
                  ,{'mapper', crossbar_view:get_value_fun()}
                  ],
    crossbar_view:load(Context, ?KZD_LIST_BY_TYPE_ID, ViewOptions).

-spec fetch_device_hotdesks(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
fetch_device_hotdesks(UserId, Context) ->
    Options = [{'key', [<<"hotdesk">>, UserId]}
              ,{'mapper', crossbar_view:get_value_fun()}
              ],
    crossbar_view:load(Context, ?KZD_LIST_BY_TYPE_ID, Options).
