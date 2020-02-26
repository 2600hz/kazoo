%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc API for retrieving attachment handlers errors.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_att_handlers_errors).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2, validate/3
        ]).

-include("crossbar.hrl").

-define(CB_LIST_ALL, <<"att_handlers_errors/crossbar_listing">>).
-define(CB_LIST_BY_HANDLER, <<"att_handlers_errors/list_by_handler">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods">>, 'allowed_methods'}
               ,{<<"*.resource_exists">>, 'resource_exists'}
               ,{<<"*.validate">>, 'validate'}
               ],
    _ = [crossbar_bindings:bind(<<Binding/binary, ".att_handlers_errors">>, ?MODULE, Fun)
         || {Binding, Fun} <- Bindings
        ],
    ok.

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_ErrorId) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(<<"handler">>, _HandlerId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource?
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_ErrorId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(<<"handler">>, _HandlerId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_requests(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_request(Context, Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Resource, Id) ->
    validate_request(Context, Resource, Id, cb_context:req_verb(Context)).

-spec validate_requests(cb_context:context(), http_method()) -> cb_context:context().
validate_requests(Context, ?HTTP_GET) ->
    summary(Context).

-spec validate_request(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_request(Context, Id, ?HTTP_GET) ->
    read(Id, Context).

-spec validate_request(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().
validate_request(Context, Resource, Id, ?HTTP_GET) ->
    read(Resource, Id, Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(<<Year:4/binary, Month:2/binary, _/binary>> = Id, Context) ->
    MODB = kazoo_modb:get_modb(cb_context:account_id(Context), Year, Month),
    Context1 = cb_context:set_db_name(Context, MODB),
    crossbar_doc:load(Id, Context1, ?TYPE_CHECK_OPTION(<<"attachment_handler_error">>)).

-spec read(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(<<"handler">>, HandlerId, Context) ->
    Options = [{'mapper', crossbar_view:get_value_fun()}
              ,{'range_keymap', [HandlerId]}
              ],
    crossbar_view:load_modb(Context, ?CB_LIST_BY_HANDLER, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    Options = [{'mapper', crossbar_view:get_value_fun()}],
    crossbar_view:load_modb(Context, ?CB_LIST_ALL, Options).
