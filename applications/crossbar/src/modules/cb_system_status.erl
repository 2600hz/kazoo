%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @author kirill.sysoev@gmail.com
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_system_status).

-export([init/0
        ,authorize/1
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").
-type authorize_return() :: boolean() | {'stop', cb_context:context()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize.system_status">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.system_status">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.system_status">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.system_status">>, ?MODULE, 'validate').

-spec authorize(cb_context:context()) -> authorize_return().
authorize(Context) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> 'true';
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end.

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_summary(Context, cb_context:req_verb(Context)).

-spec validate_summary(cb_context:context(), http_method()) -> cb_context:context().
validate_summary(Context, ?HTTP_GET) ->
    summary(Context).

-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    StatusJObj = kz_nodes:status_to_json(),
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, StatusJObj}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).
