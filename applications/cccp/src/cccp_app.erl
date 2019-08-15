%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author OnNet (Kirill Sysoev github.com/onnet)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cccp_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = kapps_maintenance:bind_and_register_views('cccp', 'cccp_util', 'register_views'),
    cccp_util:init_db(),
    _ = declare_exchanges(),
    cccp_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind('register_views', 'cccp_util', 'register_views'),
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kapi_self:declare_exchanges(),
    kapi_call:declare_exchanges(),
    kapi_route:declare_exchanges(),
    kapi_resource:declare_exchanges(),
    kapi_offnet_resource:declare_exchanges().
