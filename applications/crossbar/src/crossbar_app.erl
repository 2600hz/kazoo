%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_app).
-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-include("crossbar.hrl").

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    declare_exchanges(),
    _ = kapps_maintenance:bind('migrate', 'crossbar_maintenance', 'migrate'),
    _ = kapps_maintenance:bind({'refresh_account', <<"*">>}, 'crossbar_util', 'descendants_count'),
    kapps_maintenance:bind_and_register_views('crossbar', 'crossbar_maintenance', 'register_views'),
    _ = crossbar_maintenance:db_init(),
    crossbar_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kapps_maintenance:unbind('migrate', 'crossbar_maintenance', 'migrate'),
    _ = kapps_maintenance:unbind({'refresh_account', <<"*">>}, 'crossbar_util', 'descendants_count'),
    _ = kapps_maintenance:unbind('register_views', 'crossbar_maintenance', 'register_views'),
    _ = cowboy:stop_listener('api_resource'),
    _ = cowboy:stop_listener('api_resource_ssl'),
    _ = crossbar_bindings:flush(),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Ensures that all exchanges used are declared.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_money:declare_exchanges(),
    _ = kapi_conference:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    _ = kapi_presence:declare_exchanges(),
    _ = kapi_registration:declare_exchanges(),
    _ = kapi_resource:declare_exchanges(),
    _ = kapi_switch:declare_exchanges(),
    _ = kapi_sysconf:declare_exchanges(),
    _ = kapi_call:declare_exchanges(),
    _ = kapi_dialplan:declare_exchanges(),
    kapi_self:declare_exchanges().
