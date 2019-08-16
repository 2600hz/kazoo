%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_services_app).

-behaviour(application).

-include("services.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    kapi_bookkeepers:declare_exchanges(),
    kapps_maintenance:bind({'refresh_account', <<"*">>}, 'kazoo_services_maintenance', 'migrate_service_plans'),
    kapps_maintenance:bind_and_register_views('kazoo_services', 'kazoo_services_maintenance', 'register_views'),
    kazoo_services_maintenance:db_init(),
    kazoo_services_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    kapps_maintenance:unbind({'refresh_account', <<"*">>}, 'kazoo_services_maintenance', 'migrate_service_plans'),
    kapps_maintenance:unbind('register_views', 'kazoo_services_maintenance', 'register_views'),
    'ok'.
