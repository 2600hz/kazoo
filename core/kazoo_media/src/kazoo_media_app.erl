%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_media_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    Ret = kazoo_media_sup:start_link(),
    kapps_maintenance:bind_and_register_views('kazoo_media', 'kazoo_media_maintenance', 'register_views'),
    kapps_maintenance:bind('migrate', 'kazoo_media_maintenance', 'migrate'),
    kazoo_media_maintenance:refresh(),
    Ret.

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    kapps_maintenance:unbind('migrate', 'kazoo_media_maintenance', 'migrate'),
    kapps_maintenance:unbind('register_views', 'kazoo_media_maintenance', 'register_views'),
    'ok'.
