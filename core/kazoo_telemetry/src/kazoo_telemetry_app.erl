%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_telemetry_app).

-behaviour(application).

-export([start/2
        ,stop/1
        ,request/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = kz_util:set_startup(),
    _ = kz_nodes_bindings:bind('kazoo_telemetry'),
    kazoo_telemetry_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    _ = kz_nodes_bindings:unbind('kazoo_telemetry'),
    'ok'.

-spec request(kz_nodes:request_acc()) -> kz_nodes:request_acc().
request(Acc) -> Acc.
