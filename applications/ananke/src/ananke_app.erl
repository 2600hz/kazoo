%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ananke_app).
-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = kapi_self:declare_exchanges(),
    _ = kapi_notifications:declare_exchanges(),
    ananke_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) -> 'ok'.
