%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_endpoint_maintenance).

-export([flush/0]).

-include("kazoo_endpoint.hrl").

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).
