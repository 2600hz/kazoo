%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_endpoint_maintenance).

-export([flush/0]).

-include("kazoo_endpoint.hrl").

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).
