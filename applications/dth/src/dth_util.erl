%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc DTH Utilities.
%%%
%%% @author James Aimonetti <james@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(dth_util).

-export([blacklist_cache_key/0]).

-spec blacklist_cache_key() -> {module(), atom()}.
blacklist_cache_key() ->
    {?MODULE, blacklist}.
