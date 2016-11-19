%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @author James Aimonetti <james@2600hz.org>
%%% @doc
%%%
%%% @end
%%% Created : 29 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_util).

-export([blacklist_cache_key/0]).

-spec blacklist_cache_key() -> {module(), atom()}.
blacklist_cache_key() ->
    {?MODULE, blacklist}.
