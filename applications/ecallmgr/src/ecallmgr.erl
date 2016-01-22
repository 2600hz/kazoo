%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ecallmgr).

-export([start/0]).


start() ->
    {'ok', _Apps} = application:ensure_all_started(?MODULE).
