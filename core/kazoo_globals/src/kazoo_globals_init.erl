%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Wait for Globals
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kazoo_globals_init).

-export([start_link/0]).

-include("kazoo_globals.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    wait_for_globals('false').

wait_for_globals('true') ->
    lager:info("kazoo globals is ready"),
    'ignore';
wait_for_globals('false') ->
    timer:sleep(?MILLISECONDS_IN_SECOND),
    wait_for_globals(kz_globals:is_ready()).
