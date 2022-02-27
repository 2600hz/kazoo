%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Wait for Globals
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_globals_init).

-export([start_link/0]).

-include("kazoo_globals.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    wait_for_globals('false').

wait_for_globals('true') ->
    lager:info("kazoo globals is ready"),
    'ignore';
wait_for_globals('false') ->
    timer:sleep(?MILLISECONDS_IN_SECOND),
    wait_for_globals(kz_globals:is_ready()).
