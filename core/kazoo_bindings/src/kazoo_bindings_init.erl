%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Wait for Bindings
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(kazoo_bindings_init).

-export([start_link/0]).

-include("kazoo_bindings.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    wait_for_bindings('false').

wait_for_bindings('true') ->
    lager:info("kazoo bindings is ready"),
    'ignore';
wait_for_bindings('false') ->
    timer:sleep(?MILLISECONDS_IN_SECOND),
    wait_for_bindings(kazoo_bindings:is_ready()).
