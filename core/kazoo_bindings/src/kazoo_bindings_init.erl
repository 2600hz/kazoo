%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Wait for Bindings
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_bindings_init).

-export([start_link/0]).

-include("kazoo_bindings.hrl").

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    wait_for_bindings('false').

wait_for_bindings('true') ->
    lager:info("kazoo bindings is ready"),
    'ignore';
wait_for_bindings('false') ->
    timer:sleep(?MILLISECONDS_IN_SECOND),
    wait_for_bindings(kazoo_bindings:is_ready()).
