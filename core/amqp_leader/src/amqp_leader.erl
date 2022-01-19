%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_leader).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start_link/6]).


-spec start_link(atom(), kz_term:atoms(), list(), atom(), list(), list()) -> kz_types:startlink_ret().
start_link(Name, Nodes, Opts, Module, [], []) ->
    amqp_leader_sup:start_leader(Name, Nodes, Opts, Module, [], []).
