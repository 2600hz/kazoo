%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(amqp_leader).

-include_lib("kazoo/include/kz_types.hrl").

-export([start_link/6]).


-spec start_link(atom(), atoms(), list(), atom(), list(), list()) -> startlink_ret().
start_link(Name, Nodes, Opts, Module, [], []) ->
    amqp_leader_sup:start_leader(Name, Nodes, Opts, Module, [], []).
