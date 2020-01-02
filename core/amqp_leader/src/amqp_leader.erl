%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_leader).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-export([start_link/6]).


-spec start_link(atom(), kz_term:atoms(), list(), atom(), list(), list()) -> kz_types:startlink_ret().
start_link(Name, Nodes, Opts, Module, [], []) ->
    amqp_leader_sup:start_leader(Name, Nodes, Opts, Module, [], []).
