%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Wait for Bindings
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
