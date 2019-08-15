%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gcm_sup).
-behaviour(supervisor).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(CHILDREN, [?WORKER_TYPE('gcm', 'transient')]).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_child(atom(),string()) -> kz_types:sup_startchild_ret().
start_child(Name, ApiKey) ->
    supervisor:start_child(?SERVER, [Name, ApiKey]).

-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
