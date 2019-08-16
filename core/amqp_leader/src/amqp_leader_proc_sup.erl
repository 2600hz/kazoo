%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_leader_proc_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

-include("amqp_leader.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_ARGS('amqp_leader_listener', [Name])
                  ,?WORKER_ARGS('amqp_leader_proc', [Name, Nodes, Opts, Module, [], []])
                  ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:atoms(), list(), atom(), [], []) -> kz_types:startlink_ret().
start_link(Name, Nodes, Opts, Module, [], []) ->
    supervisor:start_link(?SERVER, {Name, Nodes, Opts, Module, [], []}).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init({atom(), kz_term:atoms(), list(), atom(), [], []}) -> kz_types:sup_init_ret().
init({Name, Nodes, Opts, Module, [], []}) ->
    RestartStrategy = 'rest_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
