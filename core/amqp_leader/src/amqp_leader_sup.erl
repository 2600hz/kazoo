%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_leader_sup).

-behaviour(supervisor).

-export([start_leader/6]).
-export([start_link/0]).
-export([init/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, []).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_leader(atom(), kz_term:atoms(), list(), atom(), list(), list()) -> kz_types:startlink_ret().
start_leader(Name, Nodes, Opts, Module, [], []) ->
    supervisor:start_child(?SERVER
                          ,?SUPER_NAME_ARGS_TYPE(Name
                                                ,'amqp_leader_proc_sup'
                                                ,[Name, Nodes, Opts, Module, [], []]
                                                ,'transient')).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
