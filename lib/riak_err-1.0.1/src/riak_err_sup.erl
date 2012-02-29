%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(riak_err_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% From supervisor
-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%% @doc Starts the supervisor
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% R14B spec is commented, use R13B spec for now.
%-spec init([]) -> {ok, {{one_for_one, 1000, 3600}, [supervisor:child_spec()]}}.
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 1000, 3600}, [tuple()]}}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,

    Monitor = {riak_err_monitor, {riak_err_monitor, start_link, []},
                Restart, Shutdown, worker, [riak_err_monitor]},

    {ok, {SupFlags, [Monitor]}}.
