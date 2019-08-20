%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_workers_sup).

-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0
        ,new_worker/3, new_workers/4
        ,workers/1, worker_count/1
        ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?SUPER_TYPE('acdc_queue_worker_sup', 'transient')]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link(?SERVER, []).

-spec new_worker(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
new_worker(WorkersSup, AcctId, QueueId) ->
    new_workers(WorkersSup, AcctId, QueueId, 1).

-spec new_workers(pid(), kz_term:ne_binary(), kz_term:ne_binary(), integer()) -> 'ok'.
new_workers(_, _,_,N) when N =< 0 -> 'ok';
new_workers(WorkersSup, AcctId, QueueId, N) when is_integer(N) ->
    _ = supervisor:start_child(WorkersSup, [self(), AcctId, QueueId]),
    new_workers(WorkersSup, AcctId, QueueId, N-1).

-spec workers(pid()) -> kz_term:pids().
workers(Super) ->
    [Pid || {_, Pid, 'supervisor', [_]} <- supervisor:which_children(Super), is_pid(Pid)].

-spec worker_count(pid()) -> non_neg_integer().
worker_count(Super) -> length(supervisor:which_children(Super)).

-spec status(pid()) -> 'ok'.
status(Super) ->
    ?PRINT("  Workers Supervisor: ~p", [Super]),
    lists:foreach(fun acdc_queue_worker_sup:status/1, workers(Super)).

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
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
