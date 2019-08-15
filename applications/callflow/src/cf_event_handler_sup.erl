%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_event_handler_sup).

-behaviour(supervisor).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([new/3]).
-export([workers/0, worker/1]).

%% Supervisor callbacks
-export([init/1]).

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

-spec new(any(), atom(), list()) -> kz_types:sup_startchild_ret().
new(Name, M, A) ->
    supervisor:start_child(?SERVER, ?WORKER_NAME_ARGS_TYPE(Name, M, A, 'temporary')).

-spec workers() -> kz_term:pids().
workers() ->
    [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER)].

-spec worker(kz_term:ne_binary()) -> kz_term:api_pid().
worker(Name) ->
    case [Pid || {Worker, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER), Worker =:= Name] of
        [] -> 'undefined';
        [P |_] -> P
    end.

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
