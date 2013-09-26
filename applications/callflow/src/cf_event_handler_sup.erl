%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_event_handler_sup).

-behaviour(supervisor).

-include("callflow.hrl").

%% API
-export([start_link/0]).
-export([new/3]).
-export([workers/0]).

%% Supervisor callbacks
-export([init/1]).

-include("callflow.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec new(term(), atom(), list()) -> sup_startchild_ret().
new(Name, M, A) ->
    supervisor:start_child(?MODULE, ?WORKER_NAME_ARGS_TYPE(Name, M, A, 'temporary')).

-spec workers() -> pids().
workers() ->
    [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, []}}.
