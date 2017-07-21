%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzc_recordings_sup).

-behaviour(supervisor).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([start_recording/2, stop_recording/1]).
-export([workers/0, worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_TYPE('kzc_recording', 'temporary')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_recording(kapps_call:call(), kz_json:object()) -> sup_startchild_ret().
start_recording(Call, Data) ->
    supervisor:start_child(?SERVER, [Call, Data]).

-spec stop_recording(pid()) -> 'ok'.
stop_recording(Pid) ->
    gen_server:cast(Pid, 'stop_recording').

-spec workers() -> pids().
workers() ->
    [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER)].

-spec worker(ne_binary()) -> api_pid().
worker(Name) ->
    case [Pid
          || {Worker, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER),
             Worker =:= Name
         ]
    of
        [] -> 'undefined';
        [P |_] -> P
    end.

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
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
