%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_requests_sup).

-behaviour(supervisor).

-include("fax.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([new/3]).
-export([workers/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_TYPE('fax_request', 'temporary')]).

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

-spec new(kapps_call:call(), kz_json:object(), fax_storage()) -> kz_types:sup_startchild_ret().
new(Call, JObj, Storage) ->
    supervisor:start_child(?SERVER, [Call, JObj, Storage]).

-spec workers() -> kz_term:pids().
workers() ->
    [ Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER)].

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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
