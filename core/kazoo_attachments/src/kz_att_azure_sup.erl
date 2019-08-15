%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_azure_sup).

-behaviour(supervisor).

-include("kz_att.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([start_azure/2]).
-export([workers/0, worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_TYPE('erlazure', 'temporary')]).

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

-spec start_azure(kz_term:ne_binary() | string(), kz_term:ne_binary() | string()) -> kz_types:sup_startchild_ret().
start_azure(Account, Key) when is_binary(Account) ->
    start_azure(kz_term:to_list(Account), Key);
start_azure(Account, Key) when is_binary(Key) ->
    start_azure(Account, kz_term:to_list(Key));
start_azure(Account, Key) ->
    supervisor:start_child(?SERVER, [{local, list_to_atom("erlazure_" ++ Account)}, Account, Key]).


-spec workers() -> kz_term:pids().
workers() ->
    [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER)].

-spec worker(kz_term:ne_binary() | string()) -> kz_term:api_pid().
worker(Name) when is_binary(Name) ->
    worker(kz_term:to_list(Name));
worker(Name) ->

    case [Pid
          || {Worker, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER),
             Worker =:= Name
                 orelse Worker =:= "erlazure_" ++ Name
         ]
    of
        [] -> whereis(list_to_atom("erlazure_" ++ Name));
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
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
