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
-module(acdc_queue_sup).
-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/2
        ,stop/1
        ,manager/1
        ,workers_sup/1
        ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?SUPER('acdc_queue_workers_sup')
                  ,?WORKER_ARGS('acdc_queue_manager', [self() | Args])
                  ]).

%%%=============================================================================
%%% api functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(AcctId, QueueId) ->
    supervisor:start_link(?SERVER, [AcctId, QueueId]).

-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(Super) ->
    supervisor:terminate_child('acdc_queues_sup', Super).

-spec manager(pid()) -> kz_term:api_pid().
manager(Super) ->
    hd([P || {_, P, 'worker', _} <- supervisor:which_children(Super)]).

-spec workers_sup(pid()) -> kz_term:api_pid().
workers_sup(Super) ->
    hd([P || {_, P, 'supervisor', _} <- supervisor:which_children(Super)]).

-spec status(pid()) -> 'ok'.
status(Supervisor) ->
    Manager = manager(Supervisor),
    WorkersSup = workers_sup(Supervisor),

    {AcctId, QueueId} = acdc_queue_manager:config(Manager),

    ?PRINT("Queue ~s (Account ~s)", [QueueId, AcctId]),
    ?PRINT("  Supervisor: ~p", [Supervisor]),
    ?PRINT("  Manager: ~p", [Manager]),

    ?PRINT("    Known Agents:"),
    _ = case acdc_queue_manager:status(Manager) of
            [] -> ?PRINT("      NONE");
            As -> [?PRINT("      ~s", [A]) || A <- As]
        end,

    _ = acdc_queue_workers_sup:status(WorkersSup),
    'ok'.

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
-spec init(list()) -> kz_types:sup_init_ret().
init(Args) ->
    RestartStrategy = 'one_for_all',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
