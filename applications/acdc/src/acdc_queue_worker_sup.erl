%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_worker_sup).
-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/3
        ,stop/1
        ,listener/1
        ,shared_queue/1, start_shared_queue/5
        ,fsm/1, start_fsm/3
        ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_ARGS('acdc_queue_listener', [self() | Args])]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:startlink_ret().
start_link(MgrPid, AccountId, QueueId) ->
    supervisor:start_link(?SERVER, [MgrPid, AccountId, QueueId]).

-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(WorkerSup) -> supervisor:terminate_child('acdc_queues_sup', WorkerSup).

-spec listener(pid()) -> kz_term:api_pid().
listener(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec shared_queue(pid()) -> kz_term:api_pid().
shared_queue(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_shared') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec start_shared_queue(pid(), pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_integer()) -> kz_term:sup_startchild_ret().
start_shared_queue(WorkerSup, FSMPid, AccountId, QueueId, Priority) ->
    supervisor:start_child(WorkerSup, ?WORKER_ARGS('acdc_queue_shared', [FSMPid, AccountId, QueueId, Priority])).

-spec fsm(pid()) -> kz_term:api_pid().
fsm(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_fsm') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec start_fsm(pid(), pid(), kz_json:object()) -> kz_term:sup_startchild_ret().
start_fsm(WorkerSup, MgrPid, QueueJObj) ->
    ListenerPid = self(),
    supervisor:start_child(WorkerSup, ?WORKER_ARGS('acdc_queue_fsm', [MgrPid, ListenerPid, QueueJObj])).

-spec child_of_type(pid(), atom()) -> [pid()].
child_of_type(WSup, T) ->
    [P || {Type, P,'worker', [_]} <- supervisor:which_children(WSup), T =:= Type].

-spec status(pid()) -> 'ok'.
status(Supervisor) ->
    ?PRINT("    Worker Supervisor: ~p", [Supervisor]),
    FSM = fsm(Supervisor),
    LPid = listener(Supervisor),
    Shared = shared_queue(Supervisor),

    Status = acdc_queue_fsm:status(FSM),

    ?PRINT("      Listener: ~p", [LPid]),
    ?PRINT("      Shared: ~p", [Shared]),
    ?PRINT("      FSM: ~p", [FSM]),

    print_status(Status).

print_status([]) -> 'ok';
print_status([{_, 'undefined'}|T]) -> print_status(T);
print_status([{K, V}|T]) when is_binary(V) ->
    ?PRINT("        ~s: ~s", [K, V]),
    print_status(T);
print_status([{K, V}|T]) ->
    ?PRINT("        ~s: ~p", [K, V]),
    print_status(T).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
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
