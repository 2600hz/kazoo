%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
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
        ,shared_queue/1
        ,fsm/1
        ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_ARGS('acdc_queue_listener', [self() | Args])
                  ,?WORKER_ARGS('acdc_queue_shared', [self() | Args])
                  ,?WORKER_ARGS('acdc_queue_fsm', [self() | Args])
                  ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(MgrPid, AcctId, QueueId) ->
    supervisor:start_link(?SERVER, [MgrPid, AcctId, QueueId]).

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

-spec fsm(pid()) -> kz_term:api_pid().
fsm(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_fsm') of
        [] -> 'undefined';
        [P] -> P
    end.

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
