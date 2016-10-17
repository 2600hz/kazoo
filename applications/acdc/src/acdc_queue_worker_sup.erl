%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
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

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link(pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(MgrPid, AcctId, QueueId) ->
    supervisor:start_link(?SERVER, [MgrPid, AcctId, QueueId]).

-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(WorkerSup) -> supervisor:terminate_child('acdc_queues_sup', WorkerSup).

-spec listener(pid()) -> api_pid().
listener(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec shared_queue(pid()) -> api_pid().
shared_queue(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_shared') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec start_shared_queue(pid(), pid(), ne_binary(), ne_binary(), api_integer()) -> sup_startchild_ret().
start_shared_queue(WorkerSup, FSMPid, AcctId, QueueId, Priority) ->
    supervisor:start_child(WorkerSup, ?WORKER_ARGS('acdc_queue_shared', [FSMPid, AcctId, QueueId, Priority])).

-spec fsm(pid()) -> api_pid().
fsm(WorkerSup) ->
    case child_of_type(WorkerSup, 'acdc_queue_fsm') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec start_fsm(pid(), pid(), kz_json:object()) -> sup_startchild_ret().
start_fsm(WorkerSup, MgrPid, QueueJObj) ->
    ListenerPid = self(),
    supervisor:start_child(WorkerSup, ?WORKER_ARGS('acdc_queue_fsm', [MgrPid, ListenerPid, QueueJObj])).

-spec child_of_type(pid(), atom()) -> [pid()].
child_of_type(WSup, T) ->
    [P || {Type, P,'worker', [_]} <- supervisor:which_children(WSup), T =:= Type].

-spec status(pid()) -> 'ok'.
status(Supervisor) ->
    lager:info("    Worker Supervisor: ~p", [Supervisor]),
    FSM = fsm(Supervisor),
    LPid = listener(Supervisor),
    Shared = shared_queue(Supervisor),

    Status = acdc_queue_fsm:status(FSM),

    lager:info("      Listener: ~p", [LPid]),
    lager:info("      Shared: ~p", [Shared]),
    lager:info("      FSM: ~p", [FSM]),

    print_status(Status).

print_status([]) -> 'ok';
print_status([{_, 'undefined'}|T]) -> print_status(T);
print_status([{K, V}|T]) when is_binary(V) ->
    lager:info("        ~s: ~s", [K, V]),
    print_status(T);
print_status([{K, V}|T]) ->
    lager:info("        ~s: ~p", [K, V]),
    print_status(T).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = 'one_for_all',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
