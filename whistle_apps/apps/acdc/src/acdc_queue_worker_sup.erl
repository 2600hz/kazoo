%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_worker_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/3
         ,stop/1
         ,queue/1
         ,shared_queue/1, start_shared_queue/4
         ,fsm/1, start_fsm/3
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Args),
        {Name, {Name, start_link, Args}, transient, 5000, worker, [Name]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/3 :: (pid(), ne_binary(), ne_binary()) -> startlink_ret().
start_link(MgrPid, AcctId, QueueId) ->
    supervisor:start_link(?MODULE, [MgrPid, AcctId, QueueId]).

-spec stop/1 :: (pid()) -> 'ok' | {'error', 'not_found'}.
stop(WorkerSup) ->
    supervisor:terminate_child(acdc_queues_sup, WorkerSup).

-spec queue/1 :: (pid()) -> pid() | 'undefined'.
queue(WorkerSup) ->
    case child_of_type(WorkerSup, acdc_queue) of
        [] -> undefined;
        [P] -> P
    end.

-spec shared_queue/1 :: (pid()) -> pid() | 'undefined'.
shared_queue(WorkerSup) ->
    case child_of_type(WorkerSup, acdc_queue_shared) of
        [] -> undefined;
        [P] -> P
    end.

-spec start_shared_queue/4 :: (pid(), pid(), ne_binary(), ne_binary()) -> sup_startchild_ret().
start_shared_queue(WorkerSup, FSMPid, AcctId, QueueId) ->
    supervisor:start_child(WorkerSup, ?CHILD(acdc_queue_shared, [FSMPid, AcctId, QueueId])).

-spec fsm/1 :: (pid()) -> pid() | 'undefined'.
fsm(WorkerSup) ->
    case child_of_type(WorkerSup, acdc_queue_fsm) of
        [] -> undefined;
        [P] -> P
    end.

-spec start_fsm/3 :: (pid(), pid(), wh_json:object()) -> sup_startchild_ret().
start_fsm(WorkerSup, MgrPid, QueueJObj) ->
    ListenerPid = self(),
    supervisor:start_child(WorkerSup, ?CHILD(acdc_queue_fsm, [MgrPid, ListenerPid, QueueJObj])).

-spec child_of_type/2 :: (pid(), atom()) -> list(pid()).
child_of_type(WorkerSup, T) ->
    [ Pid || {Type, Pid, worker, [_]} <- supervisor:which_children(WorkerSup), T =:= Type].

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
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init/1 :: (list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(acdc_queue_listener, [self() | Args])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
