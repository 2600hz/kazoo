%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_workers_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/0
         ,new_worker/3, new_workers/4
         ,workers/1, worker_count/1
         ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Restart, Shutdown, Type),
        {Name, {Name, start_link, []}, Restart, Shutdown, Type, [Name]}).

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
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link(?MODULE, []).

new_worker(WorkersSup, AcctId, QueueId) ->
    new_workers(WorkersSup, AcctId, QueueId, 1).

-spec new_workers(pid(), ne_binary(), ne_binary(), integer()) -> 'ok'.
new_workers(_, _,_,N) when N =< 0 -> ok;
new_workers(WorkersSup, AcctId, QueueId, N) when is_integer(N) ->
    _ = supervisor:start_child(WorkersSup, [self(), AcctId, QueueId]),
    new_workers(WorkersSup, AcctId, QueueId, N-1).

-spec workers(pid()) -> [pid(),...] | [].
workers(Super) ->
    [Pid || {_, Pid, supervisor, [_]} <- supervisor:which_children(Super), is_pid(Pid)].

-spec worker_count(pid()) -> non_neg_integer().
worker_count(Super) -> length(supervisor:which_children(Super)).

status(Super) ->
    lager:info("  Workers Supervisor: ~p", [Super]),
    [acdc_queue_worker_sup:status(WorkerSup) || WorkerSup <- workers(Super)].

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
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(acdc_queue_worker_sup, transient, infinity, supervisor)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
