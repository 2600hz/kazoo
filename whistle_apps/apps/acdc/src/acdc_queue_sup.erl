%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/2
         ,stop/1
         ,manager/1
         ,workers_sup/1
         ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% api functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(ne_binary(), ne_binary()) -> startlink_ret().
start_link(AcctId, QueueId) ->
    supervisor:start_link(?MODULE, [AcctId, QueueId]).

-spec stop(pid()) -> 'ok' | {'error', 'not_found'}.
stop(Super) ->
    supervisor:terminate_child('acdc_queues_sup', Super).

-spec manager(pid()) -> api_pid().
manager(Super) ->
    hd([P || {_, P, 'worker', _} <- supervisor:which_children(Super)]).

-spec workers_sup(pid()) -> api_pid().
workers_sup(Super) ->
    hd([P || {_, P, 'supervisor', _} <- supervisor:which_children(Super)]).

-spec status(pid()) -> 'ok'.
status(Supervisor) ->
    Manager = manager(Supervisor),
    WorkersSup = workers_sup(Supervisor),

    {AcctId, QueueId} = acdc_queue_manager:config(Manager),

    lager:info("Queue ~s (Account ~s)", [QueueId, AcctId]),
    lager:info("  Supervisor: ~p", [Supervisor]),
    lager:info("  Manager: ~p", [Manager]),

    lager:info("    Known Agents:"),
    case acdc_queue_manager:status(Manager) of
        [] -> lager:info("      NONE");
        As -> [lager:info("      ~s", [A]) || A <- As]
    end,

    _ = acdc_queue_workers_sup:status(WorkersSup),
    'ok'.

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
-spec init(list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = 'one_for_all',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [
                       ?SUPER('acdc_queue_workers_sup')
                       ,?WORKER_ARGS('acdc_queue_manager', [self() | Args])
                      ]
           }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
