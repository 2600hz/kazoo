%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queues_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/0
         ,new/2
         ,workers/0
         ,find_acct_supervisors/1
         ,find_queue_supervisor/2
         ,queues_running/0
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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new(AcctId, QueueId) ->
    case find_queue_supervisor(AcctId, QueueId) of
        P when is_pid(P) -> {ok, P};
        undefined -> supervisor:start_child(?MODULE, [AcctId, QueueId])
    end.

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [_]} <- supervisor:which_children(?MODULE), is_pid(Pid)].

-spec find_acct_supervisors/1 :: (ne_binary()) -> [pid(),...] | [].
find_acct_supervisors(AcctId) ->
    [Super || Super <- workers(), is_queue_in_acct(Super, AcctId)].

-spec is_queue_in_acct/2 :: (pid(), ne_binary()) -> boolean().
is_queue_in_acct(Super, AcctId) ->
    case catch acdc_queue_manager:config(acdc_queue_sup:manager(Super)) of
        {'EXIT', _} -> false;
        {AcctId, _} -> true;
        _ -> false
    end.

-spec find_queue_supervisor/2 :: (ne_binary(), ne_binary()) -> pid() | 'undefined'.
-spec find_queue_supervisor/3 :: (ne_binary(), ne_binary(), [pid(),...] | []) -> pid() | 'undefined'.
find_queue_supervisor(AcctId, QueueId) ->
    find_queue_supervisor(AcctId, QueueId, workers()).

find_queue_supervisor(_AcctId, _QueueId, []) -> undefined;
find_queue_supervisor(AcctId, QueueId, [Super|Rest]) ->
    case catch acdc_queue_manager:config(acdc_queue_sup:manager(Super)) of
        {'EXIT', _} -> find_queue_supervisor(AcctId, QueueId, Rest);
        {AcctId, QueueId} -> Super;
        _ -> find_queue_supervisor(AcctId, QueueId, Rest)
    end.

queues_running() ->
    [{W, catch acdc_queue_manager:config(acdc_queue_sup:manager(W))} || W <- workers()].

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

    {ok, {SupFlags, [?CHILD(acdc_queue_sup, transient, infinity, supervisor)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
