%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Simple-One-For-One strategy for restarting call event processes
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_call_event_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_proc/1]).
-export([init/1]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('ecallmgr_call_events', 'transient')]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_proc(list()) -> sup_startchild_ret().
start_proc([Node, CallId|_]=Args) ->
    case gproc:lookup_values(?FS_CALL_EVENTS_PROCESS_REG(Node, CallId)) of
        [] ->
            lager:debug("starting event handler for ~s", [Node]),
            supervisor:start_child(?SERVER, Args);
        [{Pid, _V}] when is_pid(Pid) ->
            lager:debug("recycling existing call events worker ~p for ~s", [Pid, CallId]),
            ecallmgr_call_events:update_node(Pid, Node),
            {'ok', Pid};
        _V ->
            lager:debug("unexpected event process: ~p", [_V]),
            {'error', 'multiple_handlers'}
    end.

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
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
