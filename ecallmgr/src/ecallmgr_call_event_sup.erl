%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_proc([Node, CallId|_]=Args) ->
    case gproc:lookup_pids({p, l, {call_events, CallId}}) of
        [] -> supervisor:start_child(?SERVER, Args);
        [Pid] -> 
            lager:debug("recycling existing call events worker ~p", [Pid]),
            ecallmgr_call_events:update_node(Pid, Node),
            {ok, Pid}
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
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {ecallmgr_call_events, {ecallmgr_call_events, start_link, []},
              Restart, Shutdown, Type, [ecallmgr_call_events]},

    {ok, {{simple_one_for_one, 5, 10}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
