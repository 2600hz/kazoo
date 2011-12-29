%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Simple-One-For-One strategy for restarting call event processes
%%% @end
%%% Created :  2 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_event_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_proc/1]).
-export([workers/0]).
-export([find_worker/1]).
-export([find_control_queue/1]).

%% Supervisor callbacks
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

start_proc([_, CallId]=Args) ->
    case find_worker(CallId) of
        {ok, Srv}=Ok -> 
            ?LOG("recycling existing call events worker ~p", [Srv]),
            Ok;
        _ -> 
            supervisor:start_child(?SERVER, Args)
    end.

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [Worker]} <- supervisor:which_children(?SERVER),
             Worker =:= ecallmgr_call_events
    ].

-spec find_worker/1 :: (ne_binary()) -> {'error', 'not_found'} | {'ok', pid()}.
-spec do_find_worker/2 :: ([pid(),...] | [], ne_binary()) -> {'error', 'not_found'} | {'ok', pid()}.

find_worker(CallId) ->
    do_find_worker(workers(), CallId).

do_find_worker([], _) ->
    {error, not_found};
do_find_worker([Srv|T], CallId) ->
    case ecallmgr_call_events:callid(Srv) of
        CallId -> {ok, Srv};
        _ -> do_find_worker(T, CallId)
    end.

-spec find_control_queue/1 :: (ne_binary()) -> {'error', 'not_found'} | {'ok', ne_binary()}.
find_control_queue(CallId) ->    
    case find_worker(CallId) of
        {error, _}=E -> E;
        {ok, Worker} ->
            {ok, ecallmgr_call_events:queue_name(Worker)}
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
