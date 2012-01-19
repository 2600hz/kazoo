%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Simple-One-For-One strategy for restarting call event processes
%%% @end
%%%
%%% @contributors
%%% James Aimonetti <james@2600hz.org>
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created :  2 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_proc/1]).
-export([workers/0]).
-export([find_worker/1]).
-export([find_control_queue/1]).

-include("ecallmgr.hrl").

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

start_proc(Args) ->
    supervisor:start_child(?SERVER, Args).

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [Worker]} <- supervisor:which_children(?SERVER),
             Worker =:= ecallmgr_call_control
    ].

-spec find_worker/1 :: (ne_binary()) -> {'error', 'not_found'} | {'ok', pid()}.
-spec do_find_worker/2 :: ([pid(),...] | [], ne_binary()) -> {'error', 'not_found'} | {'ok', pid()}.
find_worker(CallID) ->
    do_find_worker(workers(), CallID).

do_find_worker([], _CallId) ->
    {error, not_found};
do_find_worker([Srv|T], CallID) ->
    case catch(ecallmgr_call_control:callid(Srv)) of
        CallID -> {ok, Srv};
        _E -> do_find_worker(T, CallID)
    end.

-spec find_control_queue/1 :: (ne_binary()) -> {'error', 'not_found'} | {'ok', ne_binary()}.
find_control_queue(CallID) ->    
    case find_worker(CallID) of
        {error, _}=E -> E;
        {ok, Worker} ->
            {ok, ecallmgr_call_control:queue_name(Worker)}
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

    AChild = {ecallmgr_call_control, {ecallmgr_call_control, start_link, []},
              Restart, Shutdown, Type, [ecallmgr_call_control]},

    {ok, {{simple_one_for_one, 5, 10}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
