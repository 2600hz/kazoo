%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Simple-One-For-One strategy for restarting call event processes
%%% @end
%%% Created :  2 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_call_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_proc/1]).

%% Supervisor callbacks
-export([init/1]).

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

    AChild = {ts_call_handler, {ts_call_handler, start_link, []},
	      Restart, Shutdown, Type, [ts_call_handler]},

    {ok, {{simple_one_for_one, 5, 10}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
