%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(media_shout_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_shout/4]).

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

start_shout(MediaID, To, Type, Port) ->
    supervisor:start_child(?SERVER, [MediaID, To, Type, Port]).

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

    AChild = {media_shout, {media_shout, start_link, []},
	      Restart, Shutdown, Type, [media_shout]},

    {ok, {{simple_one_for_one, 1, 2}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
