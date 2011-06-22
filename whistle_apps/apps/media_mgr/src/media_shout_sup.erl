%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(media_shout_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_shout/5]).

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

start_shout(Media, To, Type, Port, CallID) ->
    supervisor:start_child(?SERVER, [Media, To, Type, Port, CallID]).

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
    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {media_shout, {media_shout, start_link, []},
	      Restart, Shutdown, Type, [media_shout]},

    {ok, {{simple_one_for_one, 1, 2}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
