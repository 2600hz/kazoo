%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Start SHOUT servers to receive data from FreeSWITCH after a "record"
%%% finishes.
%%% @end
%%% Created :  4 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_shout_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_srv/1]).

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

start_srv(FilePath) ->
    logger:format_log(info, "shout_sup(~p): ~p~n", [self(), FilePath]),
    Res = supervisor:start_child(?SERVER, [FilePath]),
    logger:format_log(info, "shout_sup(~p): ~p~n", [self(), Res]),
    Res.

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

    AChild = {ecallmgr_shout, {ecallmgr_shout, start_link, []},
	      Restart, Shutdown, Type, [ecallmgr_shout]},

    {ok, {{simple_one_for_one, 5, 10}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
