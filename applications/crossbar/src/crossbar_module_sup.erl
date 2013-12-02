%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_module_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
         ,start_child/1, start_child/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("crossbar.hrl").

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
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

start_child(Mod) ->
    start_child(Mod, 'worker').
start_child(Mod, 'worker') ->
    supervisor:start_child(?SERVER, ?WORKER(Mod));
start_child(Mod, 'supervisor') ->
    supervisor:start_child(?SERVER, ?SUPER(Mod)).

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [?SUPER('cb_buckets_sup')],

    {'ok', {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
