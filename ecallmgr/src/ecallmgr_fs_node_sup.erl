%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_node_sup).

-behaviour(supervisor).

-include_lib("ecallmgr/src/ecallmgr.hrl").

-export([start_link/2]).
-export([init/1]).

-define(CHILD(Name, Type, Args), fun(N, cache, _) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                                    (N, T, A) -> {N, {N, start_link, A}, permanent, 5000, T, [N]} end(Name, Type, Args)).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (atom(), proplist()) -> startlink_ret().
start_link(Node, Options) ->
    supervisor:start_link({local, Node}, ?MODULE, [Node, Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> sup_init_ret().
init([Node, Options]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Children = [?CHILD(ecallmgr_fs_handler_sup, supervisor, [Node, Options])],

    {ok, {SupFlags, Children}}.
