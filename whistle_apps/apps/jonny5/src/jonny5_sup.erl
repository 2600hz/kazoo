%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(jonny5_sup).

-behaviour(supervisor).

-include("jonny5.hrl").

-export([start_link/0]).
-export([init/1]).

-define(ORIGIN_BINDINGS, [[{type, <<"limits">>}]]).
-define(CACHE_PROPS, [{origin_bindings, ?ORIGIN_BINDINGS}]).
-define(CHILD(Name, Type), fun(N, cache) -> {N, {wh_cache, start_link, [N, ?CACHE_PROPS]}
                                             ,permanent, 5000, worker, [wh_cache]};
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).
-define(CHILDREN, [{?JONNY5_CACHE, cache}
                   ,{jonny5_listener, worker}
                   ,{j5_reconciler, worker}
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
