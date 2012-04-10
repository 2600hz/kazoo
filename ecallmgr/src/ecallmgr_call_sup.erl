%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_call_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

-export([start_link/2]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type, Args), fun(N, cache, _) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                              (N, T, A) -> {N, {N, start_link, A}, permanent, 5000, T, [N]} end(Name, Type, Args)).
-define(CHILDREN, [{ecallmgr_call_events, worker}, {ecallmgr_call_control, worker}]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (atom(), ne_binary()) -> startlink_ret().
-spec start_link/3 :: (atom(), ne_binary(), 'undefined' | ne_binary()) -> startlink_ret().

start_link(Node, UUID) ->
    start_link(Node, UUID, undefined).

start_link(Node, UUID, SendTo) ->
    supervisor:start_link(?MODULE, [Node, UUID, SendTo]).

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
init([Node, UUID]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type, [Node, UUID]) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
