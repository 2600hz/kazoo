%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Root supervisor tree for stepswitch routing WhApp
%%% @end
%%%-------------------------------------------------------------------

-module(stepswitch_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("stepswitch/src/stepswitch.hrl").

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type), fun(N, cache) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                              (N, pool) -> {N, {poolboy, start_link, [[{worker_module, stepswitch_cnam}
                                                                       ,{name, {local, N}}
                                                                       ,{size, 10}
                                                                       ,{max_overflow, 50}
                                                                       ,{neg_resp_threshold, 1}
                                                                      ]
                                                                     ]}
                                            ,permanent, 5000, worker, [poolboy]
                                           };
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).

-define(CHILDREN, [{?STEPSWITCH_CACHE, cache}
                   ,{?STEPSWITCH_CNAM_POOL, pool}
                   ,{stepswitch_listener, worker}
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
