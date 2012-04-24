%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_sup).

-behaviour(supervisor).

-include_lib("ecallmgr/src/ecallmgr.hrl").

-export([start_link/0]).
-export([init/1]).

-define(CHILD(Name, Type), fun(N, pool) -> {N, {poolboy, start_link, [[{name, {local, N}}
                                                                       ,{worker_module, wh_amqp_worker}
                                                                       ,{size, 200}
                                                                       ,{max_overflow, 200}
                                                                      ]
                                                                     ]}
                                            ,permanent, 5000, worker, [poolboy]
                                           };
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).
-define(CHILDREN, [{?ECALLMGR_AMQP_POOL, pool}
                   ,{ecallmgr_util_sup, supervisor}
                   ,{ecallmgr_call_sup, supervisor}
                   ,{ecallmgr_fs_sup, supervisor}
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
