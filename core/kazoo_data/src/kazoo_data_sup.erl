%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_data_sup).

-behaviour(supervisor).

-include("kz_data.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0
        ,init/1
        ]).

-define(ORIGIN_BINDINGS, [[]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                     ]).

-define(DP_ORIGIN_BINDINGS, [[{'db', ?KZ_DATA_DB}]]).
-define(DP_CACHE_PROPS, [{'origin_bindings', ?DP_ORIGIN_BINDINGS}]).

-define(CHILDREN, [?WORKER('kazoo_data_init')
                  ,?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?CACHE_ARGS(?KAZOO_DATA_PLAN_CACHE, ?DP_CACHE_PROPS)
                  ,?SUPER('kz_dataconnection_sup')
                  ,?WORKER('kz_dataconnections')
                  ,?WORKER('kazoo_data_bootstrap')
                  ,?WORKER('kz_data_tracing')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

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
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
