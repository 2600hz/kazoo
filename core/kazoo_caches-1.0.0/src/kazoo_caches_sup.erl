%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_caches_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("kz_caches.hrl").

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                          ,[{'db', ?WH_CONFIG_DB}]
                         ]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}]).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE_ARGS(?WHAPPS_CONFIG_CACHE, ?CACHE_PROPS)
                   ,?CACHE(?WHAPPS_CALL_CACHE)
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
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
