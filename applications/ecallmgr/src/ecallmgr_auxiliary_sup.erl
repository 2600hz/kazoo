%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_auxiliary_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([cache_proc/0]).
-export([init/1]).

-define(CACHE_AUTHN_PROPS, [{'origin_bindings', [[{'type', <<"account">>}]
                                                ,[{'type', <<"device">>}]
                                                ,[{'type', <<"user">>}]
                                                ]
                            }
                           ]).
-define(CACHE_UTIL_PROPS, [{'origin_bindings', [[{'db', ?KZ_CONFIG_DB}]
                                               ,[{'type', <<"media">>}]
                                               ]
                           }
                          ]).

-define(CHILDREN, [?CACHE_ARGS(?ECALLMGR_UTIL_CACHE, ?CACHE_UTIL_PROPS)
                  ,?CACHE_ARGS(?ECALLMGR_AUTH_CACHE, ?CACHE_AUTHN_PROPS)
                  ,?CACHE(?ECALLMGR_CALL_CACHE)
                  ,?CACHE(?ECALLMGR_INTERACTION_CACHE)
                  ,?SUPER('ecallmgr_originate_sup')
                  ,?WORKER('ecallmgr_registrar')
                  ,?WORKER('ecallmgr_balance_crawler_fsm')
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

-spec cache_proc() -> atom().
cache_proc() -> ?ECALLMGR_UTIL_CACHE.

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
