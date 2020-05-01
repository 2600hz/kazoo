%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_auxiliary_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

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
                  ,?SUPER('ecallmgr_originate_sup')
                  ,?WORKER('ecallmgr_registrar')
                  ,?WORKER('ecallmgr_balance_crawler_statem')
                  ,?WORKER('ecallmgr_discovery')
                  ,?WORKER('ecallmgr_usurp_monitor')
                  ,?WORKER('ecallmgr_trusted')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec cache_proc() -> atom().
cache_proc() -> ?ECALLMGR_UTIL_CACHE.

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
