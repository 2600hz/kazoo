%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_sup).
-behaviour(supervisor).

-export([start_link/0
        ,init/1
        ,start_child/1
        ]).

-include("kazoo_apps.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").

-define(SERVER, ?MODULE).

-define(CONFIG_SECTION, <<"amqp">>).
-define(POOL_THRESHOLD, kz_config:get_integer(?CONFIG_SECTION, <<"pool_threshold">>, ?DEFAULT_POOL_THRESHOLD)).

-define(KAPPS_GETBY_ORIGIN_BINDINGS, [[{'type', <<"account">>}]]).

-define(KAPPS_GETBY_PROPS, [{'origin_bindings', ?KAPPS_GETBY_ORIGIN_BINDINGS}]).

-define(KAPPS_CONFIG_ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                                      ,[{'type', <<"account_config">>}]
                                      ,[{'db', ?KZ_CONFIG_DB}]
                                      ]).

-define(KAPPS_CONFIG_PROPS, [{'origin_bindings', ?KAPPS_CONFIG_ORIGIN_BINDINGS}]).

-ifdef(TEST).
-define(CHILDREN, [?CACHE_ARGS(?KAPPS_CONFIG_CACHE, [])
                  ,?CACHE_ARGS(?KAPPS_GETBY_CACHE, [])
                  ]).
-else.
-define(CHILDREN, [?WORKER('kazoo_apps_init')
                  ,?CACHE_ARGS(?KAPPS_CONFIG_CACHE, ?KAPPS_CONFIG_PROPS)
                  ,?WORKER('kapps_controller')
                  ,?CACHE_ARGS(?KAPPS_GETBY_CACHE, ?KAPPS_GETBY_PROPS)
                  ,?WORKER('kz_epmd')
                  ]).
-endif.

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

-spec start_child(supervisor:child_spec()) -> kz_types:sup_startchild_ret().
start_child(Spec) ->
    supervisor:start_child(?SERVER, Spec).

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
    _ = kz_nodes:bind_for_pool_state('kz_amqp_sup', self()),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PoolSize =
        case kz_config:get_integer(?CONFIG_SECTION, <<"pool_size">>) of
            [] -> kz_config:get_integer(?CONFIG_SECTION, <<"pool_size">>, ?DEFAULT_POOL_SIZE);
            [Size|_] -> Size
        end,

    PoolOverflow =
        case kz_config:get_integer(?CONFIG_SECTION, <<"pool_overflow">>) of
            [] -> kz_config:get_integer(?CONFIG_SECTION, <<"pool_overflow">>, ?DEFAULT_POOL_OVERFLOW);
            [Overflow|_] -> Overflow
        end,

    PoolThreshold =
        case kz_config:get_integer(?CONFIG_SECTION, <<"pool_threshold">>) of
            [] -> ?POOL_THRESHOLD;
            [Threshold|_] -> Threshold
        end,
    PoolServerConfirms = kz_config:get_boolean(?CONFIG_SECTION, <<"pool_server_confirms">>, ?DEFAULT_POOL_SERVER_CONFIRMS),

    PoolArgs = [{'worker_module', 'kz_amqp_worker'}
               ,{'name', {'local', kz_amqp_sup:pool_name()}}
               ,{'size', PoolSize}
               ,{'max_overflow', PoolOverflow}
               ,{'strategy', 'fifo'}
               ,{'neg_resp_threshold', PoolThreshold}
               ,{'amqp_server_confirms', PoolServerConfirms}
               ],

    {'ok', {SupFlags, ?CHILDREN ++ [?POOL_NAME_ARGS(kz_amqp_sup:pool_name(), [PoolArgs])]}}.
