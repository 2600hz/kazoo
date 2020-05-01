%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(trunkstore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,pool_name/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("ts.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                         ,[{'type', <<"connectivity">>}]
                         ,[{'type', <<"sys_info">>}]
                         ,[{'type', <<"number">>}]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                     ]).

-define(CHILDREN, [?SUPER('ts_onnet_sup') %% handles calls originating onnet (customer)
                  ,?WORKER('ts_offnet_sup') %% handles calls originating offnet (carrier)
                  ,?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?WORKER('ts_responder')
                  ,?WORKER('trunkstore_listener')
                  ]).

-define(POOL_NAME, 'ts_amqp_pool').

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec pool_name() -> ?POOL_NAME.
pool_name() -> ?POOL_NAME.

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    _ = kz_util:set_startup(),

    _ = kz_nodes:bind_for_pool_state('kz_amqp_sup', self()),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    PoolSize = kapps_config:get_integer(?CONFIG_CAT, <<"pool_size">>, ?DEFAULT_POOL_SIZE),

    PoolOverflow = kapps_config:get_integer(?CONFIG_CAT, <<"pool_overflow">>, ?DEFAULT_POOL_OVERFLOW),

    PoolThreshold = kapps_config:get_integer(?CONFIG_CAT, <<"pool_threshold">>, ?DEFAULT_POOL_THRESHOLD),
    PoolServerConfirms = kapps_config:get_boolean(?CONFIG_CAT, <<"pool_server_confirms">>, ?DEFAULT_POOL_SERVER_CONFIRMS),

    PoolArgs = [{'worker_module', 'kz_amqp_worker'}
               ,{'name', {'local', ?POOL_NAME}}
               ,{'size', PoolSize}
               ,{'max_overflow', PoolOverflow}
               ,{'strategy', 'fifo'}
               ,{'neg_resp_threshold', PoolThreshold}
               ,{'amqp_server_confirms', PoolServerConfirms}
               ],

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, [?POOL_NAME_ARGS(?POOL_NAME, [PoolArgs]) | ?CHILDREN]}}.
