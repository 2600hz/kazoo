%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(callflow_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([listener_proc/0
        ,pool_name/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("callflow.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                         ,[{'type', <<"user">>}]
                         ,[{'type', <<"callflow">>}]
                         ,[{'type', <<"device">>}]
                         ,[{'type', <<"parked_calls">>}]
                         ,[{'doc_id', ?MANUAL_PRESENCE_DOC}]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                     ,'new_node_flush'
                     ,'channel_reconnect_flush'
                     ]).

-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)

                  ,?WORKER('cf_shared_listener')
                  ,?WORKER('cf_listener')
                  ,?SUPER('cf_event_handler_sup')
                  ,?SUPER('cf_exe_sup')
                  ]).

-define(POOL_NAME, 'cf_amqp_pool').

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

-spec listener_proc() -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?SERVER),
                Mod =:= 'cf_listener'
          ],
    {'ok', P}.

-spec pool_name() -> ?POOL_NAME.
pool_name() -> ?POOL_NAME.

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
    _ = kz_util:set_startup(),

    _ = kz_nodes:bind_for_pool_state('kz_amqp_sup', self()),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    PoolSize = kapps_config:get_integer(?CF_CONFIG_CAT, <<"pool_size">>, ?DEFAULT_POOL_SIZE),

    PoolOverflow = kapps_config:get_integer(?CF_CONFIG_CAT, <<"pool_overflow">>, ?DEFAULT_POOL_OVERFLOW),

    PoolThreshold = kapps_config:get_integer(?CF_CONFIG_CAT, <<"pool_threshold">>, ?DEFAULT_POOL_THRESHOLD),
    PoolServerConfirms = kapps_config:get_boolean(?CF_CONFIG_CAT, <<"pool_server_confirms">>, ?DEFAULT_POOL_SERVER_CONFIRMS),

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
