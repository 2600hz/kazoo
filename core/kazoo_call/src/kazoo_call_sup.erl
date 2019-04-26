%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_call_sup).
-behaviour(supervisor).

-export([start_link/0
        ,pool_name/0
        ,init/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").
-include("kapps_call_command.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?CACHE(?KAPPS_CALL_CACHE)
                  ,?WORKER('kapps_call_events')
                  ,?SUPER('kzc_recordings_sup')
                  ]).

-define(POOL_NAME, 'kzc_recordings_pool').

-define(POOL_CONFIG_CAT, <<"kazoo_recordings">>).

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
    _ = kz_nodes:bind_for_pool_state('kz_amqp_sup', self()),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    PoolSize = kapps_config:get_integer(?POOL_CONFIG_CAT, <<"pool_size">>, ?DEFAULT_POOL_SIZE),

    PoolOverflow = kapps_config:get_integer(?POOL_CONFIG_CAT, <<"pool_overflow">>, ?DEFAULT_POOL_OVERFLOW),

    PoolThreshold = kapps_config:get_integer(?POOL_CONFIG_CAT, <<"pool_threshold">>, ?DEFAULT_POOL_THRESHOLD),
    PoolServerConfirms = kapps_config:get_boolean(?POOL_CONFIG_CAT, <<"pool_server_confirms">>, ?DEFAULT_POOL_SERVER_CONFIRMS),

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
