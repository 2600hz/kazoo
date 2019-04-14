%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_sup).
-behaviour(supervisor).

-export([start_link/0
        ,pool_name/0
        ]).
-export([start_control_process/3
        ,start_control_process/6
        ]).
-export([start_event_process/2]).

-export([init/1]).

-include("ecallmgr.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").

-define(SERVER, ?MODULE).
-define(CHILDREN, [?SUPER('ecallmgr_call_event_sup')
                  ,?SUPER('ecallmgr_call_control_sup')
                  ]).

-define(POOL_NAME, 'ectl_amqp_pool').

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

-spec start_event_process(atom(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_event_process(Node, UUID) ->
    ecallmgr_call_event_sup:start_proc([Node, UUID]).

-spec start_control_process(atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_control_process(Node, CallId, FetchId) ->
    start_control_process(Node, CallId, FetchId, 'undefined', kz_json:new(), 'undefined').

-spec start_control_process(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:object(), kz_term:api_pid()) ->
                                   kz_types:sup_startchild_ret().
start_control_process(Node, CallId, FetchId, ControllerQ, CCVs, AMQPWorker) ->
    ecallmgr_call_control_sup:start_proc([Node
                                         ,CallId
                                         ,FetchId
                                         ,ControllerQ
                                         ,CCVs
                                         ,AMQPWorker
                                         ]).

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
    kz_amqp_sup:bind_for_pool_state(),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    PoolSize = kapps_config:get_integer(?APP_NAME, [<<"call_control">>, <<"pool_size">>], ?DEFAULT_POOL_SIZE),

    PoolOverflow = kapps_config:get_integer(?APP_NAME, [<<"call_control">>, <<"pool_overflow">>], ?DEFAULT_POOL_OVERFLOW),

    PoolThreshold = kapps_config:get_integer(?APP_NAME, [<<"call_control">>, <<"pool_threshold">>], ?DEFAULT_POOL_THRESHOLD),
    PoolServerConfirms = kapps_config:get_boolean(?APP_NAME, [<<"call_control">>, <<"pool_server_confirms">>], ?DEFAULT_POOL_SERVER_CONFIRMS),

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
