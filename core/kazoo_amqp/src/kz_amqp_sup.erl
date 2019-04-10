%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_sup).

-behaviour(supervisor).

-export([start_link/0
        ,stop_bootstrap/0
        ,pool_name/0
        ,add_amqp_pool/4, add_amqp_pool/5, add_amqp_pool/6, add_amqp_pool/7
        ,pool_pid/1
        ,pools/0, pools/1
        ,bind_for_pool_state/0
        ]).

-export([init/1]).

-include("kz_amqp_util.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").

-define(SERVER, ?MODULE).

-define(CONFIG_SECTION, 'amqp').

-define(POOL_NAME, 'kz_amqp_pool').

-define(CHILDREN, [?WORKER('kz_amqp_connections')
                  ,?SUPER('kz_amqp_connection_sup')
                  ,?WORKER('kz_amqp_history')
                  ,?WORKER('kz_amqp_assignments')
                  ,?WORKER('kz_amqp_bootstrap')
                  ]).

-define(POOL_THRESHOLD, kz_config:get_integer(?CONFIG_SECTION, 'pool_threshold', ?DEFAULT_POOL_THRESHOLD)).
-define(POOL_SERVER_CONFIRMS, kz_config:get_boolean(?CONFIG_SECTION, 'pool_server_confirms', ?DEFAULT_POOL_SERVER_CONFIRMS)).

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

-spec stop_bootstrap() -> 'ok' | {'error', any()}.
stop_bootstrap() ->
    _ = supervisor:terminate_child(?SERVER, 'kz_amqp_bootstrap').

-spec pools() -> [{atom(), pid()}].
pools() -> pools(?MODULE).

-spec pools(kz_types:server_ref()) -> [{atom(), pid()}].
pools(Supervisor) ->
    [{Pool, Pid}
     || {Pool, Pid, _Type, ['poolboy']} <- supervisor:which_children(Supervisor)
    ].

-spec pool_name() -> atom().
pool_name() ->
    case get('$amqp_pool') of
        'undefined' -> ?POOL_NAME;
        Name ->
            lager:debug("using pool with name ~s", [Name]),
            Name
    end.

-spec add_amqp_pool(atom() | binary(), binary(), integer(), integer()) -> kz_types:sup_startchild_ret().
add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow) ->
    add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, []).

-spec add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings) -> kz_types:sup_startchild_ret() when
      UUID :: atom() | binary(),
      Broker :: binary(),
      PoolSize :: integer(),
      PoolOverflow :: integer(),
      Bindings :: kz_term:proplist().
add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings) ->
    add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, []).

-type exchange() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()}.
-type exchanges() :: [exchange()].

-spec add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges) -> kz_types:sup_startchild_ret() when
      UUID :: atom() | binary(),
      Broker :: binary(),
      PoolSize :: integer(),
      PoolOverflow :: integer(),
      Bindings :: kz_term:proplist(),
      Exchanges :: exchanges().
add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges) ->
    add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, 'false').

-spec add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, ServerAck) -> kz_types:sup_startchild_ret() when
      UUID :: atom() | binary(),
      Broker :: binary(),
      PoolSize :: integer(),
      PoolOverflow :: integer(),
      Bindings :: kz_term:proplist(),
      Exchanges :: exchanges(),
      ServerAck :: boolean().
add_amqp_pool(Uuid, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, ServerAck) ->
    UUID = kz_term:to_atom(Uuid, 'true'),
    Args = ?ADD_POOL_ARGS(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, ServerAck),
    supervisor:start_child(?SERVER, ?POOL_NAME_ARGS(UUID, Args)).

-spec pool_pid(atom() | binary()) -> kz_term:api_pid().
pool_pid(Pool) ->
    ID = kz_term:to_atom(Pool, 'true'),
    case [ Pid || {Id,Pid,_,_} <- supervisor:which_children(?SERVER), Id == ID] of
        [] -> 'undefined';
        [P | _] -> P
    end.

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
    bind_for_pool_state(),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PoolSize =
        case kz_config:get_integer(?CONFIG_SECTION, 'pool_size') of
            [] -> kz_config:get_integer(?CONFIG_SECTION, 'pool_size', ?DEFAULT_POOL_SIZE);
            [Size|_] -> Size
        end,

    PoolOverflow =
        case kz_config:get_integer(?CONFIG_SECTION, 'pool_overflow') of
            [] -> kz_config:get_integer(?CONFIG_SECTION, 'pool_overflow', ?DEFAULT_POOL_OVERFLOW);
            [Overflow|_] -> Overflow
        end,

    PoolThreshold =
        case kz_config:get_integer(?CONFIG_SECTION, 'pool_threshold') of
            [] -> ?POOL_THRESHOLD;
            [Threshold|_] -> Threshold
        end,
    PoolServerConfirms = kz_config:get_boolean(?CONFIG_SECTION, 'pool_server_confirms', ?DEFAULT_POOL_SERVER_CONFIRMS),

    PoolArgs = [{'worker_module', 'kz_amqp_worker'}
               ,{'name', {'local', ?POOL_NAME}}
               ,{'size', PoolSize}
               ,{'max_overflow', PoolOverflow}
               ,{'strategy', 'fifo'}
               ,{'neg_resp_threshold', PoolThreshold}
               ,{'amqp_server_confirms', PoolServerConfirms}
               ],

    Children = ?CHILDREN ++ [?POOL_NAME_ARGS(?POOL_NAME, [PoolArgs])],

    {'ok', {SupFlags, Children}}.

-spec bind_for_pool_state() -> kazoo_bindings:bind_result().
bind_for_pool_state() ->
    _ = kazoo_bindings:bind(kz_nodes:pool_state_binding(), ?MODULE, 'pools', self()).
