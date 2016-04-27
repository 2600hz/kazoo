%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_amqp_sup).

-behaviour(supervisor).

-export([start_link/0
         ,stop_bootstrap/0
         ,pool_name/0
         ,add_amqp_pool/4, add_amqp_pool/5, add_amqp_pool/6, add_amqp_pool/7
         ,pool_pid/1
        ]).

-export([init/1]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).

-define(CONFIG_SECTION, wh_config:get_node_section_name()).

-define(POOL_NAME, 'wh_amqp_pool').

-define(DEFAULT_POOL_SIZE, 150).
-define(DEFAULT_POOL_OVERFLOW, 100).
-define(DEFAULT_POOL_THRESHOLD, 5).

%%% Move the section to whistle_apps or ecallmgr for per-vm control

-define(POOL_NAME_ARGS(Name, Args), ?WORKER_NAME_ARGS('poolboy', Name, Args)).

-define(CHILDREN, [?WORKER('wh_amqp_connections')
                   ,?SUPER('wh_amqp_connection_sup')
                   ,?WORKER('wh_amqp_assignments')
                   ,?WORKER('wh_amqp_history')
                   ,?WORKER('wh_amqp_bootstrap')
                  ]).

-define(POOL_THRESHOLD, wh_config:get_integer(?CONFIG_SECTION, 'pool_threshold', ?DEFAULT_POOL_THRESHOLD)).

-define(ADD_POOL_ARGS(Pool, Broker, Size, Overflow, Bindings, Exchanges, ServerAck),
        [[{'worker_module', 'wh_amqp_worker'}
          ,{'name', {'local', Pool}}
          ,{'size', Size}
          ,{'max_overflow', Overflow}
          ,{'neg_resp_threshold', ?POOL_THRESHOLD}
          ,{'amqp_broker', Broker}
          ,{'amqp_queuename_start', Pool}
          ,{'amqp_bindings', Bindings}
          ,{'amqp_exchanges', Exchanges}
          ,{'amqp_server_confirms', ServerAck}
         ]]).

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

-spec stop_bootstrap() -> 'ok' | {'error', any()}.
stop_bootstrap() ->
    _ = supervisor:terminate_child(?SERVER, 'wh_amqp_bootstrap').

-spec pool_name() -> atom().
pool_name() ->
    case get('$amqp_pool') of
        'undefined' -> ?POOL_NAME;
        Name -> Name
    end.

-spec add_amqp_pool(atom() | binary(), binary(), integer(), integer()) -> sup_startchild_ret().
add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow) ->
    add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, []).

-spec add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings) -> sup_startchild_ret() when
    UUID :: atom() | binary(),
    Broker :: binary(),
    PoolSize :: integer(),
    PoolOverflow :: integer(),
    Bindings :: wh_proplist().
add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings) ->
    add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, []).

-type exchange() :: {ne_binary(), ne_binary(), wh_proplist()}.
-type exchanges() :: [exchange()].

-spec add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges) -> sup_startchild_ret() when
    UUID :: atom() | binary(),
    Broker :: binary(),
    PoolSize :: integer(),
    PoolOverflow :: integer(),
    Bindings :: wh_proplist(),
    Exchanges :: exchanges().
add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges) ->
    add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, 'false').

-spec add_amqp_pool(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, ServerAck) -> sup_startchild_ret() when
    UUID :: atom() | binary(),
    Broker :: binary(),
    PoolSize :: integer(),
    PoolOverflow :: integer(),
    Bindings :: wh_proplist(),
    Exchanges :: exchanges(),
    ServerAck :: boolean().
add_amqp_pool(Uuid, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, ServerAck) ->
    UUID = wh_util:to_atom(Uuid, 'true'),
    Args = ?ADD_POOL_ARGS(UUID, Broker, PoolSize, PoolOverflow, Bindings, Exchanges, ServerAck),
    supervisor:start_child(?SERVER, ?POOL_NAME_ARGS(UUID, Args)).

-spec pool_pid(atom() | binary()) -> api_pid().
pool_pid(Pool) ->
    ID = wh_util:to_atom(Pool, 'true'),
    case [ Pid || {Id,Pid,_,_} <- supervisor:which_children(?SERVER), Id == ID] of
        [] -> 'undefined';
        [P | _] -> P
    end.


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

    PoolSize =
        case wh_config:get(?CONFIG_SECTION, 'pool_size') of
            [] -> wh_config:get_integer(?CONFIG_SECTION, 'pool_size', ?DEFAULT_POOL_SIZE);
            [Size|_] -> wh_util:to_integer(Size)
        end,

    PoolOverflow =
        case wh_config:get(?CONFIG_SECTION, 'pool_overflow') of
            [] -> wh_config:get_integer(?CONFIG_SECTION, 'pool_overflow', ?DEFAULT_POOL_OVERFLOW);
            [Overflow|_] -> wh_util:to_integer(Overflow)
        end,

    PoolThreshold =
        case wh_config:get(?CONFIG_SECTION, 'pool_threshold') of
            [] -> ?POOL_THRESHOLD;
            [Threshold|_] -> wh_util:to_integer(Threshold)
        end,

    PoolArgs = [{'worker_module', 'wh_amqp_worker'}
                ,{'name', {'local', ?POOL_NAME}}
                ,{'size', PoolSize}
                ,{'max_overflow', PoolOverflow}
                ,{'neg_resp_threshold', PoolThreshold}
               ],

    Children = ?CHILDREN ++ [?POOL_NAME_ARGS(?POOL_NAME, [PoolArgs])],

    {'ok', {SupFlags, Children}}.
