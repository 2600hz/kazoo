%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([start_control_process/1]).
-export([control_context/0, control_context/1]).
-export([release_context/1]).
-export([wait_for_exit/2]).

-export([init/1]).

-include("ecallmgr.hrl").
-include_lib("kazoo_amqp/include/kazoo_amqp_pool.hrl").

-define(SERVER, ?MODULE).

-define(USE_POOL_KEY,[<<"call_control">>, <<"use_pool">>]).
-define(USE_POOL, kapps_config:get_boolean(?APP_NAME, ?USE_POOL_KEY, 'false')).

-define(POOL_NAME, 'ectl_amqp_pool').

-define(CHILDREN, [?SUPER('ecallmgr_call_control_sup')
                  ,?SUPER('ecallmgr_call_control_listener_sup')
                   | pool_spec(?USE_POOL)
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

%% -spec pool_name() -> ?POOL_NAME.
%% pool_name() -> ?POOL_NAME.

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
    _ = maybe_bind_for_pool_state(?USE_POOL),

    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

-spec maybe_bind_for_pool_state(boolean()) -> any().
maybe_bind_for_pool_state('true') ->
    kz_nodes:bind_for_pool_state('kz_amqp_sup', self());
maybe_bind_for_pool_state('false') -> 'ok'.

-spec pool_spec(boolean()) -> list().
pool_spec('true') ->
    [?POOL_NAME_ARGS(?POOL_NAME, [pool_args()])];
pool_spec('false') -> [].

-spec pool_args() -> list().
pool_args() ->
    PoolSize = kapps_config:get_integer(?APP_NAME, [<<"call_control">>, <<"pool_size">>], ?DEFAULT_POOL_SIZE),
    PoolOverflow = kapps_config:get_integer(?APP_NAME, [<<"call_control">>, <<"pool_overflow">>], ?DEFAULT_POOL_OVERFLOW),
    PoolThreshold = kapps_config:get_integer(?APP_NAME, [<<"call_control">>, <<"pool_threshold">>], ?DEFAULT_POOL_THRESHOLD),
    PoolServerConfirms = kapps_config:get_boolean(?APP_NAME, [<<"call_control">>, <<"pool_server_confirms">>], ?DEFAULT_POOL_SERVER_CONFIRMS),
    [{'worker_module', 'kz_amqp_worker'}
    ,{'name', {'local', ?POOL_NAME}}
    ,{'size', PoolSize}
    ,{'max_overflow', PoolOverflow}
    ,{'strategy', 'fifo'}
    ,{'neg_resp_threshold', PoolThreshold}
    ,{'amqp_server_confirms', PoolServerConfirms}
    ].

-spec start_control_process(map()) -> kz_types:sup_startchild_ret().
start_control_process(#{call_id := CallId} = Context) ->
    lager:debug("starting call control for ~s", [CallId]),
    ecallmgr_call_control_sup:start_proc(Context).

-spec control_context() -> {'ok', map()} | {'error', any()}.
control_context() ->
    control_context(?USE_POOL).

-spec control_context(boolean()) -> {'ok', map()} | {'error', any()}.
control_context('false') ->
    {'ok', #{}};
control_context('true') ->
    case kz_amqp_worker:checkout_worker(?POOL_NAME) of
        {'ok', AMQPWorker} ->
            gen_listener:add_binding(AMQPWorker, 'dialplan', []),
            AMQPQueue = gen_listener:queue_name(AMQPWorker),
            {'ok', #{amqp_worker => AMQPWorker
                    ,control_q => AMQPQueue
                    ,init_fun => fun(#{amqp_worker := Worker}) ->
                                         kz_amqp_worker:relay_to(Worker, self()),
                                         kz_amqp_channel:consumer_pid(Worker)
                                 end
                    ,exit_fun => fun(#{amqp_worker := Worker}) ->
                                         kz_amqp_worker:stop_relay(Worker, self()),
                                         kz_amqp_worker:checkin_worker(Worker, ?POOL_NAME)
                                 end
                    }
            };
        {'error', _E} = Error ->
            lager:warning("unable to create call context due to AMQP worker error ~p", [_E]),
            Error
    end.


-spec release_context(map()) -> 'ok'.
release_context(#{exit_fun := Fun}=Context) ->
    Fun(Context),
    'ok';
release_context(_) -> 'ok'.

-spec wait_for_exit(tuple(), map()) -> 'ok'.
wait_for_exit({'error', _}, Ctx) ->
    release_context(Ctx);
wait_for_exit({'ok', Pid}, _Ctx) ->
    MRef = erlang:monitor('process', Pid),
    receive
        {'DOWN', MRef, _, _, _} -> 'ok'
    end.
