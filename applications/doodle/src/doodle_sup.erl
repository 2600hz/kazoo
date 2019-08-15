%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("doodle.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'db', ?KZ_SIP_DB }
                          ,{'type', <<"device">>}
                          ]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                     ]).

-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?WORKER('doodle_listener')
                  ,?WORKER('doodle_shared_listener')
                  ,?SUPER('doodle_event_handler_sup')
                  ,?SUPER('doodle_exe_sup')
                  ,?SUPER('doodle_inbound_listener_sup')
                  ,?DOODLE_POOL_NAME_ARGS(?OUTBOUND_POOL, [outbound_pool_args()])
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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

outbound_pool_args() ->
    PoolSize = kz_json:get_integer_value(?OUTBOUND_POOL_ARG(<<"size">>), config(), 5),
    PoolOverflow = kz_json:get_integer_value(?OUTBOUND_POOL_ARG(<<"overflow">>), config(), 5),
    PoolThreshold = kz_json:get_integer_value(?OUTBOUND_POOL_ARG(<<"threshold">>), config(), 5),
    PoolServerConfirms = kz_json:is_true(?OUTBOUND_POOL_ARG(<<"confirms">>), config(), 'true'),
    Broker = kz_json:get_ne_binary_value(?OUTBOUND_POOL_ARG(<<"broker">>), config(), ?DEFAULT_BROKER),
    Exchange = kz_json:get_ne_binary_value(?OUTBOUND_EXCHANGE_ARG(<<"name">>), config(), kz_binary:rand_hex(16)),
    ExchangeType = kz_json:get_ne_binary_value(?OUTBOUND_EXCHANGE_ARG(<<"type">>), config(), <<"topic">>),
    ExchangeOptions = kz_json:get_json_value(?OUTBOUND_EXCHANGE_ARG(<<"options">>), config(), ?DEFAULT_EXCHANGE_OPTIONS_JOBJ),
    Exchanges = [{Exchange, ExchangeType, amqp_exchange_options(ExchangeOptions)}],
    [{'worker_module', 'kz_amqp_worker'}
    ,{'name', {'local', ?OUTBOUND_POOL}}
    ,{'size', PoolSize}
    ,{'max_overflow', PoolOverflow}
    ,{'strategy', 'fifo'}
    ,{'neg_resp_threshold', PoolThreshold}
    ,{'amqp_broker', Broker}
    ,{'amqp_queuename_start', ?OUTBOUND_POOL}
    ,{'amqp_bindings', []}
    ,{'amqp_exchanges', Exchanges}
    ,{'amqp_server_confirms', PoolServerConfirms}
    ].

config() ->
    case kapps_config:get_category(?APP_NAME) of
        {'ok', JObj} -> JObj;
        _ -> kz_json:new()
    end.

-spec amqp_exchange_options(kz_term:api_object()) -> kz_term:proplist().
amqp_exchange_options('undefined') -> [];
amqp_exchange_options(JObj) ->
    [{kz_term:to_atom(K, 'true'), V}
     || {K, V} <- kz_json:to_proplist(JObj)
    ].
