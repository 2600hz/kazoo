%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_events_app).
-behaviour(application).

-include("kazoo_events.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-export([start/2, stop/1]).


%% Application callbacks

%% @doc Implement the application start behaviour
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    kazoo_events_sup:start_link().

%% @doc Implement the application stop behaviour
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kapi_call:declare_exchanges(),
    ExchangeOptions = [{auto_delete, true}
                      ,{arguments, [{<<"hash-header">>, longstr, <<"call-id">>}]}
                      ],
    kz_amqp_util:new_exchange(?KZ_CALL_EVENTS_EXCHANGE, <<"x-consistent-hash">>, ExchangeOptions),
    BindCmd = #'exchange.bind'{destination=?KZ_CALL_EVENTS_EXCHANGE
                              ,source = ?EXCHANGE_CALLEVT
                              ,routing_key= <<"call.#">>
                              ,nowait=false
                              ,arguments=[]
                              },
    kz_amqp_channel:command(BindCmd).
