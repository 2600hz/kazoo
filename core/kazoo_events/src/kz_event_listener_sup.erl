%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_event_listener_sup).

-behaviour(supervisor).

-include("kazoo_events.hrl").
-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1]).

-define(CHILDREN(I), [?WORKER_ARGS_TYPE('kz_event_listener', [I], 'temporary')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    {'ok', Exchange} = init_exchange(),
    {'ok', Pid} = supervisor:start_link({'local', ?SERVER}, ?MODULE, [Exchange]),
    _ = init_workers(Pid),
    {'ok', Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> kz_types:sup_init_ret().
init([Exchange]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN(Exchange)}}.

init_exchange() ->
    kz_util:spawn(fun init_exchange/1, [self()]),
    receive
        {'amqp', Exchange} -> {'ok', Exchange}
    end.

init_exchange(Parent) ->
    Exchange = list_to_binary(["kz_events_"
                              ,kz_term:to_binary(node())
                              ]),
    Assignment = kz_amqp_assignments:get_channel(),
    ExchangeOptions = [{auto_delete, true}
                      ,{arguments, [{<<"hash-header">>, longstr, <<"call-id">>}]}
                      ],
    ExchangeCmd = kz_amqp_util:declare_exchange(Exchange, <<"x-consistent-hash">>, ExchangeOptions),
    BindCmd = #'exchange.bind'{destination=Exchange
                              ,source = ?EXCHANGE_CALLEVT
                              ,routing_key= <<"call.#">>
                              ,nowait=false
                              ,arguments=[]
                              },
    {'ok', #'exchange.declare_ok'{}} = kz_amqp_channel:command(Assignment, ExchangeCmd),
    {'ok', #'exchange.bind_ok'{}} = kz_amqp_channel:command(Assignment, BindCmd),
    kz_amqp_assignments:release(self()),
    Parent ! {'amqp', Exchange}.

init_workers(Pid) ->
    Workers = kapps_config:get_integer(?CONFIG_CAT, <<"event_listeners">>, 5),
    _ = kz_util:spawn(fun() -> [begin
                                    _ = supervisor:start_child(Pid, []),
                                    timer:sleep(500)
                                end
                                || _N <- lists:seq(1, Workers)
                               ]
                      end).
