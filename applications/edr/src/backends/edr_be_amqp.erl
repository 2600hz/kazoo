%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc Relays EDR messages to AMQP via {@link kapi_edr_amqp}.
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_be_amqp).

-behaviour(gen_edr_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/2
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {}).
-type state() :: #state{}.

-spec start_link(backend()) -> kz_types:startlink_ret().
start_link(Args) ->
    gen_edr_backend:start_link(?MODULE, Args).

-spec init(backend())-> init_ret(state()).
init(#backend{})->
    lager:info("starting edr_be_amqp"),
    {'ok', #state{}};
init(_Other)->
    'ignore'.

-spec push(state(), edr_event()) -> 'ok'.
push(_State, Event=#edr_event{})->
    kapi_edr_amqp:publish_event(Event).

-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.

-spec async_response_handler(any()) -> work_result().
async_response_handler(_Response)->
    'ok'.
