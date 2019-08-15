%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017 Conversant Ltd
%%% @doc Receives a ping, responds with a pong.
%%% Useful for both keeping a websocket connection up, and figuring
%%% out if a websocket connection is still up
%%%
%%% @author Conversant Ltd (Max Lay)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_ping).

-export([init/0
        ,validate/2
        ,ping/2
        ,authorize/2
        ]).

-include("blackhole.hrl").

-spec init() -> 'ok'.
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.authorize.ping">>, ?MODULE, 'authorize'),
    _ = blackhole_bindings:bind(<<"blackhole.validate.ping">>, ?MODULE, 'validate'),
    _ = blackhole_bindings:bind(<<"blackhole.command.ping">>, ?MODULE, 'ping'),
    'ok'.

-spec authorize(bh_context:context(), kz_json:object()) -> bh_context:context().
authorize(Context, _Payload) ->
    Context.

-spec validate(bh_context:context(), kz_json:object()) -> bh_context:context().
validate(Context, _Payload) ->
    Context.

-spec ping(bh_context:context(), kz_json:object()) -> bh_context:context().
ping(Context, _Payload) ->
    bh_context:set_resp_data(Context, kz_json:from_list([{<<"response">>, <<"pong">>}])).
