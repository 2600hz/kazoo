%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Conversant Ltd
%%% @doc
%%% Recieves a ping, responds with a pong.
%%% Useful for both keeping a websocket connection up, and figuring
%%% out if a websocket connection is still up
%%% @end
%%% @contributors
%%% Conversant Ltd (Max Lay)
%%%-------------------------------------------------------------------
-module(bh_ping).

-export([init/0
        ,validate/2
        ,ping/2
        ,authorize/2
        ]).

-include("blackhole.hrl").

-spec init() -> 'ok'.
init() ->
    blackhole_bindings:bind(<<"blackhole.authorize.ping">>, ?MODULE, 'authorize'),
    blackhole_bindings:bind(<<"blackhole.validate.ping">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.command.ping">>, ?MODULE, 'ping'),
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
