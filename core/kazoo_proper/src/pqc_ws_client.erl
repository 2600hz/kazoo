%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_ws_client).

-export([connect/2
        ,send/2
        ,close/1
        ,recv/1, recv/2
        ]).

-include("kazoo_proper.hrl").

-type conn() :: {pid(), reference()}.

-spec connect(kz_term:text(), inet:port_number()) ->
          conn() |
          {'ws_upgrade_failed', any()}.
connect(Host, Port) ->
    {'ok', ConnPid} = gun:open(Host, Port, #{protocols => [http], retry => 0}),
    lager:info("started WS client(~p) to ~s:~p", [ConnPid, Host, Port]),
    {'ok', 'http'} = gun:await_up(ConnPid),

    _UpgradeRef = gun:ws_upgrade(ConnPid, "/", [], #{compress => true}),

    receive
        {'gun_upgrade', ConnPid, StreamRef, [<<"websocket">>], Headers} ->
            lager:info("stream open: ~p(~p): ~p", [ConnPid, StreamRef, Headers]),
            {ConnPid, StreamRef};
        {'gun_response', ConnPid, _, _, Status, Headers} ->
            {'ws_upgrade_failed', {Status, Headers}};
        {'gun_error', ConnPid, _StreamRef, Reason} ->
            {'ws_upgrade_failed', Reason}

    after 1000 ->
            exit(timeout)
    end.

-spec send(conn(), binary()) -> 'ok'.
send({ConnPid, _}, Payload) ->
    gun:ws_send(ConnPid, {'text', Payload}).

-spec close(conn()) -> 'ok'.
close({ConnPid, _}) ->
    gun:ws_send(ConnPid, 'close').

-spec recv(conn()) ->
          binary() |
          {'error', 'timeout'}.
recv(ConnPid) ->
    recv(ConnPid, 0).

-spec recv(conn(), timeout()) ->
          {'json', kz_json:object()} |
          {'frame', binary()} |
          {'error', 'timeout'}.
recv({ConnPid, StreamRef}, Timeout) ->
    receive
        {'gun_ws', ConnPid, StreamRef, {'text', Binary}} ->
            {'json', kz_json:decode(Binary)};
        {'gun_ws', ConnPid, StreamRef, Frame} ->
            lager:info("frame recv: ~p", [Frame]),
            {'frame', Frame}
    after Timeout ->
            {'error', 'timeout'}
    end.
