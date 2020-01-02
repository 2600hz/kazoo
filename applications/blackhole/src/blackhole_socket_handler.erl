%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_socket_handler).

-export([init/2
        ,websocket_init/1
        ,websocket_handle/2
        ,websocket_info/2
        ,terminate/3
        ]).

-include("blackhole.hrl").

-define(IDLE_TIMEOUT, ?MILLISECONDS_IN_HOUR).

-type blackhole_init() :: {inet:ip_address(), kz_term:ne_binary()}.

-spec init(cowboy_req:req(), cowboy_websocket:opts()) ->
          {'ok' , cowboy_req:req(), cowboy_websocket:opts()} |
          {'cowboy_websocket', cowboy_req:req(), blackhole_init(), cowboy_websocket:opts()}.
init(Req, HandlerOpts) ->
    lager:info("handling socket init"),
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        'undefined' ->
            {RemoteIP, _} = cowboy_req:peer(Req),
            lager:info("no sub protocols defined by remote client ~p", [RemoteIP]),
            {'cowboy_websocket', Req, {RemoteIP, session_id(Req)}, #{idle_timeout => ?IDLE_TIMEOUT}};
        _SubProtocols ->
            lager:warning("sub-protocols are not supported at the moment: ~p", [_SubProtocols]),
            {'ok', cowboy_req:reply(400, Req), HandlerOpts}
    end.

-spec terminate(any(), any(), any()) -> 'ok'.
terminate(_Reason, _Req, State) ->
    lager:info("bh socket going down: ~p", [_Reason]),
    _ = blackhole_socket_callback:close(State),
    'ok'.

-spec websocket_init(blackhole_init()) -> {'ok', bh_context:context()}.
websocket_init({RemoteIP, SessionsId}) ->
    lager:info("init from ~p(~p)", [RemoteIP, SessionsId]),
    {'ok', _State} = blackhole_socket_callback:open(self(), SessionsId, RemoteIP).

-spec websocket_handle(any(), bh_context:context()) ->
          {'ok', bh_context:context(), 'hibernate'}.
websocket_handle({'text', Data}, State) ->
    Obj    = kz_json:decode(Data),
    Action = kz_json:get_value(<<"action">>, Obj, <<"noop">>),
    Msg    = kz_json:delete_key(<<"action">>, Obj),

    case blackhole_socket_callback:recv({Action, Msg}, State) of
        {'ok', NewState} -> {'ok', NewState, 'hibernate'};
        'error' -> {'ok', State, 'hibernate'}
    end;
websocket_handle(ping, State) ->
    {'ok', State, 'hibernate'};
websocket_handle(_Other, State) ->
    lager:debug("not handling message : ~p", [_Other]),
    {'ok', State, 'hibernate'}.

-spec websocket_info(any(), bh_context:context()) -> {'ok', bh_context:context()}.
websocket_info({'$gen_cast', _}, State) ->
    {'ok', State};
websocket_info({'send_data', Data}, State) ->
    {'reply', {'text', kz_json:encode(Data)}, State};
websocket_info(pong, State) ->
    {'reply', pong, State};
websocket_info(Info, State) ->
    lager:info("unhandled websocket info: ~p", [Info]),
    {'ok', State}.

-spec session_id(cowboy_req:req()) -> binary().
session_id(Req) ->
    {IP, Port} = cowboy_req:peer(Req),

    BinIP   = kz_term:to_binary(inet_parse:ntoa(IP)),
    BinPort = kz_term:to_binary(Port),

    <<BinIP/binary, ":", BinPort/binary>>.
