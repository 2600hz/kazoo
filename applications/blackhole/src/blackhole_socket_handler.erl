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
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        'undefined' -> maybe_allow_connection(Req, HandlerOpts);
        _SubProtocols ->
            lager:warning("sub-protocols are not supported at the moment: ~p", [_SubProtocols]),
            {'ok', cowboy_req:reply(400, Req), HandlerOpts}
    end.

-spec maybe_allow_connection(cowboy_req:req(), cowboy_websocket:opts()) ->
          {'ok', cowboy_req:req(), cowboy_websocket:opts()} |
          {'cowboy_websocket', cowboy_req:req(), blackhole_init(), cowboy_websocket:opts()}.
maybe_allow_connection(Req, HandlerOpts) ->
    {RemoteIP, _Port} = cowboy_req:peer(Req),
    MaxConnectionsPerIP = kapps_config:get_integer(?CONFIG_CAT, <<"max_connections_per_ip">>),
    case maybe_allow_connection(Req
                               ,RemoteIP
                               ,MaxConnectionsPerIP
                               ,blackhole_tracking:session_count_by_ip(RemoteIP)
                               )
    of
        {'ok', Req1} -> {'ok', Req1, HandlerOpts};
        Resp -> Resp
    end.

maybe_allow_connection(Req, RemoteIP, 'undefined', _ActiveConns) ->
    %% no max connection limit set
    allow_connection(Req, RemoteIP);
maybe_allow_connection(Req, RemoteIP, MaxConns, ActiveConns) when ActiveConns < MaxConns ->
    lager:debug("allowing connection from ~p (~p of ~p up)", [RemoteIP, ActiveConns, MaxConns]),
    allow_connection(Req, RemoteIP);
maybe_allow_connection(Req, RemoteIP, _MaxConns, ActiveConns) ->
    lager:warning("connection from ~p denied: max limit ~p reached", [RemoteIP, ActiveConns]),
    {'ok', cowboy_req:reply(429, Req)}.

allow_connection(Req, RemoteIP) ->
    SessionId = session_id(Req),
    lager:info("allowing connection from ~s", [SessionId]),
    {'cowboy_websocket', Req, {RemoteIP, SessionId}, #{idle_timeout => ?IDLE_TIMEOUT}}.

-spec terminate(any(), cowboy_req:req(), bh_context:context() | cowboy_websocket:opts())  -> 'ok'.
terminate(_Reason, Req, Opts) when is_list(Opts) ->
    lager:info("socket for session ~s down early: ~p", [session_id(Req), _Reason]);
terminate(_Reason, Req, Context) ->
    SessionId = session_id(Req),
    _ = blackhole_socket_callback:close(Context),
    lager:info("socket for session ~s down: ~p", [SessionId, _Reason]).

-spec websocket_init(blackhole_init()) -> {'ok', bh_context:context()}.
websocket_init({RemoteIP, SessionId}) ->
    lager:info("init from ~p(~p)", [RemoteIP, SessionId]),
    {'ok', _Context} = blackhole_socket_callback:open(self(), SessionId, RemoteIP).

-spec websocket_handle(any(), bh_context:context()) ->
          {'ok', bh_context:context(), 'hibernate'}.
websocket_handle({'text', Data}, Context) ->
    JObj   = kz_json:decode(Data),
    Action = kz_json:get_ne_binary_value(<<"action">>, JObj, <<"noop">>),
    Msg    = kz_json:delete_key(<<"action">>, JObj),

    case blackhole_socket_callback:recv({Action, Msg}, Context) of
        {'ok', NewContext} -> {'ok', NewContext, 'hibernate'};
        'error' -> {'ok', Context, 'hibernate'}
    end;
websocket_handle('ping', Context) ->
    {'ok', Context, 'hibernate'};
websocket_handle(_Other, Context) ->
    lager:debug("not handling message : ~p", [_Other]),
    {'ok', Context, 'hibernate'}.

-spec websocket_info(any(), bh_context:context()) ->
          {'ok', bh_context:context()} |
          {'reply', {'text', binary()} | 'pong', bh_context:context()}.
websocket_info({'$gen_cast', _}, Context) ->
    {'ok', Context};
websocket_info({'send_data', Data}, Context) ->
    {'reply', {'text', kz_json:encode(Data)}, Context};
websocket_info('pong', Context) ->
    {'reply', 'pong', Context};
websocket_info(Info, Context) ->
    lager:info("unhandled websocket info: ~p", [Info]),
    {'ok', Context}.

-spec session_id(cowboy_req:req()) -> binary().
session_id(Req) ->
    {IP, Port} = cowboy_req:peer(Req),

    BinIP   = kz_network_utils:iptuple_to_binary(IP),
    BinPort = kz_term:to_binary(Port),
    <<BinIP/binary, ":", BinPort/binary>>.
