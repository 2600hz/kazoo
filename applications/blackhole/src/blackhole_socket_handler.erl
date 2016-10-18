%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(blackhole_socket_handler).

-export([init/3
        ,websocket_init/3
        ,websocket_handle/3
        ,websocket_info/3
        ,websocket_terminate/3
        ]).

-include("blackhole.hrl").

-spec init({any(), 'http'}, any(), any()) -> tuple().
init({_Any, 'http'}, _Req0, _HandlerOpts) ->
    {'upgrade', 'protocol', 'cowboy_websocket'}.

-spec websocket_init(any(), cowboy_req:req(), any()) -> {'ok', cowboy_req:req(), any()}.
websocket_init(_Type, Req, _Opts) ->
    {Peer, _}  = cowboy_req:peer(Req),
    {RemIp, _} = Peer,

    {'ok', State} = blackhole_socket_callback:open(self(), session_id(Req), RemIp),
    {'ok', Req, State}.

-spec websocket_handle(any(), cowboy_req:req(), A) -> {'ok', cowboy_req:req(), A}.
websocket_handle({'text', Data}, Req, State) ->
    Obj    = kz_json:decode(Data),
    Action = kz_json:get_value(<<"action">>, Obj, <<"noop">>),
    Msg    = kz_json:delete_key(<<"action">>, Obj),

    case blackhole_socket_callback:recv({Action, Msg}, State) of
        {'ok', NewState} -> {'ok', Req, NewState};
        'error' -> {'ok', Req, State}
    end;

websocket_handle(_Other, Req, State) ->
    lager:debug("not handling message : ~p", [_Other]),
    {'ok', Req, State}.

-spec websocket_info(any(), cowboy_req:req(), A) -> {'ok', cowboy_req:req(), A}.
websocket_info({'$gen_cast', _}, Req, State) ->
    {'ok', Req, State};

websocket_info({'send_data', Data}, Req, State) ->
    {'reply', {'text', kz_json:encode(Data)}, Req, State};

websocket_info(Info, Req, State) ->
    lager:info("unhandled websocket info: ~p", [Info]),
    {'ok', Req, State}.

-spec websocket_terminate(any(), cowboy_req:req(), bh_context:context()) -> bh_context:context().
websocket_terminate(_Reason, _Req, State) ->
    blackhole_socket_callback:close(State).

-spec session_id(cowboy_req:req()) -> binary().
session_id(Req) ->
    {Peer, _}  = cowboy_req:peer(Req),
    {Ip, Port} = Peer,

    BinIp   = kz_util:to_binary(inet_parse:ntoa(Ip)),
    BinPort = kz_util:to_binary(integer_to_list(Port)),

    <<BinIp/binary, ":", BinPort/binary>>.
