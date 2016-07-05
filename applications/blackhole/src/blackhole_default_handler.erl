%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(blackhole_default_handler).

-export([
	 init/3,
	 websocket_init/3,
	 websocket_handle/3,
	 websocket_info/3,
	 websocket_terminate/3
	]).

-include("blackhole.hrl").

init({_Any, 'http'}, _Req0, _HandlerOpts) ->
    {'upgrade', 'protocol', 'cowboy_websocket'}.

websocket_init(_Type, Req, _Opts) ->
    {Peer, _}  = cowboy_req:peer(Req),
    {RemIp, _} = Peer,

    {'ok', State} = blackhole_socket_callback:open(self(), session_id(Req), RemIp),
    {'ok', Req, State}.

websocket_handle({'text', Data}, Req, State) ->
    Obj    = kz_json:decode(Data),
    Action = kz_json:get_value(<<"action">>, Obj),
    Msg    = kz_json:delete_key(<<"action">>, Obj),

    {'ok', NewState} = blackhole_socket_callback:recv(self(), session_id(Req), {Action, Msg}, State),
    {'ok', Req, NewState}.

websocket_info({'$gen_cast', _}, Req, State) ->
    {'ok', Req, State};

websocket_info({'send_event', Event, Data}, Req, State) ->
    Msg = kz_json:set_value(<<"routing_key">>, Event, Data),
    {'reply', {'text', kz_json:encode(Msg)}, Req, State};

websocket_info(Info, Req, State) ->
    lager:info("unhandled websocket info: ~p", [Info]),
    {'ok', Req, State}.

websocket_terminate(_Reason, Req, State) ->
    blackhole_socket_callback:close(self(), session_id(Req), State).

-spec session_id(cowboy_req:req()) -> binary().
session_id(Req) ->
    {Peer, _}  = cowboy_req:peer(Req),
    {Ip, Port} = Peer,

    BinIp   = kz_util:to_binary(inet_parse:ntoa(Ip)),
    BinPort = kz_util:to_binary(integer_to_list(Port)),

    <<BinIp/binary, ":", BinPort/binary>>.
