-module(sockjs_cowboy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

%% Cowboy http callbacks
-export([init/3, handle/2, terminate/2]).

%% Cowboy ws callbacks
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

init({_Any, http}, Req, Service) ->
    case sockjs_handler:is_valid_ws(Service, {cowboy, Req}) of
        {true, {cowboy, _Req1}, _Reason} ->
            {upgrade, protocol, cowboy_http_websocket};
        {false, {cowboy, Req1}, _Reason} ->
            {ok, Req1, Service}
    end.

handle(Req, Service) ->
    {cowboy, Req3} = sockjs_handler:handle_req(Service, {cowboy, Req}),
    {ok, Req3, Service}.

terminate(_Req, _Service) ->
    ok.

%% --------------------------------------------------------------------------

websocket_init(_TransportName, Req, Service = #service{logger = Logger}) ->
    Req0 = Logger(Service, {cowboy, Req}, websocket),

    Service1 = Service#service{disconnect_delay = 5*60*1000},

    {Info, Req1} = sockjs_handler:extract_info(Req0),
    SessionPid = sockjs_session:maybe_create(undefined, Service1, Info),
    {RawWebsocket, {cowboy, Req3}} =
        case sockjs_handler:get_action(Service, Req1) of
            {{match, WS}, Req2} when WS =:= websocket orelse
                                     WS =:= rawwebsocket ->
                {WS, Req2}
        end,
    self() ! go,
    {ok, Req3, {RawWebsocket, SessionPid}}.

websocket_handle({text, Data}, Req, {RawWebsocket, SessionPid} = S) ->
    case sockjs_ws_handler:received(RawWebsocket, SessionPid, Data) of
        ok       -> {ok, Req, S};
        shutdown -> {shutdown, Req, S}
    end;
websocket_handle(_Unknown, Req, S) ->
    {shutdown, Req, S}.

websocket_info(go, Req, {RawWebsocket, SessionPid} = S) ->
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait          -> {ok, Req, S};
        {ok, Data}    -> self() ! go,
                         {reply, {text, Data}, Req, S};
        {close, <<>>} -> {shutdown, Req, S};
        {close, Data} -> self() ! shutdown,
                         {reply, {text, Data}, Req, S}
    end;
websocket_info(shutdown, Req, S) ->
    {shutdown, Req, S}.

websocket_terminate(_Reason, _Req, {RawWebsocket, SessionPid}) ->
    sockjs_ws_handler:close(RawWebsocket, SessionPid),
    ok.
