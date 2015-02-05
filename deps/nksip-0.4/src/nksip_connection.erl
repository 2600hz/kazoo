%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Generic tranport connection process
-module(nksip_connection).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([is_max/1, send/2, async_send/2, stop/2]).
-export([start_refresh/3, stop_refresh/1, set_timeout/2, get_transport/1, get_refresh/1]).
-export([incoming/2, stop_all/0]).
-export([start_link/4, init/1, terminate/2, code_change/3, handle_call/3,   
            handle_cast/2, handle_info/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").
-include_lib("wsock/include/wsock.hrl").

-define(MAX_MSG, 65535).
-define(MAX_UDP, 1500).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Checks if we already have the maximum number of connections
-spec is_max(nksip:app_id()) ->
    boolean().

is_max(AppId) ->
    Max = nksip_config_cache:global_max_connections(),
    case nksip_counters:value(nksip_connections) of
        Current when Current > Max -> 
            true;
        _ -> 
            AppMax = AppId:config_max_connections(),
            case nksip_counters:value({nksip_connections, AppId}) of
                AppCurrent when AppCurrent > AppMax -> 
                    true;
                _ ->
                    false
            end
    end.


%% @doc Starts a new connection
-spec start_link(nksip:app_id(), nksip:transport(), port()|pid(), integer()) ->
    {ok, pid()}.

start_link(AppId, Transport, SocketOrPid, Timeout) -> 
    Args = [AppId, Transport, SocketOrPid, Timeout],
    gen_server:start_link(?MODULE, Args, []).


%% @doc Sends a new request or response to a started connection
-spec send(pid(), #sipmsg{}|binary()) ->
    ok | {error, term()}.

send(Pid, #sipmsg{}=SipMsg) ->
    #sipmsg{app_id=AppId, call_id=CallId, transport=Transp} = SipMsg,
    #transport{proto=Proto} = Transp,
    Packet = nksip_unparse:packet(SipMsg),
    case do_send(Pid, Proto, Packet) of
        ok ->
            AppId:nkcb_connection_sent(SipMsg, Packet);
        udp_too_large ->
            {error, udp_too_large};
        {error, Error} ->
            ?notice(AppId, CallId, "could not send ~p message: ~p", [Proto, Error]),
            {error, Error}
    end;

send(Pid, Packet) when is_binary(Packet) ->
    case catch gen_server:call(Pid, {send, Packet}, 30000) of
        ok -> ok;
        {error, Error} -> {error, Error};
        {'EXIT', Error} -> {error, Error}
    end.


%% @private
do_send(_Pid, udp, Packet) when byte_size(Packet) > ?MAX_UDP ->
    udp_too_large;

do_send(Pid, _Proto, Packet) ->
    case catch gen_server:call(Pid, {send, Packet}, 30000) of
        ok -> ok;
        {error, Error} -> {error, Error};
        {'EXIT', Error} -> {error, Error}
    end.


%% @private Sends a new request or response to a started connection
-spec async_send(pid(), binary()) ->
    ok.

async_send(Pid, Packet) when is_binary(Packet) ->
    gen_server:cast(Pid, {send, Packet}).


%% @doc Stops a started connection
stop(Pid, Reason) ->
    gen_server:cast(Pid, {stop, Reason}).


%% @doc Start a time-alive series, with result notify
%% If `Ref' is not `undefined', a message will be sent to self() using `Ref'
%% (self() ! Ref) after the fist successful ping response
-spec start_refresh(pid(), pos_integer(), term()) ->
    ok | error.

start_refresh(Pid, Secs, Ref) when is_integer(Secs), Secs>0 ->
    case catch gen_server:call(Pid, {start_refresh, Secs, Ref, self()}) of
        ok -> ok;
        _ -> error
    end.

%% @doc Start a time-alive series, with result notify
-spec stop_refresh(pid()) ->
    ok.

stop_refresh(Pid) ->
    gen_server:cast(Pid, stop_refresh).


%% @doc Updates timeout on no incoming packet
-spec set_timeout(pid(), pos_integer()) ->
    ok | error.

set_timeout(Pid, Secs) ->
    case catch gen_server:call(Pid, {set_timeout, Secs}) of
        ok -> ok;
        _ -> error
    end.


%% @private Gets the transport record (and extends the timeout)
-spec get_transport(pid()) ->
    {ok, nksip:transport()} | error.

get_transport(Pid) ->
    case is_process_alive(Pid) of
        true ->
            case catch gen_server:call(Pid, get_transport) of
                {ok, Transp} -> {ok, Transp};
                _ -> error
            end;
        false ->
            error 
    end.


%% @private Gets remaining and total refresh time  (and extends the timeout)
-spec get_refresh(pid()) ->
    {true, pos_integer(), pos_integer()} | {false, pos_integer()} | error.

get_refresh(Pid) ->
    case is_process_alive(Pid) of
        true ->
            case catch gen_server:call(Pid, get_refresh) of
                {ok, Refresh} -> Refresh;
                _ -> error
            end;
        false ->
            error 
    end.


%% @private 
-spec incoming(pid(), binary()) ->
    ok.

incoming(Pid, Packet) when is_binary(Packet) ->
    gen_server:cast(Pid, {incoming, Packet}).


%% @private
stop_all() ->
    [stop(Pid, normal) || {_, _, Pid} <- nksip_transport:get_all_connected()].



%% ===================================================================
%% gen_server
%% ===================================================================



-record(state, {
    app_id :: nksip:app_id(),
    proto :: nksip:protocol(),
    transport :: nksip_transport:transport(),
    socket :: port() | ssl:sslsocket(),
    timeout :: non_neg_integer(),
    nat_ip :: inet:ip_address(),
    nat_port :: inet:port_number(),
    in_refresh :: boolean(),
    refresh_timer :: reference(),
    refresh_time :: pos_integer(),
    refresh_notify = [] :: [from()],
    buffer = <<>> :: binary(),
    rnrn_pattern :: binary:cp(),
    ws_frag = #message{},            % store previous ws fragmented message
    ws_pid :: pid()                  % ws protocol's pid
}).


%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([AppId, Transport, SocketOrPid, Timeout]) ->
    #transport{proto=Proto, remote_ip=Ip, remote_port=Port, resource=Res} = Transport,
    nksip_proc:put({nksip_connection, {AppId, Proto, Ip, Port, Res}}, Transport), 
    nksip_proc:put(nksip_transports, {AppId, Transport}),
    nksip_counters:async([nksip_connections, {nksip_connections, AppId}]),
    case is_pid(SocketOrPid) of
        true ->
            Socket = undefined,
            Pid = SocketOrPid,
            link(Pid);
        false ->
            Socket = SocketOrPid,
            Pid = undefined
    end,
    ?debug(AppId, <<>>, "Created ~p connection ~p (~p) ~p", 
                [Proto, {Ip, Port}, self(), Timeout]),
    State = #state{
        app_id = AppId,
        proto = Proto,
        transport = Transport, 
        socket = Socket, 
        timeout = Timeout,
        in_refresh = false,
        buffer = <<>>,
        rnrn_pattern = binary:compile_pattern(<<"\r\n\r\n">>),
        ws_frag = undefined,
        ws_pid = Pid
    },
    {ok, State, Timeout}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({send, Packet}, From, State) ->
    Reply = do_send(Packet, State),
    gen_server:reply(From, Reply),
    case Reply of
        ok -> do_noreply(State);
        {error, _} -> do_stop(normal, State)
    end;

handle_call({start_refresh, Secs, Ref, Pid}, From, State) ->
    #state{refresh_timer=RefreshTimer, refresh_notify=RefreshNotify} = State,
    nksip_lib:cancel_timer(RefreshTimer),
    gen_server:reply(From, ok),
    RefreshNotify1 = case Ref of
        undefined -> RefreshNotify;
        _ -> [{Ref, Pid}|RefreshNotify]
    end,
    State1 = State#state{refresh_time=1000*Secs, refresh_notify=RefreshNotify1},
    handle_info({timeout, none, refresh}, State1);

handle_call({set_timeout, Secs}, From, State) ->
    gen_server:reply(From, ok),
    do_noreply(State#state{timeout=1000*Secs});

handle_call(get_transport, From, #state{transport=Transp}=State) ->
    gen_server:reply(From, {ok, Transp}),
    do_noreply(State);

handle_call(get_refresh, From, State) ->
    #state{
        in_refresh = InRefresh, 
        refresh_timer = RefreshTimer, 
        refresh_time = RefreshTime,
        timeout=Timeout
    } = State,
    Reply = case InRefresh of
        true -> 
            {true, 0, round(RefreshTime/1000)};
        false when is_reference(RefreshTimer) -> 
            {true, round(erlang:read_timer(RefreshTimer)/1000), round(RefreshTime/1000)};
        false -> 
            {false, round(Timeout/1000)}
    end,
    gen_server:reply(From, {ok, Reply}),
    do_noreply(State);

handle_call(Msg, _From, State) ->
    lager:error("Module ~p received unexpected call: ~p", [?MODULE, Msg]),
    do_noreply(State).


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast({send, Packet}, State) ->
    case do_send(Packet, State) of
        ok -> do_noreply(State);
        {error, _} -> do_stop(send_error, State)
    end;

handle_cast({incoming, Packet}, State) ->
    parse(Packet, State);

handle_cast({stun, {ok, StunIp, StunPort}}, State) ->
    #state{
        app_id = AppId,
        nat_ip = NatIp, 
        nat_port = NatPort, 
        refresh_time = RefreshTime,
        refresh_notify = RefreshNotify
    } = State,
    ?debug(AppId, <<>>, "transport received STUN", []),
    case 
        {NatIp, NatPort} == {undefined, undefined} orelse
        {NatIp, NatPort} == {StunIp, StunPort}
    of
        true ->
            lists:foreach(fun({Ref, Pid}) -> Pid ! Ref end, RefreshNotify),
            State1 = State#state{
                nat_ip = StunIp,
                nat_port = StunPort,
                refresh_timer = erlang:start_timer(RefreshTime, self(), refresh),
                refresh_notify = []
            },
            do_noreply(State1);
        false ->
            do_stop(stun_changed, State)
    end;

handle_cast({stun, error}, State) ->
    do_stop(stun_error, State);

handle_cast(stop_refresh, #state{refresh_timer=RefreshTimer}=State) ->
    nksip_lib:cancel_timer(RefreshTimer),
    State1 = State#state{
        in_refresh = false, 
        refresh_timer = undefined, 
        refresh_time = undefined
    },
    do_noreply(State1);

handle_cast({stop, Reason}, State) ->
    do_stop(Reason, State);

handle_cast(Msg, State) ->
    lager:error("Module ~p received unexpected cast: ~p", [?MODULE, Msg]),
    do_noreply(State).


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({tcp, Socket, Packet}, #state{proto=Proto, socket=Socket}=State) ->
    inet:setopts(Socket, [{active, once}]),
    case Proto of
        tcp -> parse(Packet, State);
        ws -> parse_ws(Packet, State)
    end;

handle_info({ssl, Socket, Packet}, #state{proto=Proto, socket=Socket}=State) ->
    ssl:setopts(Socket, [{active, once}]),
    case Proto of
        tls -> parse(Packet, State);
        wss -> parse_ws(Packet, State)
    end;

handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    do_stop(normal, State);

handle_info({ssl_closed, Socket}, #state{socket=Socket}=State) ->
    do_stop(normal, State);

% Received from Ranch when the listener is ready
handle_info({shoot, _ListenerPid}, #state{proto=tcp, socket=Socket}=State) ->
    inet:setopts(Socket, [{active, once}]),
    do_noreply(State);

handle_info({shoot, _ListenerPid}, #state{proto=tls, socket=Socket}=State) ->
    ssl:setopts(Socket, [{active, once}]),
    do_noreply(State);

handle_info({timeout, _, refresh}, #state{proto=udp}=State) ->
    #state{app_id=AppId, transport=Transp} = State,
    #transport{remote_ip=Ip, remote_port=Port} = Transp,
    Class = case size(Ip) of 4 -> ipv4; 8 -> ipv6 end,
    case nksip_transport:get_listening(AppId, udp, Class) of
        [{_, Pid}|_] -> 
            ?debug(AppId, <<>>, "transport sending STUN", []),
            nksip_transport_udp:send_stun_async(Pid, Ip, Port),
            do_noreply(State#state{refresh_timer=undefined});
        [] ->
            do_stop(no_listening_transport, State)
    end;

handle_info({timeout, _, refresh}, #state{app_id=AppId}=State) ->
    ?debug(AppId, <<>>, "transport sending refresh", []),
    case do_send(<<"\r\n\r\n">>, State) of
        ok -> 
            do_noreply(State#state{in_refresh=true, refresh_timer=undefined});
        {error, _} -> 
            do_stop(send_error, State)
    end;

handle_info({timeout, _, refreshed}, State) ->
    do_stop(refreshed_timeout, State);
    
handle_info(timeout, State) ->
    do_stop(process_timeout, State);

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Info]),
    do_noreply(State).


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, #state{app_id=AppId, ws_pid=Pid, proto=Proto}) when is_pid(Pid) ->
    ?debug(AppId, <<>>, "Connection ~p process stopped (~p)", [Proto, self()]),
    Pid ! stop;

terminate(_Reason, State) ->
    #state{app_id=AppId, socket=Socket, transport=Transp} = State,
    #transport{proto=Proto, sctp_id=AssocId} = Transp,
    ?debug(AppId, <<>>, "Connection ~p process stopped (~p)", [Proto, self()]),
    case Proto of
        udp -> ok;
        tcp -> gen_tcp:close(Socket);
        tls -> ssl:close(Socket);
        sctp -> gen_sctp:eof(Socket, #sctp_assoc_change{assoc_id=AssocId});
        ws -> gen_tcp:close(Socket);
        wss -> ssl:close(Socket);
        _ -> ok
    end.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
-spec do_send(binary(), #state{}) ->
    ok | {error, term()}.

do_send(Packet, #state{ws_pid=Pid}) when is_pid(Pid) ->
    Pid ! {send, [{text, Packet}]},
    ok;

do_send(Packet, State) ->
    #state{app_id=AppId, socket=Socket, transport=Transp} = State,
    #transport{proto=Proto, remote_ip=Ip, remote_port=Port, sctp_id=AssocId} = Transp,
    case
        case Proto of
            udp -> gen_udp:send(Socket, Ip, Port, Packet);
            tcp -> gen_tcp:send(Socket, Packet);
            tls -> ssl:send(Socket, Packet);
            sctp -> gen_sctp:send(Socket, AssocId, 0, Packet);
            ws ->  gen_tcp:send(Socket, wsock_message:encode(Packet, [mask, text]));
            wss ->  ssl:send(Socket, wsock_message:encode(Packet, [mask, text]))
        end
    of
        ok -> 
            ok;
        {error, Error} ->
            ?notice(AppId, <<>>, "could not send ~p message: ~p", [Proto, Error]),
            {error, Error}
    end.

    
%% @private Parse for UDP/TCP/TLS/SCTP
-spec parse(binary(), #state{}) ->
    gen_server_info(#state{}).

parse(Binary, #state{buffer=Buffer}=State) ->
    Data = case Buffer of
        <<>> -> Binary;
        _ -> <<Buffer/binary, Binary/binary>>
    end,
    case do_parse(Data, State) of
        {ok, State1} -> 
            do_noreply(State1);
        {error, Error} -> 
            do_stop(Error, State)
    end.


%% @private
-spec do_parse(binary(), #state{}) ->
    {ok, #state{}} | {error, term()}.

do_parse(<<>>, State) ->
    {ok, State#state{buffer = <<>>}};

%% For TCP, we send a \r\n\r\n, remote must reply with \r\n
do_parse(<<"\r\n\r\n", Rest/binary>>, #state{app_id=AppId, proto=Proto}=State) 
        when Proto==tcp; Proto==tls; Proto==sctp ->
    ?debug(AppId, <<>>, "transport responding to refresh", []),
    case do_send(<<"\r\n">>, State) of
        ok -> do_parse(Rest, State);
        {error, _} -> {error, send_error}
    end;

do_parse(<<"\r\n">>, #state{proto=udp}=State) ->
    {ok, State};

do_parse(<<"\r\n", Rest/binary>>, #state{proto=Proto}=State) 
        when Proto==tcp; Proto==tls; Proto==sctp ->
    #state{
        app_id = AppId,
        refresh_notify = RefreshNotify, 
        refresh_time = RefreshTime,
        in_refresh = InRefresh
    } = State,
    lists:foreach(fun({Ref, Pid}) -> Pid ! Ref end, RefreshNotify),
    RefreshTimer = case InRefresh of
        true -> 
            ?debug(AppId, <<>>, "transport received refresh, next in ~p secs", 
                        [round(RefreshTime/1000)]),
            erlang:start_timer(RefreshTime, self(), refresh);
        false -> 
            undefined
    end,
    State1 = State#state{
        in_refresh = false, 
        refresh_timer = RefreshTimer,
        refresh_notify = [],
        buffer = Rest
    },
    do_parse(Rest, State1);

do_parse(Data, #state{app_id=AppId, proto=Proto})
        when (Proto==tcp orelse Proto==tls) andalso byte_size(Data) > ?MAX_MSG ->
    ?warning(AppId, <<>>, "dropping TCP/TLS closing because of max_buffer", []),
    {error, msg_too_large};

do_parse(Data, State) ->
    #state{
        app_id = AppId,
        proto = Proto,
        transport = Transp,
        rnrn_pattern = RNRN
    } = State,
    case binary:match(Data, RNRN) of
        nomatch when Proto==tcp; Proto==tls ->
            {ok, State#state{buffer=Data}};
        nomatch ->
            ?notice(AppId, <<>>, "ignoring partial ~p msg: ~p", [Proto, Data]),
            {error, parse_error};
        {Pos, 4} ->
            do_parse(AppId, Transp, Data, Pos+4, State)
    end.


%% @private
-spec do_parse(nksip:app_id(), nksip:transport(), binary(), integer(), #state{}) ->
    {ok, #state{}} | {error, term()}.

do_parse(AppId, Transp, Data, Pos, State) ->
    #transport{proto=Proto} = Transp,
    case extract(Proto, Data, Pos) of
        {ok, CallId, Msg, Rest} ->
            AppId:nkcb_connection_recv(AppId, CallId, Transp, Msg),
            case nksip_router:incoming_sync(AppId, CallId, Transp, Msg) of
                ok -> 
                    do_parse(Rest, State);
                {error, Error} -> 
                    ?notice(AppId, <<>>, 
                            "error processing ~p request: ~p", [Proto, Error]),
                    {error, Error}
            end;
        partial when Proto==tcp; Proto==tls ->
            {ok, State#state{buffer=Data}};
        partial ->
            ?notice(AppId, <<>>, "ignoring partial msg ~p: ~p", [Proto, Data]),
            {ok, State};
        {error, Error} ->
            reply_error(Data, Error, State),
            {error, parse_error}
    end.

%% @private Parse for WS/WSS
-spec parse_ws(binary(), #state{}) ->
    gen_server_info(#state{}).

parse_ws(Packet, #state{app_id=AppId, ws_frag=FragMsg}=State) ->
    {Result, State1} = case FragMsg of
        undefined -> 
            {
                wsock_message:decode(Packet, []), 
                State
            };
        _ -> 
            {
                wsock_message:decode(Packet, FragMsg, []), 
                State#state{ws_frag=undefined}
            }
    end,
    case Result of
        Msgs when is_list(Msgs) ->
            case do_parse_ws_messages(Msgs, State1) of
                {ok, State2} -> 
                    do_noreply(State2);
                {error, Error} -> 
                    ?warning(AppId, <<>>, "websocket parsing error: ~p", [Error]),
                    do_stop(Error, State)
            end;
        {error, Error} ->
            ?notice(AppId, <<>>, "websocket parsing error: ~p", [Error]),
            do_stop(ws_error, State1)
    end.


%% @private
-spec do_parse_ws_messages([#message{}], #state{}) ->
    {ok, #state{}} | {error, term()}.

do_parse_ws_messages([], State) ->
    {ok, State};
        
do_parse_ws_messages([#message{type=fragmented}=Msg|Rest], State) ->
    do_parse_ws_messages(Rest, State#state{ws_frag=Msg});

do_parse_ws_messages([#message{type=Type, payload=Data}|Rest], State) 
        when Type==text; Type==binary ->
    case do_parse(nksip_lib:to_binary(Data), State) of
        {ok, State1} -> 
            do_parse_ws_messages(Rest, State1);
        {error, Error} -> 
            {error, Error}
    end;

do_parse_ws_messages([#message{type=close}|_], _State) ->
    {error, ws_close};

do_parse_ws_messages([#message{type=ping}|Rest], State) ->
    do_parse_ws_messages(Rest, State);

do_parse_ws_messages([#message{type=pong}|Rest], State) ->
    do_parse_ws_messages(Rest, State).


%% @private
do_noreply(#state{in_refresh=true}=State) -> 
    {noreply, State, 10000};

do_noreply(#state{timeout=Timeout}=State) -> 
    {noreply, State, Timeout}.


%% @private
do_stop(Reason, #state{app_id=AppId, proto=Proto}=State) ->
    case Reason of
        normal -> ok;
        _ -> ?debug(AppId, <<>>, "~p connection stop: ~p", [Proto, Reason])
    end,
    {stop, normal, State}.


%% @private
-spec extract(nksip:protocol(), binary(), integer()) ->
    {ok, nksip:call_id(), binary(), binary()} | partial | {error, binary()}.

extract(Proto, Data, Pos) ->
    case 
        re:run(Data, nksip_config_cache:re_call_id(), [{capture, all_but_first, binary}])
    of
        {match, [_, CallId]} ->
            case 
                re:run(Data, nksip_config_cache:re_content_length(), 
                       [{capture, all_but_first, list}])
            of
                {match, [_, CL0]} ->
                    case catch list_to_integer(CL0) of
                        CL when is_integer(CL), CL>=0 ->
                            MsgSize = Pos+CL,
                            case byte_size(Data) of
                                MsgSize ->
                                    {ok, CallId, Data, <<>>};
                                BS when BS<MsgSize andalso (Proto==tcp orelse Proto==tls) ->
                                    partial;
                                BS when BS<MsgSize ->
                                    {error, <<"Invalid Content-Length">>};
                                _ when Proto==tcp; Proto==tls ->
                                    {Msg, Rest} = split_binary(Data, MsgSize),
                                    {ok, CallId, Msg, Rest};
                                _ ->
                                    {error, <<"Invalid Content-Length">>}
                            end;
                        _ ->
                            {error, <<"Invalid Content-Length">>}
                    end;
                _ ->
                    {error, <<"Invalid Content-Length">>}
            end;
        _ ->
            {error, <<"Invalid Call-ID">>}
    end.


%% @private
-spec reply_error(binary(), binary(), #state{}) ->
    ok.

reply_error(Data, Msg, #state{app_id=AppId}=State) ->
    case nksip_parse_sipmsg:parse(Data) of
        {ok, {req, _, _}, Headers, _} ->
            Resp = nksip_unparse:response(Headers, 400, Msg),
            do_send(Resp, State);
        O ->
            ?notice(AppId, <<>>, "error parsing request: ~s", [O])
    end.

