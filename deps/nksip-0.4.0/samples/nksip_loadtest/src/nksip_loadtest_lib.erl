%% -------------------------------------------------------------------
%%
%% Utilities for speed testing
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

-module(nksip_loadtest_lib).
-export([start_server/0, start_server/2, stop_server/0, stop_server/1]).
-export([start_clients/1, stop_clients/1, launch/1]).

-define(TIMEOUT, 10000).
-define(BASE, 1371735635).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Start a test server SipApp called `server' listening on port `5060'.
start_server() ->
    start_server(loadtest, 5060).


%% @doc Start a test server SipApp called `Name' listening on port `Port' for 
%% udp and tcp, and `Port+1' for tls.
start_server(Name, Port) ->
    Opts = [
        {plugins, [nksip_registrar, nksip_stats]},
        {transports, [{udp, all, Port}, {tls, all, Port+1}]},
        {log_level, notice},
        no_100
    ],
    case nksip:start(Name, nksip_loadtest_sipapp, [], Opts) of
        {ok, _} -> ok;
        {error, already_started} -> ok
    end.


%% @doc Stops SipApp called `server'.
stop_server() ->
    stop_server(loadtest).


%% @doc Stops SipApp called `Name'.
stop_server(Name) ->
    nksip:stop(Name).


%% @doc Launchs a new load test.
%%
%% Recognized options are:
%% <br/>
%% <table border="1">
%%  <tr><th>Option</th><th>Description</th></tr>
%%  <tr>
%%      <td>`tcp'</td>
%%      <td>Use tcp transport (default is udp)</td>
%%  </tr>
%%  <tr>
%%      <td>`tls'</td>
%%      <td>Use tls transport (default is udp)</td>
%%  </tr>
%%  <tr>
%%      <td>`messages'</td>
%%      <td>Number of requests to send (default 1)</td>
%%  </tr>
%%  <tr>
%%      <td>`clients'</td>
%%      <td>Number of parallel clients to start (default 10)</td>
%%  </tr>
%%  <tr>
%%      <td>`stateless'</td>
%%      <td>Process the request in stateless mode at the server</td>
%%  </tr>
%%  <tr>
%%      <td>`invite'</td>
%%      <td>Selects the INVITE test (default is OPTIONS)</td>
%%  </tr>
%%  <tr>
%%      <td>`register'</td>
%%      <td>Selects the REGISTER test (default is OPTIONS)</td>
%%  </tr>
%%  <tr>
%%      <td>`raw'</td>
%%      <td>Selects "raw" mode. The requests will be generated locally instead of
%%          using NkSIP request generation functions. This mode is useful to test 
%%          a locally started NkSIP as an UAS server, without the UAC processing 
%%          overhead.</td>
%%  </tr>
%%  <tr>
%%      <td>`host'</td>
%%      <td>Use it in case the NkSIP server being tested is started at another node.
%%          Default is "127.0.0.1"</td>
%%  </tr>
%%  <tr>
%%      <td>`port'</td>
%%      <td>NkSIP server listening port. Default is 5060 for udp and tcp,
%%          and 5061 for tls</td>
%%  </tr>
%% </table>
%%
%% <br/>
%% It returns the number of requests sent, the percentage of correct responses and
%% the number or requests per second.

-spec launch([Opt]) -> {Total, Ok, PerSecond}
    when Opt :: tcp | tls  | {messages, integer()} | {clients, integer()} | 
                stateless | register | invite | raw |
                {host, string()} | {port, integer()} |
                no_auto_start,
         Total :: integer(), Ok :: float(), PerSecond :: integer().

launch(Opts) ->
    Transport = case lists:member(tls, Opts) of
        true ->
            tls;
        false ->
            case lists:member(tcp, Opts) of
                true -> tcp;
                false -> udp           
            end
    end,
    Messages = proplists:get_value(messages, Opts, 1),
    Processes = case proplists:get_value(clients, Opts, 10) of
        P when P < Messages -> P;
        _ -> 1
    end,
    State = case lists:member(stateless, Opts) of
        true -> "stateless";
        false -> "stateful"
    end,
    Port = case proplists:get_value(port, Opts) of
        undefined when Transport==tls -> 5061;
        undefined -> 5060;
        P0 -> P0
    end,
    MsgType = case lists:member(invite, Opts) of
        true -> 
            invite;
        false ->
            case lists:member(register, Opts) of
                true -> 
                    nksip_registrar:clear(loadtest),
                    register;
                _ -> 
                    options
            end
    end,

    PerProcess = round(Messages/Processes),
    Total = Processes * PerProcess,
    Raw = case lists:member(raw, Opts) of true -> raw; _ -> full end,
    {ok, [Ip|_]} = inet:getaddrs(proplists:get_value(host, Opts, "127.0.0.1"), inet),
    Host = inet_parse:ntoa(Ip),
    NoAutoStart = lists:member(no_auto_start, Opts),
    case NoAutoStart of
        true -> ok;
        false -> start_server()
    end,
    Pid = self(),
    CallId = integer_to_list(nksip_lib:timestamp() - ?BASE),
    io:format("Starting test ~s (~p, ~p, ~s) for ~p messages with ~p clients: ", 
        [string:to_upper(atom_to_list(MsgType)), Raw, Transport, 
            State, Messages, Processes]),
    case Raw of
        raw ->
            Fun = fun(Pos) ->
                case Transport of
                    udp ->
                        {ok, S} = gen_udp:open(0, [binary, {active, false}]),
                        ok = iter_raw(MsgType, Pos, Host, "UDP", State,
                                      {udp, Ip, Port, S}, Pid, CallId, PerProcess),
                        gen_udp:close(S);
                    tcp ->
                        {ok, S} = gen_tcp:connect(Ip, Port, [binary, {active, false}]),
                        ok = iter_raw(MsgType, Pos, Host, "TCP", State, 
                                      {tcp, S}, Pid, CallId, PerProcess),
                        gen_tcp:close(S);
                    tls ->
                        Cert = filename:join(code:priv_dir(nksip), "cert.pem"),
                        Key = filename:join(code:priv_dir(nksip), "key.pem"),
                        TcpOpts = [{certfile, Cert}, {keyfile, Key}, 
                                    binary, {active, false}],
                        {ok, S} = ssl:connect(Ip, Port, TcpOpts),
                        ok = iter_raw(MsgType, Pos, Host, "TLS", State, 
                                    {tls, S}, Pid, CallId, PerProcess),
                        ssl:close(S)
                end
            end,
            empty(),
            Start = now(),
            [proc_lib:spawn(fun() -> Fun(Pos) end) || Pos <- lists:seq(1, Processes)],
            Ok = wait(Total, 0),
            Stop = now();
        full ->
            case NoAutoStart of
                true -> ok;
                false -> ok = start_clients(Processes)
            end,
            RUri = "<sip:"++State++"@"++Host++":"++integer_to_list(Port)++";transport="++
                    atom_to_list(Transport) ++ ">",
            Fun = fun(Pos) -> 
                ok = iter_full(MsgType, Pos, RUri, Pid, CallId, PerProcess) 
            end,
            timer:sleep(100),
            empty(),
            Start = now(),
            [proc_lib:spawn(fun() -> Fun(Pos) end) || Pos <- lists:seq(1, Processes)],
            Ok = wait(Total, 0),
            Stop = now(),
            case NoAutoStart of
                true -> ok;
                false -> ok = stop_clients(Processes)
            end
    end,
    io:format("\n"),
    Secs = timer:now_diff(Stop, Start) / 1000000,
    {Total, 100*Ok/Total, round(Total / Secs)}.



%% @doc Starts `N' simultaneous clients.
start_clients(N) ->
    start_clients(1, N).

start_clients(Pos, Max) when Pos > Max ->
    ok;
start_clients(Pos, Max) ->
    Opts = [{transports, [udp,tcp,tls]}],
    case nksip:start({client, Pos}, nksip_loadtest_sipapp, [{client, Pos}], Opts) of
        {ok, _} -> start_clients(Pos+1, Max);
        {error, already_started} -> start_clients(Pos+1, Max);
        _ -> error
    end.
                    

%% @doc Stops `N' simultaneous clients started using {@link start_clients/1}.
stop_clients(N) ->
    stop_clients(1, N).

stop_clients(Pos, Max) when Pos > Max ->
    ok;
stop_clients(Pos, Max) ->
    nksip:stop({client, Pos}),
    stop_clients( Pos+1, Max).



%% ===================================================================
%% Private
%% ===================================================================

%% @private
iter_full(_, _, _, _, _, 0) -> 
    ok;

iter_full(MsgType, Pos, RUri, Pid, CallId0, Messages) ->
    CallId = list_to_binary([integer_to_list(Pos), $-, integer_to_list(Messages),
                             $-, CallId0]),
    Opts =[{call_id, CallId}],
    Ok = try
        case MsgType of
            options -> 
                case nksip_uac:options({client, Pos}, RUri, Opts) of
                    {ok, 200, []} -> ok;
                    Other -> throw({invalid_options_response, Other})
                end;
            register ->
                From = <<"sip:", (nksip_lib:to_binary(Pos))/binary, "@localhost">>,
                Opts1 = [contact, {from, From}, to_as_from|Opts],
                case nksip_uac:register({client, Pos}, RUri, Opts1) of
                    {ok, 200, []} -> ok;
                    Other -> throw({invalid_register_response, Other})
                end;
            invite ->
                case nksip_uac:invite({client, Pos}, RUri, Opts) of
                    {ok, 200, [{dialog, D}]} -> 
                        case nksip_uac:ack(D, []) of
                            ok -> 
                                case nksip_uac:bye(D, []) of
                                    {ok, 200, []} -> ok;
                                    Other3 -> throw({invalid_bye_response, Other3}) 
                                end;
                            Other2 ->
                                throw({invalid_ack_response, Other2})   
                        end;
                    Other1 -> 
                        throw({invalid_invite_response, Other1})
                end
        end,
        true
    catch
        throw:E -> 
            io:format("\nError in ~s: ~p\n", [CallId, E]),
            false
    end,
    Pid ! {nk, Ok},
    iter_full(MsgType, Pos, RUri, Pid, CallId0, Messages-1).



%% @private
iter_raw(_, _, _, _, _, _, _, _, 0) -> 
    ok;

iter_raw(MsgType, Pos, Host, TransStr, State, Transport, Pid, CallId0, Messages) 
         when MsgType==options; MsgType==register->
    CallId = list_to_binary([integer_to_list(Pos), $-, integer_to_list(Messages),
                             $-, CallId0]),
    Msg = case MsgType of
        options -> options(State++"@"++Host, TransStr, CallId);
        register -> register(State++"@"++Host, TransStr, CallId)
    end,
    send(Transport, Msg),
    Ok = try 
        <<"SIP/2.0 200", _/binary>> = Resp = recv(Transport),
        case MsgType of
            options ->
                {_, _} = binary:match(Resp, <<"Call-ID: ", CallId/binary>>);
            register ->
                {_, _} = binary:match(Resp, <<"Contact: <sip:contact@server:1234>">>)
        end,
        true
    catch
        throw:E -> 
            io:format("\nError in ~s: ~p\n", [CallId, E]),
            false
    end,
    Pid ! {nk, Ok},
    iter_raw(MsgType, Pos, Host, TransStr, State, Transport, Pid, CallId0, Messages-1);

iter_raw(MsgType, Pos, Host, TransStr, State, Transport, Pid, CallId0, Messages) 
        when MsgType==invite ->
    CallId = list_to_binary([integer_to_list(Pos), $-, integer_to_list(Messages),
                             $-, CallId0]),
    Invite = invite("stateful@"++Host, TransStr, CallId),
    send(Transport, Invite),
    Ok = try
        <<"SIP/2.0 200", _/binary>> = Resp1 = recv(Transport),
        {match, [To]} = re:run(Resp1, <<"To: (.*)\r">>, 
                                        [{capture, all_but_first, list}]),
        Ack = ack(Host, TransStr, CallId, To),
        send(Transport, Ack),
        Bye = bye(Host, TransStr, CallId, To),
        send(Transport, Bye),
        <<"SIP/2.0 200", _/binary>> = recv(Transport),
        true
    catch
        throw:E -> 
            io:format("\nError in ~s: ~p\n", [CallId, E]),
            false
    end,
    Pid ! {nk, Ok},
    iter_raw(MsgType, Pos, Host, TransStr, State, Transport, Pid, CallId0, Messages-1).


%% @private
empty() -> 
    receive {nk, _T} -> empty() after 0 -> ok end.


%% @private
wait(0, Ok) -> 
    Ok;
wait(Messages, Ok) ->
    if Messages rem 1000 == 0 -> io:format("~p ", [Messages]); true -> ok end,
    receive
        {nk, true} -> wait(Messages-1, Ok+1);
        {nk, false} -> wait(Messages-1, Ok)
    after
        2*?TIMEOUT -> Ok
    end.


%% @private
send({udp, Ip, Port, S}, Msg) -> ok = gen_udp:send(S, Ip, Port, Msg);
send({tcp, S}, Msg) -> ok = gen_tcp:send(S, Msg);
send({tls, S}, Msg) ->ok = ssl:send(S, Msg).

%% @private
recv({udp, _Ip, _Port, S}) -> {ok, {_, _, Bin}} = gen_udp:recv(S, 0, ?TIMEOUT), Bin;
recv({tcp, S}) -> {ok, Bin} = gen_tcp:recv(S, 0, ?TIMEOUT), Bin;
recv({tls, S}) -> {ok, Bin} = ssl:recv(S, 0, ?TIMEOUT), Bin.


%% @private
options(Host, Transport, CallId) -> 
    Tag = erlang:phash2(CallId),
    [
        <<"OPTIONS sip:">>, Host, <<" SIP/2.0\r\n">>,
        <<"Via: SIP/2.0/">>, Transport, 32, <<" 127.0.0.1;branch=z9hG4bK">>, 
            nksip_lib:luid(), <<";rport\r\n">>,
        <<"From: <sip:test@nksip>;tag=">>, integer_to_list(Tag), <<"\r\n">>,
        <<"To: <sip:test@nksip>\r\n">>,
        <<"Call-ID: ">>, CallId,  <<"\r\n">>,
        <<"CSeq: 1 OPTIONS\r\n">>,
        <<"Content-Length: 4\r\n">>,
        <<"Max-Forwards: 70\r\n">>,
        <<"Accept: text/plain\r\n\r\n1234">> 
    ].

%% @private
register(Host, Transport, CallId) -> 
    Tag = erlang:phash2(CallId),
    [
        <<"REGISTER sip:">>, Host, <<" SIP/2.0\r\n">>,
        <<"Via: SIP/2.0/">>, Transport, 32, <<" 127.0.0.1;branch=z9hG4bK">>, 
            nksip_lib:luid(), <<";rport\r\n">>,
        <<"From: <sip:test@nksip>;tag=">>, integer_to_list(Tag), <<"\r\n">>,
        <<"To: <sip:test@nksip>\r\n">>,
        <<"Call-ID: ">>, CallId,  <<"\r\n">>,
        <<"CSeq: 1 REGISTER\r\n">>,
        <<"Content-Length: 0\r\n">>,
        <<"Max-Forwards: 70\r\n">>,
        <<"Contact: <sip:contact@server:1234>\r\n\r\n">> 
    ].


%% @private
invite(Host, Transport, CallId) -> 
    Tag = erlang:phash2(CallId),
    [
        <<"INVITE sip:">>, Host, <<" SIP/2.0\r\n">>,
        <<"Via: SIP/2.0/">>, Transport, 32, <<" 127.0.0.1;branch=z9hG4bK">>, 
            nksip_lib:luid(), <<";rport\r\n">>,
        <<"From: <sip:test@nksip>;tag=">>, integer_to_list(Tag), <<"\r\n">>,
        <<"To: <sip:test@nksip>\r\n">>,
        <<"Call-ID: ">>, CallId,  <<"\r\n">>,
        <<"CSeq: 1 INVITE\r\n">>,
        <<"Content-Length: 131\r\n">>,
        <<"Max-Forwards: 70\r\n">>,
        <<"Contact: <sip:contact@server:1234>\r\n\r\n"
        "v=0\r\n"
        "o=- 1363363687 1363363687 IN IP4 127.0.0.1\r\n"
        "s=nksip\r\n"
        "c=IN IP4 127.0.0.1\r\n"
        "t=0 0\r\n"
        "m=audio 1080 RTP/AVP 0\r\n"
        "a=rtpmap:0 PCMU/8000\r\n">>
    ].

%% @private
ack(Host, Transport, CallId, To) -> 
    Tag = erlang:phash2(CallId),
    [
        <<"ACK sip:">>, Host, <<" SIP/2.0\r\n">>,
        <<"Via: SIP/2.0/">>, Transport, 32, <<" 127.0.0.1;branch=z9hG4bK">>, 
            nksip_lib:luid(), <<";rport\r\n">>,
        <<"From: <sip:test@nksip>;tag=">>, integer_to_list(Tag), <<"\r\n">>,
        <<"To: ">>, To, <<"\r\n">>,
        <<"Call-ID: ">>, CallId, <<"\r\n">>,
        <<"CSeq: 1 ACK\r\n">>,
        <<"Content-Length: 0\r\n">>,
        <<"Max-Forwards: 70\r\n">>,
        <<"Contact: <sip:contact@server:1234>\r\n\r\n">>
    ].

%% @private
bye(Host, Transport, CallId, To) -> 
    Tag = erlang:phash2(CallId),
    [
        <<"BYE sip:">>, Host, <<" SIP/2.0\r\n">>,
        <<"Via: SIP/2.0/">>, Transport, 32, <<" 127.0.0.1;branch=z9hG4bK">>, 
            nksip_lib:luid(), <<";rport\r\n">>,
        <<"From: <sip:test@nksip>;tag=">>, integer_to_list(Tag), <<"\r\n">>,
        <<"To: ">>, To, <<"\r\n">>,
        <<"Call-ID: ">>, CallId, <<"\r\n">>,
        <<"CSeq: 2 BYE\r\n">>,
        <<"Content-Length: 0\r\n">>,
        <<"Max-Forwards: 70\r\n">>,
        <<"Contact: <sip:contact@server:1234>\r\n\r\n">>
    ].



