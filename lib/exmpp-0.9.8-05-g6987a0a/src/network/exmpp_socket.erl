%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Emilio Bustos <emilionicolas@gmail.com>

%% @doc
%% The module <strong>{@module}</strong> manages TCP/IP socket
%% connections to an XMPP server with or without TLS/SSL encryption.
%%
%% <p>
%% This module is intended to be used directly by client developers.
%% </p>

-module(exmpp_socket).

-export([connect/3, send/2, close/2, reset_parser/1, get_property/2,
        compress/1, starttls/2, wping/1
    ]).

%% Internal export
-export([receiver/3]).


% None implemented so far.
get_property(_Socket, _Prop) ->
    {error, undefined}.


reset_parser(ReceiverPid) when is_pid(ReceiverPid) ->
    ReceiverPid ! reset_parser.
    
%% Connect to XMPP server
%% Returns:
%% Ref or throw error
%% Ref is a socket
connect(ClientPid, StreamRef, {Host, Port, Options}) ->
    LocalIP = proplists:get_value(local_ip, Options, undefined),                     
    LocalPort= proplists:get_value(local_port, Options, undefined),                  
    SckType = proplists:get_value(socket_type, Options, gen_tcp),                  
    IPOptions = case LocalIP of                                                                                          
                        undefined -> [];                                           
                        _ ->  case LocalPort of                                                                        
                                undefined -> [{ip, LocalIP}];                     
                                _ -> [{ip, LocalIP}, {port, LocalPort()}]         
                              end                                                                                      
                end,                                                                                                   
    DefaultOptions = [{packet,0}, binary, {active, false}] ++ IPOptions,
    Opts = [{reuseaddr,true}|DefaultOptions],
    case SckType:connect(Host, Port, Opts, 30000) of
	{ok, Socket} ->
            ESocket = {SckType, Socket},
	    %% TODO: Hide receiver failures in API
	    ReceiverPid = spawn_link(?MODULE, receiver,
				     [ClientPid, ESocket, StreamRef]),
	    SckType:controlling_process(Socket, ReceiverPid),
            {ESocket, ReceiverPid};
	{error, Reason} ->
	    erlang:throw({socket_error, Reason})
    end.

% we do a synchronous shutdown of the receiver process, 
% because we want to make sure it isn't going be pushing
% more data to the exmpp_xmlstream after closed. This
% avoid the -useless- crash reports produced when the 
% receiver process read data from socket before received
% the stop message, but after the xmlstream was closed.
% See shutdown order in exmpp_session:terminate/3.
close(_Socket, ReceiverPid) ->
    ReceiverPid ! stop.

send(Socket, XMLPacket) when is_tuple(XMLPacket) ->
    Bin = exmpp_xml:document_to_binary(XMLPacket),
 %     io:format("- SENDING:~n~s~n", [Bin]),
    exmpp_internals:gen_send(Socket, Bin).

wping(Socket) ->
	exmpp_internals:gen_send(Socket, <<"\n">>).

compress(ReceiverPid) ->
    Ref = erlang:make_ref(),
    ReceiverPid ! {compress, self(), Ref},
    receive
        {ok, Ref, Socket} -> {ok, Socket}
    after
        1000 -> timeout
    end.

%% Mode -> client | server
starttls(ReceiverPid, Mode) ->
    Ref = erlang:make_ref(),
    ReceiverPid ! {starttls, self(), Ref, Mode},
    receive
        {ok, Ref, Socket} -> {ok, Socket}
    after
        1000 -> timeout
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
receiver(ClientPid, Socket, StreamRef) ->
    receiver_loop(ClientPid, Socket, StreamRef).

receiver_loop(ClientPid, ESocket, StreamRef) ->
    Socket = get_socket(ESocket),
    exmpp_internals:gen_setopts(ESocket, [{active, once}]),
    receive
	stop -> 
            exmpp_internals:gen_close(ESocket),
	    ok;
	{compress, From, Ref} -> 
            ZSocket = {exmpp_compress, exmpp_compress:enable_compression(ESocket, [])},
            From ! {ok, Ref, ZSocket},
	    receiver_loop(ClientPid, ZSocket, StreamRef);
	{starttls, From, Ref, Mode} -> 
            exmpp_internals:gen_setopts(ESocket, [{active, false}]),
            TSocket = {exmpp_tls, exmpp_tls:handshake(Mode, ESocket, undefined, false, [])},
            From ! {ok, Ref, TSocket},
	    receiver_loop(ClientPid, TSocket, StreamRef);
        {tcp, Socket, Data} ->
            {ok, Str} = recv_data(ESocket, Data),
 %             io:format("- RECEIVING:~n~s~n", [Str]),
	    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, Str),
	    receiver_loop(ClientPid, ESocket, NewStreamRef);
	{ssl, Socket, Data} ->
            {ok, Str} = recv_data(ESocket, Data),
 %             io:format("- RECEIVING:~n~s~n", [Str]),
	    {ok, NewStreamRef} = exmpp_xmlstream:parse(StreamRef, Str),
	    receiver_loop(ClientPid, ESocket, NewStreamRef);
	{tcp_closed, Socket} ->
	    gen_fsm:send_all_state_event(ClientPid, tcp_closed);
	{ssl_closed, Socket} ->
	    gen_fsm:send_all_state_event(ClientPid, tcp_closed);
	{ssl_error,Socket,Reason} ->
	    error_logger:warning_msg([ssl_error,{ssl_socket,Socket},Reason]),
	    gen_fsm:send_all_state_event(ClientPid, tcp_closed);
        reset_parser ->
            receiver_loop(ClientPid, ESocket, exmpp_xmlstream:reset(StreamRef))
    end.

get_socket(Socket) when is_port(Socket) ->
    Socket;
get_socket({sslsocket, _, _} = Socket) ->
    Socket;
get_socket({_, Socket, _, _}) ->
    get_socket(Socket);
get_socket({_Type, Socket}) ->
    get_socket(Socket).

%% exmpp_internals:recv_data
recv_data({gen_tcp, _Socket}, Data) ->
    {ok, Data};
recv_data({ssl, _Socket}, Data) ->
    {ok, Data};
recv_data({_, Socket, _, _}, Data) ->
    recv_data(Socket, Data);
recv_data({Module, Socket}, EData) ->
    {ok, Data} = recv_data(Socket, EData),
    Module:recv_data(Socket, Data).
