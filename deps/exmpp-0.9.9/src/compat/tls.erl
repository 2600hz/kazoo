%%%----------------------------------------------------------------------
%%% File    : tls.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to openssl
%%% Created : 24 Jul 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%% @doc
%% This <strong>{@module}</strong> module is for compatibility with ejabberd.

-module(tls).
-author('alexey@process-one.net').

-export([start/0, start_link/0,
	 tcp_to_tls/2, tls_to_tcp/1,
	 send/2,
	 recv/2, recv/3, recv_data/2,
	 setopts/2,
	 sockname/1, peername/1,
	 controlling_process/2,
	 close/1,
	 get_peer_certificate/1,
	 get_verify_result/1]).

-define(SET_CERTIFICATE_FILE_ACCEPT, 1).
-define(SET_CERTIFICATE_FILE_CONNECT, 2).
-define(SET_ENCRYPTED_INPUT,  3).
-define(SET_DECRYPTED_OUTPUT, 4).
-define(GET_ENCRYPTED_OUTPUT, 5).
-define(GET_DECRYPTED_INPUT,  6).
-define(GET_PEER_CERTIFICATE, 7).
-define(GET_VERIFY_RESULT,    8).

start() ->
    exmpp_tls:start().

start_link() ->
    exmpp_tls:start_link().


tcp_to_tls(TCPSocket, Options) ->
    case lists:keysearch(certfile, 1, Options) of
	{value, {certfile, CertFile}} ->
	    Socket_Desc = {gen_tcp, TCPSocket},
	    Command = case lists:member(connect, Options) of
			  true ->
			      connect;
			  false ->
			      accept
		      end,
	    try
		TLSSock = exmpp_tls:Command(Socket_Desc,
					    {x509, CertFile, CertFile},
					    false, []),
		{ok, TLSSock}
	    catch
		Exception ->
		    {error, Exception}
	    end;
	false ->
	    {error, no_certfile}
    end.

tls_to_tcp(TLSSock) ->
    {gen_tcp, TCPSocket} = exmpp_tls:quiet_shutdown(TLSSock),
    TCPSocket.

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(TLSSock, _Length, Timeout) ->
    exmpp_tls:recv(TLSSock, Timeout).

recv_data(_TLSSock, "") ->
    %% This catch an recv_data with no data that ejabberd send to initiate
    %% implicitly the handshake.
    {ok, <<>>};
recv_data(TLSSock, Packet) ->
    exmpp_tls:recv_data(TLSSock, Packet).

send(TLSSock, Packet) ->
    exmpp_tls:send(TLSSock, Packet).


setopts(TLSSock, Opts) ->
    exmpp_tls:setopts(TLSSock, Opts).

sockname(TLSSock) ->
    exmpp_tls:sockname(TLSSock).

peername(TLSSock) ->
    exmpp_tls:peername(TLSSock).

controlling_process(TLSSock, Pid) ->
    exmpp_tls:controlling_process(TLSSock, Pid).

close(TLSSock) ->
    exmpp_tls:close(TLSSock).

get_peer_certificate(TLSSock) ->
    try
	Cert = exmpp_tls:get_peer_certificate(TLSSock),
	{ok, Cert}
    catch
	_Exception ->
	    error
    end.

get_verify_result(TLSSock) ->
    exmpp_tls:get_verify_result(TLSSock).


