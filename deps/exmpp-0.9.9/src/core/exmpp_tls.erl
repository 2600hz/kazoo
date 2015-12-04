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

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions to handle a
%% TLS session.

-module(exmpp_tls).

-behaviour(gen_server).

%% Initialization.
-export([
	 start/0,
	 start_link/0
	]).

%% Registry handling.
-export([
	 register_engine/3,
	 register_engine/4,
	 get_auth_methods/0,
	 get_engine_names/0,
	 get_engine_names/1,
	 get_prefered_engine_name/1,
	 is_engine_available/1,
	 get_engine_driver/1
	]).

%% Handshake and other TLS operations.
-export([
	 connect/4,
	 accept/4,
         handshake/5,
	 get_peer_certificate/1,
	 get_verify_result/1,
	 shutdown/1,
	 shutdown/2,
	 shutdown/3,
	 quiet_shutdown/1,
	 get_peer_finished/1,
	 get_finished/1
	]).

%% Common socket API.
-export([
	 send/2,
	 send_data/2,
	 recv/1,
	 recv/2,
	 recv_data/2,
	 getopts/2,
	 setopts/2,
	 peername/1,
	 sockname/1,
	 controlling_process/2,
	 close/1,
	 port_revision/1
	]).

%% gen_server(3erl) callbacks.
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-record(state, {
	  engines,
	  by_auth_method
	 }).

-record(tls_engine, {
	  name,
	  driver_path,
	  driver,
	  auth_methods = []
	 }).

-record(tls_socket, {
	  socket,
	  packet_mode = binary,
	  port
	 }).

-define(SERVER, ?MODULE).

-define(COMMAND_SET_MODE,              1).
-define(COMMAND_SET_IDENTITY,          2).
-define(COMMAND_SET_PEER_VERIF,        3).
-define(COMMAND_SET_TRUSTED_CERTS,     4).
-define(COMMAND_SET_OPTIONS,           5).
-define(COMMAND_PREPARE_HANDSHAKE,     6).
-define(COMMAND_HANDSHAKE,             7).
-define(COMMAND_SET_ENCRYPTED_INPUT,   8).
-define(COMMAND_GET_DECRYPTED_INPUT,   9).
-define(COMMAND_SET_DECRYPTED_OUTPUT, 10).
-define(COMMAND_GET_ENCRYPTED_OUTPUT, 11).
-define(COMMAND_GET_PEER_CERTIFICATE, 12).
-define(COMMAND_GET_VERIFY_RESULT,    13).
-define(COMMAND_SHUTDOWN,             14).
-define(COMMAND_QUIET_SHUTDOWN,       15).
-define(COMMAND_PORT_REVISION,        16).
-define(COMMAND_GET_PEER_FINISHED,    17).
-define(COMMAND_GET_FINISHED,         18).

%% --------------------------------------------------------------------
%% Initialization.
%% --------------------------------------------------------------------

%% @hidden

start() ->
    Ret = gen_server:start({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    Ret.

%% @hidden

start_link() ->
    Ret = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    Ret.


-define(REGISTER_OPENSSL,
	%% crypto module installs various global OpenSSL callbacks
	%% that make OpenSSL thread-safe. The OpenSSL driver will
	%% detect and make use of it, so ensure that crypto is
	%% started before loading the driver.
	crypto:start(),
	register_builtin_engine(openssl, exmpp_tls_openssl,
				[{x509, 10}])).

-ifdef(HAVE_GNUTLS).
-define(REGISTER_GNUTLS,
	register_builtin_engine(gnutls, exmpp_tls_gnutls,
				[{x509, 20}, {openpgp, 10}])).
-else.
-define(REGISTER_GNUTLS, ok).
-endif.

register_builtin_engines() ->
	 %io:format("exmpp tls builtin~n",[]),
    ?REGISTER_OPENSSL,
    ?REGISTER_GNUTLS,
    ok.

register_builtin_engine(Name, Driver, Auth_Methods) ->
    try
        register_engine(Name, Driver, Auth_Methods)
    catch
        throw:{port_driver, load, Reason, Driver_Name} ->
            error_logger:warning_msg("Failed to load driver \"~s\": ~s~n",
				     [Driver_Name,
				      erl_ddll:format_error(Reason)])
    end.

%% --------------------------------------------------------------------
%% Registry handling.
%% --------------------------------------------------------------------

%% @spec (Name, Driver, Auth_Methods) -> ok
%%     Name = atom()
%%     Driver = atom()
%%     Auth_Mehods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new TLS engine.

register_engine(Name, Driver, Auth_Methods) ->
    register_engine(Name, undefined, Driver, Auth_Methods).

%% @spec (Name, Driver_Path, Driver, Auth_Methods) -> ok
%%     Name = atom()
%%     Driver_Path = string()
%%     Driver = atom()
%%     Auth_Mehods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new TLS engine.

register_engine(Name, Driver_Path, Driver, Auth_Methods)
  when is_atom(Name), is_list(Auth_Methods), length(Auth_Methods) > 0 ->
    Engine = #tls_engine{
      name = Name,
      driver_path = Driver_Path,
      driver = Driver,
      auth_methods = Auth_Methods
     },
    case gen_server:call(?SERVER, {register_engine, Engine}) of
        ok                 -> ok;
        {error, Exception} -> throw(Exception)
    end.

%% @spec () -> [Auth_Method]
%%     Auth_Method = atom()
%% @doc Return the list of supported auth methods.

get_auth_methods() ->
    gen_server:call(?SERVER, get_auth_methods).

%% @spec () -> [Engine_Name]
%%     Engine_Name = atom()
%% @doc Return the list of TLS engines.

get_engine_names() ->
    gen_server:call(?SERVER, get_engine_names).

%% @spec (Auth_Method) -> [Engine_Name]
%%     Auth_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the list of TLS engines which support the given auth method.
%%
%% The list is sorted from the most to the least prefered engine.

get_engine_names(Auth_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Auth_Method}),
    [E#tls_engine.name || E <- Engines].

%% @spec (Auth_Method) -> [Engine_Name]
%%     Auth_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the name of the prefered TLS engines which support the
%% given auth method.

get_prefered_engine_name(Auth_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Auth_Method}),
    case Engines of
        []           -> undefined;
        [Engine | _] -> Engine#tls_engine.name
    end.

%% @spec (Engine_Name) -> boolean()
%%     Engine_Name = atom()
%% @doc Tell if `Engine_Name' is available.

is_engine_available(Engine_Name) ->
    case gen_server:call(?SERVER, {get_engine, Engine_Name}) of
        undefined -> false;
        _         -> true
    end.

%% @spec (Engine_Name) -> Driver_Name
%%     Engine_Name = atom()
%%     Driver_Name = atom()
%% @doc Return the port driver name associated to the given engine.

get_engine_driver(Engine_Name) ->
    case gen_server:call(?SERVER, {get_engine, Engine_Name}) of
        undefined                         -> undefined;
        #tls_engine{driver = Driver_Name} -> Driver_Name
    end.

%% --------------------------------------------------------------------
%% Handshake and other TLS operations.
%% --------------------------------------------------------------------

%% @spec (Socket_Desc, Identity, Peer_Verification, Options) -> TLS_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Identity = {Auth_Method, Certificate, Private_Key} | undefined
%%     Auth_Method = atom()
%%     Certificate = string()
%%     Private_Key = string()
%%     Peer_Verification = boolean() | Peer_Name
%%     Peer_Name = string()
%%     Options = [Option]
%%     Option = {engine, Engine} | {mode, Mode} | {trusted_certs, Auth_Method, Certs} | peer_cert_required | accept_expired_cert | accept_revoked_cert | accept_non_trusted_cert | accept_corrupted_cert
%%     Engine = atom()
%%     Mode = binary | list
%%     TLS_Socket = tls_socket()
%% @doc Start TLS handshake as a client.

connect(Socket_Desc, Identity, Peer_Verification, Options) ->
    handshake(client, Socket_Desc, Identity, Peer_Verification, Options).

%% @spec (Socket_Desc, Identity, Peer_Verification, Options) -> TLS_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Identity = {Auth_Method, Certificate, Private_Key}
%%     Auth_Method = atom()
%%     Certificate = string()
%%     Private_Key = string()
%%     Peer_Verification = boolean() | Peer_Name
%%     Peer_Name = string()
%%     Options = [Option]
%%     Option = {engine, Engine} | {mode, Mode} | {trusted_certs, {Auth_Method, Certs}} | peer_cert_required | accept_expired_cert | accept_revoked_cert | accept_non_trusted_cert | accept_corrupted_cert
%%     Engine = atom()
%%     Mode = binary | list
%%     TLS_Socket = tls_socket()
%% @doc Start TLS handshake as a server.

accept(Socket_Desc, Identity, Peer_Verification, Options) ->
    handshake(server, Socket_Desc, Identity, Peer_Verification, Options).

handshake(Mode, Socket_Desc, Identity, Peer_Verification, Options) ->
    handshake(Mode, Socket_Desc, Identity, Peer_Verification, Options,
	      infinity).

handshake(Mode, Socket_Desc, Identity, Peer_Verification, Options,
	  Recv_Timeout) ->
    %% Check arguments.
    check_identity(Identity, Mode),
    check_peer_verification(Peer_Verification, Mode),

    %% We save the 'active' state of the socket.
    Is_Active = case exmpp_internals:gen_getopts(Socket_Desc, [active]) of
		    {ok, [{active, Active}]} ->
			    Active;
		    {error, Reason} ->
			    throw({tls, handshake, getopts, Reason})
		end,

    %% Start a port driver instance.
    Driver_Name = get_engine_from_args(Identity, Peer_Verification, Options),
    Port = exmpp_internals:open_port(Driver_Name),

    %% Initialize the port and handshake.
    try
	%% Set mode (client vs. server).
        engine_set_mode(Port, Mode),

	%% Set local peer identity.
        engine_set_identity(Port, Identity),

	%% Enable (or not) peer's certificate verification.
        engine_set_peer_verification(Port, Peer_Verification),

	%% Packet mode.
        Packet_Mode = proplists:get_value(mode, Options, binary),

	%% Set trusted certificates.
        engine_set_trusted_certs(Port,
				 proplists:get_value(trusted_certs, Options)),

	%% Set flags.
        engine_set_options(Port, peer_cert_required,
			   proplists:get_bool(peer_cert_required, Options)),
        engine_set_options(Port, accept_expired_cert,
			   proplists:get_bool(accept_expired_cert, Options)),
        engine_set_options(Port, accept_non_trusted_cert,
			   proplists:get_bool(accept_non_trusted_cert, Options)),
        engine_set_options(Port, accept_revoked_cert,
			   proplists:get_bool(accept_revoked_cert, Options)),
        engine_set_options(Port, accept_corrupted_cert,
			   proplists:get_bool(accept_corrupted_cert, Options)),

	%% Before anything use of the socket, we must disable the 'active'
	%% mode. Otherwise, we can't receive any data.
        exmpp_internals:gen_setopts(Socket_Desc, [{active, false}]),

	%% Handshake!
        engine_prepare_handshake(Port),
        TLS_Socket = handshake2(Mode, Socket_Desc, Port, Recv_Timeout),

	%% We can now restore the 'active' mode.
        exmpp_internals:gen_setopts(Socket_Desc, [{active, Is_Active}]),

        TLS_Socket#tls_socket{packet_mode = Packet_Mode}
    catch
        _:Exception ->
            exmpp_internals:close_port(Port),
            exmpp_internals:gen_setopts(Socket_Desc, [{active, Is_Active}]),
            throw(Exception)
    end.

handshake2(client = Mode, Socket_Desc, Port, Recv_Timeout) ->
    %% Try to handshake.
    case engine_handshake(Port) of
        want_read ->
	    %% Send the current data.
            New_Packet = engine_get_encrypted_output(Port),
            case exmpp_internals:gen_send(Socket_Desc, New_Packet) of
                ok ->
		    %% Wait for a packet from the client.
                    case exmpp_internals:gen_recv(Socket_Desc, Recv_Timeout) of
                        {ok, Packet} ->
                            engine_set_encrypted_input(Port, Packet),
			    %% Recurse!
                            handshake2(Mode, Socket_Desc, Port, Recv_Timeout);
                        {error, Reason} ->
                            throw({tls, handshake, underlying_recv, Reason})
                    end;
                {error, Reason} ->
                    throw({tls, handshake, underlying_send, Reason})
            end;
        ok ->
	    %% Handshake done.
            #tls_socket{socket = Socket_Desc, port = Port}
    end;
handshake2(server = Mode, Socket_Desc, Port, Recv_Timeout) ->
    %% Wait for a packet from the client.
    case exmpp_internals:gen_recv(Socket_Desc, Recv_Timeout) of
        {ok, Packet} ->
            engine_set_encrypted_input(Port, Packet),
	    %% Try to handshake.
            case engine_handshake(Port) of
                want_read ->
		    %% Send the current data.
                    New_Packet = engine_get_encrypted_output(Port),
                    case exmpp_internals:gen_send(Socket_Desc, New_Packet) of
                        ok ->
			    %% Recurse!
                            handshake2(Mode, Socket_Desc, Port, Recv_Timeout);
                        {error, Reason} ->
                            throw({tls, handshake, underlying_send, Reason})
                    end;
                ok ->
                    New_Packet = engine_get_encrypted_output(Port),
                    case exmpp_internals:gen_send(Socket_Desc, New_Packet) of
                        ok ->
			    %% Handshake done.
                            #tls_socket{socket = Socket_Desc, port = Port};
                        {error, Reason} ->
                            throw({tls, handshake, underlying_send, Reason})
                    end
            end;
        {error, Reason} ->
            throw({tls, handshake, underlying_recv, Reason})
    end.

%% @spec (TLS_Socket) -> Certificate | undefined
%%     TLS_Socket = tls_socket()
%%     Certificate = certificate()
%%     Reason = term()
%% @throws {tls, peer_certificate, decode_failed, Reason}
%% @doc Return the peer certificate if he provided one.
%%
%% Note that a client will only send a certificate when requested by a server.
%% This means that in the server case, this function will return anything
%% only when peer verification is enabled.
%%
%% Certificate is returned as a
%% <a href="http://erlang.org/doc/apps/public_key/cert_records.html">
%% public_key certificate record</a>.

get_peer_certificate(#tls_socket{port = Port}) ->
    case engine_get_peer_certificate(Port) of
        undefined ->
            undefined;
        Bin_Cert ->
	    try
		case public_key:pkix_decode_cert(Bin_Cert, plain) of
		    {ok, Cert} ->
			%% R13 and earlier
			Cert;
		    {error, Reason} ->
			%% R13 and earlier
			throw({tls, peer_certificate, decode_failed, Reason});
		    Certificate ->
			%% starting from R14, pkix_decode_cert/2 simply returns
			%% decoded certificate and uses erlang:error/1 for errors
			Certificate
		end
	    catch
		_:Exception ->
		    throw({tls, peer_certificate, decode_failed, Exception})
	    end
    end.

%% @spec (TLS_Socket) -> Result
%%     TLS_Socket = tls_socket()
%%     Result = integer()
%% @doc Return verify result.

get_verify_result(#tls_socket{port = Port}) ->
    engine_get_verify_result(Port).

%% @spec (TLS_Socket) -> Socket_Desc
%%     TLS_Socket = tls_socket()
%%     Socket_Desc = {Mod, Socket}
%% @doc Shutdown the TLS session.
%%
%% Only a unidirectional shutdown is made.
%%
%% The underlying socket is NOT closed.

shutdown(TLS_Socket) ->
    shutdown(TLS_Socket, unidirectional).

%% @spec (TLS_Socket, Mode) -> Socket_Desc
%%     TLS_Socket = tls_socket()
%%     Mode = unidirectional | bidirectional
%%     Socket_Desc = {Mod, Socket}
%% @doc Shutdown the TLS session.
%%
%% For bidirectional shutdown, there's no timeout for the peer's
%% "close notify".
%%
%% The underlying socket is NOT closed.

shutdown(TLS_Socket, Mode) ->
    shutdown(TLS_Socket, Mode, infinity).

%% @spec (TLS_Socket, Mode, Timeout) -> Socket_Desc
%%     TLS_Socket = tls_socket()
%%     Mode = unidirectional | bidirectional
%%     Timeout = infinity | integer()
%%     Socket_Desc = {Mod, Socket}
%% @doc Shutdown the TLS session.
%%
%% For bidirectional shutdown, the peer must send his "close notify" within
%% `Timeout' milliseconds.
%%
%% If the peer doesn't send its "close notify" or if the underlying socket
%% is closed, the function still succeeds.
%%
%% The underlying socket is NOT closed by this function.

shutdown(#tls_socket{socket = Socket_Desc, port = Port} = TLS_Socket,
	 Mode, Timeout) ->
    %% Start/continue the shutdown process.
    case engine_shutdown(Port) of
        want_read when Mode == bidirectional ->
	    %% Wait for a packet from the client.
            case exmpp_internals:gen_recv(Socket_Desc, Timeout) of
                {ok, Packet} ->
                    engine_set_encrypted_input(Port, Packet),
		    %% Recurse!
                    shutdown(TLS_Socket);
                {error, timeout} ->
		    %% The peer didn't send its "close notify".
		    %% XXX Should this be treated as an error (the caller
		    %% asked for a bidirectional shutdown)?
                    exmpp_internals:close_port(Port),
                    Socket_Desc;
                {error, closed} ->
		    %% The peer closed the underlying socket.
		    %% XXX Should this be treated as an error (the purpose
		    %% was not to close the socket)?
                    exmpp_internals:close_port(Port),
                    Socket_Desc;
                {error, Reason} ->
                    throw({tls, shutdown, underlying_recv, Reason})
            end;
        want_write ->
	    %% The "close notify" is ready to be sent.
            New_Packet = engine_get_encrypted_output(Port),
            case exmpp_internals:gen_send(Socket_Desc, New_Packet) of
                ok ->
		    %% Skip to the next step.
                    shutdown(TLS_Socket);
                {error, Reason} ->
                    throw({tls, shutdown, underlying_send, Reason})
            end;
        _ ->
	    %% The shutdown is complete (or unidirectionnal shutdown was
	    %% prefered).
            exmpp_internals:close_port(Port),
            Socket_Desc
    end.

%% @spec (TLS_Socket) -> Socket_Desc
%%     TLS_Socket = tls_socket()
%%     Socket_Desc = {Mod, Socket}
%% @doc Flag the TLS session as down but do not exchange "close notify".
%%
%% The underlying socket is NOT closed.

quiet_shutdown(#tls_socket{socket = Socket_Desc, port = Port}) ->
    engine_quiet_shutdown(Port),
    exmpp_internals:close_port(Port),
    Socket_Desc.

%% @spec (TLS_Socket) -> Finished
%%     TLS_Socket = tls_socket()
%%     Finished = binary()
%% @doc Retrieve latest "Finished" message (received on this side).
%%
%% "Finished" message is needed for tls-unique channel binding,
%% used for example by SCRAM-SHA-1-PLUS SASL method.

get_peer_finished(#tls_socket{port = Port}) ->
    engine_get_peer_finished(Port).

%% @spec (TLS_Socket) -> Finished
%%     TLS_Socket = tls_socket()
%%     Finished = binary()
%% @doc Retrieve latest "Finished" message (sent out from this side).
%%
%% "Finished" message is needed for tls-unique channel binding,
%% used for example by SCRAM-SHA-1-PLUS SASL method.

get_finished(#tls_socket{port = Port}) ->
    engine_get_finished(Port).

%% --------------------------------------------------------------------
%% Handshake helpers.
%% --------------------------------------------------------------------

%% Choose the most appropriate engine.
get_engine_from_args(Identity, _Peer_Verification, Options) ->
    Engine_Name =
	case get_engine_from_options(Options) of
	    undefined ->
		case get_engine_from_identity(Identity) of
		    undefined ->
			case get_engine_from_verification(Options) of
			    undefined ->
				throw({tls, options, no_engine_available,
				       undefined});
			    Name ->
				Name
			end;
		    Name ->
			Name
		end;
	    Name ->
		case is_engine_available(Name) of
		    true ->
			Name;
		    false ->
			throw({tls, options, engine_unavailable, Name})
		end
	end,
    get_engine_driver(Engine_Name).

get_engine_from_options(Options) ->
    proplists:get_value(engine, Options).

get_engine_from_identity(undefined) ->
    get_prefered_engine_name(x509);
get_engine_from_identity({Auth_Method, _, _}) ->
    get_prefered_engine_name(Auth_Method).

get_engine_from_verification(Options) ->
    case proplists:get_value(trusted_certs, Options) of
        undefined   -> undefined;
        Auth_Method -> get_prefered_engine_name(Auth_Method)
    end.

check_identity(Identity, Mode) ->
    case Identity of
        {AM, Cert, PK} when is_atom(AM), is_list(Cert), is_list(PK) ->
            case io_lib:deep_char_list(Cert) of
                false -> throw({tls, handshake, invalid_certificate, Cert});
                _     -> ok
            end,
            case io_lib:deep_char_list(PK) of
                false -> throw({tls, handshake, invalid_private_key, PK});
                _     -> ok
            end;
        undefined when Mode == client ->
            ok;
        undefined when Mode == server ->
            throw({tls, handshake, identity_mandatory, Identity});
        _ ->
            throw({tls, handshake, invalid_identity, Identity})
    end.

check_peer_verification(Peer_Verif, _Mode) ->
    case Peer_Verif of
        true ->
            ok;
        false ->
            ok;
        E when is_list(E) ->
            case io_lib:deep_char_list(E) of
                false -> throw({tls, handshake, invalid_peer_verification, E});
                _     -> ok
            end;
        _ ->
            throw({tls, handshake, invalid_peer_verification, Peer_Verif})
    end.

%% --------------------------------------------------------------------
%% Common socket API.
%% --------------------------------------------------------------------

%% @spec (TLS_Socket, Orig_Packet) -> ok | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Send `Orig_Packet' over a TLS-protected connection.

send(#tls_socket{socket = Socket_Desc} = Socket, Packet) ->
    try
        {ok, Encrypted} = send_data(Socket, Packet),
        exmpp_internals:gen_send(Socket_Desc, Encrypted)
    catch
        Exception ->
            {error, Exception}
    end.

send_data(#tls_socket{port = Port}, Packet) ->
        engine_set_decrypted_output(Port, Packet),
        {ok, engine_get_encrypted_output(Port)}.

%% @spec (TLS_Socket) -> {ok, Orig_Packet} | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a TLS-protected connection.

recv(TLS_Socket) ->
    recv(TLS_Socket, infinity).

%% @spec (TLS_Socket, Timeout) -> {ok, Orig_Packet} | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Timeout = integer()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a TLS-protected connection.

recv(#tls_socket{socket = Socket_Desc, port = Port} = TLS_Socket, Timeout) ->
    try
        case recv_common(TLS_Socket) of
            want_read ->
		%% Ok, we need more data.
                {Recv, New_Timeout} =
		    case Timeout of
			infinity ->
			    {
			  exmpp_internals:gen_recv(Socket_Desc, Timeout),
			  Timeout
			 };
			_ ->
			    {Elapsed, Ret} = timer:tc(exmpp_internals, gen_recv,
						      [Socket_Desc, Timeout]),
			    {Ret, Timeout - Elapsed div 1000}
		    end,
                case Recv of
                    {ok, Packet} ->
                        engine_set_encrypted_input(Port, Packet),
			%% Try to decipher it.
                        recv(TLS_Socket, New_Timeout);
                    {error, Reason} ->
                        {error, Reason}
                end;
            Data ->
		%% Got a chunk of plain-text data.
                {ok, Data}
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (TLS_Socket, Packet) -> {ok, Orig_Packet} | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Packet = binary() | list()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Decrypt already received data.
%%
%% This function won't read anything from the underlying socket but WILL
%% write to it.

recv_data(#tls_socket{port = Port} = TLS_Socket,
	  Packet) ->
    try
	%% Give available encrypted data to the port driver.
        engine_set_encrypted_input(Port, Packet),
	%% Ask for available unencrypted data.
        case recv_all(TLS_Socket) of
            {error, Reason} ->
                {error, Reason};
            Data -> 
                {ok, Data}
        end
    catch
        Exception ->
            {error, Exception}
    end.

recv_all(TLS_Socket) ->
    recv_all(TLS_Socket, <<>>, recv_common(TLS_Socket)).

recv_all(_TLS_Socket, _Accum, {error, Reason}) ->
    {error, Reason};
%%couldn't be want_write also?, in the c driver it says no because it is a BIO_mem
recv_all(#tls_socket{packet_mode = PacketMode}, Accum, want_read) -> 
    case PacketMode of
        list -> binary_to_list(Accum);
        binary -> Accum
    end;
recv_all(TLS_Socket, Accum, Data) when is_binary(Data) ->
    recv_all(TLS_Socket, <<Accum/binary, Data/binary>>, recv_common(TLS_Socket)).


recv_common(#tls_socket{socket = Socket_Desc, port = Port}) ->
    %% Ask for available unencrypted data.
    case engine_get_decrypted_input(Port) of
        want_read ->
	    %% Nothing is ready yet.
            want_read;
        Data ->
	    %% Got a chunk of plain-text data.
            Ack = engine_get_encrypted_output(Port),
            case exmpp_internals:gen_send(Socket_Desc, Ack) of
                ok ->
                    Data;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @spec (TLS_Socket, Options) -> {ok, Option_Values} | {error, posix()}
%%     TLS_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%%     Option_Values = list()
%% @doc Gets one or more options for a socket.

getopts(#tls_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_getopts(Socket_Desc, Options).

%% @spec (TLS_Socket, Options) -> ok | {error, posix()}
%%     TLS_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%% @doc Sets one or more options for a socket.

setopts(#tls_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_setopts(Socket_Desc, Options).

%% @spec (TLS_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     TLS_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the address and port for the other end of a connection.

peername(#tls_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_peername(Socket_Desc).

%% @spec (TLS_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     TLS_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the local address and port number for a socket.

sockname(#tls_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_sockname(Socket_Desc).

%% @spec (TLS_Socket, Pid) -> ok | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Pid = pid()
%%     Reason = term()
%% @doc Change the controlling socket of the underlying socket.

controlling_process(#tls_socket{socket = Socket_Desc}, Pid) ->
    exmpp_internals:gen_controlling_process(Socket_Desc, Pid).

%% @spec (TLS_Socket) -> ok | {error, Reason}
%%     TLS_Socket = tls_socket()
%%     Reason = term()
%% @doc Shutdown the TLS session and close the underlying socket.

close(#tls_socket{socket = Socket_Desc} = TLS_Socket) ->
    try
	%% First, shutdown the TLS session (unidirectional).
        shutdown(TLS_Socket),
	%% Close the underlying socket.
        exmpp_internals:gen_close(Socket_Desc)
    catch
        Exception ->
            {error, Exception}
    end.

%% @hidden

port_revision(#tls_socket{port = Port}) ->
    engine_port_revision(Port).

%% --------------------------------------------------------------------
%% Engine function wrappers.
%% --------------------------------------------------------------------

control(Port, Command, Data) ->
    case port_control(Port, Command, Data) of
        <<0, Result/binary>> -> Result;
        <<1, Error/binary>>  -> {error, binary_to_term(Error)};
        <<2>>                -> want_read;
        <<3>>                -> want_write
    end.

engine_set_mode(Port, Mode) ->
    Mode_ID = case Mode of
		  server -> 1;
		  client -> 2
	      end,
    case control(Port, ?COMMAND_SET_MODE, term_to_binary(Mode_ID)) of
        {error, Reason} ->
            throw({tls, handshake, set_mode_failure, Reason});
        _ ->
            ok
    end.

engine_set_identity(_Port, undefined) ->
    ok;
engine_set_identity(Port, Identity) ->
    case control(Port, ?COMMAND_SET_IDENTITY, term_to_binary(Identity)) of
        {error, Reason} ->
            throw({tls, handshake, set_identity_failure, Reason});
        _ ->
            ok
    end.

engine_set_peer_verification(Port, Peer_Verif) ->
    case control(Port, ?COMMAND_SET_PEER_VERIF, term_to_binary(Peer_Verif)) of
        {error, Reason} ->
            throw({tls, handshake, set_peer_verification, Reason});
        _ ->
            ok
    end.

engine_set_trusted_certs(_Port, undefined) ->
    ok;
engine_set_trusted_certs(Port, Trusted_Certs) ->
    case control(Port, ?COMMAND_SET_TRUSTED_CERTS,
		 term_to_binary(Trusted_Certs)) of
        {error, Reason} ->
            throw({tls, handshake, set_trusted_certs, Reason});
        _ ->
            ok
    end.

engine_set_options(Port, Option, Flag) ->
    case control(Port, ?COMMAND_SET_OPTIONS,
		 term_to_binary({Option, Flag})) of
        {error, Reason} ->
            throw({tls, handshake, set_options, {Option, Reason}});
        _ ->
            ok
    end.

engine_prepare_handshake(Port) ->
    case control(Port, ?COMMAND_PREPARE_HANDSHAKE, <<>>) of
        {error, Reason} ->
            throw({tls, handshake, prepare_handshake, Reason});
        _ ->
            ok
    end.

engine_handshake(Port) ->
    case control(Port, ?COMMAND_HANDSHAKE, <<>>) of
        {error, Reason} ->
            throw({tls, handshake, do_handshake, Reason});
        <<>> ->
            ok;
        Result ->
            Result
    end.

engine_set_encrypted_input(Port, Data) when is_list(Data) ->
    engine_set_encrypted_input(Port, list_to_binary(Data));
engine_set_encrypted_input(Port, Data) ->
    case control(Port, ?COMMAND_SET_ENCRYPTED_INPUT, Data) of
        {error, Reason} ->
            throw({tls, recv, set_encrypted_input, Reason});
        _ ->
            ok
    end.

engine_get_decrypted_input(Port) ->
    case control(Port, ?COMMAND_GET_DECRYPTED_INPUT, term_to_binary(0)) of
        {error, Reason} ->
            throw({tls, recv, get_decrypted_input, Reason});
        Result ->
            Result
    end.

engine_set_decrypted_output(Port, Data) when is_list(Data) ->
    engine_set_decrypted_output(Port, list_to_binary(Data));
engine_set_decrypted_output(Port, Data) ->
    case control(Port, ?COMMAND_SET_DECRYPTED_OUTPUT, Data) of
        {error, Reason} ->
            throw({tls, send, set_decrypted_output, Reason});
        _ ->
            ok
    end.

engine_get_encrypted_output(Port) ->
    case control(Port, ?COMMAND_GET_ENCRYPTED_OUTPUT, <<>>) of
        {error, Reason} ->
            throw({tls, send, get_encrypted_output, Reason});
        Result ->
            Result
    end.

engine_get_peer_certificate(Port) ->
    case control(Port, ?COMMAND_GET_PEER_CERTIFICATE, <<>>) of
        {error, no_certificate} ->
            undefined;
        {error, Reason} ->
            throw({tls, get_peer_certificate, get_peer_certificate_failed,
		   Reason});
        Result ->
            Result
    end.

engine_get_verify_result(Port) ->
    case control(Port, ?COMMAND_GET_VERIFY_RESULT, <<>>) of
        {error, Reason} ->
            throw({tls, get_verify_result, get_verify_result_failed,
		   Reason});
        Result ->
            binary_to_term(Result)
    end.

engine_shutdown(Port) ->
    case control(Port, ?COMMAND_SHUTDOWN, <<>>) of
        {error, Reason} ->
            throw({tls, shutdown, shutdown_failed, Reason});
        <<>> ->
            ok;
        Result ->
            Result
    end.

engine_quiet_shutdown(Port) ->
    case control(Port, ?COMMAND_QUIET_SHUTDOWN, <<>>) of
        {error, Reason} ->
            throw({tls, quiet_shutdown, shutdown_failed, Reason});
        _ ->
            ok
    end.

engine_port_revision(Port) ->
    case control(Port, ?COMMAND_PORT_REVISION, <<>>) of
        {error, Reason} ->
            throw({tls, port_revision, port_revision, Reason});
        Revision ->
            binary_to_term(Revision)
    end.

engine_get_peer_finished(Port) ->
    case control(Port, ?COMMAND_GET_PEER_FINISHED, <<>>) of
	{error, Reason} ->
	    throw({tls, get_peer_finished, get_peer_finished, Reason});
	Result ->
	    Result
    end.

engine_get_finished(Port) ->
    case control(Port, ?COMMAND_GET_FINISHED, <<>>) of
	{error, Reason} ->
	    throw({tls, get_finished, get_finished, Reason});
	Result ->
	    Result
    end.

%% --------------------------------------------------------------------
%% gen_server(3erl) callbacks.
%% --------------------------------------------------------------------

%% @hidden

init([]) ->
    Engines = dict:new(),
    By_AM = dict:new(),
    {ok, #state{engines = Engines, by_auth_method = By_AM}}.

%% @hidden

handle_call({register_engine,
	     #tls_engine{name = Name,
			 auth_methods = Auth_Methods,
			 driver_path = Driver_Path,
			 driver = Driver_Name} = Engine},
	    _From,
	    #state{engines = Engines, by_auth_method = By_AM} = State) ->
    try
	%% Load the driver now.
        case Driver_Path of
            undefined ->
                exmpp_internals:load_driver(Driver_Name);
            _ ->
                exmpp_internals:load_driver(Driver_Name, [Driver_Path])
        end,
	%% Add engine to the global list.
        New_Engines = dict:store(Name, Engine, Engines),
	%% Index engine by its auth methods.
        Fun = fun({AM, Prio}, {E, AM_Dict}) ->
		      New_AM_Dict =
			  case dict:is_key(AM, AM_Dict) of
			      true ->
				  L = [{E, Prio} | dict:fetch(AM, AM_Dict)],
				  New_L = lists:keysort(2, L),
				  dict:store(AM, New_L, AM_Dict);
			      false ->
				  dict:store(AM, [{E, Prio}], AM_Dict)
			  end,
		      {E, New_AM_Dict}
	      end,
        {_, New_By_AM} = lists:foldl(Fun, {Engine, By_AM}, Auth_Methods),
        {reply, ok, State#state{
		      engines = New_Engines,
		      by_auth_method = New_By_AM
		     }}
    catch
        _:Exception ->
            {reply, {error, Exception}, State}
    end;

handle_call(get_auth_methods, _From,
	    #state{by_auth_method = By_AM} = State) ->
    {reply, dict:fetch_keys(By_AM), State};

handle_call(get_engine_names, _From,
	    #state{engines = Engines} = State) ->
    {reply, dict:fetch_keys(Engines), State};

handle_call({get_engines, AM}, _From,
	    #state{by_auth_method = By_AM} = State) ->
    case dict:is_key(AM, By_AM) of
        true  -> {reply, [E || {E, _P} <- dict:fetch(AM, By_AM)], State};
        false -> {reply, [], State}
    end;

handle_call({get_engine, Engine_Name}, _From,
	    #state{engines = Engines} = State) ->
    case dict:is_key(Engine_Name, Engines) of
        true  -> {reply, dict:fetch(Engine_Name, Engines), State};
        false -> {reply, undefined, State}
    end;

handle_call(Request, From, State) ->
    error_logger:info_msg("~p:handle_call/3:~n- Request: ~p~n- From: ~p~n"
			  "- State: ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

%% @hidden

handle_cast(Request, State) ->
    error_logger:info_msg("~p:handle_cast/2:~n- Request: ~p~n"
			  "- State: ~p~n", [?MODULE, Request, State]),
    {noreply, State}.

%% @hidden

handle_info(Info, State) ->
    error_logger:info_msg("~p:handle_info/2:~n- Info: ~p~n"
			  "- State: ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

%% @hidden

code_change(Old_Vsn, State, Extra) ->
    error_logger:info_msg("~p:code_change/3:~n- Old_Vsn: ~p~n- Extra: ~p~n"
			  "- State: ~p~n", [?MODULE, Old_Vsn, Extra, State]),
    {ok, State}.

%% @hidden

terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type tls_socket().
%% TLS socket obtained with {@link connect/4} or {@link accept/4}.
