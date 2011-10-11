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
%% The module <strong>{@module}</strong> provides functions to handle
%% stream compression.

-module(exmpp_compress).

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
	 get_compress_methods/0,
	 get_engine_names/0,
	 get_engine_names/1,
	 get_prefered_engine_name/1,
	 is_engine_available/1,
	 get_engine_driver/1
	]).

%% Compression activation.
-export([
	 enable_compression/2,
	 disable_compression/1
	]).

%% Common socket API.
-export([
	 send/2,
	 recv/1,
	 recv/2,
	 getopts/2,
	 setopts/2,
	 peername/1,
	 sockname/1,
	 controlling_process/2,
	 close/1,
	 port_revision/1,
	 recv_data/2,
	 send_data/2
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

-record(state, {engines,
		by_compress_method
	       }).

-record(compress_engine, {name,
			  driver_path,
			  driver,
			  compress_methods = []
			 }).

-record(compress_socket, {socket,
			  packet_mode = binary,
			  port
			 }).

-define(SERVER, ?MODULE).
-define(DEFAULT_ENGINE, zlib).

-define(COMMAND_SET_COMPRESS_METHOD, 1).
-define(COMMAND_SET_COMPRESS_LEVEL,  2).
-define(COMMAND_PREPARE_COMPRESS,    3).
-define(COMMAND_PREPARE_UNCOMPRESS,  4).
-define(COMMAND_COMPRESS,            5).
-define(COMMAND_UNCOMPRESS,          6).
-define(COMMAND_SVN_REVISION,        7).

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

-ifdef(HAVE_ZLIB).
-define(REGISTER_ZLIB,
	register_builtin_engine(zlib, exmpp_compress_zlib,
				[{zlib, 10}, {gzip, 10}])).
-else.
-define(REGISTER_ZLIB, ok).
-endif.

register_builtin_engines() ->
    ?REGISTER_ZLIB,
    ok.

register_builtin_engine(Name, Driver, Compress_Methods) ->
    try
        register_engine(Name, Driver, Compress_Methods)
    catch
        throw:{port_driver, load, Reason, Driver_Name} ->
            error_logger:warning_msg("Failed to load driver \"~s\": ~s~n",
				     [Driver_Name,
				      erl_ddll:format_error(Reason)])
    end.

%% --------------------------------------------------------------------
%% Registry handling.
%% --------------------------------------------------------------------

%% @spec (Name, Driver, Compress_Methods) -> ok
%%     Name = atom()
%%     Driver = atom()
%%     Compress_Methods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new compression engine.

register_engine(Name, Driver, Compress_Methods) ->
    register_engine(Name, undefined, Driver, Compress_Methods).

%% @spec (Name, Driver_Path, Driver, Compress_Methods) -> ok
%%     Name = atom()
%%     Driver_Path = string()
%%     Driver = atom()
%%     Compress_Methods = [{atom(), Priority}]
%%     Priority = integer()
%% @doc Add a new compression engine.

register_engine(Name, Driver_Path, Driver, Compress_Methods)
  when is_atom(Name), is_list(Compress_Methods), length(Compress_Methods) > 0 ->
    Engine = #compress_engine{name = Name,
			      driver_path = Driver_Path,
			      driver = Driver,
			      compress_methods = Compress_Methods
			     },
    case gen_server:call(?SERVER, {register_engine, Engine}) of
        ok                 -> ok;
        {error, Exception} -> throw(Exception)
    end.

%% @spec () -> [Compress_Method]
%%     Compress_Method = atom()
%% @doc Return the list of supported compress methods.

get_compress_methods() ->
    gen_server:call(?SERVER, get_compress_methods).

%% @spec () -> [Engine_Name]
%%     Engine_Name = atom()
%% @doc Return the list of compression engines.

get_engine_names() ->
    gen_server:call(?SERVER, get_engine_names).

%% @spec (Compress_Method) -> [Engine_Name]
%%     Compress_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the list of compression engines which support the given compress method.
%%
%% The list is sorted from the most to the least prefered engine.

get_engine_names(Compress_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Compress_Method}),
    [E#compress_engine.name || E <- Engines].

%% @spec (Compress_Method) -> [Engine_Name]
%%     Compress_Method = atom()
%%     Engine_Name = atom()
%% @doc Return the name of the prefered compression engines which support the
%% given compress method.

get_prefered_engine_name(Compress_Method) ->
    case get_prefered_engine(Compress_Method) of
        undefined -> undefined;
        Engine    -> Engine#compress_engine.name
    end.

get_prefered_engine(Compress_Method) ->
    Engines = gen_server:call(?SERVER, {get_engines, Compress_Method}),
    case Engines of
        []           -> undefined;
        [Engine | _] -> Engine
    end.

%% @spec (Engine_Name) -> bool()
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
        undefined                              -> undefined;
        #compress_engine{driver = Driver_Name} -> Driver_Name
    end.

%% --------------------------------------------------------------------
%% Compression activation.
%% --------------------------------------------------------------------

%% @spec (Socket_Desc, Options) -> Compress_Socket
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Options = [Option]
%%     Option = {compress_method, Method} | {engine, Engine} | {mode, Mode} | {compress_level, Level}
%%     Method = atom()
%%     Engine = atom()
%%     Mode = binary | list
%%     Level = integer()
%%     Compress_Socket = compress_socket()
%% @doc Enable compression over the given socket.

enable_compression(Socket_Desc, Options) ->
    %% Start a port driver instance.
    Driver_Name = get_engine_from_options(Options),
    Port = exmpp_internals:open_port(Driver_Name),

    %% Initialize the port.
    try
	%% Set compression method.
        case proplists:get_value(compress_method, Options) of
            undefined -> ok;
            CM        -> engine_set_compress_method(Port, CM)
        end,

	%% Set compression level.
        case proplists:get_value(compress_level, Options) of
            undefined -> ok;
            Level     -> engine_set_compress_level(Port, Level)
        end,

	%% Packet mode.
        Packet_Mode = proplists:get_value(mode, Options, binary),

	%% Enable compression.
        engine_prepare_compress(Port),
        engine_prepare_uncompress(Port),
        #compress_socket{socket = Socket_Desc,
			 packet_mode = Packet_Mode,
			 port = Port}
    catch
        _:Exception ->
            exmpp_internals:close_port(Port),
            throw(Exception)
    end.

%% @spec (Compress_Socket) -> Socket_Desc
%%     Compress_Socket = compress_socket()
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%% @doc Disable compression and return the underlying socket.

disable_compression(#compress_socket{socket = Socket_Desc, port = Port}) ->
    exmpp_internals:close_port(Port),
    Socket_Desc.

%% --------------------------------------------------------------------
%% Activation helpers.
%% --------------------------------------------------------------------

%% Choose the most appropriate engine.
get_engine_from_options(Options) ->
    Engine_Name =
	case proplists:get_value(engine, Options) of
	    undefined ->
		case proplists:get_value(compress_method, Options) of
		    undefined ->
			case get_engine_names() of
			    [] ->
				throw({compress, options, no_engine_available,
				       undefined});
			    [Name | _] = Names ->
				case lists:member(?DEFAULT_ENGINE, Names) of
				    true  -> ?DEFAULT_ENGINE;
				    false -> Name
				end
			end;
		    CM ->
			get_prefered_engine_name(CM)
		end;
	    Name ->
		case is_engine_available(Name) of
		    true ->
			Name;
		    false ->
			throw({compress, options, engine_unavailable, Name})
		end
	end,
    get_engine_driver(Engine_Name).

%% --------------------------------------------------------------------
%% Common socket API.
%% --------------------------------------------------------------------

%% @spec (Compress_Socket, Orig_Packet) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Send `Orig_Packet' over a compressed connection.

send(#compress_socket{socket = Socket_Desc, port = Port}, Packet) ->
    try
        Compressed = engine_compress(Port, Packet),
        exmpp_internals:gen_send(Socket_Desc, Compressed)
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Orig_Data) -> {ok, CompressedData} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Data = binary() | list()
%%     Reason = term()
%% @doc Compress `Orig_Data' before sending over compressed connection.

send_data(#compress_socket{port = Port}, Data) ->
    try
        Compressed = engine_compress(Port, Data),
        {ok, Compressed}
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a compressed connection.

recv(Compress_Socket) ->
    recv(Compress_Socket, infinity).

%% @spec (Compress_Socket, Timeout) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Timeout = integer()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Receive data over a compressed connection.

recv(#compress_socket{socket = Socket_Desc} = Compress_Socket, Timeout) ->
    try
        case exmpp_internals:gen_recv(Socket_Desc, Timeout) of
            {ok, Packet} ->
                recv_data(Compress_Socket, Packet);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Packet) -> {ok, Orig_Packet} | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Packet = binary() | list()
%%     Orig_Packet = binary() | list()
%%     Reason = term()
%% @doc Uncompress already received data.

recv_data(#compress_socket{port = Port, packet_mode = Packet_Mode}, Packet) ->
    try
        Uncompressed = engine_uncompress(Port, Packet),
        case Packet_Mode of
            binary -> {ok, Uncompressed};
            list   -> {ok, binary_to_list(Uncompressed)}
        end
    catch
        Exception ->
            {error, Exception}
    end.

%% @spec (Compress_Socket, Options) -> {ok, Option_Values} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%%     Option_Values = list()
%% @doc Sets one or more options for a socket.

getopts(#compress_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_getopts(Socket_Desc, Options).

%% @spec (Compress_Socket, Options) -> ok | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%% @doc Sets one or more options for a socket.

setopts(#compress_socket{socket = Socket_Desc}, Options) ->
    exmpp_internals:gen_setopts(Socket_Desc, Options).

%% @spec (Compress_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the address and port for the other end of a connection.

peername(#compress_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_peername(Socket_Desc).

%% @spec (Compress_Socket) -> {ok, {Address, Port}} | {error, posix()}
%%     Compress_Socket = tls_socket()
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Returns the local address and port number for a socket.

sockname(#compress_socket{socket = Socket_Desc}) ->
    exmpp_internals:gen_sockname(Socket_Desc).

%% @spec (Compress_Socket, Pid) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Pid = pid()
%%     Reason = term()
%% @doc Change the controlling socket of the underlying socket.

controlling_process(#compress_socket{socket = Socket_Desc}, Pid) ->
    exmpp_internals:gen_controlling_process(Socket_Desc, Pid).

%% @spec (Compress_Socket) -> ok | {error, Reason}
%%     Compress_Socket = compress_socket()
%%     Reason = term()
%% @doc Turn off compression and close the underlying socket.

close(#compress_socket{socket = Socket_Desc} = Compress_Socket) ->
    %% First, turn off compression.
    disable_compression(Compress_Socket),
    %% Close the underlying socket.
    exmpp_internals:gen_close(Socket_Desc).

%% @hidden

port_revision(#compress_socket{port = Port}) ->
    engine_svn_revision(Port).

%% --------------------------------------------------------------------
%% Engine function wrappers.
%% --------------------------------------------------------------------

control(Port, Command, Data) ->
    case port_control(Port, Command, Data) of
        <<0, Result/binary>> -> Result;
        <<1, Error/binary>>  -> {error, binary_to_term(Error)}
    end.

engine_set_compress_method(Port, Method) ->
    case control(Port, ?COMMAND_SET_COMPRESS_METHOD,
		 term_to_binary(Method)) of
        {error, Reason} ->
            throw({compress, compress, set_compress_method, Reason});
        _ ->
            ok
    end.

engine_set_compress_level(Port, Level) ->
    case control(Port, ?COMMAND_SET_COMPRESS_LEVEL,
		 term_to_binary(Level)) of
        {error, Reason} ->
            throw({compress, compress, set_compress_level, Reason});
        _ ->
            ok
    end.

engine_prepare_compress(Port) ->
    case control(Port, ?COMMAND_PREPARE_COMPRESS, <<>>) of
        {error, Reason} ->
            throw({compress, compress, prepare_compress, Reason});
        _ ->
            ok
    end.

engine_prepare_uncompress(Port) ->
    case control(Port, ?COMMAND_PREPARE_UNCOMPRESS, <<>>) of
        {error, Reason} ->
            throw({compress, compress, prepare_uncompress, Reason});
        _ ->
            ok
    end.

engine_compress(Port, Data) when is_list(Data) ->
    engine_compress(Port, list_to_binary(Data));
engine_compress(_Port, <<>>) ->
    <<>>;
engine_compress(Port, Data) ->
    case control(Port, ?COMMAND_COMPRESS, Data) of
        {error, Reason} ->
            throw({compress, compress, do_compress, Reason});
        Result ->
            Result
    end.

engine_uncompress(Port, Data) when is_list(Data) ->
    engine_uncompress(Port, list_to_binary(Data));
engine_uncompress(_Port, <<>>) ->
    <<>>;
engine_uncompress(Port, Data) ->
    case control(Port, ?COMMAND_UNCOMPRESS, Data) of
        {error, Reason} ->
            throw({compress, uncompress, do_uncompress, Reason});
        Result ->
            Result
    end.

engine_svn_revision(Port) ->
    case control(Port, ?COMMAND_SVN_REVISION, <<>>) of
        {error, Reason} ->
            throw({compress, handshake, svn_revision, Reason});
        Revision ->
            binary_to_term(Revision)
    end.

%% --------------------------------------------------------------------
%% gen_server(3erl) callbacks.
%% --------------------------------------------------------------------

%% @hidden

init([]) ->
    Engines = dict:new(),
    By_CM = dict:new(),
    {ok, #state{engines = Engines, by_compress_method = By_CM}}.

%% @hidden

handle_call({register_engine,
	     #compress_engine{name = Name,
			      compress_methods = Compress_Methods,
			      driver_path = Driver_Path,
			      driver = Driver_Name} = Engine},
	    _From,
	    #state{engines = Engines, by_compress_method = By_CM} = State) ->
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
	%% Index engine by its compress methods.
        Fun = fun({CM, Prio}, {E, CM_Dict}) ->
		      New_CM_Dict =
			  case dict:is_key(CM, CM_Dict) of
			      true ->
				  L = [{E, Prio} | dict:fetch(CM, CM_Dict)],
				  New_L = lists:keysort(2, L),
				  dict:store(CM, New_L, CM_Dict);
			      false ->
				  dict:store(CM, [{E, Prio}], CM_Dict)
			  end,
		      {E, New_CM_Dict}
	      end,
        {_, New_By_CM} = lists:foldl(Fun, {Engine, By_CM}, Compress_Methods),
        {reply, ok, State#state{engines = New_Engines,
				by_compress_method = New_By_CM
			       }}
    catch
        _:Exception ->
            {reply, {error, Exception}, State}
    end;

handle_call(get_compress_methods, _From,
	    #state{by_compress_method = By_CM} = State) ->
    {reply, dict:fetch_keys(By_CM), State};

handle_call(get_engine_names, _From,
	    #state{engines = Engines} = State) ->
    {reply, dict:fetch_keys(Engines), State};

handle_call({get_engines, CM}, _From,
	    #state{by_compress_method = By_CM} = State) ->
    case dict:is_key(CM, By_CM) of
        true  -> {reply, [E || {E, _P} <- dict:fetch(CM, By_CM)], State};
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

%% @type compress_socket().
%% Compression socket obtained with {@link compress/2}.
