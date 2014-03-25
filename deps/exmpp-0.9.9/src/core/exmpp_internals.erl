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

%% @author Jean-Sebastien Pedron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides internal functions for
%% other modules.

-module(exmpp_internals).

%% Port driver handling.
-export([
	 driver_dirs/0,
	 load_driver/1,
	 load_driver/2,
	 unload_driver/1,
	 open_port/1,
	 close_port/1
	]).

%% Generic socket handling.
-export([
	 gen_recv/2,
	 gen_send/2,
	 gen_getopts/2,
	 gen_setopts/2,
	 gen_peername/1,
	 gen_sockname/1,
	 gen_controlling_process/2,
	 gen_close/1
	]).

%% --------------------------------------------------------------------
%% Port driver loading/unloading.
%% --------------------------------------------------------------------

%% @spec () -> Dirs | []
%%     Dirs = [string()]
%% @doc Return a list of directories to search port drivers in.

-spec(driver_dirs/0 :: () -> [string()]).

driver_dirs() ->
    Mod_Path = case code:is_loaded(?MODULE) of
		   {file, preloaded} ->
		       undefined;
		   {file, cover_compiled} ->
		       case code:is_loaded(check_coverity) of
			   {file, preloaded}      -> undefined;
			   {file, cover_compiled} -> undefined;
			   {file, Path}           -> Path
		       end;
		   {file, Path} ->
		       Path
	       end,
    Dirs0 = case Mod_Path of
		undefined ->
		    [];
		_ ->
		    Base_Dir = filename:dirname(filename:dirname(Mod_Path)),
		    [
		     filename:join([Base_Dir, "priv", "lib"]),
		     filename:join([Base_Dir, "c_src", ".libs"]),
		     filename:join([Base_Dir, "c_src"])
		    ]
	    end,
    case code:priv_dir(exmpp) of
        {error, _Reason} -> Dirs0;
        Priv_Dir         -> Dirs0 ++ [filename:join(Priv_Dir, "lib")]
    end.

%% @spec (Driver_Name) -> ok
%%     Driver_Name = atom()
%% @throws {port_driver, load, Reason, Driver_Name}
%% @doc Load the port driver `Driver_Name'.

-spec(load_driver/1 :: (atom()) -> ok).

load_driver(Driver_Name) ->
    Dirs = driver_dirs(),
    load_driver(Driver_Name, Dirs).

%% @spec (Driver_Name, Dirs) -> ok
%%     Driver_Name = atom()
%%     Dirs = [string()]
%% @throws {port_driver, load, Reason, Driver_Name}
%% @doc Load the port driver `Driver_Name'.
%%
%% The driver is search in `Dirs'.

-spec(load_driver/2 :: (atom(), [string()]) -> ok).

load_driver(Driver_Name, Dirs) ->
    load_driver1(Driver_Name, Dirs, undefined).

%% This function will try to load `Driver_Name' from each `Dir' in the list.
load_driver1(Driver_Name, [Dir | Rest], _Reason) ->
    case erl_ddll:load_driver(Dir, Driver_Name) of
        ok ->
            ok;
        {error, Reason} ->
	    %% Next directory.
            load_driver1(Driver_Name, Rest, Reason)
    end;
load_driver1(Driver_Name, [], Reason) ->
    %% We walk through each directories without being able to load the driver.
    throw({port_driver, load, Reason, Driver_Name}).

%% @spec (Driver_Name) -> ok
%%     Driver_Name = atom()
%% @doc Unload the port driver `Driver_Name'.

-spec(unload_driver/1 :: (atom()) -> ok).

unload_driver(Driver_Name) ->
    erl_ddll:unload_driver(Driver_Name),
    ok.

%% @spec (Driver_Name) -> Port
%%     Driver_Name = atom()
%%     Port = port()
%% @throws {port_driver, open, {posix, Posix_Code}, Driver_Name}
%% @doc Spawn a new port driver instance.

-spec(open_port/1 :: (atom()) -> port()).

open_port(Driver_Name) ->
    try
        erlang:open_port({spawn, atom_to_list(Driver_Name)}, [])
    catch
        exit:{Posix_Code, _Stack} ->
            throw({port_driver, open, {posix, Posix_Code}, Driver_Name})
    end.

%% @spec (Port) -> true
%%     Port = port()
%% @doc Close a previously spawned port.
%%
%% `Port' was obtained with {@link open_port/1}.

-spec(close_port/1 :: (port()) -> true).

close_port(Port) ->
    erlang:port_close(Port).

%% --------------------------------------------------------------------
%% Generic socket handling.
%% --------------------------------------------------------------------

%% @spec (Socket_Desc, Timeout) -> {ok, Packet} | {error, Reason}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Timeout = integer() | infinity
%%     Packet = [char()] | binary()
%%     Reason = closed | posix()
%% @doc Wrapper to abstract the `recv' function of multiple communication
%% modules.

-spec(gen_recv/2 ::
      ({atom(), any()}, integer() | infinity) -> {ok, binary()} | {error, any()}).

gen_recv({gen_tcp, Socket}, Timeout) ->
    gen_tcp:recv(Socket, 0, Timeout);
gen_recv({Mod, Socket}, Timeout) ->
    Mod:recv(Socket, Timeout).

%% @spec (Socket_Desc, Packet) -> ok | {error, Reason}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Packet = [char()] | binary()
%%     Reason = closed | posix()
%% @doc Wrapper to abstract the `send' function of multiple communication
%% modules.

-spec(gen_send/2 :: ({atom(), any()}, binary()) -> ok | {error, any()}).

gen_send({Mod, Socket}, Packet) ->
    Mod:send(Socket, Packet).

%% @spec (Socket_Desc, Options) -> Option_Values | {error, posix()}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%%     Option_Values = {ok,list()}
%% @doc Wrapper to abstract the `getopts' function of multiple communication
%% modules.

-spec(gen_getopts/2 :: ({atom(), any()}, list()) -> list() | {error, any()}).

gen_getopts({gen_tcp, Socket}, Options) ->
    inet:getopts(Socket, Options);
gen_getopts({Mod, Socket}, Options) ->
    Mod:getopts(Socket, Options).

%% @spec (Socket_Desc, Options) -> ok | {error, posix()}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Options = list()
%% @doc Wrapper to abstract the `setopts' function of multiple communication
%% modules.

-spec(gen_setopts/2 :: ({atom(), any()}, list()) -> ok | {error, any()}).

gen_setopts({gen_tcp, Socket}, Options) ->
    inet:setopts(Socket, Options);
gen_setopts({Mod, Socket}, Options) ->
    Mod:setopts(Socket, Options).

%% @spec (Socket_Desc) -> {ok, {Address, Port}} | {error, posix()}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Wrapper to abstract the `peername' function of multiple communication
%% modules.

-spec(gen_peername/1 :: ({atom(), any()}) -> {ok, any()} | {error, any()}).

gen_peername({gen_tcp, Socket}) ->
    inet:peername(Socket);
gen_peername({Mod, Socket}) ->
    Mod:peername(Socket).

%% @spec (Socket_Desc) -> {ok, {Address, Port}} | {error, posix()}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%%     Address = ip_address()
%%     Port = integer()
%% @doc Wrapper to abstract the `sockname' function of multiple communication
%% modules.

-spec(gen_sockname/1 :: ({atom(), any()}) -> {ok, any()} | {error, any()}).

gen_sockname({gen_tcp, Socket}) ->
    inet:sockname(Socket);
gen_sockname({Mod, Socket}) ->
    Mod:sockname(Socket).

%% @spec (Socket_Desc, Pid) -> ok | {error, Reason}
%%     Socket_Desc = {Mod, Socket}
%%     Pid = pid()
%%     Mod = atom()
%%     Socket = term()
%%     Reason = close | not_owner | posix()
%% @doc Wrapper to abstract the `controlling_process' function of
%% multiple communication modules.

-spec(gen_controlling_process/2 ::
      ({atom(), any()}, pid()) -> ok | {error, any()}).

gen_controlling_process({Mod, Socket}, Pid) ->
    Mod:controlling_process(Socket, Pid).

%% @spec (Socket_Desc) -> ok | {error, posix()}
%%     Socket_Desc = {Mod, Socket}
%%     Mod = atom()
%%     Socket = term()
%% @doc Wrapper to abstract the `close' function of multiple communication
%% modules.

-spec(gen_close/1 :: ({atom(), any()}) -> ok | {error, any()}).

gen_close({Mod, Socket}) ->
    Mod:close(Socket).
