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
%% The module <strong>{@module}</strong> provides functions to use
%% NODEPREP, NAMEPREP and RESOURCEPREP stringprep profiles.
%%
%% <p>
%% It's not intended to be used directly.
%% </p>

-module(exmpp_stringprep).

-behaviour(gen_server).

%% Initialization.
-export([
	 start/0,
	 start_link/0
	]).

%% Stringprep profiles.
-export([
	 nodeprep/1,
	 nameprep/1,
	 resourceprep/1
	]).

%% Tools.
-export([
	 is_node/1,
	 is_name/1,
	 is_resource/1,
	 to_lower/1,
	 port_revision/0
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

-record(state, {ports}).

-define(SERVER, ?MODULE).
-define(DRIVER_NAME, exmpp_stringprep).

%% http://www.erlang.org/doc/efficiency_guide/drivers.html#id68009
-define(PORT_REGISTERED_NAMES, {exmpp_stringprep_port01,
				exmpp_stringprep_port02,
				exmpp_stringprep_port03,
				exmpp_stringprep_port04,
				exmpp_stringprep_port05,
				exmpp_stringprep_port06,
				exmpp_stringprep_port07,
				exmpp_stringprep_port08,
				exmpp_stringprep_port09,
				exmpp_stringprep_port10,
				exmpp_stringprep_port11,
				exmpp_stringprep_port12,
				exmpp_stringprep_port13,
				exmpp_stringprep_port14,
				exmpp_stringprep_port15,
				exmpp_stringprep_port16}).

-define(COMMAND_LOWERCASE,     0).
-define(COMMAND_NAMEPREP,      1).
-define(COMMAND_NODEPREP,      2).
-define(COMMAND_RESOURCEPREP,  3).
-define(COMMAND_PORT_REVISION, 4).

%% --------------------------------------------------------------------
%% Initialization.
%% --------------------------------------------------------------------

%% @hidden

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% @hidden

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Stringprep profiles.
%% --------------------------------------------------------------------

%% @spec (String) -> Prepd_String
%%     String = binary() | string()
%%     Prepd_String = binary() | string()
%% @throws {stringprep, nodeprep, exmpp_not_started, String} |
%%         {stringprep, nodeprep, invalid_string, String}
%% @doc Apply the NODEPREP stringprep profile to `String'.

-spec nodeprep
(binary()) -> binary();
(string()) -> string().

nodeprep(String) ->
    case control_reuse_arg(?COMMAND_NODEPREP, String) of
        {error, Reason} ->
            throw({stringprep, nodeprep, Reason, String});
        Result ->
            Result
    end.

%% @spec (String) -> Prepd_String
%%     String = binary() | string()
%%     Prepd_String = binary() | string()
%% @throws {stringprep, nameprep, exmpp_not_started, String} |
%%         {stringprep, nameprep, invalid_string, String}
%% @doc Apply the NAMEPREP stringprep profile to `String'.

-spec nameprep
(binary()) -> binary();
(string()) -> string().

nameprep(String) ->
    case control_reuse_arg(?COMMAND_NAMEPREP, String) of
        {error, Reason} ->
            throw({stringprep, nameprep, Reason, String});
        Result ->
            Result
    end.

%% @spec (String) -> Prepd_String
%%     String = binary() | string()
%%     Prepd_String = binary() | string()
%% @throws {stringprep, resourceprep, exmpp_not_started, String} |
%%         {stringprep, resourceprep, invalid_string, String}
%% @doc Apply the RESOURCEPREP stringprep profile to `String'.

-spec resourceprep
(binary()) -> binary();
(string()) -> string().

resourceprep(String) ->
    case control_reuse_arg(?COMMAND_RESOURCEPREP, String) of
        {error, Reason} ->
            throw({stringprep, resourceprep, Reason, String});
        Result ->
            Result
    end.

%% --------------------------------------------------------------------
%% Tools.
%% --------------------------------------------------------------------

%% @spec (String) -> boolean()
%%     String = binary() | string()
%% @throws {stringprep, nodeprep, exmpp_not_started, String}
%% @doc Tell if `String' conforms the NODEPREP stringprep profile.

-spec is_node
(binary() | string()) -> boolean().

is_node(<<>>) ->
    false;
is_node("") ->
    false;
is_node(String) ->
    try
        nodeprep(String),
        true
    catch
        throw:_Exception ->
            false
    end.

%% @spec (String) -> boolean()
%%     String = binary() | string()
%% @throws {stringprep, nameprep, exmpp_not_started, String}
%% @doc Tell if `String' conforms the NAMEPREP stringprep profile.

-spec is_name
(binary() | string()) -> boolean().

is_name(<<>>) ->
    false;
is_name("") ->
    false;
is_name(String) ->
    try
        nameprep(String),
        true
    catch
        throw:_Exception ->
            false
    end.

%% @spec (String) -> boolean()
%%     String = binary() | string()
%% @throws {stringprep, resourceprep, exmpp_not_started, String}
%% @doc Tell if `String' conforms the RESOURCEPREP stringprep profile.

-spec is_resource
(binary() | string()) -> boolean().

is_resource(<<>>) ->
    false;
is_resource("") ->
    false;
is_resource(String) ->
    try
        resourceprep(String),
        true
    catch
        throw:_Exception ->
            false
    end.

%% @spec (String) -> Lowered_String
%%     String = binary() | string()
%%     Lowered_String = binary() | string()
%% @throws {stringprep, lowercase, exmpp_not_started, String} |
%%         {stringprep, lowercase, invalid_string, String}
%% @doc Convert `String' to lowercase.

-spec to_lower
(binary()) -> binary();
(string()) -> string().

to_lower(String) ->
    case control_reuse_arg(?COMMAND_LOWERCASE, String) of
        {error, Reason} ->
            throw({stringprep, lowercase, Reason, String});
        Result ->
            Result
    end.

%% @hidden

port_revision() ->
    case control(?COMMAND_PORT_REVISION, "") of
        {error, Reason} ->
            throw({stringprep, port_revision, Reason, ""});
        Result ->
            Result
    end.

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

-spec control
(non_neg_integer(), binary() | string()) ->
    string() | {error, invalid_string | exmpp_not_started}.

control(Command, String) ->
    PortName = port_name(erlang:system_info(scheduler_id)),
    try
        case port_control(PortName, Command, String) of
            [0 | _]      -> {error, invalid_string};
            [1 | Result] -> Result
        end
    catch
        error:badarg ->
            case erlang:port_info(PortName, registered_name) of
                {registered_name, PortName} ->
                    {error, invalid_string};
                undefined ->
                    {error, exmpp_not_started}
            end
    end.

-spec control_reuse_arg
(non_neg_integer(), binary()) ->
    binary() | {error, invalid_string | exmpp_not_started};
(non_neg_integer(), string()) ->
    string() | {error, invalid_string | exmpp_not_started}.

control_reuse_arg(Command, String) ->
    %% If applying a STRINGPREP profile doesn't modify the original string,
    %% keep the former and throw away the returned string.
    %%
    %% Guard expressions (eg. Result =:= String) must be used instead of
    %% pattern matching, otherwise the two copies are still maintained.
    case control(Command, String) of
        {error, _} = Error ->
            Error;
        Result when is_binary(String) ->
            case list_to_binary(Result) of
                Result_B when Result_B =:= String -> String;
                Result_B                          -> Result_B
            end;
        Result when Result =:= String ->
            String;
        Other ->
            Other
    end.

-spec port_name(pos_integer()) -> atom().
port_name(N) ->
    element(N rem tuple_size(?PORT_REGISTERED_NAMES) + 1,
	    ?PORT_REGISTERED_NAMES).

%% --------------------------------------------------------------------
%% gen_server(3erl) callbacks.
%% --------------------------------------------------------------------

%% @hidden

init([]) ->
    try
        exmpp_internals:load_driver(?DRIVER_NAME),
	Schedulers = erlang:system_info(schedulers),
	PortCount =
	    if Schedulers > tuple_size(?PORT_REGISTERED_NAMES) ->
		    tuple_size(?PORT_REGISTERED_NAMES);
	       true ->
		    Schedulers
	    end,
	Ports =
	    lists:map(fun(N) ->
			      Port = exmpp_internals:open_port(?DRIVER_NAME),
			      register(port_name(N), Port),
			      Port
		      end,
		      lists:seq(1, PortCount)),
        State = #state{
          ports = Ports
	 },
        {ok, State}
    catch
        throw:{port_driver, load, _, _} = Exception ->
            {stop, Exception};
	  throw:{port_driver, open, _, _} = Exception ->
            exmpp_internals:unload_driver(?DRIVER_NAME),
            {stop, Exception}
    end.

%% @hidden

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

terminate(_Reason, #state{ports = Ports} = _State) ->
    lists:foreach(fun(Port) ->
			  exmpp_internals:close_port(Port)
		  end, Ports),
    exmpp_internals:unload_driver(?DRIVER_NAME),
    ok.
