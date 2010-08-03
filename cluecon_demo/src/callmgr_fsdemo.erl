
%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Listen for CHANNEL_PARKED events on FS socket
%%% If the UUID is a newly encountered one, create a new callmgr_demo
%%% process to start the demo;
%%% otherwise ignore the event as its in progress with the demo
%%%
%%% Listen for CHANNEL_DESTROY events
%%% Remove destroyed UUID from list of known calls
%%%
%%% @end
%%% Created : 31 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(callmgr_fsdemo).

-behaviour(gen_server).

-import(callmgr_logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3, delete/2]).

%% API
-export([start_link/0, new_event/1, exec_cmd/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/fs.hrl").

-define(SERVER, ?MODULE).
-define(EVENTS, ["CHANNEL_CREATE", "CHANNEL_PARK", "CHANNEL_DESTROY", "CHANNEL_EXECUTE_COMPLETE"]).

%% calls [{call UUID, pid of demo process}]
-record(state, {fs=#fs_conn{}, calls=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_event(Prop) ->
    gen_server:cast(?MODULE, {new_event, Prop}).

exec_cmd(UUID, Cmd) ->
    gen_server:cast(?MODULE, {exec_cmd, UUID, Cmd}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    FS = #fs_conn{},
    format_log(info, "CALLMGR_FSDEMO(~p): Starting FS Event Socket Listener~n", [self()]),
    Hostname = FS#fs_conn.host,
    Port = FS#fs_conn.port,
    Auth = FS#fs_conn.auth,

    Socket = case gen_tcp:connect(Hostname, Port, [list, {active, false}]) of
		 {ok, FsSock} ->
		     inet:setopts(FsSock, [{packet, line}]),
		     format_log(info, "CALLMGR_FSDEMO: Opened FreeSWITCH event socket to ~p~n", [Hostname]),
		     clear_socket(FsSock),
		     ok = gen_tcp:send(FsSock, lists:concat(["auth ", Auth, "\n\n"])),
		     clear_socket(FsSock),
		     ListenCmd = lists:concat(["event plain "
					       ,string:join(?EVENTS, " ")
					       ,"\n\n"
					      ]),
		     format_log(info, "CALLMGR_FSDEMO: Sending ~p to socket~n", [ListenCmd]),
		     ok = gen_tcp:send(FsSock, ListenCmd),
		     clear_socket(FsSock),
		     spawn(fun() -> evt_loop(FsSock) end),
		     FsSock;
		 {error, Reason} ->
		     format_log(error, "Unable to open socket: ~p~n", [Reason]),
		     undefined
	     end,
    {ok, #state{fs=FS#fs_conn{socket=Socket}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({exec_cmd, UUID, Cmd}, #state{fs=Fs, calls=Calls}=State) ->
    case get_value(UUID, Calls) of
	undefined -> {noreply, State};
	nopid -> {noreply, State};
	Pid when is_pid(Pid) ->
	    format_log(info, "CALLMGR_FSDEMO: Running ~p for ~p(~p)~n", [Cmd, Pid, UUID]),
	    run_cmd(Fs#fs_conn.socket, Cmd),
	    {noreply, State};
	_ -> {noreply, State}
    end;
handle_cast({new_event, Prop}, #state{calls=Calls}=State) ->
    Calls1 = handle_event(get_value("Event-Name", Prop), Prop, Calls),
    {noreply, State#state{calls=Calls1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
run_cmd(FsSock, Cmd) ->
    ok = gen_tcp:send(FsSock, Cmd).

handle_event("CHANNEL_CREATE", Prop, Calls) ->
    UUID = get_value("Unique-ID", Prop),
    format_log(info, "CALLMGR_FSDEMO: EvtNew for ~p~n", [UUID]),
    case get_value(UUID, Calls) of
	undefined ->
	    [{UUID, nopid} | Calls];
	_ ->
	    Calls
    end;
handle_event("CHANNEL_PARK", Prop, Calls) ->
    UUID = get_value("Unique-ID", Prop),
    format_log(info, "CALLMGR_FSDEMO: EvtPark for ~p~n", [UUID]),
    case get_value(UUID, Calls) of
	undefined ->
	    [{UUID, new_demo(UUID)} | Calls];
	nopid ->
	    [{UUID, new_demo(UUID)} | Calls];
	Pid when is_pid(Pid) ->
	    Pid ! {event_park, UUID},
	    Calls;
	_ ->
	    Calls
    end;
handle_event("CHANNEL_EXECUTE_COMPLETE", Prop, Calls) ->
    UUID = get_value("Unique-ID", Prop),
    format_log(info, "CALLMGR_FSDEMO: EvtExe for ~p~n", [UUID]),
    case get_value(UUID, Calls) of
	undefined ->
	    %% some error we may want to look into
	    Calls;
	nopid ->
	    %% again, an error of some kind
	    Calls;
	Pid when is_pid(Pid) ->
	    Pid ! {event_exec_completed, Prop},
	    Calls;
	_ ->
	    Calls
    end;
handle_event("CHANNEL_DESTROY", Prop, Calls) ->
    UUID = get_value("Unique-ID", Prop),
    format_log(info, "CALLMGR_FSDEMO: EvtDstry for ~p~n", [UUID]),
    case get_value(UUID, Calls) of
	undefined ->
	    Calls;
	nopid ->
	    delete(UUID, Calls);
	Pid when is_pid(Pid) ->
	    Pid ! {event_destroy, UUID},
	    delete(UUID, Calls);
	_ ->
	    delete(UUID, Calls)
    end;
handle_event(Unhandled, _Prop, Calls) ->
    format_log(info, "CALLMGR_FSDEMO: Unhandled event ~p~n", [Unhandled]),
    Calls.

new_demo(UUID) ->
    callmgr_demo:start(UUID).

process_evt("text/event-plain", Prop) ->
    callmgr_fsdemo:new_event(Prop);
process_evt(_CT, _Prop) ->
    format_log(info, "CALLMGR_FSDEMO: Content-type ~p unhandled for evt ~p~n", [_CT, get_value("Event-Name", _Prop)]),
    ignore.

event_to_proplist(Str) ->
    L = string:tokens(Str, "\n"),
    lists:map(fun(S) -> [K, V0] = string:tokens(S, ":"),
			V1 = string:strip(V0, left, $ ),
			{V2, []} = mochiweb_util:parse_qs_value(V1),
			{K, V2}
	      end, L).

evt_loop(Socket) ->
    evt_loop(Socket, [], 0).

evt_loop(Socket, Headers, ContentLength) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
		    inet:setopts(Socket, [{packet, line}]),
		    %format_log(info, "CALLMGR_FSDEMO: With Body, Headers: ~p~n", [Headers]),
		    process_evt(proplists:get_value("Content-Type", Headers), event_to_proplist(Body));
		_ ->
		    %format_log(info, "CALLMGR_FSDEMO: No body. Headers: ~p~n", [Headers]),
		    ok
            end,
	    evt_loop(Socket, [], 0);
        {ok, Data} ->
            %% Parse the line
	    KV = split(Data),
	    {K, V} = KV,

            %% Is this a content-length string? If so, we'll need to gather extra data later
            case K =:= "Content-Length" of
		true ->
		    Length = list_to_integer(V);
		_ ->
		    Length = ContentLength
            end,

            evt_loop(Socket, [KV | Headers], Length);
        {error, closed}=Err ->
            Err
    end.

clear_socket(Socket) ->
    clear_socket(Socket, [], 0).

clear_socket(Socket, Data, ContentLength) ->
    log(info, "CALLMGR_FSDEMO: clear_socket called"),
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            %% End of message. Parse
	    %%log(info, ["COMPLETE HEADER: ", Headers]),

            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, _Body} = gen_tcp:recv(Socket, ContentLength),
		    format_log(info, "CALLMGR_FSDEMO: Clear. Data: ~p~nBody: ~p~n", [Data, _Body]),
		    inet:setopts(Socket, [{packet, line}]);
		_ ->
		    format_log(info, "CALLMGR_FSDEMO: Clear: No body. Data: ~p~n", [Data]),
		    inet:setopts(Socket, [{packet, line}])
            end;
        {ok, Datum} ->
            %% Parse the line
	    format_log(info, "CALLMGR_FSDEMO: Clear-line: Data: ~p~n", [Datum]),
	    KV = split(Datum),
	    {K, V} = KV,

            %% Is this a content-length string? If so, we'll need to gather extra data later
            case K =:= "Content-Length" of
		true ->
		    Length = list_to_integer(V);
		_ ->
		    Length = ContentLength
            end,

            clear_socket(Socket, [KV | Data], Length);
        {error, closed}=Err ->
            Err;
	Other ->
	    format_log(info, "CALLMGR_FSDEMO: get_tcp:recv Unexpected: ~p~n", [Other]),
	    {error, unexpected}
    end.

%% Takes K: V\n and returns {K, V}
split(Data) ->
    [K | V] = string:tokens(Data, ": "),
    V1 = case length(V) of
	     0 -> "";
	     1 -> string:strip(hd(V), right, $\n);
	     _ -> lists:map(fun(S) -> string:strip(S, right, $\n) end, V)
	 end,
    {K, V1}.
