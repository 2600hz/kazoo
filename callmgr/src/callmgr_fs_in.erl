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
%%% Created : 02 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(callmgr_fs_in).

-behaviour(gen_server).

-import(callmgr_logger, [log/2, format_log/3]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/fs.hrl").

-define(SERVER, ?MODULE). 

-record(state, {fs=#fs_conn{}}).

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
    format_log(info, "CALLMGR_FS_IN(~p): Starting FS Event Socket Listener~n", [self()]),
    Hostname = FS#fs_conn.host,
    Port = FS#fs_conn.port,
    Auth = FS#fs_conn.auth,

    Socket = case gen_tcp:connect(Hostname, Port, [list, {active, false}]) of
		 {ok, FsSock} ->
		     inet:setopts(FsSock, [{packet, line}]),
		     format_log(info, "CALLMGR_FS_IN: Opened FreeSWITCH event socket to ~p~n", [Hostname]),
		     clear_socket(FsSock),
		     ok = gen_tcp:send(FsSock, lists:concat(["auth ", Auth, "\n\n"])),
		     clear_socket(FsSock),
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
handle_cast({originate, Name, Num, Receiver}, #state{fs=Fs}=State) ->
    originate(Fs, Name, Num, Receiver),
    {noreply, State};
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
originate(Fs, Name, Num, Receiver) when is_integer(Num) ->
    originate(Fs, Name, integer_to_list(Num), Receiver);
originate(Fs, Name, Num, Receiver) ->
    Cmd = lists:concat(["api originate {effective_caller_id_name="
	   ,Name,"}{effective_caller_id_number="
	   ,Num,"}sofia/gateway/proxy1.switchfreedom.com/+14158867905 &park\n\n"
	  ]),
    format_log(info, "CALLMGR_FS_IN: Sending to Originate CMD: ~p~n", [Cmd]),
    ok = gen_tcp:send(Fs#fs_conn.socket, Cmd),
    %clear_socket(Fs#fs_conn.socket), % we'll get a api/response first
    %log(info, "CALLMGR_FS_IN: Cleared custom command from socket"),
    spawn(fun() -> get_uuid(Fs, Receiver) end).

reader_loop(Socket) ->
    reader_loop(Socket, [], 0).

reader_loop(Socket, Headers, ContentLength) ->
    log(info, "CALLMGR_FS_IN: reader_loop called."),
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            %% End of message. Parse
	    %%log(info, ["COMPLETE HEADER: ", Headers]),

            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
		    inet:setopts(Socket, [{packet, line}]),
		    %%log(info, [" COMPLETE DATA: ", Body]),
		    extract_uuid(proplists:get_value("Content-Type", Headers), Body);
		_ ->
		    format_log(info, "CALLMGR_FS_IN: No body. Headers: ~p~nTrying again to get ID~n", [Headers]),
		    reader_loop(Socket, [], 0)
            end;
        {ok, Data} ->
            log(info, ["Received a line: ", Data]),

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

            reader_loop(Socket, [KV | Headers], Length);
        {error, closed}=Err ->
            Err
    end.

clear_socket(Socket) ->
    clear_socket(Socket, [], 0).

clear_socket(Socket, Data, ContentLength) ->
    log(info, "CALLMGR_FS_IN: clear_socket called"),
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            %% End of message. Parse
	    %%log(info, ["COMPLETE HEADER: ", Headers]),

            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, _Body} = gen_tcp:recv(Socket, ContentLength),
		    format_log(info, "CALLMGR_FS_IN: Clear. Data: ~p~nBody: ~p~n", [Data, _Body]),
		    inet:setopts(Socket, [{packet, line}]);
		_ ->
		    format_log(info, "CALLMGR_FS_IN: Clear: No body. Data: ~p~n", [Data]),
		    inet:setopts(Socket, [{packet, line}])
            end;
        {ok, Datum} ->
            %% Parse the line
	    format_log(info, "CALLMGR_FS_IN: Clear-line: Data: ~p~n", [Datum]),
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
	    format_log(info, "CALLMGR_FS_IN: get_tcp:recv Unexpected: ~p~n", [Other]),
	    {error, unexpected}
    end.

get_uuid(Fs, Receiver) ->
    CallId = reader_loop(Fs#fs_conn.socket),
    format_log(info, "CALLMGR_FS_IN: Got ~p off the socket~nSending back to Q: ~p~n", [CallId, Receiver]),
    create_ctl_evt_mgrs(Fs, CallId),
    callmgr_req:recv_callid(CallId, Receiver).

create_ctl_evt_mgrs(Fs, CallId) ->
    %% todo - register these processes with the server so they can be supervised
    format_log(info, "CALLMGR_FS_IN: starting evt and ctl mgrs for CallId: ~p~n", [CallId]),
    _CtlRes = spawn(callmgr_ctl, start_link, [Fs, CallId]),
    format_log(info, "CALLMGR_FS_IN: started callmgr_ctl(~p)~n", [_CtlRes]),
    _EvtRes = spawn(callmgr_evt, start_link, [Fs, CallId]),
    format_log(info, "CALLMGR_FS_IN: started callmgr_evt(~p)~n", [_EvtRes]),
    ok.

extract_uuid("api/response", [$+,$O,$K,$ | ID]) ->
    clean_endline(lists:reverse(ID));
extract_uuid("api/response", [$\n,$+,$O,$K,$ | ID]) ->
    clean_endline(lists:reverse(ID));
extract_uuid(_ResponseType, _Body) ->
    format_log(info, "CALLMGR_FS_IN: Failed to extract UUID. ResType: ~p Body: ~p||~n"
	       ,[_ResponseType, _Body]),
    {error, bad_response}.

clean_endline([$\n | Ls]) -> clean_endline(Ls);
clean_endline(Ls) -> lists:reverse(Ls).

%% Takes K: V\n and returns {K, V}
split(Data) ->
    [K | V] = string:tokens(Data, ": "),
    V1 = case length(V) of
	     0 -> "";
	     1 -> string:strip(hd(V), right, $\n);
	     _ -> lists:map(fun(S) -> string:strip(S, right, $\n) end, V)
	 end,
    {K, V1}.
