%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Recv a request to spin up a call.
%%% Get Callid back.
%%% Create two queues, one in CallCtlXc, one in CallEvtXc, with the 
%%% queue name Callid
%%% @end
%%% Created : 31 Jul 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(callmgr_fs).

-behaviour(gen_server).

-import(logger, [log/2, format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

%% API
-export([start_link/0, originate/3, originate_loopback/1]).

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

%% effective_caller_id_{name, number}
%% Receiver is probably the Queue id of the 3rd-party app, but this
%% module doesn't care about this. Just passes it back with the callid
originate(Name, Num, Receiver) ->
    gen_server:cast(?MODULE, {originate, Name, Num, Receiver}).

originate_loopback(Receiver) ->
    gen_server:cast(?MODULE, {originate_loopback, Receiver}).


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
    format_log(info, "CALLMGR_FS(~p): Starting FS Event Socket Listener~n", [self()]),
    FS = freeswitch:new_fs_socket(#fs_conn{}),
    {ok, #state{fs=FS}}.

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
handle_cast({originate_loopback, Receiver}, #state{fs=Fs}=State) ->
    originate_loopback(Fs, Receiver),
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
originate_loopback(Fs, Receiver) ->
    Cmd = "api originate loopback/wait &park\n\n",
    spawn(fun() -> get_uuid(Cmd, freeswitch:new_fs_socket(Fs), Receiver) end).
    
originate(Fs, Name, Num, Receiver) when is_integer(Num) ->
    originate(Fs, Name, integer_to_list(Num), Receiver);
originate(Fs, Name, Num, Receiver) ->
    Cmd = lists:concat(["api originate {effective_caller_id_name="
	   ,Name,"}{effective_caller_id_number="
	   ,Num,"}sofia/gateway/proxy1.switchfreedom.com/+14158867905 &park\n\n"
	  ]),
    spawn(fun() -> get_uuid(Cmd, freeswitch:new_fs_socket(Fs), Receiver) end).

get_uuid(Cmd, Fs, Receiver) ->
    format_log(info, "CALLMGR_FS.UUID(~p): Cmd ~p on sock ~p~n", [self(), Cmd, Fs#fs_conn.socket]),
    ok = gen_tcp:send(Fs#fs_conn.socket, Cmd),
    get_uuid(Fs, Receiver).
get_uuid(Fs, Receiver) ->
    format_log(info, "CALLMGR_FS.UUID(~p) Sock: ~p~n", [self(), Fs#fs_conn.socket]),
    case freeswitch:read_socket(Fs#fs_conn.socket) of
	{ok, _H, []} ->
	    format_log(error, "CALLMGR_FS.UUID(~p) Empty B. H: ~p~n", [self(), _H]),
	    get_uuid(Fs, Receiver);
	{ok, Headers, Body} ->
	    format_log(info, "CALLMGR_FS: Socket read H: ~p B: ~p~n", [Headers, Body]),
	    case extract_uuid(get_value("Content-Type", Headers), Body) of
		{error, bad_response} ->
		    format_log(error, "CALLMGR_FS.UUID(~p): Bad UUID body ~p~nExiting~n", [self(), Body]);
		CallId ->
		    io:format("CALLMGR_FS.UUID(~p): UUID: ~p~n", [self(), CallId]),
		    create_ctl_evt_mgrs(Fs, CallId),
		    callmgr_req:recv_callid(CallId, Receiver)
	    end;
	{error, ealready} ->
	    get_uuid(Fs, Receiver);
	_Other ->
	    format_log(error, "CALLMGR_FS.UUID(~p): get_uuid:read_socket() return unexpected: ~p~n", [self(), _Other]),
	    get_uuid(Fs, Receiver)
    end.

create_ctl_evt_mgrs(Fs, CallId) ->
    %% todo - register these processes with the server so they can be supervised
    format_log(info, "CALLMGR_FS(~p): starting evt and ctl mgrs for CallId: ~p~n", [self(), CallId]),
    CtlPid = callmgr_ctl:start_link(Fs, CallId), %spawn(callmgr_ctl, start_link, [Fs, CallId]),
    format_log(info, "CALLMGR_FS: started callmgr_ctl(~p)~n", [CtlPid]),
    _EvtRes = callmgr_evt:start_link(Fs, CallId, CtlPid), %spawn(callmgr_evt, start_link, [Fs, CallId]),
    format_log(info, "CALLMGR_FS: started callmgr_evt(~p)~n", [_EvtRes]),
    ok.

extract_uuid("api/response", [$+,$O,$K,$ | ID]) ->
    clean_endline(lists:reverse(ID));
extract_uuid("api/response", [$\n,$+,$O,$K,$ | ID]) ->
    clean_endline(lists:reverse(ID));
extract_uuid(_ResponseType, _Body) ->
    format_log(info, "CALLMGR_FS: Failed to extract UUID. ResType: ~p Body: ~p||~n"
	       ,[_ResponseType, _Body]),
    {error, bad_response}.

clean_endline([$\n | Ls]) -> clean_endline(Ls);
clean_endline(Ls) -> lists:reverse(Ls).
