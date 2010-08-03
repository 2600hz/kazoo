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

-import(callmgr_logger, [log/2, format_log/3]).

%% API
-export([start_link/0, originate/3, register_event_listener/3]).

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

register_event_listener(Pid, RequestId, Event) ->
    gen_server:cast(?MODULE, {register_event_listener, Pid, RequestId, Event}).


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
handle_cast({register_event_listener, Pid, RequestId, Event}, #state{fs=Fs}=State) ->
    %register_event(Fs, Pid, RequestId, Event),
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
    format_log(info, "CALLMGR_FS: Sending to Originate CMD: ~p~n", [Cmd]),
    ok = gen_tcp:send(Fs#fs_conn.socket, Cmd),
    spawn(fun() -> get_uuid(Fs, Receiver) end).

get_uuid(Fs, Receiver) ->
    case freeswitch:read_socket(Fs#fs_conn.socket) of
	{_H, []} -> get_uuid(Fs, Receiver);
	{Headers, Body} ->
	    case extract_uuid(proplists:get_value("Content-Type", Headers), Body) of
		{error, bad_response} -> get_uuid(Fs, Receiver);
		CallId ->
		    format_log(info, "CALLMGR_FS: Got ~p off the socket~nSending back to Q: ~p~n", [CallId, Receiver]),
		    create_ctl_evt_mgrs(Fs, CallId),
		    callmgr_req:recv_callid(CallId, Receiver)
	    end
    end.

create_ctl_evt_mgrs(Fs, CallId) ->
    %% todo - register these processes with the server so they can be supervised
    format_log(info, "CALLMGR_FS: starting evt and ctl mgrs for CallId: ~p~n", [CallId]),
    _CtlRes = spawn(callmgr_ctl, start_link, [Fs, CallId]),
    format_log(info, "CALLMGR_FS: started callmgr_ctl(~p)~n", [_CtlRes]),
    _EvtRes = spawn(callmgr_evt, start_link, [Fs, CallId]),
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
