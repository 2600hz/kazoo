%%%-------------------------------------------------------------------
%%% File    : amqp_manager.erl
%%% Authors  : K Anderson
%%%          : James Aimonetti
%%% Description : The AMQP connection manager.
%%%
%%% Created :  March 24 2010
%%%-------------------------------------------------------------------
-module(amqp_manager).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [log/2, format_log/3]).
-import(props, [get_value/2, get_value/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, start/0, open_channel/1, open_channel/2, close_channel/2, stop/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_AMQP_HOST, "whistle-erl001-fmt.2600hz.org").

%% Indicies for pids in their tuples
-define(PROCESS_PID, 1).
-define(CHANNEL_PID, 3).
-define(CONNECTION_PID, 2).

%% [ {connection, Connection, MRef} OR {ProcessPid, ProcessMRef, ChannelPid, ChanMRef, Ticket} ]
%% state = [ {Host, host_info()} ]
-type amqp_host_info() :: list(tuple(connection, pid(), reference()) | tuple(pid(), reference(), pid(), reference(), integer())).

-type amqp_mgr_state() :: list(tuple(string(), amqp_host_info())).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% returns {ok, Channel, Ticket}
-spec(open_channel/1 :: ( Pid :: pid()) -> {ok, pid(), integer()}).
open_channel(Pid) ->
    format_log(error, "AMQP_MGR: open_channel/1 is deprecated. Please use open_channel/2 (Pid, Host)~n", []),
    gen_server:call(?SERVER, {open_channel, Pid, ?DEFAULT_AMQP_HOST}, infinity).

-spec(open_channel/2 :: ( Pid :: pid(), Host :: string() ) -> {ok, pid(), integer()}).
open_channel(Pid, Host) ->
    gen_server:call(?SERVER, {open_channel, Pid, Host}, infinity).

-spec(close_channel/2 :: (Pid :: pid(), Host :: string() ) -> ok).
close_channel(Pid, Host) ->
    gen_server:cast(?SERVER, {close_channel, Pid, Host}).

-spec(stop/0 :: () -> no_return()).
stop() ->
    exit(whereis(?SERVER), normal).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1 :: (list()) -> tuple(ok, amqp_mgr_state())).
init([]) ->
    %% Start a connection to the AMQP broker server
    process_flag(trap_exit, true),
    {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%
%% Hosts = [{Host, HostInfo}]
%% HostInfo =
%% [ {connection, Connection, MRef} OR {ProcessPid, ProcessMRef, ChannelPid, ChanMRef, Ticket} ]
%%--------------------------------------------------------------------
handle_call({open_channel, Pid, Host}, _From, Hosts) ->
    case lists:keyfind(Host, 1, Hosts) of
	%% Unknown Host, need to create a Connection before a channel
	false -> start_connection_and_channel(Pid, Host, Hosts);
	%% Host is known, now look for whether Pid is known
	{Host, HostInfo} ->
	    case lists:keyfind(Pid, ?PROCESS_PID, HostInfo) of
		%% Pid is not known for the host, create channel
		false ->
		    start_channel(Pid, Host, HostInfo, Hosts);
		%% Pid is known, meaning channel exists to this Host
		{Pid, _PidMRef, Channel, _ChanMRef, Ticket} ->
		    case erlang:is_process_alive(Channel) of
			true -> {reply, {ok, Channel, Ticket}, Hosts};
			false -> start_channel(Pid, Host, lists:keydelete(Channel, ?CHANNEL_PID, HostInfo), Hosts)
		    end
	    end
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({close_channel, Pid, Host}, Hosts) ->
    case lists:keyfind(Host, 1, Hosts) of
	false ->
	    format_log(error, "AMQP_MGR(~p): Host ~p is not known, can't close channels for ~p~n", [self(), Host, Pid]),
	    {noreply, Hosts};
	{Host, HostInfo} ->
	    case lists:keyfind(Pid, ?PROCESS_PID, HostInfo) of
		false ->
		    format_log(error, "AMQP_MGR(~p): Host ~p is known, but pid ~p is not to close channel.~n", [self(), Host, Pid]),
		    {noreply, Hosts};
		{Pid, PidMRef, Channel, ChanMRef, _Ticket} ->
		    format_log(info, "AMQP_MGR(~p): Closing down channel(~p) for ~p on host ~p~n", [self(), Channel, Pid, Host]),
		    close_channel_down(Channel, ChanMRef),
		    erlang:demonitor(PidMRef),
		    {noreply, [{Host, lists:keydelete(Pid, 1, HostInfo)} | lists:keydelete(Host, 1, Hosts)]}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, Reason}, Hosts) ->
    format_log(error, "AMQP_MGR(~p): ~p went down (~p)~n", [self(), Pid, Reason]),
    Hosts1 = lists:foldl(fun(HostTuple, Hosts0) -> find_down_pid(Pid, HostTuple, Hosts0) end, [], Hosts),
    {noreply, Hosts1};
handle_info({'EXIT', Pid, Reason}, Hosts) ->
    format_log(error, "AMQP_MGR(~p): EXIT received for ~p with reason ~p~n", [self(), Pid, Reason]),
    Hosts1 = lists:foldl(fun(HostTuple, Hosts0) -> find_down_pid(Pid, HostTuple, Hosts0) end, [], Hosts),
    {noreply, Hosts1};
handle_info(_Info, Hosts) ->
    format_log(error, "AMQP_MGR(~p): Unhandled info req: ~p~nHosts: ~p~n", [self(), _Info, Hosts]),
    {noreply, Hosts}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, Hosts) ->
    close_server(Hosts),
    Reason.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, Hosts, _Extra) ->
    {ok, Hosts}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec(start_channel/4 :: (Pid :: pid(), Host :: string(), HostInfo :: amqp_host_info(), Hosts :: amqp_mgr_state()) ->
			      tuple(reply, tuple(ok, pid(), integer()), amqp_mgr_state())).
start_channel(Pid, Host, HostInfo, Hosts) ->
    {_, Conn, _} = lists:keyfind(connection, 1, HostInfo),
    {Channel, ChanMRef, ProcessMRef, Ticket} = open_amqp_channel(Conn, Pid),
    HostInfo1 = [{Pid, ProcessMRef, Channel, ChanMRef, Ticket} | HostInfo],
    format_log(info, "AMQP_MGR(~p): Open Channel for Known Host(~p) and Process pid ~p: ~n", [self(), Host, Pid]),
    {reply, {ok, Channel, Ticket}, [{Host, HostInfo1} | lists:keydelete(Host, 1, Hosts)]}.

-spec(start_connection_and_channel/3 :: (Pid :: pid(), Host :: string(), Hosts :: amqp_mgr_state()) ->
					     tuple(reply, tuple(ok, pid(), integer()) | tuple(error, term()), amqp_mgr_state())).
start_connection_and_channel(Pid, Host, Hosts) ->
    {Conn, ConnMRef} = get_new_connection(create_amqp_params(Host)),
    start_channel(Pid, Host, [{connection, Conn, ConnMRef}], Hosts).

-spec(find_down_pid/3 :: (DownPid :: pid(), Host0 :: tuple(string(), amqp_host_info()), Hosts0 :: amqp_mgr_state()) -> amqp_mgr_state()).
find_down_pid(DownPid, {Host, HostInfo}=Host0, Hosts0) ->
    %% check HostInfo's connection pid, each channel pid, and each process pid
    case lists:keyfind(DownPid, ?CONNECTION_PID, HostInfo) of
	{connection, DownPid, _MRef} -> % connection is down
	    format_log(info, "AMQP_MGR(~p): conn went down~n", [self()]),
	    close_host(Host, HostInfo), %% close all channels
	    try
		%% restart connection
		{Conn, ConnMRef} = get_new_connection(create_amqp_params(Host)),
		HostInfo0 = [{connection, Conn, ConnMRef}],

		%% reopen channels for all process pids
		HostInfo1 = lists:foldl(fun({ProcessPid, _OldProcessMRef, _OldChannel, _OldMRef, _OldTicket}, HostInfoTmp) ->
						{Channel, ChanMRef, ProcessMRef, Ticket} = open_amqp_channel(Conn, ProcessPid),
						[{ProcessPid, ProcessMRef, Channel, ChanMRef, Ticket} | HostInfoTmp];
					   ({connection, _, _}, HostInfoTmp) -> HostInfoTmp
					end, HostInfo0, HostInfo),
		[{Host, HostInfo1} | Hosts0]
	    catch
		_Class:_Error ->
		    format_log(error, "AMQP_MGR(~p): Exception when recovering a conn to ~p: ~p::~p~n", [self(), Host, _Class, _Error]),
		    Hosts0
	    end;
	false ->
	    case lists:keyfind(DownPid, ?CHANNEL_PID, HostInfo) of
		{ProcessPid, ProcessMRef, DownPid, OldChanMRef, _Ticket} ->
		    format_log(info, "AMQP_MGR(~p): channel went down~n", [self()]),
		    %% close channel down
		    close_channel_down(DownPid, OldChanMRef),
		    erlang:demonitor(ProcessMRef),

		    %% restart channel
		    {connection, Conn, _} = lists:keyfind(connection, 1, HostInfo),
		    {Channel, ChanMRef, ProcessMRef, Ticket} = open_amqp_channel(Conn, ProcessPid),
		    [{Host, [{ProcessPid, ProcessMRef, Channel, ChanMRef, Ticket} | lists:keydelete(DownPid, ?CHANNEL_PID, HostInfo)]} | Hosts0];
		false ->
		    case lists:keyfind(DownPid, ?PROCESS_PID, HostInfo) of
			{DownPid, DownMRef, Channel, ChanMRef, _Ticket} ->
			    format_log(info, "AMQP_MGR(~p): client pid went down~n", [self()]),
			    close_channel_down(Channel, ChanMRef),
			    erlang:demonitor(DownMRef),
			    %% Process is down, remove channel
			    [{Host, lists:keydelete(DownPid, ?PROCESS_PID, HostInfo)} | lists:keydelete(Host, 1, Hosts0)];
			false ->
			    %% unchanged
			    [Host0 | Hosts0]
		    end
	    end
    end.

-spec(close_server/1 :: (Hosts :: amqp_mgr_state()) -> no_return()).
close_server(Hosts) ->
    format_log(info, "AMQP_MGR(~p): Closing server down~n", [self()]),
    lists:foreach(fun({Host, HostInfo}) ->
			  close_host(Host, HostInfo)
		  end, Hosts).

-spec(close_host/2 :: (Host :: string(), HostInfo :: amqp_host_info()) -> no_return()).
close_host(Host, HostInfo) ->
    format_log(info, "AMQP_MGR(~p): Closing host ~p down~n", [self(), Host]),
    {connection, Conn, Ref} = lists:keyfind(connection, 1, HostInfo),
    ConnUp = erlang:is_process_alive(Conn),
    lists:foreach(fun({Pid, PidMRef, Channel, ChanMRef, _Ticket}) ->
			  case ConnUp of
			      true ->
				  close_channel_down(Channel, ChanMRef),
				  erlang:demonitor(PidMRef),
				  case erlang:is_process_alive(Pid) of
				      true -> Pid ! {amqp_host_down, Host};
				      false -> ok
				  end;
			      false ->
				  ok
			  end
		  end, lists:keydelete(connection, 1, HostInfo)),
    ConnUp andalso close_conn(Conn, Ref).

-spec(close_conn/2 :: (Conn :: pid(), Ref :: reference()) -> no_return()).
close_conn(Conn, Ref) ->
    erlang:demonitor(Ref),
    amqp_connection:close(Conn, 200, <<"Goodbye">>).

-spec(close_channel_down/2 :: (Chan :: pid(), Ref :: reference()) -> none()).
close_channel_down(Chan, Ref) ->
    erlang:demonitor(Ref),
    try
	case erlang:is_process_alive(Chan) of
	    true -> amqp_channel:close(Chan);
	    false -> ok
	end
    catch
	_:_ -> ok
    end.

-spec(create_amqp_params/1 :: (Host :: string()) -> tuple()).
create_amqp_params(Host) ->
    create_amqp_params(Host, 5672).
-spec(create_amqp_params/2 :: (Host :: string(), Port :: integer()) -> tuple()).
create_amqp_params(Host, Port) ->
    format_log(info, "AMQP_MGR(~p): H: ~p P: ~p~n", [self(), Host, Port]),
    #'amqp_params'{
      port = Port
      ,host = Host
     }.

-spec(get_new_connection/1 :: (P :: tuple()) -> tuple(pid(), reference())).
get_new_connection(#'amqp_params'{}=P) ->
    Localhost = net_adm:localhost(),
    {ok, Connection} = amqp_connection:start(network, P),
    format_log(info, "AMQP_MGR(~p): Conn ~p started.~n", [Connection]),
    MRefConn = erlang:monitor(process, Connection),
    {Connection, MRefConn}.

-spec(open_amqp_channel/2 :: (Connection :: pid(), Pid :: pid()) -> tuple(pid(), reference(), reference(), integer())).
open_amqp_channel(Connection, Pid) ->
    %% Open an AMQP channel to access our realm
    {ok, Channel} = amqp_connection:open_channel(Connection),
    format_log(info, "AMQP_MGR(~p): Open channel(~p) for ~p on conn ~p~n", [self(), Channel, Pid, Connection]),

    %% if a message is returned, we need to handle it
    amqp_channel:register_return_handler(Channel, Pid),

    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, amqp_util:access_request()),

    MRef = erlang:monitor(process, Pid),
    ChanMRef = erlang:monitor(process, Channel),
    {Channel, ChanMRef, MRef, Ticket}.
