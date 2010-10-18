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

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, start/0, open_channel/1, open_channel/2, close_channel/2, stop/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_AMQP_HOST, "whistle-erl001-fmt.2600hz.org").

%% [ {connection, Connection, MRef} OR {ProcessPid, ChannelPid, MRef, Ticket} ]
%% state = [ {Host, host_info()} ]
-type amqp_host_info() :: list(tuple(connection, pid(), reference()) | tuple(pid(), pid(), reference(), integer())).

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
%% HostInfo = [
%%   {connection, Conn, MRef}, <- 1 and only 1
%%   {ProcessPid, Channel, Ticket, MRef} <- 0 or more
%% ]
%%--------------------------------------------------------------------
handle_call({open_channel, Pid, Host}, _From, Hosts) ->
    case lists:keyfind(Host, 1, Hosts) of
	%% Unknown Host, need to create a Connection before a channel
	false ->
	    {Conn, ConnMRef} = get_new_connection(create_amqp_params(Host)),
	    case open_amqp_channel(Conn, Pid) of
		{Channel, MRef, Ticket} ->
		    HostInfo = [{connection, Conn, ConnMRef}, {Pid, Channel, Ticket, MRef}],
		    format_log(info, "AMQP_MGR(~p): New Host(~p): ~p~n", [self(), Host, HostInfo]),
		    {reply, {ok, Channel, Ticket}, [{Host, HostInfo} | Hosts]};
		Fail ->
		    HostInfo = [{connection, Conn, ConnMRef}],
		    format_log(error, "AMQP_MGR(~p): Failed to open channel for ~p: ~p~n", [self(), Host, Fail]),
		    {reply, {error, Fail}, [{Host, HostInfo} | Hosts]}
	    end;
	%% Host is known, now look for whether Pid is known
	{Host, HostInfo} ->
	    case lists:keyfind(Pid, 1, HostInfo) of
		%% Pid is not known for the host, create channel
		false ->
		    {_, Conn, _} = lists:keyfind(connection, 1, HostInfo),
		    case open_amqp_channel(Conn, Pid) of
			{Channel, MRef, Ticket} ->
			    HostInfo1 = [{Pid, Channel, Ticket, MRef} | HostInfo],
			    format_log(info, "AMQP_MGR(~p): New Channel for Host(~p): ~p~n", [self(), Host, HostInfo1]),
			    {reply, {ok, Channel, Ticket}, [{Host, HostInfo1} | lists:keydelete(Host, 1, Hosts)]};
			Fail ->
			    format_log(error, "AMQP_MGR(~p): Failed to open channel for ~p: ~p~n", [self(), Host, Fail]),
			    {reply, {error, Fail}, Hosts}
		    end;
		%% Pid is known, meaning channel exists to this Host
		{Pid, Channel, Ticket, _MRef} ->
		    {reply, {ok, Channel, Ticket}, Hosts}
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
	    case lists:keyfind(Pid, 1, HostInfo) of
		false ->
		    format_log(error, "AMQP_MGR(~p): Host ~p is known, but pid ~p is not.~n", [self(), Host, Pid]),
		    {noreply, Hosts};
		{Pid, Channel, _Ticket, MRef} ->
		    format_log(info, "AMQP_MGR(~p): Closing down ~p for ~p on host ~p~n", [self(), Channel, Pid, Host]),
		    close_channel_down(Channel, MRef),
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
    {noreply, Hosts};
handle_info({'EXIT', Pid, Reason}, Hosts) ->
    format_log(error, "AMQP_MGR(~p): EXIT received for ~p with reason ~p~n", [self(), Pid, Reason]),
    {noreply, Hosts};
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

close_server(Hosts) ->
    lists:foreach(fun({Host, HostInfo}) ->
			  close_host(Host, HostInfo)
		  end, Hosts).

close_host(Host, HostInfo) ->
    lists:foreach(fun({connection, Conn, Ref}) ->
			  close_conn(Conn, Ref);
		     ({Pid, Channel, _Ticket, Ref}) ->
			  close_channel_down(Channel, Ref),
			  Pid ! {amqp_host_down, Host}
		  end, HostInfo).

close_conn(Conn, Ref) ->
    erlang:demonitor(Ref),
    amqp_connection:close(Conn, 200, <<"Goodbye">>).

close_channel_down(Chan, Ref) ->
    erlang:demonitor(Ref),
    case erlang:is_process_alive(Chan) of
	true ->
	    amqp_util:channel_close(Chan);
	false ->
	    ok
    end.

-spec(create_amqp_params/1 :: (Host :: string()) -> tuple()).
create_amqp_params(Host) ->
    create_amqp_params(Host, 5672).
-spec(create_amqp_params/2 :: (Host :: string(), Port :: integer()) -> tuple()).
create_amqp_params(Host, Port) ->
    #'amqp_params'{
		    port = Port
		    ,host = Host
		  }.

-spec(get_new_connection/1 :: (P :: tuple()) -> tuple(pid(), reference())).
get_new_connection(#'amqp_params'{}=P) ->
    Connection = amqp_connection:start_network_link(P),
    MRefConn = erlang:monitor(process, Connection),
    {Connection, MRefConn}.

-spec(open_amqp_channel/2 :: (Connection :: pid(), Pid :: pid()) -> tuple(pid(), integer(), reference()) | any()).
open_amqp_channel(Connection, Pid) ->
    %% Open an AMQP channel to access our realm
    Channel = amqp_connection:open_channel(Connection),
    format_log(info, "AMQP_MGR(~p): Open channel(~p) for ~p~n", [self(), Channel, Pid]),

    %% if a message is returned, we need to handle it
    amqp_channel:register_return_handler(Channel, Pid),

    Access = amqp_util:access_request(),

    case amqp_channel:call(Channel, Access) of
	#'access.request_ok'{ticket = Ticket} ->
	    MRef = erlang:monitor(process, Pid),
	    {Channel, MRef, Ticket};
	Fail -> Fail
    end.
