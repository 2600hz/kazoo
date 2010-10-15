%%%-------------------------------------------------------------------
%%% File    : amqp_manager.erl
%%% Author  : K Anderson
%%% Description : The fs_toolkit AMQP connection manager.  
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
-export([start_link/0, start/0, open_channel/1, open_channel/2, close_channel/1, stop/0]).

%% channels = [{Pid, Channel, Ticket, Ref} | {Pid, Channel, Ticket, Ref, Host, HostConn, HostRef}]
-record(state, {connection, conn_params, channels}).
-define(SERVER, ?MODULE).
-define(DEFAULT_AMQP_HOST, "whistle-erl001-fmt.2600hz.org").

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

open_channel(Pid) ->
    gen_server:call(?SERVER, {open_channel, Pid}, infinity).

open_channel(Pid, Host) ->
    gen_server:call(?SERVER, {open_channel, Pid, Host}, infinity).

close_channel(Pid) ->
    gen_server:cast(?SERVER, {close_channel, Pid}).

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
init([]) ->
    %% Start a connection to the AMQP broker server
    process_flag(trap_exit, true),

    LocalP = create_amqp_params(net_adm:localhost()),
    try
	format_log(info, "AMQP_MGR(init): Trying localhost(~p) for connection~n", [net_adm:localhost()]),
	C = get_new_connection(LocalP),
	{ok, #state{connection=C, conn_params=LocalP, channels = []}}
    catch
	_:Reason ->
	    format_log(error, "AMQP_MGR(init): Failed to start localhost connection: ~p~n", [Reason]),
	    
	    P = create_amqp_params(?DEFAULT_AMQP_HOST),
	    format_log(info, "AMQP_MGR(init): Failed to find amqp on localhost. Trying ~p~n", [?DEFAULT_AMQP_HOST]),
	    try
		C1 = get_new_connection(P),
		{ok, #state{connection=C1, conn_params=P, channels = []}}
	    catch
		_:Reason ->
		    format_log(error, "AMQP_MGR(init): Failed to start connection: ~p~n", [Reason]),
		    {stop, Reason}
	    end
    end;
init([#'amqp_params'{}=P]) ->
    process_flag(trap_exit, true),
    format_log(info, "AMQP_MGR(init): Trying to connect to amqp on ~p~n", [#'amqp_params'.host]),

    try
	C = get_new_connection(P),
	{ok, #state{connection=C, conn_params=P, channels=[]}}
    catch
	_:Reason ->
	    format_log(error, "AMQP_MGR(init): Failed to start connection: ~p~n", [Reason]),
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%
%% channels = [{Pid, Channel, Ticket, Ref} | {Pid, Channel, Ticket, Ref, Host, HostConn, HostRef}]
%%--------------------------------------------------------------------
handle_call({open_channel, Pid, Host}, _From, #state{channels=Channels}=State) ->
    format_log(info, "AMQP_MGR(~p): Start channel using a different host (~p)~n", [self(), Host]),
    case lists:keyfind(Pid, 1, Channels) of
        %% This PID already has a channel, just return it
        {Pid, Channel, Ticket, _MRef} ->
	    format_log(info, "AMQP_MGR(~p): Found Channel for ~p C: ~p T: ~p~n", [self(), Pid, Channel, Ticket]),
            {reply, {ok, Channel, Ticket}, State};
	{Pid, Channel, Ticket, _MRef, Host, _HostRef} ->
	    format_log(info, "AMQP_MGR(~p): Found Channel for ~p C: ~p T: ~p from Host(~p)~n", [self(), Pid, Channel, Ticket, Host]),
	    {reply, {ok, Channel, Ticket}, State};
        false ->
	    {HostConn, HostRef} = get_new_connection(create_amqp_params(Host)),
	    case open_amqp_channel(HostConn, Pid) of
		{HostChannel, MRef, HostTicket} ->
                    {reply
		     ,{ok, HostChannel, HostTicket}
		     ,State#state{channels=[{Pid, HostChannel, HostTicket, MRef, Host, HostConn, HostRef} | Channels]}
                    };
                Fail ->
		    format_log(error, "AMQP_MGR(~p): Failed to access Channel: ~p~n", [self(), Fail]),
                    {reply, {error, Fail}, State}
            end
    end;
handle_call({open_channel, _Pid}=Req, From, #state{connection=undefined, conn_params=P}=State) ->
    format_log(info, "AMQP_MGR(~p): restart connection w/ ~p~n", [self(), P]),
    handle_call(Req, From, State#state{connection=get_new_connection(P)});
handle_call({open_channel, Pid}, _From, #state{connection={Connection, _MRefConn}, channels=Channels}=State) ->
    case lists:keyfind(Pid, 1, Channels) of
        %% This PID already has a channel, just return it
        {_Pid, Channel, Ticket, _MRef} ->
	    format_log(info, "AMQP_MGR(~p): Found Channel for ~p C: ~p T: ~p~n", [self(), Pid, Channel, Ticket]),
            {reply, {ok, Channel, Ticket}, State};
	{_Pid, Channel, Ticket, _MRef, _Host, _HostConn, _HostRef} ->
	    format_log(info, "AMQP_MGR(~p): Found Channel for ~p C: ~p T: ~p from Host(~p)~n", [self(), Pid, Channel, Ticket, _Host]),
	    {reply, {ok, Channel, Ticket}, State};
        false ->
	    case open_amqp_channel(Connection, Pid) of
		{Channel, MRef, Ticket} ->
                    {reply
		     ,{ok, Channel, Ticket}
		     ,State#state{channels=[{Pid, Channel, Ticket, MRef} | Channels]}
                    };
                Fail ->
		    format_log(error, "AMQP_MGR(~p): Failed to access Channel: ~p~n", [self(), Fail]),
                    {reply, {error, Fail}, State}
            end
    end;
handle_call(_Request, _From, State) ->
    format_log(error, "AMQP_MGR(~p): Unhandled call req: ~p~nState: ~p~n", [self(), _Request, State]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({close_channel, Pid}, #state{channels=Channels}=State) ->
    case lists:keyfind(Pid, 1, Channels) of
        {Pid, Channel, _Ticket, MRef} ->
	    case erlang:is_process_alive(Channel) of
		true ->
		    #'channel.close_ok'{} = amqp_channel:call(Channel, amqp_util:channel_close());
		false ->
		    ok
	    end,
            erlang:demonitor(MRef),
            {noreply, State#state{channels=lists:keydelete(Pid, 1, Channels)}};
        false ->
	    {noreply, State}
    end;
handle_cast(_Msg, State) ->
    format_log(error, "AMQP_MGR(~p): Unhandled cast req: ~p~nState: ~p~n", [self(), _Msg, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', MRefConn, process, Pid, Reason}, #state{connection={Pid,MRefConn}, conn_params=P, channels=Channels}=State) ->
    format_log(error, "AMQP_MGR(~p): Conn ~p went down (~p), restart with ~p~n", [self(), Pid, Reason, P]),
    close_all_channels(Channels),
    {noreply, State#state{connection=get_new_connection(P), channels=[]}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{channels=Channels}=State) ->
    format_log(error, "AMQP_MGR(~p): Channel ~p went down (~p)~n", [self(), Pid, _Reason]),
    case lists:keyfind(Pid, 1, Channels) of
        {Pid, Channel, _Ticket, MRef} ->
            case erlang:is_process_alive(Channel) of
		true ->
		    amqp_manager:close_channel(Channel);
		false ->
		    ok
	    end,
	    erlang:demonitor(MRef),
	    {noreply, State#state{channels = lists:keydelete(Pid, 1, Channels)}};
        false ->
	    {noreply, State}
    end;
handle_info({'EXIT', Pid, Reason}, #state{connection={Pid, _MRefConn}, conn_params=P, channels=Channels}=State) ->
    format_log(error, "AMQP_MGR(~p): EXIT received for Conn(~p) with reason ~p~n", [self(), Pid, Reason]),
    close_all_channels(Channels),
    {noreply, State#state{connection=get_new_connection(P), channels=[]}};
handle_info(_Info, State) ->
    format_log(error, "AMQP_MGR(~p): Unhandled info req: ~p~nState: ~p~n", [self(), _Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{connection=undefined}) ->
    Reason;
terminate(Reason, #state{connection={Conn, _MRefConn}, channels=Channels}) ->
    close_all_channels(Channels),
    ok = amqp_connection:close(Conn, 200, <<"Goodbye">>),
    Reason.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
close_all_channels([]) -> ok;
close_all_channels([{_Pid, Channel, _Ticket, MRef} | T]) ->
    case  is_process_alive(Channel) of
	true ->
	    amqp_manager:close_channel(Channel),
	    erlang:demonitor(MRef);
	false ->
	    ok
    end,
    close_all_channels(T).

-spec(create_amqp_params/1 :: (Host :: string()) -> tuple()).
create_amqp_params(Host) ->
    create_amqp_params(Host, 5672).
-spec(create_amqp_params/2 :: (Host :: string(), Port :: integer()) -> tuple()).
create_amqp_params(Host, Port) ->
    #'amqp_params'{
		    port = Port
		    ,host = Host
		  }.

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
