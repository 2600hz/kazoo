%%%-------------------------------------------------------------------
%%% File    : amqp_manager.erl
%%% Author  : K Anderson
%%% Description : The fs_toolkit AMQP connection manager.  
%%%
%%% Created :  March 24 2010
%%%-------------------------------------------------------------------
-module(amqp_manager).

-compile(export_all).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [log/2, format_log/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, start/0, open_channel/1, close_channel/1, stop/0]).

-record(state, {connection, channels, options}).
-define(SERVER, ?MODULE).

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
get_options() ->
    #'amqp_params'{
      port=5672
      ,host="fs1.voicebus.net"
     }.

init([]) ->
    %% Start a connection to the AMQP broker server
    Options = get_options(),

    process_flag(trap_exit, true),

    format_log(info, "AMQP_MGR(~p): open connection to ~p~n", [self(), Options]),
    Connection = amqp_connection:start_network_link(Options),
    MRefConn = erlang:monitor(process, Connection),

    {ok, #state{connection={Connection, MRefConn}, channels = [], options=Options}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({open_channel, _Pid}=Req, _From, #state{connection=undefined, options=Options}=State) ->
    Connection = case Options of
		     undefined ->
			 amqp_connection:start_network_link(get_options());
		     Options ->
			 amqp_connection:start_network_link(Options)
		 end,
    format_log(info, "AMQP_MGR(~p): restart connection~n", [self()]),
    MRefConn = erlang:monitor(process, Connection),
    handle_call(Req, _From, State#state{connection={Connection, MRefConn}});
handle_call({open_channel, Pid}, _From, #state{connection={Connection, _MRefConn}, channels=Channels}=State) ->
    case lists:keysearch(Pid, 1, Channels) of
        %% This PID already has a channel, just return it
        {value, {_Pid, Channel, Ticket, _MRef}} ->
	    format_log(info, "AMQP_MGR(~p): Found Channel for ~p C: ~p T: ~p~n", [self(), Pid, Channel, Ticket]),
            {reply, {ok, Channel, Ticket}, State};

        false ->
            %% Open an AMQP channel to access our realm
	    format_log(info, "AMQP_MGR(~p): Open channel for ~p~n", [self(), Pid]),
            Channel = amqp_connection:open_channel(Connection),

	    %% if a message is returned, we need to handle it
	    amqp_channel:register_return_handler(Channel, Pid),

            Access = #'access.request'{
	      realm = <<"/data">>, %% fs_toolkit_cfg:get([amqp_access, realm]),
	      exclusive = false, %% fs_toolkit_cfg:get([amqp_access, exclusive]),
	      passive = true, %% fs_toolkit_cfg:get([amqp_access, passive]),
	      active = true, %% fs_toolkit_cfg:get([amqp_access, active]),
	      write = true, %% fs_toolkit_cfg:get([amqp_access, write]),
	      read = true %% fs_toolkit_cfg:get([amqp_access, read])
	     },

            case amqp_channel:call(Channel, Access) of
                #'access.request_ok'{ticket = Ticket} ->
                    MRef = erlang:monitor(process, Pid),
                    {reply,
		     {ok, Channel, Ticket},
		     State#state{channels=[{Pid, Channel, Ticket, MRef} | Channels]}
                    };

                Fail ->
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
    case lists:keysearch(Pid, 1, Channels) of
        {value, {Pid, Channel, _Ticket, MRef}} ->
	    ChannelClose = #'channel.close'{
	      reply_code = 200,
	      reply_text = <<"Goodbye">>,
	      class_id = 0,
	      method_id = 0
	     },

            #'channel.close_ok'{} = amqp_channel:call(Channel, ChannelClose),

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
handle_info({'DOWN', MRefConn, process, Pid, Reason}, #state{connection={Pid,MRefConn}}=State) ->
    format_log(error, "AMQP_MGR(~p): Conn ~p went down (~p)~n", [self(), Pid, Reason]),
    {stop, Reason, State};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{connection={Connection,_MRefConn}, channels=Channels}=State) ->
    case lists:keysearch(Pid, 1, Channels) of
        {value, {Pid, Channel, _Ticket, MRef}} ->
            ChannelClose = #'channel.close'{
	      reply_code = 200,
	      reply_text = <<"Goodbye">>,
	      class_id = 0,
	      method_id = 0
	     },
            #'channel.close_ok'{} = amqp_channel:call(Channel, ChannelClose),

            NewChannels = lists:keydelete(Pid, 1, Channels),

            erlang:demonitor(MRef),

            {noreply, #state{connection = Connection, channels = NewChannels}};
        false ->
	    {noreply, State}
    end;
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
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
terminate(Reason, S) ->
    close_all_channels(S#state.channels),
    ok = amqp_connection:close(S#state.connection, 200, <<"Goodbye">>),
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
    ChannelClose = #'channel.close'{
      reply_code = 200,
      reply_text = <<"Goodbye">>,
      class_id = 0,
      method_id = 0
     },

    amqp_channel:call(Channel, ChannelClose),
    erlang:demonitor(MRef),
    close_all_channels(T).
