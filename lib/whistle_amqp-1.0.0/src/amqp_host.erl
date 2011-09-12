%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle a host's connection/channels
%%% @end
%%% Created : 18 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(amqp_host).

-behaviour(gen_server).

%% API
-export([start_link/2, publish/4, consume/3, misc_req/3, misc_req/4, stop/1]).
-export([register_return_handler/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(KNOWN_EXCHANGES, [{?EXCHANGE_TARGETED, ?TYPE_TARGETED}
			  ,{?EXCHANGE_CALLCTL, ?TYPE_CALLCTL}
			  ,{?EXCHANGE_CALLEVT, ?TYPE_CALLEVT}
			  ,{?EXCHANGE_CALLMGR, ?TYPE_CALLMGR}
			  ,{?EXCHANGE_MONITOR, ?TYPE_MONITOR}
			  ,{?EXCHANGE_CONFIGURATION, ?TYPE_CONFIGURATION}
			  ,{?EXCHANGE_CONFERENCE, ?TYPE_CONFERENCE}
			 ]).

%% Channel, ChannelRef, Ticket[, FromRef]
-type channel_data() :: tuple(pid(), reference(), integer()).
-type consumer_data() :: tuple(pid(), reference(), integer(), reference()).

-type consume_records() :: #'queue.declare'{} | #'queue.bind'{} | #'queue.unbind'{} | #'queue.delete'{} |
			   #'basic.consume'{} | #'basic.cancel'{}.

-record(state, {
	  connection = undefined :: undefined | tuple(pid(), reference())
          ,publish_channel = undefined :: undefined | channel_data()
          ,misc_channel = undefined :: undefined | channel_data()
          ,consumers = dict:new() :: amqp_host:dict(pid(), consumer_data())
          ,return_handlers = dict:new() %% ref, pid() - list of PIDs that are interested in returned messages
          ,manager = undefined :: undefined | pid()
          ,amqp_h = undefined :: undefined | binary()
	 }).

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
start_link(Host, Conn) ->
    gen_server:start_link(?MODULE, [Host, Conn], []).

-spec publish/4 :: (Srv, From, BasicPub, AmqpMsg) -> ok when
      Srv :: pid(),
      From :: tuple(pid(), reference()),
      BasicPub :: #'basic.publish'{},
      AmqpMsg :: binary() | iolist().
publish(Srv, From, BasicPub, AmqpMsg) ->
    gen_server:cast(Srv, {publish, From, BasicPub, AmqpMsg}).

%% return the Channel PID when executing a basic.consume{} so the channel
%% and calling process can link. This means if a channel dies, the process
%% will receive the exit signal and vice-versa.
%% Should help get unused Channels to die
-spec consume/3 :: (Srv, From, Msg) -> ok when
      Srv :: pid(),
      From :: tuple(pid(), reference()),
      Msg :: consume_records().
consume(Srv, From, Msg) ->
    gen_server:cast(Srv, {consume, From, Msg}).

-spec misc_req/3 :: (Srv, From, Req) -> ok when
      Srv :: pid() | atom(),
      From :: tuple(pid(), reference()),
      Req :: tuple().
misc_req(Srv, From, Req) ->
    gen_server:cast(Srv, {misc_req, From, Req}).

-spec misc_req/4 :: (Srv, From, Req1, Req2) -> ok when
      Srv :: pid() | atom(),
      From :: tuple(pid(), reference()),
      Req1 :: tuple(),
      Req2 :: tuple().
misc_req(Srv, From, Req1, Req2) ->
    gen_server:cast(Srv, {misc_req, From, Req1, Req2}).

-spec register_return_handler/2 :: (Srv, From) -> ok when
      Srv :: pid(),
      From :: tuple(pid(), reference()).
register_return_handler(Srv, From) ->
    gen_server:cast(Srv, {register_return_handler, From}).

-spec stop/1 :: (Srv) -> ok | {error, you_are_not_my_boss} when
      Srv :: pid().
stop(Srv) ->
    gen_server:call(Srv, stop).

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
init([Host, Conn]) when is_pid(Conn) ->
    process_flag(trap_exit, true),
    ?LOG_SYS("starting amqp host for broker ~s", [Host]),
    {ok, {Host, Conn}, 0}.

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
handle_call(stop, {From, _}, #state{manager=Mgr}=State) ->
    case Mgr =:= From of
	true -> {stop, normal, ok, State};
	false -> {reply, {error, you_are_not_my_boss}, State}
    end.

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
handle_cast({publish, From, BasicPub, AmqpMsg}, #state{publish_channel={C,_,T}}=State) ->
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, BasicPub#'basic.publish'{ticket=T}, AmqpMsg)) end),
    {noreply, State};

handle_cast({register_return_handler, {FromPid, _}=From}, #state{return_handlers=RHDict}=State) ->
    gen_server:reply(From, ok),
    ?LOG_SYS("adding ~p as a return handler", [FromPid]),
    {noreply, State#state{return_handlers=dict:store(erlang:monitor(process, FromPid), FromPid, RHDict)}, hibernate};

handle_cast({consume, {FromPid, _}=From, #'basic.consume'{}=BasicConsume}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} -> % channel, channel ref, ticket
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, {C, amqp_channel:subscribe(C, BasicConsume#'basic.consume'{ticket=T}, FromPid)}),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    ?LOG_SYS("Failed to start channel: closing"),
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    ?LOG_SYS("Failed to start channel: ~p", [E]),
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    gen_server:reply(From, {C, amqp_channel:subscribe(C, BasicConsume#'basic.consume'{ticket=T}, FromPid)}),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.cancel'{}=BasicCancel}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:cast(C, BasicCancel, FromPid)),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    ?LOG_SYS("Failed to start channel: closing"),
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,_,_}} ->
	    gen_server:reply(From, amqp_channel:cast(C, BasicCancel, FromPid)),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.bind'{}=QueueBind}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:call(C, QueueBind#'queue.bind'{ticket=T})),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    ?LOG_SYS("Failed to start channel: closing"),
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    gen_server:reply(From, amqp_channel:call(C, QueueBind#'queue.bind'{ticket=T})),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.unbind'{}=QueueUnbind}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:call(C, QueueUnbind#'queue.unbind'{ticket=T})),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    gen_server:reply(From, amqp_channel:call(C, QueueUnbind#'queue.unbind'{ticket=T})),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.declare'{}=QueueDeclare}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:call(C, QueueDeclare#'queue.declare'{ticket=T})),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    Call = amqp_channel:call(C, QueueDeclare#'queue.declare'{ticket=T}),
	    gen_server:reply(From, Call),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.delete'{}=QueueDelete}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:cast(C, QueueDelete#'queue.delete'{ticket=T})),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    gen_server:reply(From, amqp_channel:cast(C, QueueDelete#'queue.delete'{ticket=T})),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.qos'{}=BasicQos}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:call(C, BasicQos)),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,_,_}} ->
	    gen_server:reply(From, amqp_channel:call(C, BasicQos)),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.ack'{}=BasicAck}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:cast(C, BasicAck)),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,_,_}} ->
	    gen_server:reply(From, amqp_channel:cast(C, BasicAck)),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.nack'{}=BasicNack}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    gen_server:reply(From, amqp_channel:cast(C, BasicNack)),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}, hibernate};
		closing ->
		    gen_server:reply(From, {error, closing}),
		    {noreply, State};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,_,_}} ->
	    gen_server:reply(From, amqp_channel:cast(C, BasicNack)),
	    {noreply, State}
    end;

handle_cast({misc_req, From, #'exchange.declare'{}=ED}, #state{misc_channel={C,_,T}}=State) ->
    spawn(fun() ->
		  case amqp_channel:call(C, ED#'exchange.declare'{ticket=T}) of
		      #'exchange.declare_ok'{} -> gen_server:reply(From, ok);
		      E -> gen_server:reply(From, E)
		  end
	  end),
    {noreply, State};

handle_cast({misc_req, From, Req}, #state{misc_channel={C,_,_}}=State) ->
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, Req)) end),
    {noreply, State};

handle_cast({misc_req, From, Req1, Req2}, #state{misc_channel={C,_,_}}=State) ->
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, Req1, Req2)) end),
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

handle_info({'DOWN', Ref, process, _Pid, Reason}, #state{connection={_, Ref}, return_handlers=RHDict}=State) ->
    ?LOG_SYS("recieved notification our connection to the amqp broker died: ~p", [Reason]),
    {stop, Reason, State#state{return_handlers=dict:erase(Ref, RHDict)}, hibernate};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{return_handlers=RHDict}=State) ->
    ?LOG_SYS("recieved notification monitored process ~p  died ~p, searching for reference", [_Pid, _Reason]),
    erlang:demonitor(Ref, [flush]),
    {noreply, remove_ref(Ref, State#state{return_handlers=dict:erase(Ref, RHDict)}), hibernate};

handle_info(timeout, {Host, Conn}) ->
    Ref = erlang:monitor(process, Conn),
    case start_channel(Conn) of
	{Channel, _, Ticket} = PubChan ->
	    load_exchanges(Channel, Ticket),
            amqp_channel:register_return_handler(Channel, self()),
	    {noreply, #state{
	       connection = {Conn, Ref}
	       ,publish_channel = PubChan
	       ,misc_channel = start_channel(Conn)
	       ,consumers = dict:new()
	       ,manager = whereis(amqp_mgr)
               ,amqp_h = Host
	      }
	     , hibernate
	    };
	{error, E} ->
            ?LOG_SYS("unable to initialize publish channel for amqp host ~s, ~p", [Host, E]),
	    erlang:demonitor(Ref, [flush]),
	    {stop, E, Conn}
    end;

handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, #state{return_handlers=RHDict}=State) ->
    spawn(fun() ->
                  ?LOG_SYS("recieved notification a message couldnt be delivered, forwarding to registered return handlers"),
                  dict:map(fun(_, Pid) -> Pid ! ReturnMsg end, RHDict)
          end),
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
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
-spec terminate/2 :: (Reason, State) -> ok when
      Reason :: term(),
      State :: #state{}.
terminate(_Reason, #state{consumers=Consumers, amqp_h=Host}) ->
    notify_consumers({amqp_host_down, Host}, Consumers),
    ?LOG_SYS("amqp host for ~s terminated ~p", [Host, _Reason]).

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
-spec start_channel/1 :: (Connection) -> channel_data() | tuple(error, no_connection) | closing when
      Connection :: undefined | {pid(), reference()} | pid().
start_channel(undefined) ->
    {error, no_connection};
start_channel({Connection, _}) ->
    start_channel(Connection);
start_channel(Connection) when is_pid(Connection) ->
    %% Open an AMQP channel to access our realm
    case amqp_connection:open_channel(Connection) of
	{ok, Channel} ->
	    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, amqp_util:access_request()),
	    ?LOG_SYS("Opened channel ~p with ticket ~b", [Channel, Ticket]),

	    ChanMRef = erlang:monitor(process, Channel),
	    {Channel, ChanMRef, Ticket};
	E ->
	    ?LOG_SYS("Error opening channel: ~p", [E]),
	    E
    end.

-spec start_channel/2 :: (Connection, Pid) -> channel_data() | {error, no_connection} | closing when
      Connection :: undefined | {pid(), reference()} | pid(),
      Pid :: pid().
start_channel(Connection, Pid) ->
    case start_channel(Connection) of
	{C, _, T} = Channel ->
	    amqp_channel:register_return_handler(C, Pid),
	    #'access.request_ok'{ticket=T} = amqp_channel:call(C, amqp_util:access_request()),
	    ?LOG_SYS("Started channel ~p for caller ~p", [C, Pid]),
	    Channel;
	E ->
            ?LOG_SYS("failed to start new channel for ~p: ~p", [Pid, E]),
            E
    end.

-spec load_exchanges/2 :: (Channel, Ticket) -> ok when
      Channel :: pid(),
      Ticket :: integer().
load_exchanges(Channel, Ticket) ->
    lists:foreach(fun({Ex, Type}) ->
			  ED = #'exchange.declare'{
			    ticket = Ticket
			    ,exchange = Ex
			    ,type = Type
			   },
			  #'exchange.declare_ok'{} = amqp_channel:call(Channel, ED)
		  end, ?KNOWN_EXCHANGES).

-spec remove_ref/2 :: (Ref, State) -> #state{} when
      Ref :: reference(),
      State :: #state{}.
remove_ref(Ref, #state{connection={Conn, _}, publish_channel={C,Ref,_}}=State) ->
    ?LOG_SYS("reference was for publish channel ~p, restarting", [C]),
    State#state{publish_channel=start_channel(Conn)};

remove_ref(Ref, #state{connection={Conn, _}, misc_channel={C,Ref,_}}=State) ->
    ?LOG_SYS("reference was for misc channel ~p, restarting", [C]),
    State#state{misc_channel=start_channel(Conn)};

remove_ref(Ref, #state{connection={Conn, _}, consumers=Cs}=State) ->
    State#state{consumers =
		    dict:fold(fun(K, V, Acc) -> clean_consumers(K, V, Acc, Ref, Conn) end, Cs, Cs)
	       }.

-spec notify_consumers/2 :: (Msg, Dict) -> ok when
      Msg :: {'amqp_host_down', 'undefined' | binary()},
      Dict :: dict().
notify_consumers(Msg, Dict) ->
    lists:foreach(fun({Pid,_}) -> Pid ! Msg end, dict:to_list(Dict)).

%% Channel died
-spec clean_consumers/5 :: (FromPid, Value, Acc, Ref, Conn) -> dict() when
      FromPid :: pid(),
      Value :: {pid(), reference(), integer(), reference()},
      Acc :: dict(),
      Ref :: reference(),
      Conn :: pid().
clean_consumers(FromPid, {C,Ref1,_,FromRef}, AccDict, Ref, Conn) when Ref =:= Ref1 ->
    ?LOG_SYS("reference was for channel ~p for ~p, restarting", [C, FromPid]),

    erlang:demonitor(Ref1, [flush]),
    erlang:is_process_alive(C) andalso amqp_channel:close(C),

    case start_channel(Conn, FromPid) of
	{CNew, RefNew, TNew} ->
	    ?LOG_SYS("New channel started for ~p", [FromPid]),
	    dict:store(FromPid, {CNew, RefNew, TNew, FromRef}, AccDict);
	{error, no_connection} ->
	    ?LOG_SYS("No connection available"),
	    FromPid ! {amqp_lost_channel, no_connection},
	    dict:erase(FromPid, AccDict);
	closing ->
	    ?LOG_SYS("Closing, no connection"),
	    FromPid ! {amqp_lost_channel, no_connection},
	    dict:erase(FromPid, AccDict)
    end;

%% Consumer died
clean_consumers(FromPid, {C,CRef,_,FromRef}, AccDict, Ref, _) when Ref =:= FromRef ->
    ?LOG_SYS("reference was for consumer ~p, removing channel ~p", [FromPid, C]),
    erlang:demonitor(CRef, [flush]),
    erlang:demonitor(FromRef, [flush]),
    erlang:is_process_alive(C) andalso amqp_channel:close(C),
    dict:erase(FromPid, AccDict);

%% Generic channel cleanup when FromPid isn't alive
clean_consumers(FromPid, {C,CRef,_,FromRef}, AccDict, _, _) ->
    case erlang:is_process_alive(FromPid) of
	true -> AccDict;
	false ->
	    ?LOG_SYS("reference was a consumer ~p that shutdown, removing channel ~p", [FromPid, C]),
	    erlang:demonitor(FromRef, [flush]),
	    erlang:demonitor(CRef, [flush]),
	    erlang:is_process_alive(C) andalso amqp_channel:close(C),
	    dict:erase(FromPid, AccDict)
    end;

%% Simple catchall
clean_consumers(_, _, AccDict,_,_) ->
    AccDict.
