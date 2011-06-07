%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Handle a host's connection/channels
%%% @end
%%% Created : 18 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(amqp_host).

-behaviour(gen_server).

%% API
-export([start_link/2, publish/4, consume/3, get_misc_channel/2, misc_req/3, misc_req/4, stop/1]).

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
          ,manager = undefined :: undefined | pid()
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

publish(Srv, From, BasicPub, AmqpMsg) ->
    gen_server:cast(Srv, {publish, From, BasicPub, AmqpMsg}).

-spec(consume/3 :: (Srv :: pid(), From :: tuple(pid(), reference()), Msg :: consume_records()) -> no_return()).
consume(Srv, From, Msg) ->
    gen_server:cast(Srv, {consume, From, Msg}).

% {ok, Channel, Ticket} -> backwards compat
get_misc_channel(Srv, From) ->
    gen_server:cast(Srv, {get_misc_channel, From}).

misc_req(Srv, From, Req) ->
    gen_server:cast(Srv, {misc_req, From, Req}).

misc_req(Srv, From, Req1, Req2) ->
    gen_server:cast(Srv, {misc_req, From, Req1, Req2}).

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
init([_Host, Conn]) when is_pid(Conn) ->
    {ok, Conn, 0}.

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
handle_cast({get_misc_channel, From}, #state{misc_channel={C,_,T}}=State) ->
    gen_server:reply(From, {ok, C, T}),
    {noreply, State};

handle_cast({publish, From, BasicPub, AmqpMsg}, #state{publish_channel={C,_,T}}=State) ->
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, BasicPub#'basic.publish'{ticket=T}, AmqpMsg)) end),
    {noreply, State};

handle_cast({consume, {FromPid, _}=From, #'basic.consume'{}=BasicConsume}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} -> % channel, channel ref, ticket
		    FromRef = erlang:monitor(process, FromPid),

		    gen_server:reply(From, amqp_channel:subscribe(C, BasicConsume#'basic.consume'{ticket=T}, FromPid)),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    gen_server:reply(From, amqp_channel:subscribe(C, BasicConsume#'basic.consume'{ticket=T}, FromPid)),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.cancel'{}=BasicCancel}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),

		    gen_server:reply(From, amqp_channel:cast(C, BasicCancel, FromPid)),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
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
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
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

		    gen_server:reply(From, amqp_channel:cast(C, QueueUnbind#'queue.unbind'{ticket=T})),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,T,_}} ->
	    gen_server:reply(From, amqp_channel:cast(C, QueueUnbind#'queue.unbind'{ticket=T})),
	    {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.declare'{}=QueueDeclare}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
	error ->
	    case start_channel(Conn, FromPid) of
		{C,R,T} ->
		    FromRef = erlang:monitor(process, FromPid),
		    Call = amqp_channel:call(C, QueueDeclare#'queue.declare'{ticket=T}),
		    gen_server:reply(From, Call),
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
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
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
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
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
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
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
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
		    {noreply, State#state{consumers=dict:store(FromPid, {C,R,T,FromRef}, Consumers)}};
		{error, _}=E ->
		    gen_server:reply(From, E),
		    {noreply, State}
	    end;
	{ok, {C,_,_,_}} ->
	    gen_server:reply(From, amqp_channel:cast(C, BasicNack)),
	    {noreply, State}
    end;

handle_cast({misc_req, From, #'exchange.declare'{}=ED}, #state{misc_channel={C,_,T}}=State) ->
    gen_server:reply(From, amqp_channel:call(C, ED#'exchange.declare'{ticket=T})),
    {noreply, State};

handle_cast({misc_req, From, Req}, #state{misc_channel={C,_,_}}=State) ->
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, Req)) end),
    {noreply, State};

handle_cast({misc_req, From, Req1, Req2}, #state{misc_channel={C,_,_}}=State) ->
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, Req1, Req2)) end),
    {noreply, State};

handle_cast(_Msg, State) ->
    logger:format_log(info, "AMQP_HOST(~p): Unmatched cast: ~p~nState: ~p~n", [self(), _Msg, State]),
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
handle_info(timeout, Conn) ->
    Ref = erlang:monitor(process, Conn),
    case start_channel(Conn) of
	{Channel, _, Ticket} = PubChan ->
	    load_exchanges(Channel, Ticket),
	    {noreply, #state{
	       connection = {Conn, Ref}
	       ,publish_channel = PubChan
	       ,misc_channel = start_channel(Conn)
	       ,consumers = dict:new()
	       ,manager = whereis(amqp_manager)
	      }
	    };
	{error, E} ->
	    logger:format_log(error, "AMQP_HOST(~p): Error starting pub channel: ~p~n", [self(), E]),
	    erlang:demonitor(Ref, [flush]),
	    {stop, E, Conn}
    end;

handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{}=State) ->
    logger:format_log(error, "AMQP_HOST(~p): Pid ~p down: ~p~n", [self(), _Pid, _Reason]),
    erlang:demonitor(Ref, [flush]),
    {noreply, remove_ref(Ref, State)};

handle_info(_Info, State) ->
    logger:format_log(info, "AMQP_HOST(~p): unhandled info: ~p", [self(), _Info]),
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
-spec(start_channel/1 :: (Connection :: undefined | tuple(pid(), reference()) | pid()) -> channel_data() | tuple(error, no_connection) | closing).
start_channel(undefined) ->
    {error, no_connection};
start_channel({Connection, _}) ->
    start_channel(Connection);
start_channel(Connection) when is_pid(Connection) ->
    %% Open an AMQP channel to access our realm
    case amqp_connection:open_channel(Connection) of
	{ok, Channel} ->
	    #'access.request_ok'{ticket = Ticket} = amqp_channel:call(Channel, amqp_util:access_request()),

	    ChanMRef = erlang:monitor(process, Channel),
	    {Channel, ChanMRef, Ticket};
	E -> E
    end.

-spec(start_channel/2 :: (Connection :: undefined | tuple(pid(), reference()) | pid(), Pid :: pid()) -> channel_data() | tuple(error, no_connection) | closing).
start_channel(Connection, Pid) ->
    case start_channel(Connection) of
	{C, _, T} = Channel ->
	    amqp_channel:register_return_handler(C, Pid),
	    #'access.request_ok'{ticket=T} = amqp_channel:call(C, amqp_util:access_request()),
	    Channel;
	E -> E
    end.

load_exchanges(Channel, Ticket) ->
    lists:foreach(fun({Ex, Type}) ->
			  ED = #'exchange.declare'{
			    ticket = Ticket
			    ,exchange = Ex
			    ,type = Type
			   },
			  #'exchange.declare_ok'{} = amqp_channel:call(Channel, ED)
		  end, ?KNOWN_EXCHANGES).

-spec(remove_ref/2 :: (Ref :: reference(), State :: #state{}) -> #state{}).
remove_ref(Ref, #state{connection={Conn, _}, publish_channel={C,Ref,_}}=State) ->
    logger:format_log(info, "AMQP_HOST(~p): publish_channel(~p) went down~n", [self(), C]),
    State#state{publish_channel=start_channel(Conn)};

remove_ref(Ref, #state{connection={Conn, _}, misc_channel={C,Ref,_}}=State) ->
    logger:format_log(info, "AMQP_HOST(~p): misc_channel(~p) went down~n", [self(), C]),
    State#state{misc_channel=start_channel(Conn)};

remove_ref(Ref, #state{connection={Conn, _}, consumers=Cs}=State) ->
    logger:format_log(info, "AMQP_HOST(~p): consumer went down, searching~n", [self()]),
    State#state{consumers =
		    dict:fold(fun(FromPid, {C,Ref1,_,FromRef}, AccDict) when Ref =:= Ref1 ->
				      logger:format_log(info, "AMQP_HOST(~p): consumer_channel(~p) went down for ~p~n", [self(), C, FromPid]),
				      case start_channel(Conn, FromPid) of
					  {CNew, RefNew, TNew} -> dict:store(FromPid, {CNew, RefNew, TNew, FromRef}, AccDict);
					  {error, no_connection} ->
					      FromPid ! {amqp_lost_channel, no_connection},
					      dict:erase(FromPid, AccDict);
					  closing ->
					      FromPid ! {amqp_lost_channel, no_connection},
					      dict:erase(FromPid, AccDict)
				      end;

				 (FromPid, {C,CRef,_,FromRef}, AccDict) when Ref =:= FromRef ->
				      logger:format_log(info, "AMQP_HOST(~p): consumer(~p) went down: channel(~p) goes down too~n", [self(), FromPid, C]),
				      erlang:demonitor(CRef, [flush]),
				      amqp_channel:close(C),
				      dict:erase(FromPid, AccDict);

				 (FromPid, {C,CRef,_,FromRef}, AccDict) ->
				      case erlang:is_process_alive(FromPid) of
					  true -> AccDict;
					  false ->
					      logger:format_log(info, "AMQP_HOST(~p): consumer(~p) went down unknowingly: channel(~p) goes down too~n", [self(), FromPid, C]),
					      erlang:demonitor(FromRef, [flush]),
					      erlang:demonitor(CRef, [flush]),
					      amqp_channel:close(C),
					      dict:erase(FromPid, AccDict)
				      end;

				 (_, _, AccDict) ->
				      AccDict
			      end, Cs, Cs)
	       }.
