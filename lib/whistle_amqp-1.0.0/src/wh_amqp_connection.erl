%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle a host's connection/channels
%%% @end
%%% @contributions
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_amqp_connection).

-behaviour(gen_server).

-export([start_link/1]).
-export([publish/3]).
-export([consume/2]).
-export([misc_req/2]).
-export([my_channel/1]).
-export([update_my_tag/2]).
-export([fetch_my_tag/1]).
-export([stop/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(START_TIMEOUT, 500).
-define(MAX_TIMEOUT, 5000).
-define(KNOWN_EXCHANGES, [{?EXCHANGE_TARGETED, ?TYPE_TARGETED}
                          ,{?EXCHANGE_CALLCTL, ?TYPE_CALLCTL}
                          ,{?EXCHANGE_CALLEVT, ?TYPE_CALLEVT}
                          ,{?EXCHANGE_CALLMGR, ?TYPE_CALLMGR}
                          ,{?EXCHANGE_MONITOR, ?TYPE_MONITOR}
                          ,{?EXCHANGE_RESOURCE, ?TYPE_RESOURCE}
                          ,{?EXCHANGE_CONFIGURATION, ?TYPE_CONFIGURATION}
                          ,{?EXCHANGE_CONFERENCE, ?TYPE_CONFERENCE}
                          ,{?EXCHANGE_WHAPPS, ?TYPE_WHAPPS}
                          ,{?EXCHANGE_SYSCONF, ?TYPE_SYSCONF}
                         ]).

%% Channel, ChannelRef
%% Channel, ChannelRef, Tag, FromRef
-type channel_data() :: {pid(), reference()}.
-type consumer_data() :: {pid(), reference(), binary(), reference()}.

-type consume_records() :: #'queue.declare'{} | #'queue.bind'{} | #'queue.unbind'{} | #'queue.delete'{} |
                           #'basic.consume'{} | #'basic.cancel'{} | #'basic.ack'{} | #'basic.nack'{} |
                           #'basic.qos'{}.
-type misc_records() :: #'exchange.declare'{}.


-export_type([consume_records/0, misc_records/0]).

-record(state, {connection = 'undefined' :: 'undefined' | {pid(), reference()}
                ,publish_channel = 'undefined' :: 'undefined' | channel_data()
                ,misc_channel = 'undefined' :: 'undefined' | channel_data()
                ,consumers = dict:new() :: dict()
                ,broker = 'undefined' :: 'undefined' | wh_amqp_broker:broker()
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
start_link(Broker) ->
    Name = wh_amqp_broker:name(Broker),
    gen_server:start_link({local, Name}, ?MODULE, [Broker], []).

-spec publish/3 :: (pid(), #'basic.publish'{}, ne_binary() | iolist()) -> 'ok'.
publish(Srv, #'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    {ok, Channel} = case my_channel(Srv, false) of
                        {error, _} -> gen_server:call(Srv, publish_channel);
                        {ok, _}=Ok -> Ok
                    end,
    lager:debug("publish to exchange '~s' with routing key '~s' via channel ~p", [_Exchange, _RK, Channel]),
    amqp_channel:call(Channel, BasicPub, AmqpMsg).

-spec consume/2 :: (pid(), consume_records()) -> 'ok' | {'error', term()}.
consume(Srv, #'basic.consume'{consumer_tag=CTag}=BasicConsume) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            case fetch_my_tag(Srv) of
                {ok, CTag} -> ok;
                _Else -> 
                    try_to_subscribe(Srv, Channel, BasicConsume)
            end
    end;
consume(Srv, #'basic.cancel'{}=BasicCancel) ->
    case fetch_my_tag(Srv) of
        {error, _}=E -> E;
        {ok, Tag} ->
            {ok, Channel} = my_channel(Srv),
            amqp_channel:call(Channel, BasicCancel#'basic.cancel'{consumer_tag=Tag})
    end;
consume(Srv, #'queue.bind'{}=QueueBind) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            case amqp_channel:call(Channel, QueueBind) of
                #'queue.bind_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'queue.unbind'{}=QueueUnbind) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            case amqp_channel:call(Channel, QueueUnbind) of
                #'queue.unbind_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'queue.declare'{}=QueueDeclare) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            case amqp_channel:call(Channel, QueueDeclare) of
                #'queue.declare_ok'{queue=Q} -> {ok, Q};
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'queue.delete'{}=QueueDelete) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            case amqp_channel:call(Channel, QueueDelete) of
                #'queue.delete_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'basic.qos'{}=BasicQos) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
%%% TODO.... FIX ME            
            amqp_channel:call(Channel, BasicQos),
            ok
    end;
consume(Srv, #'basic.ack'{}=BasicAck) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            amqp_channel:cast(Channel, BasicAck)
    end;
consume(Srv, #'basic.nack'{}=BasicAck) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
            amqp_channel:cast(Channel, BasicAck)
    end;
consume(Srv, #'exchange.declare'{}=BasicAck) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel} ->
%%% TODO.... FEDERATION STUFF...
            case amqp_channel:call(Channel, BasicAck) of
                #'exchange.declare_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end.

-spec misc_req/2 :: (pid(), misc_records()) -> 'ok'.
misc_req(Srv,  #'exchange.declare'{exchange=_Ex, type=_Ty}=ExchangeDeclare) ->
    {ok, Channel} = gen_server:call(Srv, misc_channel),
    case os:getenv("AMQP_DEBUG") of
        false -> ok;
        _Else ->
            lager:debug("attempting to create new ~s exchange '~s' via channel ~p", [_Ty, _Ex, Channel])
    end,
    case amqp_channel:call(Channel, ExchangeDeclare) of
        #'exchange.declare_ok'{} -> ok;
        {error, _}=E -> E;
        Err -> {error, Err}
    end.

-spec my_channel/1 :: (pid()) -> {'ok', pid()} |
                                 {'error', term()}.
-spec my_channel/2 :: (pid(), boolean) -> {'ok', pid()} |
                                          {'error', term()}.
my_channel(Srv) ->
    my_channel(Srv, true).

my_channel(Srv, Create) ->
    gen_server:call(Srv, {my_channel, self(), Create}).

-spec update_my_tag/2 :: (pid(), ne_binary()) -> 'ok'.
update_my_tag(Srv, Tag) ->
    gen_server:cast(Srv, {update_my_tag, self(), Tag}).

-spec fetch_my_tag/1 :: (pid()) -> {'ok', binary()} |
                                   {'error', 'not_consuming'}.
fetch_my_tag(Srv) ->
    gen_server:call(Srv, {fetch_my_tag, self()}).

-spec stop/1 :: (pid()) -> 'ok'.
stop(Srv) ->
    case erlang:is_process_alive(Srv) of
        false -> ok;
        true -> gen_server:cast(Srv, stop)
    end.

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
init([Broker]) ->
    process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    self() ! {connect, ?START_TIMEOUT},
    {ok, #state{broker=Broker}}.

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
handle_call(_, _, #state{connection=undefined}=State) ->
    %% If we are diconnected dont pay attention to requests
    {reply, {error, amqp_down}, State};
handle_call(publish_channel, _, #state{publish_channel={C,_}}=State) ->
    {reply, {ok, C}, State};
handle_call(misc_channel, _, #state{misc_channel={C,_}}=State) ->
    {reply, {ok, C}, State};
handle_call({my_channel, OwnerPid, Create}, _, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(OwnerPid, Consumers) of
        error when not Create->
            {reply, {error, not_found}, State};
        error ->
            case start_channel(Conn) of
                {C,R} when is_pid(C) andalso is_reference(R) -> % channel, channel ref
                    lager:debug("started new AMQP channel ~p for process ~p", [C, OwnerPid]),
                    FromRef = erlang:monitor(process, OwnerPid),
                    amqp_selective_consumer:register_default_consumer(C, self()),
                    {reply
                     ,{ok, C}
                     ,State#state{consumers=dict:store(OwnerPid, {C,R,<<>>,FromRef}, Consumers)}
                     ,hibernate
                    };
                closing ->
                    lager:debug("failed to start channel for ~p: closing", [OwnerPid]),
                    {reply, {error, closing}, State};
                {error, _}=E ->
                    lager:debug("failed to start new channel for ~p: ~p", [OwnerPid, E]),
                    {reply, E, State}
            end;
        {ok, {C,_,_,_}} ->
            {reply, {ok, C}, State}
    end;
handle_call({fetch_my_tag, OwnerPid}, _, #state{consumers=Consumers}=State) ->
    case dict:find(OwnerPid, Consumers) of
        error -> {reply, {error, not_consuming}, State};
        {ok, {_C, _R, T, _FromRef}} -> {reply, {ok, T}, State}
    end;
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.
    
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
handle_cast(_, #state{connection=undefined}=State) ->
    %% If we are diconnected dont pay attention to requests
    {noreply, State};
handle_cast({update_my_tag, FromPid, Tag}, #state{consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error -> {noreply, State};
        {ok, {C, R, _T, FromRef}} ->
            lager:debug("updating tag for ~p from ~p to ~p", [FromPid, _T, Tag]),
            {noreply, State#state{consumers=dict:store(FromPid, {C, R, Tag, FromRef}, Consumers)}, hibernate}
    end;
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
handle_info({'DOWN', Ref, process, ConnPid, Reason}, #state{connection={ConnPid, Ref}, consumers=Consumers, broker=Broker}=State) ->
    lager:info("connection to the AMQP broker died: ~p", [Reason]),
    Name = wh_amqp_broker:name(Broker),
    gen_server:cast(wh_amqp_mgr, {broker_unavailable, Name}),
    notify_consumers({amqp_channel_event, Reason}, Consumers),
    self() ! {connect, ?START_TIMEOUT},
    {noreply, State#state{connection=undefined}};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    lager:debug("recieved notification monitored process ~p  died ~p, searching for reference", [_Pid, _Reason]),
    erlang:demonitor(Ref, [flush]),
    {noreply, remove_ref(Ref, State), hibernate};
handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, State) ->
    wh_amqp_mgr:notify_return_handlers(ReturnMsg),
    {noreply, State};
handle_info({connect, Timeout}, #state{broker=Broker}=State) ->
    Params = wh_amqp_broker:params(Broker),
    Name = wh_amqp_broker:name(Broker),
    case amqp_connection:start(Params) of
        {error, _Reason} -> 
            lager:debug("failed to connect to AMQP broker '~s' will retry in ~p: ~p", [Name, Timeout, _Reason]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, State#state{connection=undefined}};
        {ok, Connection} ->
            Ref = erlang:monitor(process, Connection),
            case start_channel(Connection) of
                {Channel, _} = PubChan when is_pid(Channel) ->
                    amqp_channel:register_return_handler(Channel, self()),
                    lager:info("connected to AMQP broker '~s'", [Name]),
                    gen_server:cast(wh_amqp_mgr, {broker_available, Name}),
                    {noreply, #state{connection = {Connection, Ref}
                                     ,publish_channel = PubChan
                                     ,misc_channel = start_channel(Connection)
                                     ,consumers = dict:new()
                                     ,broker = Broker
                                    }, hibernate};
                {error, _Cause} ->
                    lager:debug("unable to initialize publish channel on AMQP broker '~s' will retry in ~p: ~p", [Name, Timeout, _Cause]),
                    erlang:demonitor(Ref, [flush]),
                    _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
                    {noreply, State#state{connection=undefined}}
            end
    end;
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
-spec terminate/2 :: (term(), #state{}) -> 'ok'.
terminate(_Reason, {_H, _Conn, _UseF}) ->
    lager:debug("amqp host failed to startup: ~p", [_Reason]),
    lager:debug("params: ~s on conn ~p, use federation: ~s", [_H, _Conn, _UseF]);
terminate(_Reason, #state{consumers=Consumers, broker=Broker}) ->
    Name = wh_amqp_broker:name(Broker),
    spawn(fun() ->
                  put(callid, ?LOG_SYSTEM_ID),
                  notify_consumers({amqp_channel_event, terminated}, Consumers)
          end),
    lager:debug("connection to AMQP broker '~s' terminated: ~p", [Name, _Reason]).

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
-spec start_channel/1 :: ('undefined' | {pid(), reference()} | pid()) -> channel_data() | {'error', 'no_connection'} | 'closing'.
start_channel(undefined) ->
    {error, no_connection};
start_channel({Connection, _}) ->
    start_channel(Connection);
start_channel(Connection) when is_pid(Connection) ->
    case erlang:is_process_alive(Connection) 
        andalso amqp_connection:open_channel(Connection) of
        {ok, Channel} ->
            ChanMRef = erlang:monitor(process, Channel),
            {Channel, ChanMRef};
        false -> {error, no_connection};
        E -> E
    end.

-spec remove_ref/2 :: (reference(), #state{}) -> #state{}.
remove_ref(Ref, #state{connection=undefined, publish_channel={C,Ref}}=State) ->
    lager:debug("reference was for publish channel ~p due to AMQP disconnect", [C]),
    State#state{publish_channel=undefined};
remove_ref(Ref, #state{connection=undefined, misc_channel={C,Ref}}=State) ->
    lager:debug("reference was for misc channel ~p due to AMQP disconnect", [C]),
    State#state{publish_channel=undefined};
remove_ref(Ref, #state{connection={Conn, _}, publish_channel={C,Ref}}=State) ->
    lager:debug("reference was for publish channel ~p, restarting", [C]),
    State#state{publish_channel=start_channel(Conn)};
remove_ref(Ref, #state{connection={Conn, _}, misc_channel={C,Ref}}=State) ->
    lager:debug("reference was for misc channel ~p, restarting", [C]),
    State#state{misc_channel=start_channel(Conn)};
remove_ref(Ref, #state{connection=Connection, consumers=Cs}=State) ->
    State#state{consumers=dict:fold(fun(K, V, Acc) -> 
                                            clean_consumers(K, V, Acc, Ref, Connection)
                                    end, Cs, Cs)}.

-spec notify_consumers/2 :: ({'amqp_channel_event', binary()}, dict()) -> 'ok'.
notify_consumers(Msg, Dict) ->
    lists:foreach(fun({Pid,_}) -> Pid ! Msg end, dict:to_list(Dict)).

-spec clean_consumers/5 :: (pid(), consumer_data(), dict(), reference(), 'undefined' | pid()) -> dict().
%% Channel died while disconnected
clean_consumers(ConsumerPid, {C, ChannelRef, _, ConsumerRef}, Dict, ChannelRef, undefined) ->
    lager:debug("reference was for channel ~p for ~p due to AMQP disconnect", [C, ConsumerPid]),
    erlang:demonitor(ChannelRef, [flush]),
    erlang:demonitor(ConsumerRef, [flush]),
    ConsumerPid ! {amqp_channel_event, lost_connection},
    dict:erase(ConsumerPid, Dict);

%% Channel died while connected
clean_consumers(ConsumerPid, {C, ChannelRef, _, ConsumerRef}=Consumer, Dict, ChannelRef, {Conn, _}) ->
    case is_process_alive(Conn) of
        false -> clean_consumers(ConsumerPid, Consumer, Dict, ChannelRef, undefined);
        true ->
            lager:debug("reference was for channel ~p for ~p, restarting", [C, ConsumerPid]),
            erlang:demonitor(ChannelRef, [flush]),
            erlang:is_process_alive(C) andalso amqp_channel:close(C),
            case start_channel(Conn) of
                {CNew, RefNew} when is_pid(CNew) andalso is_reference(RefNew) ->
                    lager:debug("new channel started for ~p", [ConsumerPid]),
                    ConsumerPid ! {amqp_channel_event, restarted},
                    dict:store(ConsumerPid, {CNew, RefNew, <<>>, ConsumerRef}, Dict);
                {error, no_connection} ->
                    lager:debug("no connection available"),
                    ConsumerPid ! {amqp_channel_event, no_connection},
                    dict:erase(ConsumerPid, Dict);
                closing ->
                    lager:debug("closing, no connection"),
                    ConsumerPid ! {amqp_channel_event, closing},
                    dict:erase(ConsumerPid, Dict)
            end
    end;

%% Consumer died
clean_consumers(ConsumerPid, {C, ChannelRef, _, ConsumerRef}, Dict, ConsumerRef, _) ->
    lager:debug("reference was for consumer ~p, removing channel ~p", [ConsumerPid, C]),
    erlang:demonitor(ChannelRef, [flush]),
    erlang:demonitor(ConsumerRef, [flush]),
    erlang:is_process_alive(C) andalso amqp_channel:close(C),
    dict:erase(ConsumerPid, Dict);

%% Generic channel cleanup when ConsumerPid isn't alive
clean_consumers(ConsumerPid, {C, ChannelRef, _, ConsumerRef}, Dict, _, _) ->
    case erlang:is_process_alive(ConsumerPid) of
        true -> Dict;
        false ->
            lager:debug("reference was a consumer ~p that shutdown, removing channel ~p", [ConsumerPid, C]),
            erlang:demonitor(ChannelRef, [flush]),
            erlang:demonitor(ConsumerRef, [flush]),
            erlang:is_process_alive(C) andalso amqp_channel:close(C),
            dict:erase(ConsumerPid, Dict)
    end;

%% Simple catchall
clean_consumers(_, _, Dict,_,_) ->
    Dict.

-spec try_to_subscribe/3 :: (pid(), pid(), #'basic.consume'{}) -> {'ok', term()} | {'error', term()}.
try_to_subscribe(Srv, Channel, BasicConsume) ->
    try amqp_channel:call(Channel, BasicConsume) of
        #'basic.consume_ok'{consumer_tag=Tag} -> 
            update_my_tag(Srv, Tag),
            ok;
        Other -> {error, Other}
    catch
        _E:R -> {error, R}
    end.

-spec next_timeout/1 :: (pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT=Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

exchange_declare(ED, false) ->
    ED;
exchange_declare(#'exchange.declare'{type=Type}=ED, true) ->
%    -record('exchange.declare', {ticket = 0, exchange, type = <<"direct">>, passive = false, durable = false, auto_delete = false, internal = false, nowait = false, arguments = []}).
    ED1 = ED#'exchange.declare'{
       type = <<"x-federation">>
      ,arguments = [{<<"type">>, longstr, Type}
                    ,{<<"upstream-set">>, longstr, ?RABBITMQ_UPSTREAM_SET}
                   ]
     },
    ED1.
