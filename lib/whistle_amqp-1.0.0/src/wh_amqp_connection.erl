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

-export([start_link/1
         ,publish/3
         ,consume/2
         ,misc_req/2
         ,my_channel/1
         ,update_my_tag/2
         ,use_federation/1
         ,stop/1
         ,teardown_channels/1
        ]).

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

-type consume_records() :: #'queue.declare'{} | #'queue.bind'{} | #'queue.unbind'{} | #'queue.delete'{} |
                           #'basic.consume'{} | #'basic.cancel'{} | #'basic.ack'{} | #'basic.nack'{} |
                           #'basic.qos'{}.
-type consume_ret() :: 'ok' |
                       {'ok', ne_binary() | #'queue.declare_ok'{}} |
                       {'error', _}.
-type misc_records() :: #'exchange.declare'{}.


-export_type([consume_records/0
              ,misc_records/0
              ,consume_ret/0
             ]).

-record(state, {connection = 'undefined' :: 'undefined' | {pid(), reference()}
                ,broker = 'undefined' :: 'undefined' | wh_amqp_broker:broker()
                ,broker_name = 'undefined' :: atom()
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

teardown_channels(Broker) ->
    gen_server:call(wh_amqp_broker:name(Broker), teardown_channels).

-spec publish/3 :: (atom(), #'basic.publish'{}, ne_binary() | iolist()) -> 'ok' | {'error', _}.
publish(Srv, #'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    FindChannel = [fun(Pid) when is_pid(Pid) -> my_channel(Srv, Pid, false);
                      (_) -> {error, not_found}
                   end
                   ,fun({error, _}) -> my_channel(Srv, false);
                       ({ok, _, _}=Ok) -> Ok
                    end
                   ,fun({error, _}) -> publish_channel(Srv);
                       ({ok, C, _}) -> {ok, C}
                    end
                  ],
    case lists:foldl(fun(F, C) -> F(C) end, get(amqp_publish_as), FindChannel) of
        {error, _}=E -> E;
        {ok, Channel} ->
            lager:debug("publish: broker '~s' exchange '~s' routing key '~s' channel '~p'", [Srv, _Exchange, _RK, Channel]),
            amqp_channel:call(Channel, BasicPub, AmqpMsg),
            ok
    end.

-spec consume/2 :: (atom(), consume_records()) -> consume_ret().
consume(Srv, #'basic.consume'{consumer_tag=CTag}=BasicConsume) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, _, CTag} -> ok;
        {ok, Channel, _} ->
            try_to_subscribe(Srv, Channel, BasicConsume)
    end;
consume(Srv, #'basic.cancel'{}=BasicCancel) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, _, <<>>} -> {error, not_consuming};
        {ok, Channel, Tag} ->
            amqp_channel:call(Channel, BasicCancel#'basic.cancel'{consumer_tag=Tag})
    end;
consume(Srv, #'queue.bind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=QueueBind) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            lager:debug("bind: broker '~s' exchange '~s' routing key '~s' queue '~s'", [Srv, _Exchange, _RK, _Q]),
            case amqp_channel:call(Channel, QueueBind) of
                #'queue.bind_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'queue.unbind'{}=QueueUnbind) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            case amqp_channel:call(Channel, QueueUnbind) of
                #'queue.unbind_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'queue.declare'{}=QueueDeclare) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            case amqp_channel:call(Channel, QueueDeclare) of
                {ok, DeclareOK}=OK when is_record(DeclareOK, 'queue.declare_ok') -> OK;
                {error, _}=E -> E;
                DeclareOK when is_record(DeclareOK, 'queue.declare_ok') -> {ok, DeclareOK};
                Err -> {error, Err}
            end
    end;
consume(Srv, #'queue.delete'{}=QueueDelete) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            case amqp_channel:call(Channel, QueueDelete) of
                #'queue.delete_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'basic.qos'{}=BasicQos) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            case amqp_channel:call(Channel, BasicQos) of
                #'basic.qos_ok'{} -> ok;
                {error, _}=E -> E;
                Err -> {error, Err}
            end
    end;
consume(Srv, #'basic.ack'{}=BasicAck) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            amqp_channel:cast(Channel, BasicAck)
    end;
consume(Srv, #'basic.nack'{}=BasicNack) ->
    case my_channel(Srv) of
        {error, _}=E -> E;
        {ok, Channel, _} ->
            amqp_channel:cast(Channel, BasicNack)
    end.

-spec misc_req/2 :: (atom(), misc_records()) -> 'ok'.
misc_req(Srv,  #'exchange.declare'{exchange=_Ex, type=_Ty}=ExchangeDeclare) ->
    {ok, Channel} = misc_channel(Srv),
    lager:debug("attempting to create new ~s exchange '~s' via channel ~p", [_Ty, _Ex, Channel]),
    UseFederation = use_federation(Srv),
    case amqp_channel:call(Channel, exchange_declare(ExchangeDeclare, UseFederation)) of
        #'exchange.declare_ok'{} -> ok;
        {error, _}=E -> E;
        Err -> {error, Err}
    end.

-spec my_channel/1 :: (atom()) -> {'ok', pid(), 'undefined' | ne_binary()} |
                                  {'error', term()}.
-spec my_channel/2 :: (atom(), boolean()) -> {'ok', pid(), 'undefined' | ne_binary()} |
                                             {'error', term()}.
my_channel(Srv) ->
    my_channel(Srv, true).

my_channel(Srv, Create) ->
    my_channel(Srv, self(), Create).

my_channel(Srv, Consumer, Create) ->
    case ets:lookup(Srv, Consumer) of
        [#wh_amqp_channel{channel=C, tag=T}] -> {ok, C, T};
        [] when Create ->
            case create_channel(Srv, Consumer) of
                {ok, C} -> {ok, C, <<>>};
                {error, _}=E -> E
            end;
        [] -> {error, not_found}
    end.

-spec publish_channel/1 :: (atom()) -> {'ok', pid()} |
                                       {'error', 'not_found'}.
publish_channel(Srv) ->
    case ets:lookup(Srv, publish) of
        [#wh_amqp_channel{channel=C}] -> {ok, C};
        [] -> {error, not_found}
    end.

-spec misc_channel/1 :: (atom()) -> {'ok', pid()} |
                                    {'error', 'not_found'}.
misc_channel(Srv) ->
    case ets:lookup(Srv, misc) of
        [#wh_amqp_channel{channel=C}] -> {ok, C};
        [] -> {error, not_found}
    end.

-spec create_channel/2 :: (atom(), pid()) -> {'ok', pid()} |
                                             {'error', _}.
create_channel(Srv, Consumer) ->
    case gen_server:call(Srv, {get_connection}) of
        {error, _}=E -> E;
        {ok, Conn} ->
            lager:debug("starting new AMQP channel for process ~p", [Consumer]),
            case start_channel(Conn, whereis(Srv)) of
                {error, _}=E -> E;
                closing ->
                    lager:debug("failed to start channel for ~p: closing", [Consumer]),
                    {error, closing};
                #wh_amqp_channel{channel=C}=Channel ->
                    gen_server:call(Srv, {register_channel, Channel, Consumer}),
                    {ok, C}
            end
    end.

-spec update_my_tag/2 :: (atom(), ne_binary()) -> 'ok'.
update_my_tag(Srv, Tag) ->
    gen_server:cast(Srv, {update_my_tag, self(), Tag}).

-spec use_federation/1 :: (atom()) -> boolean().
use_federation(Srv) ->
    gen_server:call(Srv, use_federation).

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

    self() ! {connect, ?START_TIMEOUT},
    Name = wh_amqp_broker:name(Broker),

    _ = put(callid, wh_amqp_broker:host(Broker)),

    _ = ets:new(Name, [set, protected, named_table, {keypos, #wh_amqp_channel.consumer}]),
    {ok, #state{broker=Broker, broker_name=Name}}.

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
handle_call(teardown_channels, _, #state{broker_name=Name}=State) ->
    _ = clear_channels(Name),
    {reply, ok, State};

handle_call(use_federation, _, #state{broker=Broker}=State) ->
    %% If we are diconnected dont pay attention to requests
    {reply, wh_amqp_broker:use_federation(Broker), State};
handle_call(_, _, #state{connection=undefined}=State) ->
    %% If we are diconnected dont pay attention to requests
    {reply, {error, amqp_down}, State};
handle_call({get_connection}, _, #state{connection=Conn}=State) ->
    {reply, {ok, Conn}, State};
handle_call({register_channel, #wh_amqp_channel{channel=C}=Channel, Consumer}, _
            ,#state{broker_name=Name}=State) ->
    ets:insert(Name, Channel#wh_amqp_channel{consumer = Consumer
                                             ,consumer_ref = erlang:monitor(process, Consumer)
                                             ,channel_ref = erlang:monitor(process, C)
                                            }),
    {reply, ok, State};
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
handle_cast({update_my_tag, Consumer, Tag}, #state{broker_name=Name}=State) ->
    ets:update_element(Name, Consumer, {#wh_amqp_channel.tag, Tag}),
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
handle_info({'DOWN', Ref, process, ConnPid, Reason}, #state{connection={ConnPid, Ref}, broker_name=Name}=State) ->
    lager:info("connection to the AMQP broker died: ~p", [Reason]),
    gen_server:cast(wh_amqp_mgr, {broker_unavailable, Name}),
    notify_consumers({amqp_channel_event, Reason}, Name),
    self() ! {connect, ?START_TIMEOUT},
    {noreply, State#state{connection=undefined}};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{connection={Connection, _}, broker_name=Name}=State) ->
    erlang:demonitor(Ref, [flush]),
    MatchSpec = [{#wh_amqp_channel{channel_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{channel, '$_'}}]},
                 {#wh_amqp_channel{consumer_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{consumer, '$_'}}]}
                ],
    case ets:select(Name, MatchSpec) of
        [{channel, #wh_amqp_channel{consumer=Consumer, consumer_ref=OtherRef}}] ->
            case start_channel(Connection) of
                #wh_amqp_channel{channel=C, channel_ref=CRef} ->
                    lager:debug("channel ~p for ~p died, restarted", [_Pid, Consumer]),
                    is_pid(Consumer) andalso (Consumer ! {amqp_channel_event, restarted}),
                    ets:update_element(Name, Consumer, [{#wh_amqp_channel.channel, C}
                                                        ,{#wh_amqp_channel.channel_ref, CRef}
                                                       ]);
                {error, no_connection} ->
                    lager:debug("channel ~p for ~p died, connection is no longer available", [_Pid, Consumer]),
                    is_pid(Consumer) andalso (Consumer ! {amqp_channel_event, no_connection}),
                    is_reference(OtherRef) andalso erlang:demonitor(OtherRef, [flush]),
                    ets:delete(Name, Consumer);
                closing ->
                    lager:debug("channel ~p for ~p died, connection is closing", [_Pid, Consumer]),
                    is_pid(Consumer) andalso (Consumer ! {amqp_channel_event, closing}),
                    is_reference(OtherRef) andalso erlang:demonitor(OtherRef, [flush]),
                    ets:delete(Name, Consumer)
            end;
        [{consumer, #wh_amqp_channel{consumer=Consumer, channel=C, channel_ref=OtherRef}}] ->
            erlang:is_process_alive(C) andalso amqp_channel:close(C),
            is_reference(OtherRef) andalso erlang:demonitor(OtherRef, [flush]),
            ets:delete(Name, Consumer);
        [] -> ok
    end,
    {noreply, State, hibernate};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{broker_name=Name}=State) ->
    erlang:demonitor(Ref, [flush]),
    MatchSpec = [{#wh_amqp_channel{channel_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{channel, '$_'}}]},
                 {#wh_amqp_channel{consumer_ref = '$1', _ = '_'},
                  [{'=:=', '$1', {const, Ref}}],
                  [{{consumer, '$_'}}]}
                ],
    case ets:select(Name, MatchSpec) of
        [{channel, #wh_amqp_channel{consumer=Consumer, consumer_ref=OtherRef}}] ->
            lager:debug("channel for ~p went down during AMQP disconnect", [Consumer]),
            is_pid(Consumer) andalso (Consumer ! {amqp_channel_event, lost_connection}),
            is_reference(OtherRef) andalso erlang:demonitor(OtherRef, [flush]),
            ets:delete(Name, Consumer);
        [{consumer, #wh_amqp_channel{consumer=Consumer, channel=C, channel_ref=OtherRef}}] ->
            lager:debug("consumer ~p went down during AMQP disconnect", [Consumer]),
            erlang:is_process_alive(C) andalso amqp_channel:close(C),
            is_reference(OtherRef) andalso erlang:demonitor(OtherRef, [flush]),
            ets:delete(Name, Consumer);
        [] -> ok
    end,
    {noreply, State, hibernate};
handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, State) ->
    wh_amqp_mgr:notify_return_handlers(ReturnMsg),
    {noreply, State};
handle_info({connect, Timeout}, #state{broker=Broker, broker_name=Name}=State) ->
    Params = wh_amqp_broker:params(Broker),
    case amqp_connection:start(Params) of
        {error, auth_failure} ->
            lager:debug("authentication failure coonection to broker '~s',  will retry in ~p", [Name, Timeout]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, State#state{connection=undefined}};
        {error, _Reason} ->
            lager:debug("failed to connect to AMQP broker '~s' will retry in ~p: ~p", [Name, Timeout, _Reason]),
            _Ref = erlang:send_after(Timeout, self(), {connect, next_timeout(Timeout)}),
            {noreply, State#state{connection=undefined}};
        {ok, Connection} ->
            Ref = erlang:monitor(process, Connection),
            try
                #wh_amqp_channel{}=PublishChannel = start_channel(Connection),
                #wh_amqp_channel{}=MiscChannel = start_channel(Connection),
                ets:insert(Name, PublishChannel#wh_amqp_channel{consumer = publish}),
                ets:insert(Name, MiscChannel#wh_amqp_channel{consumer = misc}),
                lager:info("connected to AMQP broker '~s'", [Name]),
                gen_server:cast(wh_amqp_mgr, {broker_available, Name}),
                {noreply, State#state{connection = {Connection, Ref}}, hibernate}
            catch
                _:_ ->
                    lager:debug("unable to initialize publish channel on AMQP broker '~s' will retry in ~p", [Name, Timeout]),
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
terminate(_Reason, #state{broker_name=Name}) ->
    put(callid, ?LOG_SYSTEM_ID),
    notify_consumers({amqp_channel_event, terminated}, Name),
    ets:delete(Name),
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
-spec start_channel/1 :: ('undefined' | {pid(), reference()} | pid()) -> #wh_amqp_channel{} |
                                                                         {'error', 'no_connection'} |
                                                                         'closing'.
-spec start_channel/2 :: ('undefined' | {pid(), reference()} | pid(), pid()) -> #wh_amqp_channel{} |
                                                                                {'error', 'no_connection'} |
                                                                                'closing'.
start_channel(Connection) ->
    start_channel(Connection, self()).

start_channel(undefined, _) ->
    {error, no_connection};
start_channel({Connection, _}, Srv) ->
    start_channel(Connection, Srv);
start_channel(Connection, Srv) when is_pid(Connection) ->
    case erlang:is_process_alive(Connection)
        andalso amqp_connection:open_channel(Connection) of
        {ok, Channel} ->
            amqp_selective_consumer:register_default_consumer(Channel, Srv),
            #wh_amqp_channel{channel = Channel
                             ,channel_ref = erlang:monitor(process, Channel)
                            };
        false ->
            lager:debug("unabled to start new channel: no_connection", []),
            {error, no_connection};
        E ->
            lager:debug("unable to start new channel: ~p", [E]),
            E
    end.

-spec notify_consumers/2 :: ({'amqp_channel_event', atom()}, atom()) -> 'ok'.
notify_consumers(Msg, Name) ->
    ets:foldl(fun(#wh_amqp_channel{consumer = Consumer}, _) when is_pid(Consumer) ->
                      Consumer ! Msg, ok;
                 (_, _) -> ok
              end, ok, Name).

-spec try_to_subscribe/3 :: (atom(), pid(), #'basic.consume'{}) -> 'ok' | {'error', term()}.
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

clear_channels(Name) ->
    [clear_channel(C) || #wh_amqp_channel{}=C <- ets:tab2list(Name)],
    ok.
clear_channel(#wh_amqp_channel{channel=ChPid
                               ,channel_ref=ChRef
                               ,consumer_ref=ConRef
                               ,consumer=Consumer
                              }) ->
    is_reference(ChRef) andalso erlang:demonitor(ChRef, [flush]),
    is_reference(ConRef) andalso erlang:demonitor(ConRef, [flush]),
    erlang:is_process_alive(ChPid) andalso amqp_channel:close(ChPid),
    is_pid(Consumer) andalso (Consumer ! {amqp_channel_event, closing}).
    
