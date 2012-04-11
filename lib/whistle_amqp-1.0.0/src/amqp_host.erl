%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle a host's connection/channels
%%% @end
%%% @contributions
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(amqp_host).

-behaviour(gen_server).

%% API
-export([start_link/3, publish/4, consume/3, misc_req/3, stop/1]).
-export([publish_channel/2, misc_channel/2, my_channel/2
         ,update_my_tag/3, fetch_my_tag/2
        ]).

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

-record(state, {
          connection = 'undefined' :: 'undefined' | {pid(), reference()}
          ,publish_channel = 'undefined' :: 'undefined' | channel_data()
          ,misc_channel = 'undefined' :: 'undefined' | channel_data()
          ,consumers = dict:new() :: dict()
          ,use_federation = 'true' :: boolean()
          ,return_handlers = dict:new() :: dict() %% ref, pid() - list of PIDs that are interested in returned messages
          ,manager = 'undefined' :: 'undefined' | pid()
          ,amqp_h = 'undefined' :: 'undefined' | binary()
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
start_link(Host, Conn, UseFederation) ->
    gen_server:start_link(?MODULE, [Host, Conn, UseFederation], []).

-spec publish/4 :: (pid(), call_from(), #'basic.publish'{}, ne_binary() | iolist()) -> 'ok'.
publish(Srv, From, BasicPub, AmqpMsg) ->
    {ok, C} = gen_server:call(Srv, publish_channel),
    gen_server:reply(From, amqp_channel:call(C, BasicPub, AmqpMsg)).

-spec consume/3 :: (pid(), call_from(), consume_records()) -> 'ok'.
consume(Srv, From, Msg) ->
    gen_server:cast(Srv, {consume, From, Msg}).

-spec misc_req/3 :: (pid(), call_from(), misc_records()) -> 'ok'.
misc_req(Srv, From, Req) ->
    gen_server:cast(Srv, {misc_req, From, Req}).

-spec register_return_handler/2 :: (pid(), call_from()) -> 'ok'.
register_return_handler(Srv, From) ->
    gen_server:cast(Srv, {register_return_handler, From}).

-spec publish_channel/2 :: (pid(), call_from()) -> {'ok', pid()}.
publish_channel(Srv, From) ->
    gen_server:reply(From, gen_server:call(Srv, publish_channel)).

-spec misc_channel/2 :: (pid(), call_from()) -> {'ok', pid()}.
misc_channel(Srv, From) ->
    gen_server:reply(From, gen_server:call(Srv, misc_channel)).

-spec my_channel/2 :: (pid(), call_from()) -> {'ok', pid()} |
                                              {'error', term()}.
my_channel(Srv, {FromPid, _}=From) ->
    gen_server:reply(From, gen_server:call(Srv, {my_channel, FromPid})).

-spec update_my_tag/3 :: (pid(), call_from(), ne_binary()) -> 'ok'.
update_my_tag(Srv, {FromPid, _}=From, Tag) ->
    gen_server:reply(From, gen_server:cast(Srv, {update_my_tag, FromPid, Tag})).

-spec fetch_my_tag/2 :: (pid(), call_from()) -> {'ok', binary()} |
                                                {'error', 'not_consuming'}.
fetch_my_tag(Srv, {FromPid, _}=From) ->
    gen_server:reply(From, gen_server:call(Srv, {fetch_my_tag, FromPid})).

-spec stop/1 :: (pid()) -> 'ok' | {'error', 'you_are_not_my_boss'}.
stop(Srv) ->
    case erlang:is_process_alive(Srv) of
        true -> gen_server:call(Srv, stop);
        false -> ok
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
init([Host, Conn, UseFederation]) when is_pid(Conn) ->
    process_flag(trap_exit, true),
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting amqp host for broker ~s", [Host]),

    Ref = erlang:monitor(process, Conn),
    case start_channel(Conn) of
        {Channel, _} = PubChan when is_pid(Channel) ->
            _ = load_exchanges(Channel, UseFederation),
            amqp_channel:register_return_handler(Channel, self()),
            {ok, #state{
               connection = {Conn, Ref}
               ,publish_channel = PubChan
               ,misc_channel = start_channel(Conn)
               ,consumers = dict:new()
               ,manager = whereis(amqp_mgr)
               ,amqp_h = Host
               ,use_federation = UseFederation
              }};
        {error, E} ->
            lager:debug("unable to initialize publish channel for amqp host ~s, ~p", [Host, E]),
            erlang:demonitor(Ref, [flush]),
            {stop, E, Conn}
    end.

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
handle_call(publish_channel, _, #state{publish_channel={C,_}}=State) ->
    {reply, {ok, C}, State};
handle_call(misc_channel, _, #state{misc_channel={C,_}}=State) ->
    {reply, {ok, C}, State};
handle_call({my_channel, FromPid}, _, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) -> % channel, channel ref
                    lager:debug("started new ch: ~p for proc: ~p", [C, FromPid]),
                    FromRef = erlang:monitor(process, FromPid),
                    amqp_selective_consumer:register_default_consumer(C, self()),

                    {reply
                     ,{ok, C}
                     ,State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}
                     ,hibernate
                    };
                closing ->
                    lager:debug("failed to start channel: closing"),
                    {reply, {error, closing}, State};
                {error, _}=E ->
                    lager:debug("failed to start channel: ~p", [E]),
                    {reply, E, State}
            end;
        {ok, {C,_,_,_}} ->
            lager:debug("channel ~p exists for proc ~p", [C, FromPid]),
            {reply, {ok, C}, State}
    end;

handle_call({fetch_my_tag, FromPid}, _, #state{consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error -> {reply, {error, not_consuming}, State};
        {ok, {_C, _R, T, _FromRef}} -> {reply, {ok, T}, State}
    end;


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
handle_cast({publish, From, #'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg}, #state{publish_channel={C,_}}=State) ->
    lager:debug("Pub on ch ~p x: ~s rt: ~s", [C, _Exchange, _RK]),
    spawn(fun() -> gen_server:reply(From, amqp_channel:cast(C, BasicPub, AmqpMsg)) end),
    {noreply, State};

handle_cast({register_return_handler, {FromPid, _}=From}, #state{return_handlers=RHDict}=State) ->
    gen_server:reply(From, ok),
    lager:debug("adding ~p as a return handler", [FromPid]),
    {noreply, State#state{return_handlers=dict:store(erlang:monitor(process, FromPid), FromPid, RHDict)}, hibernate};

handle_cast({update_my_tag, FromPid, Tag}, #state{consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error -> {noreply, State};
        {ok, {C, R, _T, FromRef}} ->
            lager:debug("updating tag for ~p from ~p to ~p", [FromPid, _T, Tag]),
            {noreply, State#state{consumers=dict:store(FromPid, {C, R, Tag, FromRef}, Consumers)}, hibernate}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.consume'{consumer_tag=CTag}=BasicConsume}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) -> % channel, channel ref
                    lager:debug("Consuming on ch: ~p for proc: ~p", [C, FromPid]),
                    FromRef = erlang:monitor(process, FromPid),
                    amqp_selective_consumer:register_default_consumer(C, self()),

                    case try_to_subscribe(C, BasicConsume, FromPid) of
                        {ok, Tag} ->
                            gen_server:reply(From, {ok, C}),
                            {noreply, State#state{consumers=dict:store(FromPid, {C,R,Tag,FromRef}, Consumers)}, hibernate};
                        {error, _E}=Err ->
                            gen_server:reply(From, Err),
                            {noreply, State}
                    end;
                closing ->
                    lager:debug("Failed to start channel: closing"),
                    gen_server:reply(From, {error, closing}),
                    {noreply, State};
                {error, _}=E ->
                    lager:debug("Failed to start channel: ~p", [E]),
                    gen_server:reply(From, E),
                    {noreply, State}
            end;
        {ok, {C,R,<<>>,FromRef}} ->
            amqp_selective_consumer:register_default_consumer(C, self()),
            lager:debug("Channel ~p exists for proc ~p, but we aren't consuming yet", [C, FromPid]),

            case try_to_subscribe(C, BasicConsume, FromPid) of
                {ok, Tag} ->
                    gen_server:reply(From, {ok, C}),
                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,Tag,FromRef}, Consumers)}, hibernate};
                {error, _E}=Err ->
                    gen_server:reply(From, Err),
                    {noreply, State}
            end;
        {ok, {_,_,CTag,_}} when CTag =/= <<>> ->
            gen_server:reply(From, {error, sloppy_code_detected}),
            {noreply, State, hibernate};
        {ok, {C,_,_,_}} ->
            case try_to_subscribe(C, BasicConsume, FromPid) of
                {ok, Tag} ->
                    lager:debug("started additional consumer on ch: ~p for proc: ~p but dropping tag ~p, they will not be able to cancel this consumer...", [C, FromPid, Tag]),
                    gen_server:reply(From, {ok, C});
                {error, _E}=Err ->
                    gen_server:reply(From, Err)
            end,
            {noreply, State, hibernate}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.cancel'{}=BasicCancel}, #state{consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            gen_server:reply(From, {error, not_consuming}),
            {noreply, State};
        {ok, {C,_,Tag,_}} ->
            gen_server:reply(From, amqp_channel:cast(C, BasicCancel#'basic.cancel'{consumer_tag=Tag}, FromPid)),
            {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.bind'{}=QueueBind}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),
                    case amqp_channel:call(C, QueueBind) of
                        #'queue.bind_ok'{} ->
                            gen_server:reply(From, ok),
                            {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
                        ok ->
                            gen_server:reply(From, ok),
                            {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
                        {error, _E}=Err ->
                            lager:debug("Error binding queue: ~p", [_E]),
                            gen_server:reply(From, {error, Err}),
                            {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
                        E ->
                            lager:debug("Unexpected ~p", [E]),
                            gen_server:reply(From, {error, E}),
                            {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate}
                    end;
                closing ->
                    lager:debug("Failed to start channel: closing"),
                    gen_server:reply(From, {error, closing}),
                    {noreply, State};
                {error, _}=E ->
                    gen_server:reply(From, E),
                    {noreply, State}
            end;
        {ok, {C,_,_,_}} ->
            case amqp_channel:call(C, QueueBind) of
                #'queue.bind_ok'{} ->
                    gen_server:reply(From, ok);
                ok ->
                    gen_server:reply(From, ok);
                {error, _E}=Err ->
                    gen_server:reply(From, Err);
                Err ->
                    gen_server:reply(From, {error, Err})
            end,
            {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.unbind'{}=QueueUnbind}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),
                    case amqp_channel:call(C, QueueUnbind) of
                        #'queue.unbind_ok'{} ->
                            gen_server:reply(From, ok);
                        {error, _E}=Err ->
                            gen_server:reply(From, Err);
                        Err ->
                            gen_server:reply(From, {error, Err})
                    end,
                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
                closing ->
                    gen_server:reply(From, {error, closing}),
                    {noreply, State};
                {error, _}=E ->
                    gen_server:reply(From, E),
                    {noreply, State}
            end;
        {ok, {C,_,_,_}} ->
            case amqp_channel:call(C, QueueUnbind) of
                #'queue.unbind_ok'{} ->
                    gen_server:reply(From, ok);
                ok ->
                    gen_server:reply(From, ok);
                {error, _E}=Err ->
                    gen_server:reply(From, Err);
                Err ->
                    gen_server:reply(From, {error, Err})
            end,
            {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.declare'{}=QueueDeclare}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),
                    case amqp_channel:call(C, QueueDeclare) of
                        #'queue.declare_ok'{}=QD ->
                            gen_server:reply(From, {ok, QD});
                        ok ->
                            gen_server:reply(From, ok);
                        {error, _E}=Err ->
                            gen_server:reply(From, Err);
                        Err ->
                            gen_server:reply(From, {error, Err})
                    end,
                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
                closing ->
                    gen_server:reply(From, {error, closing}),
                    {noreply, State};
                {error, _}=E ->
                    gen_server:reply(From, E),
                    {noreply, State}
            end;
        {ok, {C,_,_,_}} ->
            case amqp_channel:call(C, QueueDeclare) of
                #'queue.declare_ok'{}=QD ->
                    gen_server:reply(From, {ok, QD});
                ok ->
                    gen_server:reply(From, ok);
                {error, _E}=Err ->
                    gen_server:reply(From, Err);
                Err ->
                    gen_server:reply(From, {error, Err})
            end,
            {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'queue.delete'{}=QueueDelete}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),

                    case amqp_channel:call(C, QueueDelete) of
                        #'queue.delete_ok'{} ->
                            gen_server:reply(From, ok);
                        ok ->
                            gen_server:reply(From, ok);
                        {error, _E}=Err ->
                            gen_server:reply(From, Err);
                        Err ->
                            gen_server:reply(From, {error, Err})
                    end,

                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
                closing ->
                    gen_server:reply(From, {error, closing}),
                    {noreply, State};
                {error, _}=E ->
                    gen_server:reply(From, E),
                    {noreply, State}
            end;
        {ok, {C,_,_,_}} ->
            case amqp_channel:call(C, QueueDelete) of
                #'queue.delete_ok'{} ->
                    gen_server:reply(From, ok);
                ok ->
                    gen_server:reply(From, ok);
                {error, _E}=Err ->
                    gen_server:reply(From, Err);
                Err ->
                    gen_server:reply(From, {error, Err})
            end,
            {noreply, State}
    end;

handle_cast({consume, {FromPid, _}=From, #'basic.qos'{}=BasicQos}, #state{connection=Conn, consumers=Consumers}=State) ->
    case dict:find(FromPid, Consumers) of
        error ->
            case start_channel(Conn, FromPid) of
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),
                    gen_server:reply(From, amqp_channel:call(C, BasicQos)),
                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
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
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),
                    gen_server:reply(From, amqp_channel:cast(C, BasicAck)),
                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
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
                {C,R} when is_pid(C) andalso is_reference(R) ->
                    FromRef = erlang:monitor(process, FromPid),
                    gen_server:reply(From, amqp_channel:cast(C, BasicNack)),
                    {noreply, State#state{consumers=dict:store(FromPid, {C,R,<<>>,FromRef}, Consumers)}, hibernate};
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

handle_cast({misc_req, From, #'exchange.declare'{}=ED}
            ,#state{misc_channel={C,_}, use_federation=UseFederation}=State) ->
    spawn(fun() ->
                  put(callid, ?LOG_SYSTEM_ID),

                  lager:debug("sending exchange.declare to ~p (federated: ~s)", [C, UseFederation]),
                  case amqp_channel:call(C, exchange_declare(ED, UseFederation)) of
                      #'exchange.declare_ok'{} ->
                          lager:debug("exchange declared"),
                          gen_server:reply(From, ok);
                      {error, _E}=Err ->
                          lager:debug("error declaring exchange: ~p", [_E]),
                          gen_server:reply(From, Err);
                      E ->
                          lager:debug("error declaring exchange: ~p", [E]),
                          gen_server:reply(From, {error, E})
                  end
          end),
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
handle_info({'DOWN', Ref, process, ConnPid, Reason}, #state{connection={ConnPid, Ref}}=State) ->
    lager:debug("recieved notification our connection to the amqp broker died: ~p", [Reason]),
    {stop, normal, State};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{return_handlers=RHDict}=State) ->
    lager:debug("recieved notification monitored process ~p  died ~p, searching for reference", [_Pid, _Reason]),
    erlang:demonitor(Ref, [flush]),
    {noreply, remove_ref(Ref, State#state{return_handlers=dict:erase(Ref, RHDict)}), hibernate};

handle_info({#'basic.return'{}, #amqp_msg{}}=ReturnMsg, #state{return_handlers=RHDict}=State) ->
    spawn(fun() ->
                  put(callid, ?LOG_SYSTEM_ID),
                  lager:debug("recieved notification a message couldnt be delivered, forwarding to registered return handlers"),
                  dict:map(fun(_, Pid) -> Pid ! ReturnMsg end, RHDict)
          end),
    {noreply, State};

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
terminate(_Reason, #state{consumers=Consumers, amqp_h=Host}) ->
    spawn(fun() ->
                  put(callid, ?LOG_SYSTEM_ID),
                  notify_consumers({amqp_host_down, Host}, Consumers)
          end),
    lager:debug("amqp host for ~s terminated ~p", [Host, _Reason]).

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
    %% Open an AMQP channel to access our realm
    case erlang:is_process_alive(Connection) andalso amqp_connection:open_channel(Connection) of
        {ok, Channel} ->
            lager:debug("opened channel ~p", [Channel]),

            ChanMRef = erlang:monitor(process, Channel),
            {Channel, ChanMRef};
        false ->
            {error, no_connection};
        E ->
            lager:debug("error opening channel: ~p", [E]),
            E
    end.

-spec start_channel/2 :: ('undefined' | {pid(), reference()} | pid(), pid()) -> channel_data() | {'error', 'no_connection'} | 'closing'.
start_channel(Connection, Pid) ->
    case start_channel(Connection) of
        {C, _} = Channel when is_pid(C) ->
            lager:debug("started channel ~p for caller ~p", [C, Pid]),
            Channel;
        {error, no_connection}=E ->
            lager:debug("no connection available to start channel"),
            E;
        E ->
            lager:debug("failed to start new channel for ~p: ~p", [Pid, E]),
            E
    end.

-spec load_exchanges/2 :: (pid(), boolean()) -> 'ok'.
load_exchanges(C, UseFederation) ->
    lists:foreach(fun({Ex, Type}) ->
                          ED = #'exchange.declare'{
                            exchange = Ex
                            ,type = Type
                           },
                          amqp_channel:call(C, exchange_declare(ED, UseFederation))
                  end, ?KNOWN_EXCHANGES).

-spec remove_ref/2 :: (reference(), #state{}) -> #state{}.
remove_ref(Ref, #state{connection={Conn, _}, publish_channel={C,Ref}}=State) ->
    lager:debug("reference was for publish channel ~p, restarting", [C]),
    State#state{publish_channel=start_channel(Conn)};

remove_ref(Ref, #state{connection={Conn, _}, misc_channel={C,Ref}}=State) ->
    lager:debug("reference was for misc channel ~p, restarting", [C]),
    State#state{misc_channel=start_channel(Conn)};

remove_ref(Ref, #state{connection={Conn, _}, consumers=Cs}=State) ->
    State#state{consumers =
                    dict:fold(fun(K, V, Acc) -> clean_consumers(K, V, Acc, Ref, Conn) end, Cs, Cs)
               }.

-spec notify_consumers/2 :: ({'amqp_host_down', binary()}, dict()) -> 'ok'.
notify_consumers(Msg, Dict) ->
    lists:foreach(fun({Pid,_}) -> Pid ! Msg end, dict:to_list(Dict)).

%% Channel died
-spec clean_consumers/5 :: (pid(), consumer_data(), dict(), reference(), pid()) -> dict().
clean_consumers(FromPid, {C,Ref1,_,FromRef}, AccDict, Ref, Conn) when Ref =:= Ref1 ->
    lager:debug("reference was for channel ~p for ~p, restarting", [C, FromPid]),

    erlang:demonitor(Ref1, [flush]),
    erlang:is_process_alive(C) andalso amqp_channel:close(C),

    case start_channel(Conn, FromPid) of
        {CNew, RefNew} when is_pid(CNew) andalso is_reference(RefNew) ->
            lager:debug("new channel started for ~p", [FromPid]),
            FromPid ! {amqp_lost_channel, connection_restored},
            dict:store(FromPid, {CNew, RefNew, <<>>, FromRef}, AccDict);
        {error, no_connection} ->
            lager:debug("no connection available"),
            FromPid ! {amqp_lost_channel, no_connection},
            dict:erase(FromPid, AccDict);
        closing ->
            lager:debug("closing, no connection"),
            FromPid ! {amqp_lost_channel, no_connection},
            dict:erase(FromPid, AccDict)
    end;

%% Consumer died
clean_consumers(FromPid, {C,CRef,_,FromRef}, AccDict, Ref, _) when Ref =:= FromRef ->
    lager:debug("reference was for consumer ~p, removing channel ~p", [FromPid, C]),
    erlang:demonitor(CRef, [flush]),
    erlang:demonitor(FromRef, [flush]),
    erlang:is_process_alive(C) andalso amqp_channel:close(C),
    dict:erase(FromPid, AccDict);

%% Generic channel cleanup when FromPid isn't alive
clean_consumers(FromPid, {C,CRef,_,FromRef}, AccDict, _, _) ->
    case erlang:is_process_alive(FromPid) of
        true -> AccDict;
        false ->
            lager:debug("reference was a consumer ~p that shutdown, removing channel ~p", [FromPid, C]),
            erlang:demonitor(FromRef, [flush]),
            erlang:demonitor(CRef, [flush]),
            erlang:is_process_alive(C) andalso amqp_channel:close(C),
            dict:erase(FromPid, AccDict)
    end;

%% Simple catchall
clean_consumers(_, _, AccDict,_,_) ->
    AccDict.

-spec try_to_subscribe/3 :: (pid(), #'basic.consume'{}, pid()) -> {'ok', non_neg_integer()} | {'error', term()}.
try_to_subscribe(C, BasicConsume, FromPid) ->
    try amqp_channel:subscribe(C, BasicConsume, FromPid) of
        #'basic.consume_ok'{consumer_tag=Tag} -> {ok, Tag};
        Other -> {error, Other}
    catch
        _E:R -> {error, R}
    end.

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
