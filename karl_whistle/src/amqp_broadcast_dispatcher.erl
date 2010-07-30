%%%-------------------------------------------------------------------
%%% File    : amqp_broadcast_dispatcher.erl
%%% Author  : K Anderson
%%% Description : Handles rx and tx amqp of the fanout exchange
%%%
%%% Created : March 24 2010
%%%-------------------------------------------------------------------
-module(amqp_broadcast_dispatcher).
-vsn('1.2').
-include("../include/amqp_client.hrl").
-include("../include/common.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, start/0, publish/2, consume/0, cancel/0, stop/0]).

-record(state, {channel, ticket, tag, queue, consumers = []}).

-define(SERVER, ?MODULE).
-define(EXCHANGE, <<"broadcast_requests">>).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the direct exchange dispatcher
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


publish(Message, To) ->
    gen_server:cast(?SERVER, {publish, Message, To}).

consume() ->
    gen_server:call(?SERVER, add_consumer).

cancel() ->
    gen_server:call(?SERVER, remove_consumer).

stop() ->
    exit(whereis(?SERVER), shutdown).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Opens a new channel for this server, declares the 
%%   exchange, declares a queue named after the localhost (with the 
%%   localhost as a route_key, binds the queue to the exchange, and
%%   opens a consumer on that queue
%%--------------------------------------------------------------------
init([]) ->
    {ok, Channel, Ticket} = amqp_manager:open_channel(self()),

    process_flag(trap_exit, true),

    %% Declare the exchange
    ?DEBUG("~p declare exchange ~p~n", [self(), binary_to_list(?EXCHANGE)]),
    Exchange = #'exchange.declare'{
        ticket = Ticket,
        exchange = ?EXCHANGE,
        type = <<"fanout">>,
        passive = false,
        durable = false,
        auto_delete=false,
        internal = false,
        nowait = false,
        arguments = []
    },
    amqp_channel:call(Channel, Exchange),

    %% Declare a queue
    ?DEBUG("~p declare queue ~p~n", [self(), "broadcast." ++ net_adm:localhost()]),
    Queue = list_to_binary(["broadcast."|net_adm:localhost()]),
    QueueDeclare = #'queue.declare'{
        ticket = Ticket,
        queue = Queue,
        passive = false,
        durable = false,
        exclusive = true,
        auto_delete = true,
        nowait = false,
        arguments = []
    },
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, QueueDeclare),

    %% Bind the queue to an exchange
    ?DEBUG("~p bind queue ~p to exchange ~p~n", [self(), "broadcast." ++ net_adm:localhost(), binary_to_list(?EXCHANGE)]),
    QueueBind = #'queue.bind'{
        ticket = Ticket,
        queue = Queue,
        exchange = ?EXCHANGE,
        routing_key = <<"#">>,
        nowait = false,
        arguments = []
    },
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    %% Register a consumer to listen to the queue
    ?DEBUG("~p open consumer for ~p in ~p~n", [self(), "broadcast." ++ net_adm:localhost(), binary_to_list(?EXCHANGE)]),
    BasicConsume = #'basic.consume'{
        ticket = Ticket,
        queue = Queue,
        consumer_tag = Queue,
        no_local = false,
        no_ack = true,
        exclusive = true,
        nowait = false
    },
    #'basic.consume_ok'{consumer_tag = Tag}
        = amqp_channel:subscribe(Channel, BasicConsume, self()),

    ?INFO("~p amqp_broadcast_dispatcher init complete!~n", [self()]),

    {ok, #state{channel = Channel, ticket = Ticket, tag = Tag, queue = Queue}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State}
%% Description: Handle OTP sync messages.
%%--------------------------------------------------------------------

%% Add the calling process as a consumer of this queues messages
handle_call(add_consumer, {From, _}, State) ->
    ?DEBUG("~p amqp_broadcast_dispatcher add consumer: ~p~n", [self(), From]),
    #state{consumers = Consumers} = State,
    case lists:member(From, Consumers) of
        true -> {reply, ok, State};
        false -> {reply, ok, State#state{consumers = [From|Consumers]}}
    end;

%% Remove the calling process as a consumer of this queues messages
handle_call(remove_consumer, {From, _}, State) ->
    ?DEBUG("~p amqp_broadcast_dispatcher remove consumer: ~p~n", [self(), From]),
    #state{consumers = Consumers} = State,
    {reply, ok, State#state{consumers = lists:delete(From, Consumers)}};

%% Remove an arbitrary process as a consumer of this queues messages
handle_call({remove_consumer, From}, _From, State) ->
    ?DEBUG("~p amqp_broadcast_dispatcher remove consumer: ~p~n", [self(), From]),
    #state{consumers = Consumers} = State,
    {reply, ok, State#state{consumers = lists:delete(From, Consumers)}};

%% Catch all so we dont loose state
handle_call(Unhandled, _From, State) ->
    ?DEBUG("~p amqp_broadcast_dispatcher unknown call: ~p~n", [self(), Unhandled]),
    {reply, {error, unknown}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State}
%% Description: Handling OTP async message to publish to the direct
%%   exchange
%%--------------------------------------------------------------------

%% Publish the message to the specified direct queue
handle_cast({publish, Message, To}, State) ->
    publish_message(Message, To, State),
    {noreply, State};

%% catch all so we dont loose state
handle_cast(Unhandled, State) ->
    ?DEBUG("~p amqp_broadcast_dispatcher unknown cast: ~p~n", [self(), Unhandled]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State}
%% Description: Handling all non call/cast messages.  This is how 
%%   the AMQP erlang client consumer will message us, and we are
%%   traping exists so we handle those to.
%%--------------------------------------------------------------------

%% cleanly exit if we have been asked to exit
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};

%% take in any incoming amqp messages and distribute
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    ?DEBUG("~p amqp_broadcast_dispatcher recieved: ~p~n", [self(), Payload]),
    case Props#'P_basic'.content_type of
        <<"text/xml">> -> notify(State#state.consumers, Props, xmerl_scan:string(binary_to_list(Payload)));
        <<"text/plain">> -> notify(State#state.consumers, Props, binary_to_list(Payload));
        _ContentType -> ?WARN("~p amqp_broadcast_dispatcher recieved unknown msg type: ~p~n", [self(), _ContentType])
    end,
    {noreply, State};

%% catch all so we dont loose state
handle_info(Unhandled, State) ->
    ?WARN("~p amqp_broadcast_dispatcher unknown request: ~p~n", [self(), Unhandled]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> Reason
%% Description: This function is called when it is about to terminate 
%%   and cleans up our open resources 
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    %% cancel our consumer
    ?DEBUG("~p ampq_direct_dispatch canceling consumer~n", [self()]),
    BasicCancel = #'basic.cancel'{
        consumer_tag = State#state.tag,
        nowait = false
    },
    amqp_channel:call(State#state.channel, BasicCancel),

    %% close our channel
    amqp_manager:close_channel(self()),

    ?DEBUG("~p ampq_direct_dispatch terminated: ~p~n", [self(), Reason]),

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
publish_message(Msg, To, State) ->
    %% Create a basic publish command
    BasicPublish = #'basic.publish'{
        ticket = State#state.ticket,
        exchange = ?EXCHANGE,
        routing_key = thing_to_binary(To),
        mandatory = false,
        immediate = false
    },

    %% Add the message to the publish, converting to binary
    AmqpMsg = #'amqp_msg'{
        payload = thing_to_binary(Msg)
    },

    %% execute the publish command
    ?DEBUG("~p amqp_broadcast_dispatcher publish to ~p: ~p~n", [self(), BasicPublish#'basic.publish'.routing_key, AmqpMsg#'amqp_msg'.payload]),
    amqp_channel:cast(State#state.channel, BasicPublish, AmqpMsg).

notify([], _, _) -> ok;
notify([H|T], Props, Msg) ->
    spawn(fun() -> case is_process_alive(H) of true -> H ! {broadcast_msg, Props, Msg}; false -> gen_server:call(?SERVER, {remove_consumer, H}) end end),
    notify(T, Props, Msg).

thing_to_binary(X) when is_binary(X) -> X;
thing_to_binary(X) when is_list(X) -> list_to_binary(X);
thing_to_binary(X) when is_integer(X) -> <<X>>;
thing_to_binary(X) when is_float(X) -> <<X/float>>;
thing_to_binary(X) when is_atom(X) -> atom_to_binary('Erlang', latin1).
