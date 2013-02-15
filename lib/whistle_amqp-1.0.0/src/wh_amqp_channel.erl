%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_channel).

-export([publish/2]).
-export([consume/1]).
-export([consumer_pid/0
         ,consumer_pid/1
        ]).

-include("amqp_util.hrl").

-spec publish/2 :: (#'basic.publish'{}, ne_binary() | iolist()) -> 'ok' | {'error', _}.
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub
        ,#'amqp_msg'{props=#'P_basic'{timestamp=undefined}=Props}=AmqpMsg) ->
    Now = wh_util:now_us(now()),
    publish(BasicPub, AmqpMsg#'amqp_msg'{props=Props#'P_basic'{timestamp=Now}});
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            amqp_channel:call(Pid, BasicPub, AmqpMsg),
            lager:debug("channel '~p' published exchange '~s' routing key '~s': ~s", [Pid, _Exchange, _RK, AmqpMsg#'amqp_msg'.payload]),
            wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.last_message, AmqpMsg#'amqp_msg'.payload}),
            ok
    end.

-spec consume/1 :: (consume_records()) -> consume_ret().
consume(#'basic.consume'{consumer_tag=CTag}=BasicConsume) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{consumer_tag=CTag} -> ok;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, BasicConsume),
            handle_command_result(Result, Channel)
    end;
consume(#'basic.cancel'{}=BasicCancel) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{consumer_tag=undefined} ->
            {error, not_consuming};
        #wh_amqp_channel{channel=Pid, consumer_tag=CTag} ->
            amqp_channel:call(Pid, BasicCancel#'basic.cancel'{consumer_tag=CTag})
    end;
consume(#'queue.bind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=QueueBind) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            lager:debug("bind: exchange '~s' routing key '~s' queue '~s'", [_Exchange, _RK, _Q]),
            Result = amqp_channel:call(Pid, QueueBind),
            handle_command_result(Result, Channel)
    end;
consume(#'queue.unbind'{}=QueueUnbind) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, QueueUnbind),
            handle_command_result(Result, Channel)
    end;
consume(#'queue.declare'{}=QueueDeclare) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, QueueDeclare),
            handle_command_result(Result, Channel)
    end;
consume(#'queue.delete'{}=QueueDelete) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, QueueDelete),
            handle_command_result(Result, Channel)
    end;
consume(#'basic.qos'{}=BasicQos) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, BasicQos),
            handle_command_result(Result, Channel)
    end;
consume(#'basic.ack'{}=BasicAck) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid} ->
            amqp_channel:cast(Pid, BasicAck)
    end;
consume(#'basic.nack'{}=BasicNack) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid} ->
            amqp_channel:cast(Pid, BasicNack)
    end;
consume(#'exchange.declare'{exchange=_Ex, type=_Ty}=ExchangeDeclare) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            lager:debug("attempting to create new ~s exchange '~s' via channel ~p", [_Ty, _Ex, Pid]),
            Result = amqp_channel:call(Pid, ExchangeDeclare),
            handle_command_result(Result, Channel)
    end.


handle_command_result({error, _}=Error, _) ->
    Error;
handle_command_result({ok, Ok}, Channel) ->
    handle_command_result(Ok, Channel);
handle_command_result(#'exchange.declare_ok'{}, _) ->
    ok;
handle_command_result(#'basic.qos_ok'{}, _) ->
    ok;
handle_command_result(#'queue.delete_ok'{}, _) ->
    ok;
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok, Channel) ->
    wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.queue, Q}),
    {ok, Ok};
handle_command_result(#'queue.unbind_ok'{}, _) ->
    ok;
handle_command_result(#'queue.bind_ok'{}, _) ->
    ok;
handle_command_result(#'basic.consume_ok'{consumer_tag=Tag}, Channel) ->
    wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.consumer_tag, Tag}),
    ok;
handle_command_result(_Else, _) ->
    lager:warning("unexpected AMQP command result: ~p", [_Else]),
    {error, unexpected_result}.

my_channel() ->
    Pid = consumer_pid(),
    case wh_amqp_connections:find_channel(Pid) of
        #wh_amqp_channel{}=Channel -> Channel;
        {error, not_found} ->
            maybe_open_channel(Pid)
    end.

maybe_open_channel(Pid) ->
    case wh_amqp_connection:open_channel(#wh_amqp_channel{consumer=Pid}) of
        #wh_amqp_channel{}=Channel ->
            wh_amqp_connections:update_channel(Channel),
            Channel;
        {error, _}=E -> E
    end.

consumer_pid() ->
    case get('$wh_amqp_consumer') of
        Pid when is_pid(Pid) -> Pid;
        _Else -> self()
    end.

consumer_pid(Pid) ->
    put('$wh_amqp_consumer', Pid).
