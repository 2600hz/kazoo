%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_channel).

-export([close/0]).
-export([remove/0]).
-export([publish/2]).
-export([consume/1]).
-export([consumer_pid/0
         ,consumer_pid/1
        ]).

-include("amqp_util.hrl").

close() ->
    case my_channel() of
        {error, _} -> ok;
        #wh_amqp_channel{}=Channel ->
            wh_amqp_connection:close_channel(Channel)
    end.

remove() ->
    case my_channel() of
        {error, _} -> ok;
        #wh_amqp_channel{}=Channel ->
            wh_amqp_connection:remove_channel(Channel)
    end.

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
%%            lager:debug("published to ~s exchange (routing key ~s) via ~p: ~s", [_Exchange, _RK, Pid, AmqpMsg#'amqp_msg'.payload]),
            lager:debug("published to ~s exchange (routing key ~s) via ~p", [_Exchange, _RK, Pid]),
            wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.last_message, AmqpMsg#'amqp_msg'.payload}),
            ok
    end.

-spec consume/1 :: (consume_records()) -> consume_ret().
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
consume(#'basic.consume'{consumer_tag=CTag}=Command) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{consumer_tag=CTag} -> ok;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, Command),
            handle_command_result(Result, Command, Channel)
    end;
consume(#'basic.cancel'{}=Command) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{consumer_tag=undefined} ->
            {error, not_consuming};
        #wh_amqp_channel{channel=Pid, consumer_tag=CTag}=Channel ->
            Result = amqp_channel:call(Pid, Command#'basic.cancel'{consumer_tag=CTag}),
            handle_command_result(Result, Command, Channel)
    end;
consume(#'queue.declare'{}=QD) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Command = maybe_keep_prior_name(QD, Channel),
            Result = amqp_channel:call(Pid, Command),
            handle_command_result(Result, Command, Channel)
    end;
consume(#'exchange.declare'{}=Command) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{}=Channel ->
            maybe_declare_exchange(Command, Channel)
    end;
consume(Command) ->
    case my_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{channel=Pid}=Channel ->
            Result = amqp_channel:call(Pid, Command),
            handle_command_result(Result, Command, Channel)
    end.

maybe_declare_exchange(#'exchange.declare'{}=Command
                       ,#wh_amqp_channel{channel=Pid}=Channel) ->
    case wh_amqp_connection:does_exchange_exist(Channel, Command) of
        true -> ok;
        false ->
            Result = amqp_channel:call(Pid, Command),
            handle_command_result(Result, Command, Channel)
    end.

maybe_keep_prior_name(Declare, #wh_amqp_channel{queue=undefined}) ->
    Declare;
maybe_keep_prior_name(#'queue.declare'{}=Declare
                      ,#wh_amqp_channel{queue=Q}) ->
    io:format("keep prior Q name: ~p~n", [Q]),
    Declare#'queue.declare'{queue = Q}.

handle_command_result({error, _}=Error, _, _) ->
    Error;
handle_command_result({ok, Ok}, Command, Channel) ->
    handle_command_result(Ok, Command, Channel);
handle_command_result(#'exchange.declare_ok'{}
                      ,#'exchange.declare'{exchange=_Ex, type=_Ty}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("declared ~s exchange ~s via channel ~p", [_Ty, _Ex, Pid]),
    wh_amqp_connection:exchange_exist(Channel, Command),
    ok;
handle_command_result(#'basic.qos_ok'{}, _, _) ->
    ok;
handle_command_result(#'queue.delete_ok'{}, _, Channel) ->
    wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.queue, undefined}),
    ok;
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{}
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("declared queue ~s via channel ~p", [Q, Pid]),
    wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.queue, Q}),
    {ok, Ok};
handle_command_result(#'queue.unbind_ok'{}
                      ,#'queue.unbind'{exchange=_Exchange, routing_key=_RK, queue=_Q}
                      ,#wh_amqp_channel{channel=Pid}) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    ok;
handle_command_result(#'queue.bind_ok'{}
                      ,#'queue.bind'{exchange=_Exchange, routing_key=_RK, queue=_Q}
                      ,#wh_amqp_channel{channel=Pid}) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    ok;
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}, _
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Pid]),
    wh_amqp_connections:update_channel(Channel, {#wh_amqp_channel.consumer_tag, CTag}),
    ok;
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                      ,#'basic.cancel'{}
                      ,#wh_amqp_channel{channel=Pid}) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    ok;
handle_command_result(_Else, _, _) ->
    lager:warning("unexpected AMQP command result: ~p", [_Else]),
    {error, unexpected_result}.

my_channel() ->
    Pid = consumer_pid(),
    case wh_amqp_connections:find_channel(Pid) of
        #wh_amqp_channel{channel=undefined}=Channel ->
            wh_amqp_connection:open_channel(Channel);
        #wh_amqp_channel{}=Channel ->
            Channel;
        {error, not_found} ->
            wh_amqp_connection:open_channel(#wh_amqp_channel{consumer=Pid})
    end.

consumer_pid() ->
    case get('$wh_amqp_consumer') of
        Pid when is_pid(Pid) -> Pid;
        _Else -> self()
    end.

consumer_pid(Pid) ->
    put('$wh_amqp_consumer', Pid).
