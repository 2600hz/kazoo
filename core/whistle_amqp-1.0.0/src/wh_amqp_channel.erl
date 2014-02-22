%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_channel).

-export([consumer_pid/0
         ,consumer_pid/1
         ,remove_consumer_pid/0
        ]).
-export([consumer_broker/0
         ,consumer_broker/1
         ,remove_consumer_broker/0
        ]).
-export([requisition/0
         ,requisition/1
         ,requisition/2
        ]).
-export([release/0
         ,release/1
        ]).
-export([close/1
         ,close/2
        ]).
-export([maybe_publish/2]).
-export([publish/2]).
-export([command/1
         ,command/2
        ]).

-include("amqp_util.hrl").

-spec consumer_pid() -> pid().
consumer_pid() ->
    case get('$wh_amqp_consumer_pid') of
        Pid when is_pid(Pid) -> Pid;
        _Else -> self()
    end.

-spec consumer_pid(pid()) -> api_pid().
consumer_pid(Pid) when is_pid(Pid) ->
    put('$wh_amqp_consumer_pid', Pid).

-spec remove_consumer_pid() -> 'undefined'.
remove_consumer_pid() ->
    put('$wh_amqp_consumer_pid', 'undefined').

-spec consumer_broker() -> api_binary().
consumer_broker() ->
    case get('$wh_amqp_consumer_broker') of
        Broker when is_binary(Broker) -> Broker;
        _Else -> 'undefined'
    end.

-spec consumer_broker(ne_binary()) -> api_binary().
consumer_broker(Broker) when is_binary(Broker) ->
    put('$wh_amqp_consumer_broker', Broker).

-spec remove_consumer_broker() -> 'undefined'.
remove_consumer_broker() ->
    put('$wh_amqp_consumer_broker', 'undefined').

-spec requisition() -> boolean().
requisition() -> requisition(consumer_pid()).

-spec requisition(pid() | api_binary()) -> boolean().
requisition(Consumer) when is_pid(Consumer) ->
    requisition(Consumer, consumer_broker());
requisition(Broker) ->
    requisition(consumer_pid(), Broker).

-spec requisition(pid(), api_binary()) -> boolean().
requisition(Consumer, Broker) when is_pid(Consumer) ->
    case wh_amqp_assignments:request_channel(Consumer, Broker) of
        #wh_amqp_assignment{channel=Channel}
          when is_pid(Channel) -> 'true';
        _Else -> 'false'
    end.

-spec release() -> 'ok'.
release() -> release(consumer_pid()).

-spec release(pid()) -> 'ok'.
release(Pid) when is_pid(Pid) ->
    lager:debug("release consumer ~p channel assignment", [Pid]),
    _ = wh_amqp_history:remove(Pid),
    wh_amqp_assignments:release(Pid).

-spec close(api_pid()) -> 'ok'.
close(Channel) -> close(Channel, []).

close(Channel, []) when is_pid(Channel) ->
    _ = (catch gen_server:call(Channel, {'close', 200, <<"Goodbye">>}, 5000)),
    lager:debug("closed amqp channel ~p", [Channel]);
close(_, []) -> 'ok';
close(Channel, [#'basic.consume'{consumer_tag=CTag}|Commands]) when is_pid(Channel) ->
    lager:debug("ensuring ~s is removed", [CTag]),
    Command = #'basic.cancel'{consumer_tag=CTag, nowait='true'},
    _ = (catch amqp_channel:cast(Channel, Command)),
    close(Channel, Commands);
close(Channel, [#'queue.declare'{queue=Queue}|Commands]) when is_pid(Channel) ->
    lager:debug("ensuring queue ~s is removed", [Queue]),
    Command = #'queue.delete'{queue=Queue, if_unused='true', nowait='true'},
    _ = (catch amqp_channel:cast(Channel, Command)),
    close(Channel, Commands);
close(Channel, [_|Commands]) ->
    close(Channel, Commands).

%% maybe publish will only publish a message if there is an existing channel assignment
-spec maybe_publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
maybe_publish(#'basic.publish'{routing_key=RoutingKey}=BasicPub, AmqpMsg) ->
    case maybe_split_routing_key(RoutingKey) of
        {'undefined', _} ->
            basic_publish(wh_amqp_assignments:find(), BasicPub, AmqpMsg);        
        {ConsumerPid, RemoteRoutingKey} ->
            basic_publish(wh_amqp_assignments:find(ConsumerPid)
                          ,BasicPub#'basic.publish'{routing_key=RemoteRoutingKey}
                          ,AmqpMsg)
    end.

%% publish will wait up to 5 seconds for a valid channel before publishing
-spec publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
publish(#'basic.publish'{routing_key=RoutingKey}=BasicPub, AmqpMsg) ->
    case maybe_split_routing_key(RoutingKey) of
        {'undefined', _} ->
            basic_publish(wh_amqp_assignments:get_channel(5000), BasicPub, AmqpMsg);
        {ConsumerPid, RemoteRoutingKey} ->
            basic_publish(wh_amqp_assignments:get_channel(ConsumerPid, 5000)
                          ,BasicPub#'basic.publish'{routing_key=RemoteRoutingKey}
                          ,AmqpMsg)
    end.
        
-spec basic_publish(wh_amqp_assignment(), #'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
basic_publish(#wh_amqp_assignment{channel=Channel, broker=_Broker} 
              ,#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub
              ,AmqpMsg)
  when is_pid(Channel) ->
    _ = (catch amqp_channel:call(Channel, BasicPub, AmqpMsg)),
    lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p"
                ,[_Exchange, _Broker, _RK, Channel]);
basic_publish(_, #'basic.publish'{exchange=_Exchange, routing_key=_RK}, AmqpMsg) ->
    lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
                ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload]);
basic_publish({'error', 'no_channel'}
              ,#'basic.publish'{exchange=_Exchange, routing_key=_RK}
              ,AmqpMsg) ->
    lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
                ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload]).    

-spec maybe_split_routing_key(ne_binary()) -> {api_pid(), ne_binary()}.
maybe_split_routing_key(<<"consumer://", _/binary>> = RoutingKey) ->
    Size = byte_size(RoutingKey),
    {Start, _} = lists:last(binary:matches(RoutingKey, <<"/">>)),
    {list_to_pid(wh_util:to_list(binary:part(RoutingKey, 11, Start - 11)))
     ,binary:part(RoutingKey, Start + 1, Size - Start - 1)};
maybe_split_routing_key(RoutingKey) -> {'undefined', RoutingKey}.


-spec command(wh_amqp_command()) -> command_ret().
command(#'exchange.declare'{exchange=_Ex, type=_Ty}=Exchange) ->
    wh_amqp_history:add_exchange(Exchange);
command(Command) ->
    %% This will wait forever for a valid channel before publishing...
    %% all commands need to block till completion...
    command(wh_amqp_assignments:get_channel(), Command).

-spec command(wh_amqp_assignment(), wh_amqp_command()) -> command_ret().
command(#wh_amqp_assignment{channel=Pid}, #'basic.ack'{}=BasicAck) ->
    amqp_channel:cast(Pid, BasicAck);
command(#wh_amqp_assignment{channel=Pid}, #'basic.nack'{}=BasicNack) ->
    amqp_channel:cast(Pid, BasicNack);
command(#wh_amqp_assignment{consumer=Consumer
                            ,channel=Channel
                            ,reconnect='true'
                           }
        ,#'basic.consume'{consumer_tag=OldTag}=Command) ->
    C = Command#'basic.consume'{consumer_tag = <<>>},
    case amqp_channel:subscribe(Channel, C, Consumer) of
        #'basic.consume_ok'{consumer_tag=NewTag} ->
            wh_amqp_history:update_consumer_tag(Consumer, OldTag, NewTag);
        _Else ->
            lager:warning("failed to re-establish consumer for ~p: ~p"
                          ,[Consumer, _Else])
    end;
command(#wh_amqp_assignment{consumer=Consumer
                            ,channel=Channel
                           }=Assignment
        ,#'basic.consume'{queue=Queue}=Command) ->
    case wh_amqp_history:is_consuming(Consumer, Queue) of
        'true' ->
            lager:debug("skipping existing basic consume for queue ~s", [Queue]),
            'ok';
        'false' ->
            Result = amqp_channel:subscribe(Channel, Command, Consumer),
            handle_command_result(Result, Command, Assignment)
    end;
command(#wh_amqp_assignment{channel=Channel
                            ,consumer=Consumer}=Assignment
        ,#'basic.cancel'{nowait=NoWait}) ->
    lists:foreach(fun(#'basic.consume'{consumer_tag=CTag}) ->
                          Command = #'basic.cancel'{consumer_tag=CTag, nowait=NoWait},
                          lager:debug("sending cancel for consumer ~s to ~p", [CTag, Channel]),
                          Result = amqp_channel:call(Channel, Command),
                          handle_command_result(Result, Command, Assignment);
                     (_) -> 'ok'
                  end, wh_amqp_history:list_consume(Consumer));
command(#wh_amqp_assignment{channel=Channel}=Assignment, Command) ->
    Result = amqp_channel:call(Channel, Command),
    handle_command_result(Result, Command, Assignment).

-spec handle_command_result(command_ret(), wh_amqp_command(), wh_amqp_assignment()) -> command_ret().
handle_command_result(_, _, #wh_amqp_assignment{reconnect='true'}) -> 'ok';
handle_command_result({'error', _}=Error, _, _) -> Error;
handle_command_result({'ok', Ok}, Command, Assignment) ->
    handle_command_result(Ok, Command, Assignment);
handle_command_result(#'basic.qos_ok'{}
                      ,#'basic.qos'{prefetch_count=Prefetch}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("applied QOS prefetch ~p to channel ~p", [Prefetch, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'queue.delete_ok'{}
                      ,#'queue.delete'{queue=Q}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("deleted queue ~s via channel ~p", [Q, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{passive='true'}
                      ,#wh_amqp_assignment{channel=Channel}) ->
    lager:debug("passive declared queue ~s via channel ~p", [Q, Channel]),
    {'ok', Ok};
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("declared queue ~s via channel ~p", [Q, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command#'queue.declare'{queue=Q}),
    {'ok', Ok};
handle_command_result(#'queue.unbind_ok'{}
                      ,#'queue.unbind'{exchange=_Exchange
                                       ,routing_key=_RK
                                       ,queue=_Q
                                      }=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p"
                ,[_Q, _Exchange, _RK, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'queue.bind_ok'{}
                      ,#'queue.bind'{exchange=_Exchange
                                     ,routing_key=_RK
                                     ,queue=_Q
                                    }=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p"
                ,[_Q, _Exchange, _RK, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}
                      ,#'basic.consume'{}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command#'basic.consume'{consumer_tag=CTag}),
    'ok';
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                      ,#'basic.cancel'{}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result('ok', Command, #wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("executed AMQP command ~s with no_wait option via channel ~p"
                ,[element(1, Command), Channel]),
    _ = wh_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(_Else, _R, _) ->
    lager:warning("unexpected AMQP command result: ~p"
                  ,[lager:pr(_Else, ?MODULE)]),
    {'error', 'unexpected_result'}.
