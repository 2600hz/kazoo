%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_channel).

-export([consumer_pid/0
        ,consumer_pid/1
        ,remove_consumer_pid/0
        ]).
-export([consumer_broker/0
        ,consumer_broker/1
        ,remove_consumer_broker/0
        ]).
-export([consumer_channel/0
        ,consumer_channel/1
        ,remove_consumer_channel/0
        ]).
-export([channel_publish_method/0
        ,channel_publish_method/1
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

-define(ASSIGNMENT_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-type consumer_pid() :: pid().
-type consumer_channel() :: pid().
-type publish_method() :: 'call' | 'cast'.

-export_type([consumer_pid/0, consumer_channel/0, publish_method/0]).

-spec consumer_pid() -> pid().
consumer_pid() ->
    case get('$kz_amqp_consumer_pid') of
        Pid when is_pid(Pid) -> Pid;
        _Else -> self()
    end.

-spec consumer_pid(pid()) -> kz_term:api_pid().
consumer_pid(Pid) when is_pid(Pid) ->
    put('$kz_amqp_consumer_pid', Pid).

-spec remove_consumer_pid() -> 'undefined'.
remove_consumer_pid() ->
    put('$kz_amqp_consumer_pid', 'undefined').

-spec consumer_channel() -> pid() | kz_amqp_assignment().
consumer_channel() ->
    case get('$kz_amqp_consumer_channel') of
        'undefined' -> kz_amqp_assignments:get_channel(?ASSIGNMENT_TIMEOUT);
        Channel -> Channel
    end.

-spec consumer_channel(pid()) -> pid().
consumer_channel(Channel) ->
    put('$kz_amqp_consumer_channel', Channel).

-spec remove_consumer_channel() -> 'undefined'.
remove_consumer_channel() ->
    put('$kz_amqp_consumer_channel', 'undefined').

-spec consumer_broker() -> kz_term:api_binary().
consumer_broker() ->
    case get('$kz_amqp_consumer_broker') of
        Broker when is_binary(Broker) -> Broker;
        _Else -> 'undefined'
    end.

-spec consumer_broker(kz_term:ne_binary()) -> kz_term:api_binary().
consumer_broker(Broker) when is_binary(Broker) ->
    put('$kz_amqp_consumer_broker', Broker).

-spec remove_consumer_broker() -> 'undefined'.
remove_consumer_broker() ->
    put('$kz_amqp_consumer_broker', 'undefined').

-spec channel_publish_method() -> publish_method().
channel_publish_method() ->
    case get('$kz_amqp_channel_publish_method') of
        'undefined' -> 'call';
        Method when is_atom(Method) -> Method;
        _Else -> 'call'
    end.

-spec channel_publish_method(publish_method()) -> publish_method().
channel_publish_method(Method)
  when Method =:= 'cast';
       Method =:= 'call' ->
    put('$kz_amqp_channel_publish_method', Method).

-spec requisition() -> boolean().
requisition() -> requisition(consumer_pid()).

-spec requisition(pid() | kz_term:api_binary()) -> boolean().
requisition(Consumer) when is_pid(Consumer) ->
    requisition(Consumer, consumer_broker());
requisition(Broker) ->
    put('$kz_amqp_consumer_broker', Broker),
    requisition(consumer_pid(), Broker).

-spec requisition(pid(), kz_term:api_binary()) -> boolean().
requisition(Consumer, Broker) when is_pid(Consumer) ->
    case kz_amqp_assignments:request_channel(Consumer, Broker) of
        #kz_amqp_assignment{channel=Channel}
          when is_pid(Channel) -> 'true';
        _Else -> 'false'
    end.

-spec release() -> 'ok'.
release() -> release(consumer_pid()).

-spec release(pid()) -> 'ok'.
release(Pid) when is_pid(Pid) ->
    lager:debug("release consumer ~p channel assignment", [Pid]),
    _ = kz_amqp_history:remove(Pid),
    kz_amqp_assignments:release(Pid).

-spec close(kz_term:api_pid()) -> 'ok'.
close(Channel) -> close(Channel, []).

-spec close(kz_term:api_pid(), list()) -> 'ok'.
close(Channel, []) when is_pid(Channel) ->
    _ = (catch gen_server:call(Channel, {'close', 200, <<"Goodbye">>}, 5 * ?MILLISECONDS_IN_SECOND)),
    lager:debug("closed amqp channel ~p", [Channel]);
close(_, []) -> 'ok';
close(Channel, [#'basic.consume'{consumer_tag=CTag}|Commands]) when is_pid(Channel) ->
    lager:debug("ensuring ~s is removed", [CTag]),
    Command = #'basic.cancel'{consumer_tag=CTag, nowait='true'},
    assert_valid_amqp_method(Command),
    amqp_channel:cast(Channel, Command),
    close(Channel, Commands);
close(Channel, [#'queue.declare'{queue=Queue}|Commands]) when is_pid(Channel) ->
    lager:debug("ensuring queue ~s is removed", [Queue]),
    Command = #'queue.delete'{queue=Queue, if_unused='true', nowait='true'},
    assert_valid_amqp_method(Command),
    amqp_channel:cast(Channel, Command),
    close(Channel, Commands);
close(Channel, [_|Commands]) ->
    close(Channel, Commands).

%%------------------------------------------------------------------------------
%% @doc Publish a message only if there is an existing channel assignment.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_publish(basic_publish(), amqp_msg()) -> 'ok'.
maybe_publish(#'basic.publish'{routing_key=RoutingKey}=BasicPub, AmqpMsg) ->
    case maybe_split_routing_key(RoutingKey) of
        {'undefined', _} ->
            basic_publish(kz_amqp_assignments:find(), BasicPub, AmqpMsg);
        {ConsumerPid, RemoteRoutingKey} ->
            basic_publish(kz_amqp_assignments:find(ConsumerPid)
                         ,BasicPub#'basic.publish'{routing_key=RemoteRoutingKey}
                         ,AmqpMsg
                         )
    end.

%%------------------------------------------------------------------------------
%% @doc Publish will wait up to 5 seconds for a valid channel before publishing.
%% @end
%%------------------------------------------------------------------------------
-spec publish(basic_publish(), amqp_msg()) -> 'ok'.
publish(#'basic.publish'{routing_key=RoutingKey}=BasicPub, AmqpMsg) ->
    case maybe_split_routing_key(RoutingKey) of
        {'undefined', _} ->
            basic_publish(consumer_channel()
                         ,BasicPub
                         ,AmqpMsg
                         );
        {ConsumerPid, RemoteRoutingKey} ->
            basic_publish(kz_amqp_assignments:get_channel(ConsumerPid, ?ASSIGNMENT_TIMEOUT)
                         ,BasicPub#'basic.publish'{routing_key=RemoteRoutingKey}
                         ,AmqpMsg
                         )
    end.

-spec extract_msg_id(binary()) -> binary().
extract_msg_id(Msg) ->
    case binary:split(Msg, <<"Msg-ID\":\"">>) of
        [_First, Last] ->
            [MsgId, _] = binary:split(Last, <<"\",">>),
            MsgId;
        _AnyOther ->
            <<"undefined">>
    end.

-spec basic_publish(kz_amqp_assignment() | pid() | {'error', 'no_channel'}, basic_publish(), amqp_msg()) -> 'ok'.
basic_publish(#kz_amqp_assignment{channel=Channel
                                 ,broker=_Broker
                                 }=Assignment
             ,#'basic.publish'{exchange=_Exchange
                              ,routing_key=_RK
                              }=BasicPub
             ,AmqpMsg)
  when is_pid(Channel) ->
    assert_valid_amqp_method(BasicPub),
    _ = basic_publish(Assignment, BasicPub, AmqpMsg, channel_publish_method()),
    MsgId = extract_msg_id(AmqpMsg#'amqp_msg'.payload),
    lager:debug("published(~s) to ~s(~s) exchange (routing key ~s) via ~p"
               ,[MsgId, _Exchange, _Broker, _RK, Channel]
               );
basic_publish({'error', 'no_channel'}
             ,#'basic.publish'{exchange=_Exchange
                              ,routing_key=_RK
                              }
             ,AmqpMsg) ->
    lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
               ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload]
               );
basic_publish(Channel
             ,#'basic.publish'{exchange=_Exchange
                              ,routing_key=_RK
                              }=BasicPub
             ,AmqpMsg)
  when is_pid(Channel) ->
    assert_valid_amqp_method(BasicPub),
    _ = basic_publish(Channel, BasicPub, AmqpMsg, channel_publish_method()),
    lager:debug("published to ~s(direct) exchange (routing key ~s) via ~p"
               ,[_Exchange, _RK, Channel]
               );
basic_publish(_, #'basic.publish'{exchange=_Exchange
                                 ,routing_key=_RK
                                 }
             ,AmqpMsg) ->
    lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
               ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload]
               ).

-spec basic_publish(kz_amqp_assignment() | pid(), basic_publish(), amqp_msg(), atom()) -> 'ok'.
basic_publish(#kz_amqp_assignment{channel=Channel}
             ,#'basic.publish'{}=BasicPub
             ,AmqpMsg
             ,Method)
  when is_pid(Channel) ->
    amqp_channel:Method(Channel, BasicPub, AmqpMsg);
basic_publish(Channel
             ,#'basic.publish'{}=BasicPub
             ,AmqpMsg
             ,Method)
  when is_pid(Channel) ->
    amqp_channel:Method(Channel, BasicPub, AmqpMsg).

-spec maybe_split_routing_key(binary()) -> {kz_term:api_pid(), binary()}.
maybe_split_routing_key(<<"consumer://", _/binary>> = RoutingKey) ->
    Size = byte_size(RoutingKey),
    {Start, _} = lists:last(binary:matches(RoutingKey, <<"/">>)),
    {list_to_pid(kz_term:to_list(binary:part(RoutingKey, 11, Start - 11)))
    ,binary:part(RoutingKey, Start + 1, Size - Start - 1)
    };
maybe_split_routing_key(RoutingKey) ->
    {'undefined', RoutingKey}.

-spec command(kz_amqp_command()) -> command_ret().
command(#'exchange.declare'{exchange=_Ex, type=_Ty}=Exchange) ->
    kz_amqp_history:add_exchange(Exchange);
command(Command) ->
    %% This will wait forever for a valid channel before publishing...
    %% all commands need to block till completion...
    command(kz_amqp_assignments:get_channel(), Command).

-spec command(kz_amqp_assignment(), kz_amqp_command()) -> command_ret().
command(Assignment, Command) ->
    assert_valid_amqp_method(Command),
    exec_command(Assignment, Command).

-spec exec_command(kz_amqp_assignment(), kz_amqp_command()) -> command_ret().
exec_command(#kz_amqp_assignment{channel=Pid}, #'channel.flow_ok'{}=FlowCtl) ->
    amqp_channel:cast(Pid, FlowCtl);
exec_command(#kz_amqp_assignment{channel=Pid}, #'basic.ack'{}=BasicAck) ->
    amqp_channel:cast(Pid, BasicAck);
exec_command(#kz_amqp_assignment{channel=Channel}=Assignment, #'confirm.select'{}=Command) ->
    Result = amqp_channel:call(Channel, Command),
    handle_command_result(Result, Command, Assignment);
exec_command(#kz_amqp_assignment{channel=Pid}, #'basic.nack'{}=BasicNack) ->
    amqp_channel:cast(Pid, BasicNack);
exec_command(#kz_amqp_assignment{consumer=Consumer
                                ,channel=Channel
                                ,reconnect='true'
                                }
            ,#'basic.consume'{consumer_tag=OldTag}=Command) ->
    C = Command#'basic.consume'{consumer_tag = <<>>},
    case amqp_channel:subscribe(Channel, C, Consumer) of
        #'basic.consume_ok'{consumer_tag=NewTag} ->
            kz_amqp_history:update_consumer_tag(Consumer, OldTag, NewTag);
        _Else ->
            lager:warning("failed to re-establish consumer for ~p: ~p"
                         ,[Consumer, _Else]
                         )
    end;
exec_command(#kz_amqp_assignment{consumer=Consumer
                                ,channel=Channel
                                }=Assignment
            ,#'basic.consume'{queue=Queue}=Command) ->
    case kz_amqp_history:is_consuming(Consumer, Queue) of
        'true' ->
            lager:debug("skipping existing basic consume for queue ~s", [Queue]);
        'false' ->
            Result = amqp_channel:subscribe(Channel, Command, Consumer),
            handle_command_result(Result, Command, Assignment)
    end;
exec_command(#kz_amqp_assignment{channel=Channel
                                ,consumer=Consumer
                                }=Assignment
            ,#'basic.cancel'{nowait=NoWait}) ->
    lists:foreach(fun(#'basic.consume'{consumer_tag=CTag}) ->
                          Command = #'basic.cancel'{consumer_tag=CTag, nowait=NoWait},
                          lager:debug("sending cancel for consumer ~s to ~p", [CTag, Channel]),
                          Result = amqp_channel:call(Channel, Command),
                          handle_command_result(Result, Command, Assignment);
                     (_) -> 'ok'
                  end
                 ,kz_amqp_history:list_consume(Consumer)
                 );
exec_command(#kz_amqp_assignment{channel=Channel
                                ,consumer=Consumer
                                }=Assignment
            ,#'queue.unbind'{queue=QueueName
                            ,exchange=Exchange
                            ,routing_key=RoutingKey
                            }=Command) ->
    case kz_amqp_history:is_bound(Consumer, Exchange, QueueName, RoutingKey) of
        'true' ->
            lager:debug("unbinding ~s from ~s(~s)", [QueueName, Exchange, RoutingKey]),
            handle_command_result(amqp_channel:call(Channel, Command), Command, Assignment);
        'false' ->
            lager:debug("queue ~s is not bound to ~s(~s)", [QueueName, Exchange, RoutingKey])
    end;
exec_command(#kz_amqp_assignment{channel=Channel}=Assignment, Command) ->
    Result = try amqp_channel:call(Channel, Command)
             catch
                 E:R ->
                     lager:debug("amqp command exception ~p : ~p", [E, R]),
                     {'error', R}
             end,
    handle_command_result(Result, Command, Assignment).

-spec handle_command_result(command_ret(), kz_amqp_command(), kz_amqp_assignment()) -> command_ret().
handle_command_result(_, _, #kz_amqp_assignment{reconnect='true'}) -> 'ok';
handle_command_result({'error', _}=Error, _, _) -> Error;
handle_command_result({'ok', Ok}, Command, Assignment) ->
    handle_command_result(Ok, Command, Assignment);
handle_command_result(#'basic.qos_ok'{}
                     ,#'basic.qos'{prefetch_count=Prefetch}=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("applied QOS prefetch ~p to channel ~p", [Prefetch, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'queue.delete_ok'{}
                     ,#'queue.delete'{queue=Q}=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("deleted queue ~s via channel ~p", [Q, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'exchange.declare_ok'{}=Ok
                     ,#'exchange.declare'{passive='true',exchange=Ex}
                     ,#kz_amqp_assignment{channel=Channel}) ->
    lager:debug("passive declared exchange ~s via channel ~p", [Ex, Channel]),
    {'ok', Ok};
handle_command_result(#'exchange.declare_ok'{}=Ok
                     ,#'exchange.declare'{exchange=Ex}
                     ,#kz_amqp_assignment{channel=Channel}) ->
    lager:debug("declared exchanged ~s via channel ~p", [Ex, Channel]),
    {'ok', Ok};
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                     ,#'queue.declare'{passive='true'}
                     ,#kz_amqp_assignment{channel=Channel}) ->
    lager:debug("passive declared queue ~s via channel ~p", [Q, Channel]),
    {'ok', Ok};
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                     ,#'queue.declare'{}=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("declared queue ~s via channel ~p", [Q, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command#'queue.declare'{queue=Q}),
    {'ok', Ok};
handle_command_result(#'queue.unbind_ok'{}
                     ,#'queue.unbind'{exchange=Exchange
                                     ,routing_key=RoutingKey
                                     ,queue=Queue
                                     }=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p"
               ,[Queue, Exchange, RoutingKey, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command, 'sync'),
    'ok';
handle_command_result(#'queue.bind_ok'{}
                     ,#'queue.bind'{exchange=_Exchange
                                   ,routing_key=_RK
                                   ,queue=_Q
                                   }=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p"
               ,[_Q, _Exchange, _RK, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}
                     ,#'basic.consume'{}=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command#'basic.consume'{consumer_tag=CTag}),
    'ok';
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                     ,#'basic.cancel'{}=Command
                     ,#kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'confirm.select_ok'{}
                     ,Command
                     ,#kz_amqp_assignment{channel=Channel
                                         ,consumer=Consumer
                                         }=Assignment) ->
    lager:debug("publisher confirms activated on channel ~p", [Channel]),
    Consumer ! {'$server_confirms', 'true'},
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(#'channel.flow_ok'{active=Active}
                     ,Command
                     ,#kz_amqp_assignment{channel=Channel
                                         ,consumer=Consumer
                                         }=Assignment) ->
    lager:debug("channel flow ~p on channel ~p", [Active, Channel]),
    Consumer ! {'$channel_flow', Active},
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result('ok', Command, #kz_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("executed AMQP command ~s with no_wait option via channel ~p"
               ,[element(1, Command), Channel]),
    _ = kz_amqp_history:add_command(Assignment, Command),
    'ok';
handle_command_result(_Else, _R, _) ->
    lager:warning("unexpected AMQP command result: ~p"
                 ,[lager:pr(_Else, ?MODULE)]),
    {'error', 'unexpected_result'}.

-spec assert_valid_amqp_method(kz_amqp_command()) -> 'ok'.
assert_valid_amqp_method(Command) ->
    try rabbit_framing_amqp_0_9_1:encode_method_fields(Command)
    catch
        E:R ->
            lager:warning("~s when encoding method ~p: ~p", [E, Command, R]),
            E(R)
    end,
    'ok'.
