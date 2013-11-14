%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
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
-export([open/1]).
-export([close/1]).
-export([maybe_publish/2]).
-export([publish/2]).
-export([command/1
         ,command/2
        ]).

-include("amqp_util.hrl").

-spec consumer_pid() -> pid().
consumer_pid() ->
    case get('$wh_amqp_consumer') of
        Pid when is_pid(Pid) -> Pid;
        _Else -> self()
    end.

-spec consumer_pid(pid()) -> api_pid().
consumer_pid(Pid) when is_pid(Pid) -> put('$wh_amqp_consumer', Pid).

-spec remove_consumer_pid() -> 'undefined'.
remove_consumer_pid() -> put('$wh_amqp_consumer', 'undefined').

-spec open(wh_amqp_channel()) -> wh_amqp_channel().
open(#wh_amqp_channel{}=Channel) ->
    _ = notify_consumer(Channel),
    _ = maybe_reestablish(Channel),
    Channel#wh_amqp_channel{reconnecting='false'}.

-spec close(wh_amqp_channel()) -> wh_amqp_channel().
close(#wh_amqp_channel{commands=[_|_]
                       ,channel=Pid}=Channel)
  when is_pid(Pid) ->
    _ = cancel_consumers(Channel),
    _ = cancel_queues(Channel),
    close(Channel#wh_amqp_channel{commands=[]});
close(#wh_amqp_channel{channel=Pid, uri=URI}=Channel) when is_pid(Pid) ->
    lager:debug("closed channel ~p on ~s", [Pid, URI]),
    catch gen_server:call(Pid, {'close', 200, <<"Goodbye">>}, 5000),
    Channel#wh_amqp_channel{channel='undefined'};
close(Channel) -> Channel.

%% maybe publish will only publish a message if the requestor already has a channel
-spec maybe_publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
maybe_publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub
              ,#'amqp_msg'{props=#'P_basic'{timestamp='undefined'}=Props}=AmqpMsg
             ) ->
    Now = wh_util:now_us(now()),
    maybe_publish(BasicPub, AmqpMsg#'amqp_msg'{props=Props#'P_basic'{timestamp=Now}});
maybe_publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case wh_amqp_channels:find() of
        #wh_amqp_channel{channel=Pid, uri=URI} when is_pid(Pid) ->
            amqp_channel:call(Pid, BasicPub, AmqpMsg),
            whistle_stats:increment_counter(<<"amqp-request">>),
            lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p"
                        ,[_Exchange, URI, _RK, Pid]);
        _Else ->
            lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
                        ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload])
    end.

%% publish will attempt to create or reconnect a channel if necessary prior to publish
-spec publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub
        ,#'amqp_msg'{props=#'P_basic'{timestamp='undefined'}=Props}=AmqpMsg
       ) ->
    Now = wh_util:now_us(now()),
    publish(BasicPub, AmqpMsg#'amqp_msg'{props=Props#'P_basic'{timestamp=Now}});
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case wh_amqp_channels:get_channel() of
        #wh_amqp_channel{channel=Pid, uri=URI} when is_pid(Pid) ->
            amqp_channel:call(Pid, BasicPub, AmqpMsg),
            whistle_stats:increment_counter(<<"amqp-request">>),
            lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p"
                        ,[_Exchange, URI, _RK, Pid]);
        #wh_amqp_channel{uri=URI} ->
            lager:debug("dropping payload to ~s(~s) exchange (routing key ~s): ~s"
                        ,[_Exchange, URI, _RK, AmqpMsg#'amqp_msg'.payload])
    end.

-spec command(wh_amqp_command()) -> command_ret().
command(Command) ->
    #wh_amqp_channel{}=Channel = wh_amqp_channels:get_channel(),
    command(Channel, Command).

-spec command(wh_amqp_channel(), wh_amqp_command()) -> command_ret().
command(#wh_amqp_channel{channel=Pid}, #'basic.ack'{}=BasicAck) ->
    amqp_channel:cast(Pid, BasicAck);
command(#wh_amqp_channel{channel=Pid}, #'basic.nack'{}=BasicNack) ->
    amqp_channel:cast(Pid, BasicNack);
command(#wh_amqp_channel{consumer=Consumer
                         ,channel=Pid
                         ,commands=Commands
                        }=Channel
        ,#'basic.consume'{queue=Q}=Command) ->
    case lists:any(fun(#'basic.consume'{queue=Queue}) ->
                           Queue =:= Q;
                      (_) -> 'false'
                   end, Commands)
    of
        'true' ->
            lager:debug("skipping existing basic consume for queue ~s", [Q]),
            'ok';
        'false' ->
            Result = amqp_channel:subscribe(Pid, Command, Consumer),
            handle_command_result(Result, Command, Channel)
    end;
command(#wh_amqp_channel{channel=Pid
                         ,commands=Commands
                        }=Channel
        ,#'basic.cancel'{nowait=NoWait}) ->
    lists:foreach(fun(#'basic.consume'{consumer_tag=CTag}) ->
                          Command = #'basic.cancel'{consumer_tag=CTag, nowait=NoWait},
                          lager:debug("sending cancel for consumer ~s to ~p", [CTag, Pid]),
                          Result = amqp_channel:call(Pid, Command),
                          handle_command_result(Result, Command, Channel);
                     (_) -> 'ok'
                  end, Commands);
command(_, #'exchange.declare'{exchange=_Ex, type=_Ty}=Command) ->
    lager:debug("declared ~s exchange ~s", [_Ty, _Ex]),
    _ = wh_amqp_connections:declare_exchange(Command),
    'ok';
command(#wh_amqp_channel{channel=Pid}=Channel, Command) ->
    Result = amqp_channel:call(Pid, Command),
    handle_command_result(Result, Command, Channel).

-spec handle_command_result(command_ret(), wh_amqp_command(), wh_amqp_channel()) -> command_ret().
handle_command_result({'error', _}=Error, _, _) -> 
    whistle_stats:increment_counter(<<"amqp_error">>),
    Error;
handle_command_result({'ok', Ok}, Command, Channel) ->
    handle_command_result(Ok, Command, Channel);
handle_command_result(#'basic.qos_ok'{}
                      ,#'basic.qos'{prefetch_count=Prefetch}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("applied QOS prefetch ~p to channel ~p", [Prefetch, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    'ok';
handle_command_result(#'queue.delete_ok'{}
                      ,#'queue.delete'{queue=Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("deleted queue ~s via channel ~p", [Q, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    'ok';
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{passive='true'}
                      ,#wh_amqp_channel{channel=Pid}) ->
    lager:debug("passive declared queue ~s via channel ~p", [Q, Pid]),
    {'ok', Ok};
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("declared queue ~s via channel ~p", [Q, Pid]),
    _ = wh_amqp_channels:command(Channel, Command#'queue.declare'{queue=Q}),
    {'ok', Ok};
handle_command_result(#'queue.unbind_ok'{}
                      ,#'queue.unbind'{exchange=_Exchange
                                       ,routing_key=_RK
                                       ,queue=_Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    'ok';
handle_command_result(#'queue.bind_ok'{}
                      ,#'queue.bind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    'ok';
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}
                      ,#'basic.consume'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Pid]),
    _ = wh_amqp_channels:command(Channel, Command#'basic.consume'{consumer_tag=CTag}),
    'ok';
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                      ,#'basic.cancel'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    'ok';
handle_command_result('ok', Command, #wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("executed AMQP command ~s with no_wait option via channel ~p", [element(1, Command), Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    'ok';
handle_command_result(_Else, _R, _) ->
    whistle_stats:increment_counter(<<"amqp_error">>),
    lager:warning("unexpected AMQP command result: ~p", [_R]),
    {'error', 'unexpected_result'}.

-spec notify_consumer(wh_amqp_channel()) -> 'ok'.
notify_consumer(#wh_amqp_channel{consumer=Consumer
                                 ,reconnecting=Reconnecting}) ->
    gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', Reconnecting}}).

-spec maybe_reestablish(wh_amqp_channel()) -> 'ok' | pid().
maybe_reestablish(#wh_amqp_channel{commands=[]}) -> 'ok';
maybe_reestablish(#wh_amqp_channel{commands=Commands}=Channel) ->
    spawn(fun() -> 
                  reestablish(Channel#wh_amqp_channel{commands=lists:reverse(Commands)
                                                      ,reconnecting='true'
                                                     })
          end).

-spec reestablish(wh_amqp_channel()) -> 'ok'.
reestablish(#wh_amqp_channel{commands=[]}) -> 'ok';
reestablish(#wh_amqp_channel{commands=[Command|Commands]}=Channel) ->
    catch command(Channel#wh_amqp_channel{commands=[]}, Command),
    reestablish(Channel#wh_amqp_channel{commands=Commands}).

-spec cancel_consumers(wh_amqp_channel()) -> 'ok'.
cancel_consumers(#wh_amqp_channel{commands=[]}) -> 'ok';
cancel_consumers(#wh_amqp_channel{channel=Pid
                                  ,commands=[#'basic.consume'{consumer_tag=CTag}
                                             |Commands
                                            ]
                                 }=Channel) when is_pid(Pid) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    catch amqp_channel:call(Pid, #'basic.cancel'{consumer_tag=CTag, nowait='true'}),
    cancel_consumers(Channel#wh_amqp_channel{commands=Commands});
cancel_consumers(#wh_amqp_channel{commands=[_|Commands]}=Channel) ->
    cancel_consumers(Channel#wh_amqp_channel{commands=Commands}).

-spec cancel_queues(wh_amqp_channel()) -> 'ok'.
cancel_queues(#wh_amqp_channel{commands=[]}) -> 'ok';
cancel_queues(#wh_amqp_channel{channel=Pid
                               ,commands=[#'queue.declare'{queue=Queue}
                                          |Commands
                                         ]
                              }=Channel) when is_pid(Pid) ->
    lager:debug("removed queue ~s via channel ~p", [Queue, Pid]),
    catch amqp_channel:call(Pid, #'queue.delete'{queue=Queue
                                                 ,if_unused='true'
                                                 ,nowait='true'
                                                }),
    cancel_queues(Channel#wh_amqp_channel{commands=Commands});
cancel_queues(#wh_amqp_channel{commands=[_|Commands]}=Channel) ->
    cancel_queues(Channel#wh_amqp_channel{commands=Commands}).
