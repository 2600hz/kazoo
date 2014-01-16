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
-export([remove/0
         ,remove/1
        ]).
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

-spec remove() -> 'ok'.
remove() -> 
    remove(consumer_pid()).

-spec remove(pid()) -> 'ok'.
remove(Pid) ->
    _ = wh_amqp_history:remove(Pid),
    _ = wh_amqp_assignments:remove(Pid),
    'ok'.
    
-spec close(api_pid()) -> 'ok'.
close(Channel) when is_pid(Channel) ->
    catch gen_server:call(Channel, {'close', 200, <<"Goodbye">>}, 5000),
    lager:debug("closed channel ~p", [Channel]),
    'ok';
close(_) -> 'ok'.

%% maybe publish will only publish a message if the requestor already has a channel
-spec maybe_publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
maybe_publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case wh_amqp_assignments:find() of
        #wh_amqp_assignment{channel=Channel, broker=_Broker} when is_pid(Channel) ->
            amqp_channel:call(Channel, BasicPub, AmqpMsg),
            lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p"
                        ,[_Exchange, _Broker, _RK, Channel]);
        _Else ->
            lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
                        ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload])
    end.

%% publish will attempt to create or reconnect a channel if necessary prior to publish
-spec publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case wh_amqp_assignments:request_float() of
        #wh_amqp_assignment{channel=Channel, broker=_Broker} when is_pid(Channel) ->
            amqp_channel:call(Channel, BasicPub, AmqpMsg),
            lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p"
                        ,[_Exchange, _Broker, _RK, Channel]);
        _Else ->
            lager:debug("dropping payload to ~s exchange (routing key ~s): ~s"
                        ,[_Exchange, _RK, AmqpMsg#'amqp_msg'.payload])
    end.

-spec command(wh_amqp_command()) -> command_ret(). command(Command) ->
    #wh_amqp_assignment{}=Assignment = wh_amqp_assignments:request_float(),
    command(Assignment, Command).

-spec command(wh_amqp_assignment(), wh_amqp_command()) -> command_ret().
command(#wh_amqp_assignment{channel=Pid}, #'basic.ack'{}=BasicAck) ->
    amqp_channel:cast(Pid, BasicAck);
command(#wh_amqp_assignment{channel=Pid}, #'basic.nack'{}=BasicNack) ->
    amqp_channel:cast(Pid, BasicNack);
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
                  end, wh_amqp_history:basic_consumers(Consumer));
command(_, #'exchange.declare'{exchange=_Ex, type=_Ty}) ->
    lager:debug("declared ~s exchange ~s", [_Ty, _Ex]),
    %% TODO: shouldn't this be declaring exchanges?
    'ok';
command(#wh_amqp_assignment{channel=Channel}=Assignment, Command) ->
    Result = amqp_channel:call(Channel, Command),
    handle_command_result(Result, Command, Assignment).

-spec handle_command_result(command_ret(), wh_amqp_command(), wh_amqp_assignment()) -> command_ret().
handle_command_result({'error', _}=Error, _, _) -> Error;
handle_command_result({'ok', Ok}, Command, Assignment) ->
    handle_command_result(Ok, Command, Assignment);
handle_command_result(#'basic.qos_ok'{}
                      ,#'basic.qos'{prefetch_count=Prefetch}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("applied QOS prefetch ~p to channel ~p", [Prefetch, Channel]),
    _ = wh_amqp_history:command(Assignment, Command),
    'ok';
handle_command_result(#'queue.delete_ok'{}
                      ,#'queue.delete'{queue=Q}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("deleted queue ~s via channel ~p", [Q, Channel]),
    _ = wh_amqp_history:command(Assignment, Command),
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
    _ = wh_amqp_history:command(Assignment, Command#'queue.declare'{queue=Q}),
    {'ok', Ok};
handle_command_result(#'queue.unbind_ok'{}
                      ,#'queue.unbind'{exchange=_Exchange
                                       ,routing_key=_RK
                                       ,queue=_Q}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p"
                ,[_Q, _Exchange, _RK, Channel]),
    _ = wh_amqp_history:command(Assignment, Command),
    'ok';
handle_command_result(#'queue.bind_ok'{}
                      ,#'queue.bind'{exchange=_Exchange
                                     ,routing_key=_RK
                                     ,queue=_Q}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p"
                ,[_Q, _Exchange, _RK, Channel]),
    _ = wh_amqp_history:command(Assignment, Command),
    'ok';
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}
                      ,#'basic.consume'{}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Channel]),
    _ = wh_amqp_history:command(Assignment, Command#'basic.consume'{consumer_tag=CTag}),
    'ok';
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                      ,#'basic.cancel'{}=Command
                      ,#wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Channel]),
    _ = wh_amqp_history:command(Assignment, Command),
    'ok';
handle_command_result('ok', Command, #wh_amqp_assignment{channel=Channel}=Assignment) ->
    lager:debug("executed AMQP command ~s with no_wait option via channel ~p"
                ,[element(1, Command), Channel]),
    _ = wh_amqp_history:command(Assignment, Command),
    'ok';
handle_command_result(_Else, _R, _) ->
    lager:warning("unexpected AMQP command result: ~p", [_R]),
    {'error', 'unexpected_result'}.
