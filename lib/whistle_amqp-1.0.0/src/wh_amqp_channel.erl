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
-export([new/0
         ,new/1
        ]).
-export([open/2]).
-export([close/0
         ,close/1
        ]).
-export([remove/0
         ,remove/1
        ]).
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

-spec consumer_pid(pid()) -> 'undefined' | pid().
consumer_pid(Pid) when is_pid(Pid) -> put('$wh_amqp_consumer', Pid).

-spec remove_consumer_pid() -> 'undefined'.
remove_consumer_pid() ->
    put('$wh_amqp_consumer', 'undefined').

-spec new() -> wh_amqp_channel() | {'error', _}.
new() -> new(wh_amqp_channels:new()).

-spec new(wh_amqp_channel()) -> wh_amqp_channel() | {'error', _}.
new(#wh_amqp_connection{}=Connection) ->
    open(wh_amqp_channels:new(), Connection);
new(#wh_amqp_channel{}=Channel) ->
    case wh_amqp_connections:current() of
        {error, _}=E -> E;
        {ok, Connection} ->
            open(Channel, Connection)
    end.

-spec close() -> wh_amqp_channel().
close() ->
    case wh_amqp_channels:find() of
        {error, _} -> #wh_amqp_channel{};
        #wh_amqp_channel{}=Channel -> close(Channel)
    end.

-spec close(wh_amqp_channel()) -> wh_amqp_channel().
close(#wh_amqp_channel{channel=Pid
                       ,commands=[#'basic.consume'{consumer_tag=CTag}
                                  |Commands
                                 ]
                      }=Channel) when is_pid(Pid) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    catch amqp_channel:call(Pid, #'basic.cancel'{consumer_tag=CTag}),
    close(Channel#wh_amqp_channel{commands=Commands});
close(#wh_amqp_channel{channel=Pid
                       ,commands=[#'queue.declare'{queue=Queue}
                                  |Commands
                                 ]
                      }=Channel) when is_pid(Pid) ->
    lager:debug("removed queue ~s via channel ~p", [Queue, Pid]),
    catch amqp_channel:call(Pid, #'queue.delete'{queue=Queue, nowait='true'}),
    close(Channel#wh_amqp_channel{commands=Commands});
close(#wh_amqp_channel{commands=[_|Commands]}=Channel) ->
    close(Channel#wh_amqp_channel{commands=Commands});
close(#wh_amqp_channel{channel=Pid, uri=URI}=Channel) when is_pid(Pid) ->
    lager:debug("closed channel ~p on ~s", [Pid, URI]),
    C = wh_amqp_channels:demonitor_channel(Channel),
    catch amqp_channel:close(Pid),
    C;
close(Channel) ->
    Channel.

-spec remove() -> 'ok'.
remove() ->
    case wh_amqp_channels:find() of
        {'error', _} -> 'ok';
        #wh_amqp_channel{}=Channel ->
            remove(Channel)
    end.

-spec remove(wh_amqp_channel()) -> 'ok'.
remove(#wh_amqp_channel{consumer_ref=Ref}=Channel) when is_reference(Ref) ->
    remove(wh_amqp_channels:demonitor_consumer(Channel));
remove(#wh_amqp_channel{channel=Pid}=Channel) when is_pid(Pid) ->
    remove(close(Channel));
remove(#wh_amqp_channel{}=Channel) ->
    wh_amqp_channels:remove(Channel).

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
            lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p", [_Exchange, URI, _RK, Pid]);
        #wh_amqp_channel{} ->
            wh_amqp_channels:reconnect(),
            timer:sleep(100),
            retry_publish(BasicPub, AmqpMsg)
    end.

-spec retry_publish(#'basic.publish'{}, #'amqp_msg'{}) -> 'ok'.
retry_publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case wh_amqp_channels:get_channel() of
        #wh_amqp_channel{channel=Pid, uri=URI} when is_pid(Pid) ->
            amqp_channel:call(Pid, BasicPub, AmqpMsg),
            lager:debug("published to ~s(~s) exchange (routing key ~s) via ~p", [_Exchange, URI, _RK, Pid]);
        #wh_amqp_channel{uri=URI} ->
            lager:debug("dropping payload to ~s(~s) exchange (routing key ~s): ~s", [_Exchange, URI, _RK, AmqpMsg#'amqp_msg'.payload])
    end.

-spec command(wh_amqp_command()) -> command_ret().
command(Command) ->
    case wh_amqp_channels:get_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{}=Channel -> command(Channel, Command)
    end.

-spec command(wh_amqp_channel(), wh_amqp_command()) -> command_ret().
command(#wh_amqp_channel{channel=Pid}, #'basic.ack'{}=BasicAck) ->
    amqp_channel:cast(Pid, BasicAck);
command(#wh_amqp_channel{channel=Pid}, #'basic.nack'{}=BasicNack) ->
    amqp_channel:cast(Pid, BasicNack);
command(#wh_amqp_channel{consumer=Consumer
                         ,channel=Pid
                         ,commands=Commands
                        }=Channel
        ,#'basic.consume'{}=Command) ->
    case lists:member(#'basic.consume'{}, Commands) of
        true -> ok;
        false ->
            Result = amqp_channel:subscribe(Pid, Command, Consumer),
            handle_command_result(Result, Command, Channel)
    end;
command(#wh_amqp_channel{channel=Pid
                         ,commands=Commands
                        }=Channel
        ,#'basic.cancel'{nowait=NoWait}) ->
    lists:foreach(fun(#'basic.consume'{consumer_tag=CTag}) ->
                          Command = #'basic.cancel'{consumer_tag=CTag, nowait=NoWait},
                          Result = amqp_channel:call(Pid, Command),
                          handle_command_result(Result, Command, Channel);
                     (_) ->
                          ok
                  end, Commands);
command(_, #'exchange.declare'{exchange=_Ex, type=_Ty}=Command) ->
    lager:debug("declared ~s exchange ~s", [_Ty, _Ex]),
    _ = wh_amqp_connections:declare_exchange(Command),
    ok;
command(#wh_amqp_channel{channel=Pid}=Channel, Command) ->
    Result = amqp_channel:call(Pid, Command),
    handle_command_result(Result, Command, Channel).

-spec handle_command_result(command_ret(), wh_amqp_command(), wh_amqp_channel()) -> command_ret().
handle_command_result({error, _}=Error, _, _) ->
    Error;
handle_command_result({ok, Ok}, Command, Channel) ->
    handle_command_result(Ok, Command, Channel);
handle_command_result(#'basic.qos_ok'{}
                      ,#'basic.qos'{prefetch_count=Prefetch}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("applied QOS prefetch ~p to channel ~p", [Prefetch, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'queue.delete_ok'{}
                      ,#'queue.delete'{queue=Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("deleted queue ~s via channel ~p", [Q, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("declared queue ~s via channel ~p", [Q, Pid]),
    _ = wh_amqp_channels:command(Channel, Command#'queue.declare'{queue=Q}),
    {ok, Ok};
handle_command_result(#'queue.unbind_ok'{}
                      ,#'queue.unbind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'queue.bind_ok'{}
                      ,#'queue.bind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}
                      ,#'basic.consume'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Pid]),
    _ = wh_amqp_channels:command(Channel, Command#'basic.consume'{consumer_tag=CTag}),
    ok;
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                      ,#'basic.cancel'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    _ = wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(_Else, _, _) ->
    lager:warning("unexpected AMQP command result: ~p", [_Else]),
    {error, unexpected_result}.

-spec open(wh_amqp_channel() | pid(), wh_amqp_connection()) -> wh_amqp_channel().
open(#wh_amqp_channel{consumer=Consumer, reconnecting=Reconnecting}=Channel
     ,#wh_amqp_connection{uri=URI}=Connection) ->
    case wh_amqp_connection:get_channel(Connection) of
        {ok, Pid} ->
            gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', Reconnecting}}),
            amqp_channel:register_return_handler(Pid, Channel#wh_amqp_channel.consumer),
            Routines = [fun(C) ->
                                C#wh_amqp_channel{channel=Pid
                                                  ,uri=URI}
                        end
                        ,fun(C) -> wh_amqp_channels:monitor_channel(C) end
                        ,fun(C) -> wh_amqp_channels:monitor_consumer(C) end
                        ,fun(#wh_amqp_channel{commands=Commands}=C) ->
                                 maybe_reestablish(C#wh_amqp_channel{commands=lists:reverse(Commands)
                                                                     ,reconnecting=true
                                                                    })
                         end
                       ],
            lager:debug("create channel ~p for ~p on ~s", [Pid, Channel#wh_amqp_channel.consumer, URI]),
            lists:foldl(fun(F, C) -> F(C) end, Channel, Routines);
        {error, _R} ->
            close(Channel)
    end;
open(Channel, _) ->
    lager:critical("failed to open AMQP channel: no_connection", []),
    close(Channel).

-spec maybe_reestablish(wh_amqp_channel()) -> wh_amqp_channel().
maybe_reestablish(#wh_amqp_channel{commands=[]}=Channel) ->
    Channel#wh_amqp_channel{reconnecting=false};
maybe_reestablish(#wh_amqp_channel{commands=[Command|Commands]}=Channel) ->
    _ = command(Channel#wh_amqp_channel{commands=[]}, Command),
    maybe_reestablish(Channel#wh_amqp_channel{commands=Commands}).
