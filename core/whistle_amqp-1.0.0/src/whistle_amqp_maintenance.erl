%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(whistle_amqp_maintenance).

-include("amqp_util.hrl").

-export([connection_summary/0]).
-export([connection_details/0
         ,connection_details/1
        ]).
-export([channel_summary/0]).
-export([channel_details/1]).

connection_summary() ->
    _ = case wh_amqp_connections:all() of
            [] ->
                io:format("No AMQP connections found!~n", []);
            Connections ->
                io:format("+----------------------------------------------------+-----------+--------------+---------+------------+~n"),
                io:format("| URI                                                | Available | Crossconnect | Current |  Uptime(s) |~n"),
                io:format("+====================================================+===========+==============+=========+============+~n"),
                _ = print_connection_summary(Connections, current_uri()),
                io:format("+----------------------------------------------------+-----------+--------------+---------+------------+~n"),
                io:format("Found ~p connections~n", [length(Connections)])            
        end,
    'no_return'.

connection_details() ->
    _ = case wh_amqp_connections:all() of
            [] -> io:format("No AMQP connections found!~n", []);
            Connections -> print_connection_details(Connections)
        end,
    'no_return'.

connection_details(URI) ->    
    _  =case wh_amqp_connections:find(URI) of
            {'error', 'not_found'} ->
                io:format("No AMQP connections found!~n", []);
            Connection -> print_connection_details([Connection])
        end,
    'no_return'.

channel_summary() ->
    _ = case wh_amqp_channels:all() of
            [] ->
                io:format("No AMQP channels found!~n", []);
            Channels ->
                io:format("+------------+------------+------------+----------------------------------------------------+~n"),
                io:format("|  Channel   | Consumer   |  Uptime(s) | URI                                                |~n"),
                io:format("+============+============+============+====================================================+~n"),
                _ = print_channel_summary(Channels),
                io:format("+------------+------------+------------+----------------------------------------------------+~n"),
                io:format("Found ~p channels~n", [length(Channels)])
        end,
    'no_return'.

channel_details(Pid) ->
    P = list_to_pid(wh_util:to_list(Pid)),
    _ = case wh_amqp_channels:find(P) of
            {'error', 'no_channel'} ->
                io:format("No AMQP channels found!~n", []);
            Channel -> print_channel_details([Channel])
        end,
    'no_return'.

current_uri() ->
    case wh_amqp_connections:current() of
        {'ok', #wh_amqp_connection{uri=URI}} ->
            URI;
        _Else -> 'undefined'
    end.

print_connection_summary([], _) -> 'ok';
print_connection_summary([#wh_amqp_connection{uri=URI
                                              ,available=Available
                                              ,crossconnect=Crossconnect
                                              ,started=Started}
                          |Connections
                         ], CurrentURI) -> 
    io:format("| ~-50s | ~-9s | ~-12s | ~-7s | ~-10w |~n"
              ,[URI
                ,Available
                ,Crossconnect
                ,URI =:= CurrentURI
                ,maybe_elapsed_s(Started)
               ]),
    print_connection_summary(Connections, CurrentURI).

print_connection_details([]) -> 'ok';
print_connection_details([#wh_amqp_connection{manager=Manager
                                              ,exchanges=Exchanges}
                          |Connections
                         ]) ->
    #wh_amqp_connection{uri=URI
                        ,params=Params
                        ,connection=Connection
                        ,connection_ref=Ref
                        ,control_channel=ControlChannel
                        ,available=Available
                        ,crossconnect=Crossconnect
                        ,prechannels=Prechannels
                        ,started=Started}
        = wh_amqp_connection:get_connection(Manager),
    io:format("~-19s: ~s~n", ["URI", URI]),
    io:format("~-19s: ~p~n", ["Connection", Connection]),
    io:format("~-19s: ~p~n", ["Reference", Ref]),
    io:format("~-19s: ~p~n", ["Control", ControlChannel]),
    io:format("~-19s: ~s~n", ["Available", Available]),
    io:format("~-19s: ~s~n", ["Crossconnect", Crossconnect]), 
    io:format("~-19s: ~p~n", ["Uptime(s)", maybe_elapsed_s(Started)]),
    io:format("~-19s: ~p~n", ["Prechannels", length(Prechannels)]),
    io:format("Exchanges:~n"),
    _ = [io:format("  ~s~n", [Exchange])
         || #'exchange.declare'{exchange=Exchange} <- Exchanges
        ],
    io:format("~n", []),
    print_connection_details(Connections).


print_channel_summary([]) -> 'ok';
print_channel_summary([#wh_amqp_channel{uri=URI
                                  ,consumer=Consumer
                                  ,channel=Channel
                                  ,started=Started}
                 |Channels
                ]) ->
    io:format("| ~-10s | ~-10s | ~-10w | ~-50s |~n"
              ,[maybe_pid_to_list(Channel)
                ,maybe_pid_to_list(Consumer)
                ,maybe_elapsed_s(Started)
                ,URI
               ]),
    print_channel_summary(Channels).

print_channel_details([]) -> 'ok';
print_channel_details([#wh_amqp_channel{uri=URI
                                        ,consumer=Consumer
                                        ,consumer_ref=ConsumerRef
                                        ,channel=Channel
                                        ,channel_ref=ChannelRef
                                        ,started=Started
                                        ,commands=Commands
                                        ,reconnecting=Reconnecting}
                       | Channels
                      ]) ->
    io:format("~-19s: ~s~n", ["URI", URI]),
    io:format("~-19s: ~p~n", ["Consumer", Consumer]),
    io:format("~-19s: ~p~n", ["Consumer Ref", ConsumerRef]),
    io:format("~-19s: ~p~n", ["Channel", Channel]),
    io:format("~-19s: ~p~n", ["Channel Ref", ChannelRef]),
    io:format("~-19s: ~p~n", ["Reconnecting", Reconnecting]), 
    io:format("~-19s: ~p~n", ["Uptime(s)", maybe_elapsed_s(Started)]),
    io:format("Commands:~n", []),
    _ = [print_command(Command) || Command <- lists:reverse(Commands)],
    print_channel_details(Channels).

maybe_pid_to_list(Pid) when is_pid(Pid) -> pid_to_list(Pid);
maybe_pid_to_list(_) -> "".

maybe_elapsed_s('undefined') -> 0;
maybe_elapsed_s(Tstamp) -> wh_util:elapsed_s(Tstamp).

print_command(#'exchange.declare'{exchange=Exchange
                                  ,type=Type
                                  ,passive=Passive
                                  ,durable=Durable
                                  ,auto_delete=AutoDelete
                                  ,internal=Internal
                                  ,nowait=NoWait
                                 }) ->
    io:format("  declare exchange~n", []),
    print_command_args([{"exchange", Exchange}
                        ,{"type", Type}
                        ,{"passive", Passive}
                        ,{"durable", Durable}
                        ,{"auto-delete", AutoDelete}
                        ,{"internal", Internal}
                        ,{"no-wait", NoWait}
                       ]);
print_command(#'queue.declare'{queue=Queue
                               ,passive=Passive
                               ,durable=Durable
                               ,exclusive=Exclusive
                               ,auto_delete=AutoDelete
                               ,nowait=NoWait
                              }) ->
    io:format("  declare queue~n", []),
    print_command_args([{"name", Queue}
                        ,{"passive", Passive}
                        ,{"durable", Durable}
                        ,{"auto-delete", AutoDelete}
                        ,{"exclusive", Exclusive}
                        ,{"no-wait", NoWait}
                       ]);
print_command(#'queue.delete'{queue=Queue
                              ,if_unused=IfUnused
                              ,if_empty=IfEmpty
                              ,nowait=NoWait
                             }) ->
    io:format("  delete queue~n", []),
    print_command_args([{"name", Queue}
                        ,{"if-unused", IfUnused}
                        ,{"if-empty", IfEmpty}
                        ,{"no-wait", NoWait}
                       ]);
print_command(#'queue.bind'{queue=Queue
                            ,exchange=Exchange
                            ,routing_key=Routing
                            ,nowait=NoWait
                           }) ->
    io:format("  bind queue~n", []),
    print_command_args([{"name", Queue}
                        ,{"exchange", Exchange}
                        ,{"routing-key", Routing}
                        ,{"no-wait", NoWait}
                       ]);
print_command(#'queue.unbind'{queue=Queue
                              ,exchange=Exchange
                              ,routing_key=Routing
                             }) ->
    io:format("  unbind queue~n", []),
    print_command_args([{"name", Queue}
                        ,{"exchange", Exchange}
                        ,{"routing-key", Routing}
                       ]);
print_command(#'basic.consume'{queue=Queue
                               ,consumer_tag=Tag
                               ,no_local=NoLocal
                               ,no_ack=NoAck
                               ,exclusive=Exclusive
                               ,nowait=NoWait
                              }) ->
    io:format("  comsume queue~n", []),
    print_command_args([{"name", Queue}
                        ,{"tag", Tag}
                        ,{"no-local", NoLocal}
                        ,{"no-ack", NoAck}
                        ,{"exclusive", Exclusive}
                        ,{"no-wait", NoWait}
                       ]);
print_command(#'basic.cancel'{}) ->
    io:format("  basic cancel~n", []);
print_command(#'basic.qos'{prefetch_count = PreFetch}) ->
    io:format("  set prefetch count to ~p~n", [PreFetch]);
print_command(Else) ->
    io:format("  unknown command:~n    ~p~n", [Else]).

print_command_args(Args) ->
    io:format("    ", []),
    print_command_args(Args, 0).

print_command_args([], _) -> io:format("~n", []);
print_command_args(Args, Count) when Count >= 3 ->
    io:format("~n    ", []),
    print_command_args(Args, 0);
print_command_args([{Key, Value}|Args], Count) -> 
    Arg = <<(wh_util:to_binary(Key))/binary
               ,": "
               ,(wh_util:to_binary(Value))/binary>>,
    case size(Arg) =< 20 of
        'true' ->
            io:format("~-20s", [Arg]),
            print_command_args(Args, Count + 1);
        'false' when Count > 0 ->
            io:format("~n    ~s~n    ", [Arg]),
            print_command_args(Args, 0);
        'false' ->
            io:format("~s~n    ", [Arg]),
            print_command_args(Args, 0)
    end.
