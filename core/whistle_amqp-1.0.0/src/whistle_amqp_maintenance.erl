%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(whistle_amqp_maintenance).

-export([add_broker/1
         ,add_broker/2
        ]).
-export([remove_broker/1]).
-export([add_connection/1
         ,add_connection/2
        ]).
-export([primary_broker/0]).
-export([validate_assignments/0]).
-export([connection_summary/0]).
-export([broker_summary/0]).
-export([channel_summary/0]).
-export([consumer_details/0
         ,consumer_details/1
         ,consumer_details/3
        ]).

-include("amqp_util.hrl").

-define(ASSIGNMENTS, 'wh_amqp_assignments').
-define(CONNECTIONS, 'wh_amqp_connections').

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add_broker(Broker) ->
    add_broker(Broker, 'local').

add_broker(Broker, Zone) when not is_binary(Broker) ->
    add_broker(wh_util:to_binary(Broker), Zone);
add_broker(Broker, Zone) when not is_atom(Zone) ->
    add_broker(Broker, wh_util:to_atom(Zone, 'true'));
add_broker(Broker, Zone) ->
    case wh_amqp_connections:new(Broker, Zone) of
        {'error', 'exists'} ->
            io:format("ERROR: broker ~s currently has ~p connection(s) of which ~p is/are available~n"
                      ,[Broker
                        ,wh_amqp_connections:broker_connections(Broker)
                        ,wh_amqp_connections:broker_available_connections(Broker)
                       ]),
            io:format("~nTo add more connections use:~n sup whistle_amqp_maintenance add_connection ~s~n"
                      ,[Broker]),
            'no_return';
        {'error', Reason} ->
            io:format("ERROR: unable to add broker ~s: ~p~n"
                      ,[Broker, Reason]),
            'no_return';
        #wh_amqp_connection{} -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
remove_broker(Broker) when not is_binary(Broker) ->
    remove_broker(wh_util:to_binary(Broker));
remove_broker(Broker) ->
   wh_amqp_connections:remove(Broker).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add_connection(Broker) ->
    add_connection(Broker, 'local').

add_connection(Broker, Zone) when not is_binary(Broker) ->
    add_connection(wh_util:to_binary(Broker), Zone);
add_connection(Broker, Zone) when not is_atom(Zone) ->
    add_connection(Broker, wh_util:to_atom(Zone, 'true'));
add_connection(Broker, Zone) ->
    case wh_amqp_connections:add(Broker, Zone) of
        {'error', Reason} ->
            io:format("unable to add broker ~s: ~p~n"
                      ,[Broker, Reason]),
            'no_return';
        #wh_amqp_connection{} -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
primary_broker() ->
    io:format("~s~n", [wh_amqp_connections:primary_broker()]),
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
validate_assignments() ->
    Pattern = #wh_amqp_assignment{_='_'},
    validate_assignments(ets:match_object(?ASSIGNMENTS, Pattern, 1)).

validate_assignments('$end_of_table') -> 'ok';
validate_assignments({[#wh_amqp_assignment{timestamp={_, _, _}
                                           ,consumer='undefined'
                                           ,consumer_ref='undefined'
                                           ,assigned='undefined'
                                           ,channel=Channel
                                           ,channel_ref=ChannelRef
                                           ,connection=Connection
                                           ,broker=?NE_BINARY}=Assignment
                   ], Continuation})
  when is_pid(Channel), is_reference(ChannelRef), is_pid(Connection) ->
    %% validate prechannel
    _ = case is_process_alive(Channel) andalso is_process_alive(Connection) of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[#wh_amqp_assignment{timestamp={_, _, _}
                                           ,consumer=Consumer
                                           ,consumer_ref=ConsumerRef
                                           ,assigned='undefined'
                                           ,channel='undefined'
                                           ,channel_ref='undefined'
                                           ,connection='undefined'
                                           ,broker='undefined'
                                           ,type='float'}=Assignment
                   ], Continuation})
  when is_pid(Consumer), is_reference(ConsumerRef) ->
    %% validate float reservation
    _ = case is_process_alive(Consumer) of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[#wh_amqp_assignment{timestamp={_, _, _}
                                           ,consumer=Consumer
                                           ,consumer_ref=ConsumerRef
                                           ,assigned='undefined'
                                           ,channel='undefined'
                                           ,channel_ref='undefined'
                                           ,connection='undefined'
                                           ,broker=?NE_BINARY
                                           ,type='sticky'}=Assignment
                   ], Continuation})
  when is_pid(Consumer), is_reference(ConsumerRef) ->
    %% validate sticky reservation
    _ = case is_process_alive(Consumer) of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[#wh_amqp_assignment{timestamp={_, _, _}
                                           ,consumer=Consumer
                                           ,consumer_ref=ConsumerRef
                                           ,channel=Channel
                                           ,channel_ref=ChannelRef
                                           ,connection=Connection
                                           ,assigned=Assigned
                                           ,broker=?NE_BINARY}=Assignment
                   ], Continuation})
  when is_pid(Consumer), is_reference(ConsumerRef)
       ,is_pid(Channel), is_reference(ChannelRef)
       ,is_pid(Connection), is_integer(Assigned) ->
    %% validate assignment
    _ = case is_process_alive(Consumer)
            andalso is_process_alive(Channel)
            andalso is_process_alive(Connection)
        of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[Assignment], Continuation}) ->
    log_invalid_assignment(Assignment),
    validate_assignments(ets:match_object(Continuation)).

log_invalid_assignment(#wh_amqp_assignment{}=Assignment) ->
    io:format("invalid assignment:~n ~p~n", [lager:pr(Assignment, ?MODULE)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
connection_summary() ->
    io:format("+--------------------------------------------------+------------------+----------+-----------+------------+---------+~n"),
    io:format("| Broker                                           |    Connection    | Channels | Available | Zone       | Primary |~n"),
    io:format("+==================================================+==================+==========+===========+============+=========+~n"),
    PrimaryBroker = wh_amqp_connections:primary_broker(),
    Pattern = #wh_amqp_connections{_='_'},
    connection_summary(ets:match_object(?CONNECTIONS, Pattern, 1), PrimaryBroker).

connection_summary('$end_of_table', _) -> 'ok';
connection_summary({[#wh_amqp_connections{connection=Connection
                                          ,broker=Broker
                                          ,available=Available
                                          ,zone=Zone}
                   ], Continuation}, PrimaryBroker) ->
    MatchSpec = [{#wh_amqp_assignment{connection=Connection
                                      ,_='_'},
                  [],
                  ['true']}
                ],
    io:format("| ~-48s | ~-16w | ~-8B | ~-9s | ~-10s | ~-7s |~n"
              ,[Broker
                ,Connection
                ,ets:select_count(?ASSIGNMENTS, MatchSpec)
                ,Available
                ,Zone
                ,Broker =:= PrimaryBroker
               ]),
    io:format("+--------------------------------------------------+------------------+----------+-----------+------------+---------+~n"),
    connection_summary(ets:match_object(Continuation), PrimaryBroker).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
broker_summary() ->
    Pattern = #wh_amqp_assignment{broker='$1', _='_'},
    Brokers = ordsets:to_list(
                ordsets:from_list(
                  [Broker
                   || [Broker] <- ets:match(?ASSIGNMENTS, Pattern)
                          ,Broker =/= 'undefined'
                  ])),
    io:format("+--------------------------------------------------+-------------+-----------------------+-----------------------+-------------+~n"),
    io:format("| Broker                                           | Connections |    Sticky Channels    |    Float Channels     | Prechannels |~n"),
    io:format("|                                                  |             | Assigned | Unassigned | Assigned | Unassigned |             |~n"),
    io:format("+==================================================+=============+==========+============+==========+============+=============+~n"),
    _ = broker_summary(['undefined'|Brokers]).

broker_summary([]) -> 'ok';
broker_summary([Broker|Brokers]) ->
    _ = broker_summary_broker(Broker),
    _ = broker_summary_connections(Broker),
    _ = broker_summary_assigned_sticky(Broker),
    _ = broker_summary_unassigned_sticky(Broker),
    _ = broker_summary_assigned_float(Broker),
    _ = broker_summary_unassigned_float(Broker),
    _ = broker_summary_prechannels(Broker),
    _ = broker_summary_zone(Broker),
    io:format("+--------------------------------------------------+-------------+-----------------------+-----------------------+-------------+~n"),
    broker_summary(Brokers).

broker_summary_zone('undefined') ->
    io:format("|                                                  |             |          |            |          |            |             |~n", []);
broker_summary_zone(Broker) ->
    Zone = wh_amqp_connections:broker_zone(Broker),
    io:format("|  Zone: ~-41s |             |          |            |          |            |             |~n", [Zone]).

broker_summary_broker('undefined') ->
    io:format("| ~-48s |", [<<"ANY">>]);
broker_summary_broker(Broker) ->
    io:format("| ~-48s |", [Broker]).

broker_summary_connections(Broker) ->
    MatchSpec = [{#wh_amqp_assignment{connection='$1'
                                      ,broker=Broker
                                      ,_='_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$1']}
                ],
    Count = sets:to_list(
              sets:from_list(
                [Connection
                 || Connection <- ets:select(?ASSIGNMENTS, MatchSpec)
                ])),
    io:format(" ~-11B |", [length(Count)]).

broker_summary_assigned_sticky(Broker) ->
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='$2'
                                      ,broker=Broker
                                      ,type='sticky'
                                      ,_='_'},
                  [{'andalso', {'=/=', '$1', 'undefined'}
                    ,{'=/=', '$2', 'undefined'}
                   }],
                  ['true']}
                ],
    io:format(" ~-8B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_unassigned_sticky(Broker) ->
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='$2'
                                      ,broker=Broker
                                      ,type='sticky'
                                      ,_='_'},
                  [{'andalso', {'=:=', '$1', 'undefined'}
                    ,{'=/=', '$2', 'undefined'}
                   }],
                  ['true']}
                ],
    io:format(" ~-10B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_assigned_float(Broker) ->
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='$2'
                                      ,broker=Broker
                                      ,type='float'
                                      ,_='_'},
                  [{'andalso', {'=/=', '$1', 'undefined'}
                    ,{'=/=', '$2', 'undefined'}
                   }],
                  ['true']}
                ],
    io:format(" ~-8B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_unassigned_float(Broker) ->
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='$2'
                                      ,broker=Broker
                                      ,type='float'
                                      ,_='_'},
                  [{'andalso', {'=:=', '$1', 'undefined'}
                    ,{'=/=', '$2', 'undefined'}
                   }],
                  ['true']}
                ],
    io:format(" ~-10B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_prechannels(Broker) ->
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='undefined'
                                      ,broker=Broker
                                      ,_='_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['true']}
                ],
    io:format(" ~-11B |~n", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
channel_summary() ->
    io:format("+--------------------------------------------------+----------+----------+-----------------+-----------------+-----------------+----------+----------+~n"),
    io:format("| Broker                                           |   Age    | Assigned |     Consumer    |     Channel     |   Connection    |   Type   | Watchers |~n"),
    io:format("+==================================================+==========+==========+=================+=================+=================+==========+==========+~n"),
    Pattern = #wh_amqp_assignment{_='_'},
    channel_summary(ets:match_object(?ASSIGNMENTS, Pattern, 1)).

channel_summary('$end_of_table') -> 'ok';
channel_summary({[Assignment], Continuation}) ->
    io:format("| ~-48s | ~-8B | ~-8B | ~-15w | ~-15w | ~-15w | ~-8s | ~-8B |~n"
              ,[Assignment#wh_amqp_assignment.broker
                ,channel_summary_age(Assignment#wh_amqp_assignment.timestamp)
                ,channel_summary_age(Assignment#wh_amqp_assignment.assigned)
                ,Assignment#wh_amqp_assignment.consumer
                ,Assignment#wh_amqp_assignment.channel
                ,Assignment#wh_amqp_assignment.connection
                ,Assignment#wh_amqp_assignment.type
                ,sets:size(Assignment#wh_amqp_assignment.watchers)
               ]),
    io:format("+--------------------------------------------------+----------+----------+-----------------+-----------------+-----------------+----------+----------+~n"),
    channel_summary(ets:match_object(Continuation)).

channel_summary_age('undefined') -> 0;
channel_summary_age(Timestamp) -> wh_util:elapsed_s(Timestamp).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
consumer_details() ->
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='$2'
                                      ,_='_'},
                  [{'andalso',
                    {'=/=', '$1', 'undefined'},
                    {'=/=', '$2', 'undefined'}
                   }],
                  ['$2']
                 }],
    print_consumer_details(ets:select(?ASSIGNMENTS, MatchSpec, 1)).

consumer_details(ProcessUpper) ->
    consumer_details(<<"0">>, ProcessUpper, <<"0">>).

consumer_details(NodeNumber, ProcessUpper, ProcessLower) when not is_binary(NodeNumber) ->
    consumer_details(wh_util:to_binary(NodeNumber), ProcessUpper, ProcessLower);
consumer_details(NodeNumber, ProcessUpper, ProcessLower) when not is_binary(ProcessUpper) ->
    consumer_details(NodeNumber, wh_util:to_binary(ProcessUpper), ProcessLower);
consumer_details(NodeNumber, ProcessUpper, ProcessLower) when not is_binary(ProcessLower) ->
    consumer_details(NodeNumber, ProcessUpper, wh_util:to_binary(ProcessLower));
consumer_details(NodeNumber, ProcessUpper, ProcessLower) ->
    Pid = list_to_pid(
            wh_util:to_list(
              <<"<", NodeNumber/binary
                ,".", ProcessUpper/binary
                ,".", ProcessLower/binary
                ,">">>)),
    print_consumer_details(Pid).

print_consumer_details('$end_of_table') -> 'ok';
print_consumer_details({[Consumer], Continuation}) ->
    print_consumer_details(Consumer),
    print_consumer_details(ets:select(Continuation));
print_consumer_details(Consumer) when is_pid(Consumer) ->
    _ = io:format("Consumer ~p:~n", [Consumer]),
    _ = case wh_amqp_assignments:find(Consumer) of
            {'error', _} ->
                io:format("  ~-10s: Not currently assigned AMQP channel!~n"
                          ,["Status"]);
            #wh_amqp_assignment{channel='undefined'
                                ,type='sticky'
                                ,broker=Broker} ->
                io:format("  ~-10s: Waiting for sticky channel from ~s~n"
                          ,["Status", Broker]);
            #wh_amqp_assignment{channel='undefined'
                                ,type='float'} ->
                io:format("  ~-10s: Waiting for float channel from any broker~n"
                          ,["Status"]);
            #wh_amqp_assignment{broker=Broker
                                ,channel=Channel
                                ,connection=Connection
                                ,type=Type} ->
                io:format("  ~-10s: Assigned AMQP channel~n"
                          ,["Status"]),
                io:format("  ~-10s: ~s~n", ["Broker", Broker]),
                io:format("  ~-10s: ~p~n", ["Channel", Channel]),
                io:format("  ~-10s: ~p~n", ["Connection", Connection]),
                io:format("  ~-10s: ~p~n", ["Type", Type])
        end,
    _ = case wh_amqp_history:get(Consumer) of
            [] -> 'ok';
            History ->
                io:format("  ~-10s:~n", ["History"]),
                print_consumer_history(History)
        end,
    _ = case is_process_alive(Consumer) of
            'false' -> 'ok';
            'true' ->
                io:format("  ~-10s:~n    ", ["Backtrace"]),
                {'backtrace', Backtrace} = process_info(Consumer, 'backtrace'),
                io:format(binary:replace(Backtrace, <<"\n">>, <<"~n    ">>, ['global']), [])
        end,
    io:format("~n", []).

print_consumer_history([]) -> 'ok';
print_consumer_history([Command|Commands]) ->
    {'$lager_record', Name, Props} = lager:pr(Command, ?MODULE),
    io:format("    ~s~n      ~p~n", [Name, Props]),
    print_consumer_history(Commands).

