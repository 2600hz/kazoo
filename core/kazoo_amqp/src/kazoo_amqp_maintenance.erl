%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_amqp_maintenance).

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

-export([gc_pools/0, gc_pool/1]).

-include("kz_amqp_util.hrl").

-define(ASSIGNMENTS, 'kz_amqp_assignments').
-define(CONNECTIONS, 'kz_amqp_connections').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_broker(kz_term:ne_binary()) -> 'ok' | 'no_return'.
add_broker(Broker) ->
    add_broker(Broker, 'local').

-spec add_broker(kz_term:ne_binary(), atom()) -> 'ok' | 'no_return'.
add_broker(Broker, Zone) when not is_binary(Broker) ->
    add_broker(kz_term:to_binary(Broker), Zone);
add_broker(Broker, Zone) when not is_atom(Zone) ->
    add_broker(Broker, kz_term:to_atom(Zone, 'true'));
add_broker(Broker, Zone) ->
    case kz_amqp_connections:new(Broker, Zone) of
        {'error', 'exists'} ->
            io:format("ERROR: broker ~s currently has ~p connection(s) of which ~p is/are available~n"
                     ,[Broker
                      ,kz_amqp_connections:broker_connections(Broker)
                      ,kz_amqp_connections:broker_available_connections(Broker)
                      ]),
            io:format("~nTo add more connections use:~n sup kazoo_amqp_maintenance add_connection ~s~n"
                     ,[Broker]),
            'no_return';
        {'error', Reason} ->
            io:format("ERROR: unable to add broker ~s: ~p~n"
                     ,[Broker, Reason]),
            'no_return';
        #kz_amqp_connection{} -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_broker(kz_term:ne_binary()) -> 'ok'.
remove_broker(Broker) when not is_binary(Broker) ->
    remove_broker(kz_term:to_binary(Broker));
remove_broker(Broker) ->
    kz_amqp_connections:remove(Broker).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_connection(kz_term:ne_binary()) -> 'ok' | 'no_return'.
add_connection(Broker) ->
    add_connection(Broker, 'local').

-spec add_connection(kz_term:ne_binary(), atom()) -> 'ok' | 'no_return'.
add_connection(Broker, Zone) when not is_binary(Broker) ->
    add_connection(kz_term:to_binary(Broker), Zone);
add_connection(Broker, Zone) when not is_atom(Zone) ->
    add_connection(Broker, kz_term:to_atom(Zone, 'true'));
add_connection(Broker, Zone) ->
    case kz_amqp_connections:add(Broker, Zone) of
        {'error', Reason} ->
            io:format("unable to add broker ~s: ~p~n"
                     ,[Broker, Reason]),
            'no_return';
        #kz_amqp_connection{} -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec primary_broker() -> 'no_return'.
primary_broker() ->
    io:format("~s~n", [kz_amqp_connections:primary_broker()]),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_assignments() -> 'ok'.
validate_assignments() ->
    Pattern = #kz_amqp_assignment{_='_'},
    validate_assignments(ets:match_object(?ASSIGNMENTS, Pattern, 1)).

-spec validate_assignments({[kz_amqp_assignment()], ets:continuation()} | '$end_of_table') -> 'ok'.
validate_assignments('$end_of_table') -> 'ok';
validate_assignments({[#kz_amqp_assignment{timestamp={_, _, _}
                                          ,consumer='undefined'
                                          ,consumer_ref='undefined'
                                          ,assigned='undefined'
                                          ,channel=Channel
                                          ,channel_ref=ChannelRef
                                          ,connection=Connection
                                          ,broker=?NE_BINARY
                                          }=Assignment
                      ], Continuation})
  when is_pid(Channel), is_reference(ChannelRef), is_pid(Connection) ->
    %% validate prechannel
    _ = case is_process_alive(Channel)
            andalso is_process_alive(Connection)
        of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[#kz_amqp_assignment{timestamp={_, _, _}
                                          ,consumer=Consumer
                                          ,consumer_ref=ConsumerRef
                                          ,assigned='undefined'
                                          ,channel='undefined'
                                          ,channel_ref='undefined'
                                          ,connection='undefined'
                                          ,broker='undefined'
                                          ,type='float'
                                          }=Assignment
                      ], Continuation})
  when is_pid(Consumer), is_reference(ConsumerRef) ->
    %% validate float reservation
    _ = case is_process_alive(Consumer) of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[#kz_amqp_assignment{timestamp={_, _, _}
                                          ,consumer=Consumer
                                          ,consumer_ref=ConsumerRef
                                          ,assigned='undefined'
                                          ,channel='undefined'
                                          ,channel_ref='undefined'
                                          ,connection='undefined'
                                          ,broker=?NE_BINARY
                                          ,type='sticky'
                                          }=Assignment
                      ], Continuation})
  when is_pid(Consumer), is_reference(ConsumerRef) ->
    %% validate sticky reservation
    _ = case is_process_alive(Consumer) of
            'false' -> log_invalid_assignment(Assignment);
            'true' -> 'ok'
        end,
    validate_assignments(ets:match_object(Continuation));
validate_assignments({[#kz_amqp_assignment{timestamp={_, _, _}
                                          ,consumer=Consumer
                                          ,consumer_ref=ConsumerRef
                                          ,channel=Channel
                                          ,channel_ref=ChannelRef
                                          ,connection=Connection
                                          ,assigned=Assigned
                                          ,broker=?NE_BINARY
                                          }=Assignment
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
validate_assignments({[#kz_amqp_assignment{}=Assignment], Continuation}) ->
    log_invalid_assignment(Assignment),
    validate_assignments(ets:match_object(Continuation)).

-spec log_invalid_assignment(kz_amqp_assignment()) -> 'ok'.
log_invalid_assignment(#kz_amqp_assignment{}=Assignment) ->
    io:format("invalid assignment:~n ~p~n", [lager:pr(Assignment, ?MODULE)]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec connection_summary() -> 'ok'.
connection_summary() ->
    io:format("+--------------------------------------------------+------------------+----------+-----------+------------+---------+~n"),
    io:format("| Broker                                           |    Connection    | Channels | Available | Zone       | Primary |~n"),
    io:format("+==================================================+==================+==========+===========+============+=========+~n"),
    PrimaryBroker = kz_amqp_connections:primary_broker(),
    Pattern = #kz_amqp_connections{_='_'},
    connection_summary(ets:match_object(?CONNECTIONS, Pattern, 1), PrimaryBroker).

connection_summary('$end_of_table', _) -> 'ok';
connection_summary({[#kz_amqp_connections{connection=Connection
                                         ,broker=Broker
                                         ,available=Available
                                         ,zone=Zone
                                         }=Conn
                    ], Continuation
                   }
                  ,PrimaryBroker) ->

    io:format("| ~-48s | ~-16w | ~-8B | ~-9s | ~-10s | ~-7s |~n"
             ,[Broker
              ,Connection
              ,kz_amqp_assignments:channel_count(Conn)
              ,Available
              ,Zone
              ,Broker =:= PrimaryBroker
              ]),
    io:format("+--------------------------------------------------+------------------+----------+-----------+------------+---------+~n"),
    connection_summary(ets:match_object(Continuation), PrimaryBroker).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec broker_summary() -> 'ok'.
broker_summary() ->
    Pattern = #kz_amqp_assignment{broker='$1', _='_'},
    Brokers = ordsets:to_list(
                ordsets:from_list(
                  [Broker
                   || [Broker] <- ets:match(?ASSIGNMENTS, Pattern),
                      Broker =/= 'undefined'
                  ]
                 )),
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
    Zone = kz_amqp_connections:broker_zone(Broker),
    io:format("|  Zone: ~-41s |             |          |            |          |            |             |~n", [Zone]).

broker_summary_broker('undefined') ->
    io:format("| ~-48s |", [<<"ANY">>]);
broker_summary_broker(Broker) ->
    io:format("| ~-48s |", [Broker]).

broker_summary_connections(Broker) ->
    MatchSpec = [{#kz_amqp_assignment{connection='$1'
                                     ,broker=Broker
                                     ,_='_'
                                     }
                 ,[{'=/=', '$1', 'undefined'}]
                 ,['$1']}
                ],
    Count = sets:to_list(
              sets:from_list(
                [Connection
                 || Connection <- ets:select(?ASSIGNMENTS, MatchSpec)
                ]
               )),
    io:format(" ~-11B |", [length(Count)]).

broker_summary_assigned_sticky(Broker) ->
    MatchSpec = [{#kz_amqp_assignment{channel='$1'
                                     ,consumer='$2'
                                     ,broker=Broker
                                     ,type='sticky'
                                     ,_='_'
                                     }
                 ,[{'andalso'
                   ,{'=/=', '$1', 'undefined'}
                   ,{'=/=', '$2', 'undefined'}
                   }
                  ]
                 ,['true']
                 }
                ],
    io:format(" ~-8B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_unassigned_sticky(Broker) ->
    MatchSpec = [{#kz_amqp_assignment{channel='$1'
                                     ,consumer='$2'
                                     ,broker=Broker
                                     ,type='sticky'
                                     ,_='_'
                                     }
                 ,[{'andalso'
                   ,{'=:=', '$1', 'undefined'}
                   ,{'=/=', '$2', 'undefined'}
                   }
                  ]
                 ,['true']
                 }
                ],
    io:format(" ~-10B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_assigned_float(Broker) ->
    MatchSpec = [{#kz_amqp_assignment{channel='$1'
                                     ,consumer='$2'
                                     ,broker=Broker
                                     ,type='float'
                                     ,_='_'
                                     }
                 ,[{'andalso'
                   ,{'=/=', '$1', 'undefined'}
                   ,{'=/=', '$2', 'undefined'}
                   }]
                 ,['true']
                 }
                ],
    io:format(" ~-8B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_unassigned_float(Broker) ->
    MatchSpec = [{#kz_amqp_assignment{channel='$1'
                                     ,consumer='$2'
                                     ,broker=Broker
                                     ,type='float'
                                     ,_='_'
                                     }
                 ,[{'andalso'
                   ,{'=:=', '$1', 'undefined'}
                   ,{'=/=', '$2', 'undefined'}
                   }
                  ]
                 ,['true']
                 }
                ],
    io:format(" ~-10B |", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

broker_summary_prechannels(Broker) ->
    MatchSpec = [{#kz_amqp_assignment{channel='$1'
                                     ,consumer='undefined'
                                     ,broker=Broker
                                     ,_='_'
                                     }
                 ,[{'=/=', '$1', 'undefined'}]
                 ,['true']
                 }
                ],
    io:format(" ~-11B |~n", [ets:select_count(?ASSIGNMENTS, MatchSpec)]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec channel_summary() -> 'ok'.
channel_summary() ->
    io:format("+--------------------------------------------------+----------+----------+----------------------+-----------------+-----------------+-----------------+----------+----------+~n"),
    io:format("| Broker                                           |   Age    | Assigned |      Application     |     Consumer    |     Channel     |   Connection    |   Type   | Watchers |~n"),
    io:format("+==================================================+==========+==========+======================+=================+=================+=================+==========+==========+~n"),
    Pattern = #kz_amqp_assignment{_='_'},
    channel_summary(ets:match_object(?ASSIGNMENTS, Pattern, 1)).

channel_summary('$end_of_table') -> 'ok';
channel_summary({[#kz_amqp_assignment{}=Assignment], Continuation}) ->
    io:format("| ~-48s | ~-8B | ~-8B | ~-20w | ~-15w | ~-15w | ~-15w | ~-8s | ~-8B |~n"
             ,[Assignment#kz_amqp_assignment.broker
              ,channel_summary_age(Assignment#kz_amqp_assignment.timestamp)
              ,channel_summary_age(Assignment#kz_amqp_assignment.assigned)
              ,Assignment#kz_amqp_assignment.application
              ,Assignment#kz_amqp_assignment.consumer
              ,Assignment#kz_amqp_assignment.channel
              ,Assignment#kz_amqp_assignment.connection
              ,Assignment#kz_amqp_assignment.type
              ,sets:size(Assignment#kz_amqp_assignment.watchers)
              ]),
    io:format("+--------------------------------------------------+----------+----------+----------------------+-----------------+-----------------+-----------------+----------+----------+~n"),
    channel_summary(ets:match_object(Continuation)).

channel_summary_age('undefined') -> 0;
channel_summary_age(Timestamp) -> kz_time:elapsed_s(Timestamp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec consumer_details() -> 'ok'.
consumer_details() ->
    MatchSpec = [{#kz_amqp_assignment{channel='$1'
                                     ,consumer='$2'
                                     ,_='_'
                                     }
                 ,[{'andalso'
                   ,{'=/=', '$1', 'undefined'}
                   ,{'=/=', '$2', 'undefined'}
                   }
                  ]
                 ,['$2']
                 }],
    print_consumer_details(ets:select(?ASSIGNMENTS, MatchSpec, 1)).

-spec consumer_details(kz_term:ne_binary()) -> 'ok'.
consumer_details(ProcessUpper) ->
    consumer_details(<<"0">>, ProcessUpper, <<"0">>).

-spec consumer_details(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
consumer_details(NodeNumber, ProcessUpper, ProcessLower) when not is_binary(NodeNumber) ->
    consumer_details(kz_term:to_binary(NodeNumber), ProcessUpper, ProcessLower);
consumer_details(NodeNumber, ProcessUpper, ProcessLower) when not is_binary(ProcessUpper) ->
    consumer_details(NodeNumber, kz_term:to_binary(ProcessUpper), ProcessLower);
consumer_details(NodeNumber, ProcessUpper, ProcessLower) when not is_binary(ProcessLower) ->
    consumer_details(NodeNumber, ProcessUpper, kz_term:to_binary(ProcessLower));
consumer_details(NodeNumber, ProcessUpper, ProcessLower) ->
    ProtoPid = list_to_binary(["<", NodeNumber
                              ,".", ProcessUpper
                              ,".", ProcessLower
                              ,">"
                              ]),
    Pid = list_to_pid(kz_term:to_list(ProtoPid)),
    print_consumer_details(Pid).

print_consumer_details('$end_of_table') -> 'ok';
print_consumer_details({[Consumer], Continuation}) ->
    print_consumer_details(Consumer),
    print_consumer_details(ets:select(Continuation));
print_consumer_details(Consumer) when is_pid(Consumer) ->
    _ = io:format("Consumer ~p:~n", [Consumer]),
    _ = case kz_amqp_assignments:find(Consumer) of
            {'error', _} ->
                io:format("  ~-10s: Not currently assigned AMQP channel!~n"
                         ,["Status"]
                         );
            #kz_amqp_assignment{channel='undefined'
                               ,type='sticky'
                               ,broker=Broker
                               } ->
                io:format("  ~-10s: Waiting for sticky channel from ~s~n"
                         ,["Status", Broker]
                         );
            #kz_amqp_assignment{channel='undefined'
                               ,type='float'
                               } ->
                io:format("  ~-10s: Waiting for float channel from any broker~n"
                         ,["Status"]
                         );
            #kz_amqp_assignment{broker=Broker
                               ,channel=Channel
                               ,connection=Connection
                               ,type=Type
                               } ->
                io:format("  ~-10s: Assigned AMQP channel~n", ["Status"]),
                io:format("  ~-10s: ~s~n", ["Broker", Broker]),
                io:format("  ~-10s: ~p~n", ["Channel", Channel]),
                io:format("  ~-10s: ~p~n", ["Connection", Connection]),
                io:format("  ~-10s: ~p~n", ["Type", Type])
        end,
    _ = case is_process_alive(Consumer) of
            'false' -> 'ok';
            'true' ->
                io:format("  ~-10s:~n    ", ["Backtrace"]),
                {'backtrace', Backtrace} = process_info(Consumer, 'backtrace'),
                io:format(binary:replace(Backtrace, <<"\n">>, <<"~n    ">>, ['global']), [])
        end,
    io:format("~n", []).

-spec gc_pools() -> 'ok'.
gc_pools() ->
    _ = [gc_pool(Pool, Pid) || {Pool, Pid} <- kz_amqp_sup:pools()],
    'ok'.

-spec gc_pool(kz_term:text()) -> 'ok'.
gc_pool(Pool) when is_atom(Pool) ->
    case [P || {Name, _}=P <- kz_amqp_sup:pools(), Pool =:= Name] of
        [] -> io:format("no pool named ~p found~n", [Pool]);
        [{Pool, Pid}] -> gc_pool(Pool, Pid)
    end;
gc_pool(PoolBin) ->
    gc_pool(kz_term:to_atom(PoolBin)).

-spec gc_pool(atom(), pid()) -> 'ok'.
gc_pool(Pool, PoolPid) ->
    print_gc_results(Pool, gc_workers(PoolPid)).

-spec gc_workers(pid()) -> [{pid(), integer(), integer(), integer()}].
gc_workers(Pid) ->
    lists:reverse(
      lists:keysort(3, [gc_worker(W) || {_, W, _, _} <- gen_server:call(Pid, 'get_all_workers')])
     ).

-spec gc_worker(pid()) -> {pid(), integer(), integer(), integer()}.
gc_worker(P) ->
    [{_, S1}] = process_info(P, ['total_heap_size']),
    garbage_collect(P),
    [{_, S2}] = process_info(P, ['total_heap_size']),
    {P, abs(S2-S1), S1, S2}.

-define(GC_RESULT_FORMAT, "  ~-16s | ~-8s | ~-8s | ~-8s~n").

print_gc_results(Pool, Results) ->
    io:format("gc'd ~p:~n", [Pool]),
    print_gc_summary(Results),
    io:format(?GC_RESULT_FORMAT, ["Worker", "Delta", "Before", "After"]),
    lists:foreach(fun print_gc_result/1, Results).

-spec print_gc_summary([{pid(), integer(), integer(), integer()}]) -> 'ok'.
print_gc_summary(Results) ->
    {Min, Max, Sum, Count} = gc_summary(Results),
    Summary = kz_binary:join([kz_util:pretty_print_bytes(kz_term:words_to_bytes(Min), 'truncated')
                             ,kz_util:pretty_print_bytes(kz_term:words_to_bytes(Sum div Count), 'truncated')
                             ,kz_util:pretty_print_bytes(kz_term:words_to_bytes(Max), 'truncated')
                             ]
                            ,<<" < ">>
                            ),
    io:format("  Min/Avg/Max of ~p workers: ~s~n", [Count, Summary]).

-spec gc_summary([{pid(), integer(), integer(), integer()}]) ->
                        {integer(), integer(), integer(), integer()}.
gc_summary([{_W, Diff, _B, _A} | Results]) ->
    lists:foldl(fun gc_summary_fold/2
               ,{Diff, Diff, Diff, 1}
               ,Results
               ).

-spec gc_summary_fold({pid(), integer(), integer(), integer()}
                     ,{integer(), integer(), integer(), integer()}
                     ) ->
                             {integer(), integer(), integer(), integer()}.
gc_summary_fold({_W, Diff, _B, _A}
               ,{Min, Max, Sum, Count}
               ) ->
    {lists:min([Diff, Min])
    ,lists:max([Diff, Max])
    ,Diff + Sum
    ,Count + 1
    }.

-spec print_gc_result({pid(), integer(), integer(), integer()}) -> 'ok'.
print_gc_result({W, Diff, Before, After}) ->
    io:format(?GC_RESULT_FORMAT
             ,[kz_term:to_list(W)
              ,kz_util:pretty_print_bytes(kz_term:words_to_bytes(Diff), 'truncated')
              ,kz_util:pretty_print_bytes(kz_term:words_to_bytes(Before), 'truncated')
              ,kz_util:pretty_print_bytes(kz_term:words_to_bytes(After), 'truncated')
              ]
             ).
