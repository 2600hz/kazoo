%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(whistle_amqp_maintenance).

-export([validate_assignments/0]).
-export([consumer_details/0]).
-export([broker_summary/0]).
-export([channel_summary/0]).

-include("amqp_util.hrl").

-define(ASSIGNMENTS, 'wh_amqp_assignments').

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
consumer_details() -> 
    MatchSpec = [{#wh_amqp_assignment{consumer='$1'
                                      ,channel='$2'
                                      ,_='_'},
                  [{'andalso', 
                    {'=/=', '$1', 'undefined'},
                    {'=/=', '$2', 'undefined'}
                   }],
                  ['$1']
                 }],
    consumer_details(ets:select(?ASSIGNMENTS, MatchSpec, 1)).

consumer_details('$end_of_table') -> 'ok';
consumer_details({[Consumer], Continuation}) ->
    io:format("~p~n", [process_info(Consumer)]),
    consumer_details(ets:select(Continuation)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
validate_assignments() ->
    Pattern = #wh_amqp_assignment{ _='_'},
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
                                           ,assigned={_, _, _}
                                           ,channel=Channel
                                           ,channel_ref=ChannelRef
                                           ,connection=Connection                                           
                                           ,broker=?NE_BINARY}=Assignment
                   ], Continuation}) 
  when is_pid(Consumer), is_reference(ConsumerRef)
       ,is_pid(Channel), is_reference(ChannelRef)
       ,is_pid(Connection) ->
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
    io:format("+--------------------------------------------------+-------------+-----------------------+-----------------------+-------------+~n"),
    broker_summary(Brokers).

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
    Pattern = #wh_amqp_assignment{ _='_'},
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
