%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(whistle_amqp_maintenance).

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
