%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_history_ets).

-export([create/0

        ,is_existing_command/2
        ,is_consumer_queue_consuming/2
        ,is_consumer_queue_bound_to_exchange/4

        ,get_consumer_history/1
        ,get_consumer_basic_consumes/1
        ,get_consumer_commands_by_tag/2

        ,add_consumer_command/2

        ,update_command_tag/2

        ,delete_consumer_queue/2
        ,delete_consumer_consume/2
        ,delete_consumer/1

        ,unbind_queue/2
        ]).

-export_type([history/0]).

-include("kz_amqp_util.hrl").

-define(TAB, 'kz_amqp_history').

-record(kz_amqp_history, {timestamp = os:timestamp() :: kz_time:now() | '_'
                         ,consumer :: kz_term:api_pid() | '_'
                         ,command :: kz_amqp_command() | '_'
                         }).
-type history() :: #kz_amqp_history{}.

-spec create() -> ets:tid() | atom().
create() ->
    ets:new(?TAB, ['named_table'
                  ,{'keypos', #kz_amqp_history.timestamp}
                  ,'public'
                  ,'ordered_set'
                  ]).

-spec is_existing_command(pid(), kz_amqp_command()) -> boolean().
is_existing_command(Consumer, Command) ->
    MatchSpec = [{#kz_amqp_history{consumer=Consumer
                                  ,command=Command
                                  ,_='_'
                                  }
                 ,[]
                 ,['true']
                 }],
    ets:select_count(?TAB, MatchSpec) > 0.

-spec is_consumer_queue_consuming(pid(), kz_term:ne_binary()) -> boolean().
is_consumer_queue_consuming(Consumer, Queue) ->
    MatchSpec = [{#kz_amqp_history{consumer=Consumer
                                  ,command=#'basic.consume'{queue=Queue
                                                           ,_='_'
                                                           }
                                  ,_='_'
                                  }
                 ,[]
                 ,['true']
                 }],
    ets:select_count(?TAB, MatchSpec) =/= 0.

-spec is_consumer_queue_bound_to_exchange(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_consumer_queue_bound_to_exchange(Consumer, Queue, Exchange, RoutingKey) ->
    MatchSpec = [{#kz_amqp_history{consumer=Consumer
                                  ,command=#'queue.bind'{queue=Queue
                                                        ,exchange=Exchange
                                                        ,routing_key=RoutingKey
                                                        ,_='_'
                                                        }
                                  ,_='_'
                                  }
                 ,[]
                 ,['true']
                 }],
    ets:select_count(?TAB, MatchSpec) =/= 0.

-spec get_consumer_history(pid()) -> [kz_amqp_command()].
get_consumer_history(Consumer) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,_='_'
                              },
    [Command
     || #kz_amqp_history{command=Command}
            <- ets:match_object(?TAB, Pattern)
    ].

-spec get_consumer_basic_consumes(pid()) -> [#'basic.consume'{}].
get_consumer_basic_consumes(Consumer) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'basic.consume'{_='_'}
                              ,_='_'
                              },
    [Command
     || #kz_amqp_history{command=Command} <- ets:match_object(?TAB, Pattern)
    ].

-spec get_consumer_commands_by_tag(pid(), kz_term:ne_binary()) -> [history()].
get_consumer_commands_by_tag(Consumer, OldTag) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'basic.consume'{consumer_tag=OldTag
                                                       ,_='_'
                                                       }
                              ,_='_'
                              },
    ets:match_object(?TAB, Pattern).

-spec add_consumer_command(pid(), kz_amqp_command()) -> 'true'.
add_consumer_command(Consumer, Command) ->
    'true' = ets:insert(?TAB, #kz_amqp_history{consumer=Consumer
                                              ,command=Command
                                              }).

-spec update_command_tag(history(), kz_term:ne_binary()) -> boolean().
update_command_tag(#kz_amqp_history{timestamp=Timestamp
                                   ,command=Command
                                   }
                  ,NewTag
                  ) ->
    Props = [{#kz_amqp_history.command, Command#'basic.consume'{consumer_tag=NewTag}}],
    _ = ets:update_element(?TAB, Timestamp, Props).

-spec delete_consumer_queue(pid(), kz_term:ne_binary()) -> 'true'.
delete_consumer_queue(Consumer, Queue) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'queue.declare'{queue=Queue
                                                       ,_='_'
                                                       }
                              ,_='_'
                              },
    'true' = ets:match_delete(?TAB, Pattern).

-spec delete_consumer_consume(pid(), kz_term:ne_binary()) -> 'true'.
delete_consumer_consume(Consumer, Tag) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'basic.consume'{consumer_tag=Tag
                                                       ,_='_'
                                                       }
                              ,_='_'},
    'true' = ets:match_delete(?TAB, Pattern).

-spec delete_consumer(pid()) -> 'true'.
delete_consumer(Consumer) ->
    Pattern = #kz_amqp_history{consumer=Consumer, _='_'},
    'true' = ets:match_delete(?TAB, Pattern).

-spec unbind_queue(pid(), #'queue.unbind'{}) -> 'true'.
unbind_queue(Consumer, #'queue.unbind'{queue=Queue
                                      ,exchange=Exchange
                                      ,routing_key=RoutingKey
                                      }) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'queue.bind'{queue=Queue
                                                    ,exchange=Exchange
                                                    ,routing_key=RoutingKey
                                                    ,_='_'
                                                    }
                              ,_='_'
                              },
    'true' = ets:match_delete(?TAB, Pattern).
