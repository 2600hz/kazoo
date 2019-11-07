%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_assignments).
-behaviour(gen_server).

-export([start_link/0]).
-export([find/0
        ,find/1
        ]).
-export([get_channel/0
        ,get_channel/1
        ,get_channel/2
        ]).
-export([request_channel/2]).
-export([add_channel/3]).
-export([release/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([channel_count/1]).

-include("kz_amqp_util.hrl").

-define(SERVER, ?MODULE).

-define(TAB, ?MODULE).
-define(SERVER_RETRY_PERIOD, 30 * ?MILLISECONDS_IN_SECOND).
-record(state, {}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec find() -> kz_amqp_assignment() | {'error', 'no_channel'}.
find() -> find(kz_amqp_channel:consumer_pid()).

-spec find(pid()) -> kz_amqp_assignment() | {'error', 'no_channel'}.
find(Consumer) when is_pid(Consumer) ->
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, Pattern, 1) of
        {[#kz_amqp_assignment{}=Assignment], _} -> Assignment;
        _Else -> {'error', 'no_channel'}
    end.

-spec get_channel() -> kz_amqp_assignment().
get_channel() -> get_channel(kz_amqp_channel:consumer_pid()).

-spec get_channel(pid() | non_neg_integer() | 'infinity') ->
                         kz_amqp_assignment() |
                         {'error', 'timeout'}.
get_channel(Consumer) when is_pid(Consumer) ->
    get_channel(Consumer, 'infinity');
get_channel(Timeout) when is_integer(Timeout)
                          orelse Timeout =:= 'infinity' ->
    get_channel(kz_amqp_channel:consumer_pid(), Timeout).

-spec get_channel(pid(), non_neg_integer() | 'infinity') ->
                         kz_amqp_assignment() |
                         {'error', 'timeout'}.
get_channel(Consumer, Timeout) when is_pid(Consumer) ->
    case find(Consumer) of
        #kz_amqp_assignment{channel=Channel}=Assignment
          when is_pid(Channel) -> Assignment;
        #kz_amqp_assignment{} ->
            gen_server:cast(?SERVER, {'add_watcher', Consumer, self()}),
            wait_for_assignment(Timeout);
        {'error', 'no_channel'} ->
            request_and_wait(Consumer, kz_amqp_channel:consumer_broker(), Timeout)
    end.

-spec request_channel(pid(), kz_term:api_binary()) -> 'ok'.
request_channel(Consumer, Broker) ->
    request_channel(Consumer, Broker, 'undefined').

-spec request_channel(pid(), kz_term:api_binary(), kz_term:api_pid()) -> 'ok'.
request_channel(Consumer, 'undefined', Watcher) when is_pid(Consumer) ->
    Broker = kz_amqp_connections:primary_broker(),
    Application = kapps_util:get_application(),
    gen_server:cast(?SERVER, {'request_float', Consumer, Broker, Application, Watcher});
request_channel(Consumer, Broker, Watcher) when is_pid(Consumer) ->
    Application = kapps_util:get_application(),
    gen_server:cast(?SERVER, {'request_sticky', Consumer, Broker, Application, Watcher}).

-spec add_channel(kz_term:ne_binary(), pid(), pid()) -> 'ok'.
add_channel(Broker, Connection, Channel)
  when is_pid(Channel)
       andalso is_binary(Broker) ->
    case kz_amqp_connections:primary_broker() =:= Broker of
        'true' ->
            gen_server:cast(?SERVER, {'add_channel_primary_broker', Broker, Connection, Channel});
        'false' ->
            gen_server:cast(?SERVER, {'add_channel_alternate_broker', Broker, Connection, Channel})
    end.

-spec release(pid()) -> 'ok'.
release(Consumer) ->
    gen_server:call(?SERVER, {'release_handlers', Consumer}, 'infinity').

-spec channel_count(kz_amqp_connections() | pid()) -> non_neg_integer().
channel_count(#kz_amqp_connections{connection=AMQPConn}) ->
    channel_count(AMQPConn);
channel_count(AMQPConn) when is_pid(AMQPConn) ->
    MatchSpec = [{#kz_amqp_assignment{connection=AMQPConn,_='_'}
                 ,[]
                 ,['true']
                 }
                ],
    ets:select_count(?TAB, MatchSpec).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    _ = ets:new(?TAB, ['named_table'
                      ,{'keypos', #kz_amqp_assignment.timestamp}
                      ,'protected'
                      ,'ordered_set'
                      ]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'release_handlers', Consumer}, _, State) ->
    gen_server:cast(self(), {'release_assignments', Consumer}),
    {'reply', release_handlers(Consumer), State};
handle_call(_Msg, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'add_channel_primary_broker', Broker, Connection, Channel}, State) ->
    _ = add_channel_primary_broker(Broker, Connection, Channel),
    {'noreply', State};
handle_cast({'add_channel_alternate_broker', Broker, Connection, Channel}, State) ->
    _ = add_channel_alternate_broker(Broker, Connection, Channel),
    {'noreply', State};
handle_cast({'release_assignments', Consumer}, State) ->
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    _ = release_assignments(ets:match_object(?TAB, Pattern, 1)),
    {'noreply', State};
handle_cast({'add_watcher', Consumer, Watcher}, State) ->
    _ = add_watcher(Consumer, Watcher),
    {'noreply', State};
handle_cast({'maybe_reassign', Consumer}, State) ->
    _ = maybe_reassign(Consumer),
    {'noreply', State};
handle_cast({'maybe_defer_reassign', #kz_amqp_assignment{timestamp=Timestamp
                                                        ,consumer=Consumer
                                                        ,type=Type
                                                        }}, State) ->
    Props = reassign_props(Type),
    ets:update_element(?TAB, Timestamp, Props),
    _ = maybe_reassign(Consumer),
    {'noreply', State};

handle_cast({'request_float', Consumer, Broker, Application, Watcher}, State) ->
    _ = import_pending_channels(),
    _ = add_watcher(assign_or_reserve(Consumer, Broker, Application, 'float'), Watcher),
    {'noreply', State};

handle_cast({'request_sticky', Consumer, Broker, Application, Watcher}, State) ->
    _ = add_watcher(assign_or_reserve(Consumer, Broker, Application, 'sticky'), Watcher),
    {'noreply', State};

handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', Ref, 'process', Pid, Reason}, State) ->
    erlang:demonitor(Ref, ['flush']),
    handle_down_msg(find_reference(Ref), Pid, Reason),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("AMQP assignments terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec import_pending_channels() -> 'ok'.
import_pending_channels() ->
    receive
        %% YOU SHOULD NEVER DO THIS... Except just this once...
        %% This ensures if there a prechannels in our mailbox behind
        %% channel requests they are imported first.  Helps assignment
        %% time when rapid requests are made (on the order of ms).
        {'$gen_cast', {'add_channel_primary_broker', Broker, Connection, Channel}} ->
            'true' = add_channel_primary_broker(Broker, Connection, Channel),
            import_pending_channels()
    after
        0 -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec release_assignments({kz_amqp_assignments(), ets:continuation()} | '$end_of_table') -> 'ok'.
release_assignments('$end_of_table') -> 'ok';
release_assignments({[#kz_amqp_assignment{consumer_ref=Ref}=Assignment]
                    ,Continuation
                    })
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    release_assignments({[Assignment#kz_amqp_assignment{consumer_ref='undefined'}]
                        ,Continuation
                        });
release_assignments({[#kz_amqp_assignment{channel_ref=Ref}=Assignment]
                    ,Continuation
                    })
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    release_assignments({[Assignment#kz_amqp_assignment{channel_ref='undefined'}]
                        ,Continuation
                        });
release_assignments({[#kz_amqp_assignment{channel=Channel
                                         ,consumer=_Consumer
                                         }=Assignment
                     ]
                    ,Continuation
                    })
  when is_pid(Channel) ->
    _ = (catch kz_amqp_channel:close(Channel)),
    release_assignments({[Assignment#kz_amqp_assignment{channel='undefined'}]
                        ,Continuation
                        });
release_assignments({[#kz_amqp_assignment{timestamp=Timestamp
                                         ,consumer=Consumer
                                         }
                     ]
                    ,Continuation
                    }) ->
    lager:debug("removed assignment for consumer ~p", [Consumer]),
    _ = ets:delete(?TAB, Timestamp),
    release_assignments(ets:match(Continuation)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_reassign(pid()) -> 'ok' | 'undefined' | kz_amqp_assignment().
maybe_reassign(Consumer) when is_pid(Consumer) ->
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, Pattern, 1) of
        '$end_of_table' -> 'ok';
        {[#kz_amqp_assignment{channel=Channel}], _}
          when is_pid(Channel) -> 'ok';
        {[#kz_amqp_assignment{type='sticky'
                             ,broker=Broker
                             }=Assignment
         ], _} ->
            maybe_reassign(Assignment, Broker);
        {[#kz_amqp_assignment{type='float'}=Assignment], _} ->
            Broker = kz_amqp_connections:primary_broker(),
            maybe_reassign(Assignment, Broker)
    end.

-spec maybe_reassign(kz_amqp_assignment(), any()) -> 'undefined' | kz_amqp_assignment().
maybe_reassign(_, 'undefined') -> 'undefined';
maybe_reassign(_, '$end_of_table') -> 'undefined';
maybe_reassign(#kz_amqp_assignment{consumer=_Consumer}=ConsumerAssignment
              ,{[#kz_amqp_assignment{timestamp=Timestamp
                                    ,channel=Channel
                                    ,broker=_Broker
                                    }=ChannelAssignment
                ]
               ,Continuation
               }) ->
    %% This is a minor optimization and still allows a dying channel to be
    %% re-assigned prior to the notification that a broker is down. However,
    %% it very unlikely and well handled if it does happen (when the new
    %% primary prechannels are added)
    case is_process_alive(Channel) of
        'false' ->
            %% If we found a dead prechannel its likely a dying connection
            %% either way its no good to anybody so pull it out.
            _ = ets:delete(?TAB, Timestamp),
            maybe_reassign(ConsumerAssignment, ets:match_object(Continuation));
        'true' ->
            lager:debug("attempting to move consumer ~p to a channel on ~s"
                       ,[_Consumer, _Broker]
                       ),
            move_channel_to_consumer(ChannelAssignment, ConsumerAssignment)
    end;
maybe_reassign(#kz_amqp_assignment{}=ConsumerAssignment, Broker) ->
    %% This will attempt to find a prechannel
    %% connected to the specified broker
    Pattern = #kz_amqp_assignment{consumer='undefined'
                                 ,consumer_ref='undefined'
                                 ,broker=Broker
                                 ,_='_'
                                 },
    maybe_reassign(ConsumerAssignment, ets:match_object(?TAB, Pattern, 1)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign_or_reserve(pid(), kz_term:api_binary(), atom(), kz_amqp_type()) -> kz_amqp_assignment().
assign_or_reserve(Consumer, 'undefined', Application, Type) ->
    %% When there is no primary broker we will not be able to
    %% find a channel so just make a reservation
    maybe_reserve(Consumer, 'undefined', Application, Type);
assign_or_reserve(Consumer, Broker, Application, Type) ->
    %% This will attempt to find a prechannel
    Pattern = #kz_amqp_assignment{consumer='undefined'
                                 ,consumer_ref='undefined'
                                 ,broker=Broker
                                 ,_='_'
                                 },
    case ets:match_object(?TAB, Pattern, 1) of
        '$end_of_table' ->
            %% No channel available, reserve assignment
            %% which will be assigned when an appropriate
            %% connection adds a prechannel
            maybe_reserve(Consumer, Broker, Application, Type);
        {[#kz_amqp_assignment{}=Assignment], _} ->
            %% Attempt to assign the consumer to the channel (or
            %% maybe vice-versa, see assign_consumer)
            assign_consumer(Assignment, Consumer, Application, Type)
    end.

-spec assign_consumer(kz_amqp_assignment(), pid(), atom(), kz_amqp_type()) -> kz_amqp_assignment().
assign_consumer(#kz_amqp_assignment{}=ChannelAssignment, Consumer, Application, Type) ->
    %% This will attempt to find an existing entry for the consumer...
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, Pattern, 1) of
        '$end_of_table' ->
            %% When a new consumer requests a channel, and a prechannel
            %% is available, assign the channel to the consumer.
            add_consumer_to_channel(ChannelAssignment, Consumer, Application, Type);
        {[#kz_amqp_assignment{channel=Channel}=ConsumerAssignment], _}
          when is_pid(Channel) ->
            %% When the initial search (outside the gen_server process)
            %% did not have a channel but ahead of the request in the
            %% mailbox was a prechannel that got assigned to the consumer
            %% just return the updated assignment
            lager:debug("consumer ~p already has a valid channel ~p"
                       ,[Consumer, Channel]),
            ConsumerAssignment;
        {[#kz_amqp_assignment{}=ConsumerAssignment], _} ->
            %% When an existing consumer requests a channel, and a
            %% prechannel is available, move the channel to the consumer.
            %% This occurs when the consumer attempts an operation after
            %% losing a valid channel but before a prechannel assignment.
            move_channel_to_consumer(ChannelAssignment, ConsumerAssignment)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec move_channel_to_consumer(kz_amqp_assignment(), kz_amqp_assignment()) -> kz_amqp_assignment().
move_channel_to_consumer(#kz_amqp_assignment{timestamp=Timestamp
                                            ,channel=Channel
                                            ,channel_ref=ChannelRef
                                            ,broker=Broker
                                            ,connection=Connection
                                            }
                        ,#kz_amqp_assignment{consumer=Consumer}=ConsumerAssignment
                        ) ->
    Assignment =
        ConsumerAssignment#kz_amqp_assignment{channel=Channel
                                             ,channel_ref=ChannelRef
                                             ,broker=Broker
                                             ,connection=Connection
                                             ,assigned=kz_time:start_time()
                                             },
    %% Update the consumer assignment with all the channel information
    ets:insert(?TAB, Assignment#kz_amqp_assignment{reconnect='false'
                                                  ,watchers=sets:new()
                                                  }),
    %% Remove the channel assignment so it will not be given away twice
    ets:delete(?TAB, Timestamp),
    lager:debug("assigned existing consumer ~p an available channel ~p on ~s(~p)"
               ,[Consumer, Channel, Broker, Connection]
               ),

    register_channel_handlers(Channel, Consumer),

    send_notifications(Assignment).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_consumer_to_channel(kz_amqp_assignment(), pid(), atom(), kz_amqp_type()) -> kz_amqp_assignment().
add_consumer_to_channel(#kz_amqp_assignment{channel=Channel
                                           ,broker=_Broker
                                           ,connection=Connection
                                           }=ChannelAssignment
                       ,Consumer
                       ,Application
                       ,Type
                       ) ->
    ConsumerRef = erlang:monitor('process', Consumer),
    Assignment =
        ChannelAssignment#kz_amqp_assignment{consumer=Consumer
                                            ,consumer_ref=ConsumerRef
                                            ,application=Application
                                            ,assigned=kz_time:start_time()
                                            ,type=Type
                                            },
    %% Add the consumer to the channel assignment
    ets:insert(?TAB, Assignment#kz_amqp_assignment{reconnect='false'
                                                  ,watchers=sets:new()
                                                  }),
    lager:debug("assigned existing channel ~p on ~s(~p) to new consumer ~p"
               ,[Channel, _Broker, Connection, Consumer]
               ),

    register_channel_handlers(Channel, Consumer),

    send_notifications(Assignment).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_channel_primary_broker(kz_term:ne_binary(), pid(), pid()) -> 'true'.
add_channel_primary_broker(Broker, Connection, Channel) ->
    %% This will find any reservations that require a channel
    %% and assign it the new channel
    MatchSpec = [{#kz_amqp_assignment{type='float'
                                     ,broker='undefined'
                                     ,channel='undefined'
                                     ,_='_'
                                     },
                  [],
                  ['$_']
                 }
                ,{#kz_amqp_assignment{type='sticky'
                                     ,broker=Broker
                                     ,channel='undefined'
                                     ,_='_'
                                     },
                  [],
                  ['$_']
                 }],
    case ets:select(?TAB, MatchSpec, 1) of
        '$end_of_table' -> maybe_reclaim_channel(Broker, Connection, Channel);
        {[#kz_amqp_assignment{}=Assignment], _} ->
            assign_channel(Assignment, Broker, Connection, Channel)
    end.

-spec maybe_reclaim_channel(kz_term:ne_binary(), pid(), pid()) -> 'true'.
maybe_reclaim_channel(Broker, Connection, Channel) ->
    %% This will attempt to find a floating channel not using
    %% the primary broker and move to the new channel
    %% (assumed to be from the primary broker)
    %% NOTE: this is a relatively expensive lookup so I
    %% broke it out from the select in add_channel_primary_broker
    MatchSpec = [{#kz_amqp_assignment{type='float'
                                     ,broker='$1'
                                     ,consumer='$2'
                                     ,_='_'
                                     },
                  [{'andalso', {'=/=', '$1', Broker}
                   ,{'=/=', '$2', 'undefined'}
                   }],
                  ['$_']
                 }],
    case ets:select(?TAB, MatchSpec, 1) of
        '$end_of_table' -> cache(Broker, Connection, Channel);
        {[#kz_amqp_assignment{}=Assignment], _} ->
            assign_channel(Assignment, Broker, Connection, Channel)
    end.

-spec add_channel_alternate_broker(kz_term:ne_binary(), pid(), pid()) -> 'true'.
add_channel_alternate_broker(Broker, Connection, Channel) ->
    %% This will find any sticky reservations that require a
    %% channel from this broker and assign it the new channel
    Pattern = #kz_amqp_assignment{type='sticky'
                                 ,broker=Broker
                                 ,channel='undefined'
                                 ,_='_'
                                 },
    case ets:match_object(?TAB, Pattern, 1) of
        '$end_of_table' -> cache(Broker, Connection, Channel);
        {[#kz_amqp_assignment{}=Assignment], _} ->
            assign_channel(Assignment, Broker, Connection, Channel)
    end.

-spec assign_channel(kz_amqp_assignment(), kz_term:ne_binary(), pid(), pid()) -> 'true'.
assign_channel(#kz_amqp_assignment{channel_ref=Ref}=Assignment
              ,Broker
              ,Connection
              ,Channel
              )
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    assign_channel(Assignment#kz_amqp_assignment{channel_ref='undefined'}
                  ,Broker
                  ,Connection
                  ,Channel
                  );
assign_channel(#kz_amqp_assignment{channel=CurrentChannel
                                  ,broker=CurrentBroker
                                  ,consumer=Consumer
                                  }=Assignment
              ,Broker
              ,Connection
              ,Channel
              )
  when is_pid(CurrentChannel) ->
    lager:debug("reassigning consumer ~p, closing current channel ~p on ~s"
               ,[Consumer, CurrentChannel, CurrentBroker]
               ),

    _ = (catch kz_amqp_channel:close(CurrentChannel)),
    Consumer ! {'kz_amqp_assignment', 'lost_channel'},
    assign_channel(Assignment#kz_amqp_assignment{channel='undefined'
                                                ,reconnect='true'
                                                }
                  ,Broker
                  ,Connection
                  ,Channel
                  );
assign_channel(#kz_amqp_assignment{timestamp=Timestamp
                                  ,consumer=Consumer
                                  }=ConsumerAssignment
              ,Broker
              ,Connection
              ,Channel
              ) ->
    ChannelRef = erlang:monitor('process', Channel),

    Assigment
        = ConsumerAssignment#kz_amqp_assignment{channel=Channel
                                               ,channel_ref=ChannelRef
                                               ,broker=Broker
                                               ,connection=Connection
                                               ,assigned=kz_time:start_time()
                                               },
    %% Add the new channel to the consumer assignment (reservation/wrong broker)
    ets:insert(?TAB, Assigment#kz_amqp_assignment{reconnect='false'
                                                 ,watchers=sets:new()
                                                 }),
    lager:debug("assigned consumer ~p new channel ~p on ~s(~p) after ~pμs"
               ,[Consumer, Channel, Broker, Connection, kz_time:elapsed_us(Timestamp)]
               ),
    register_channel_handlers(Channel, Consumer),
    _ = send_notifications(Assigment),
    'true'.

-spec cache(kz_term:ne_binary(), pid(), pid()) -> 'true'.
cache(Broker, Connection, Channel) ->
    ChannelRef = erlang:monitor('process', Channel),
    lager:debug("added new channel ~p on ~s(~p) to available pool"
               ,[Channel, Broker, Connection]
               ),
    ets:insert(?TAB, #kz_amqp_assignment{channel=Channel
                                        ,channel_ref=ChannelRef
                                        ,broker=Broker
                                        ,connection=Connection
                                        }).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_notifications(kz_amqp_assignment()) -> kz_amqp_assignment().
send_notifications(#kz_amqp_assignment{}=Assignment) ->
    notify_connection(Assignment),
    notify_consumer(Assignment),
    notify_watchers(Assignment).

-spec notify_connection(kz_amqp_assignment()) -> 'ok'.
notify_connection(#kz_amqp_assignment{connection=Connection}) ->
    %% Trigger the connection to primitively open a new channel (prechannel)
    kz_amqp_connection:create_prechannel(Connection).

-spec notify_consumer(kz_amqp_assignment()) -> 'ok'.
notify_consumer(#kz_amqp_assignment{consumer=Consumer
                                   ,reconnect=Reconnect
                                   ,channel=Channel
                                   }) ->
    %% Trigger gen_server to continue with AMQP initialization
    Consumer ! {'kz_amqp_assignment', {'new_channel', Reconnect, Channel}},
    'ok'.

-spec notify_watchers(kz_amqp_assignment()) -> kz_amqp_assignment().
notify_watchers(#kz_amqp_assignment{watchers=Watchers}=Assignment) ->
    notify_watchers(Assignment, sets:to_list(Watchers)).

-spec notify_watchers(kz_amqp_assignment(), [pid()]) -> kz_amqp_assignment().
notify_watchers(#kz_amqp_assignment{}=Assignment, []) ->
    Assignment#kz_amqp_assignment{watchers=sets:new(), reconnect='false'};
notify_watchers(#kz_amqp_assignment{}=Assignment, [Watcher|Watchers]) ->
    %% Notify consumer processes waiting on a channel that they where just
    %% assigned one
    lager:debug("notifying watcher ~p of assignment", [Watcher]),
    Watcher ! Assignment,
    notify_watchers(Assignment, Watchers).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_reserve(kz_term:api_pid(), kz_term:api_binary(), atom(), kz_amqp_type()) -> kz_amqp_assignment().
maybe_reserve(Consumer, Broker, Application, Type) ->
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, Pattern, 1) of
        '$end_of_table' ->
            %% This handles the condition when a consumer requested a channel but
            %% the broker was not available (or there where none for a float)...
            %% the first time.  Add a reservation for matching prechannels.
            reserve(Consumer, Broker, Application, Type);
        {[#kz_amqp_assignment{}=ExistingAssignment], _} ->
            %% This handles the condition when a consumer requested a channel,
            %% but the broker was not available (or there where none for a float);
            %% however, this is not the first request so ignore the impatient bastard.
            lager:debug("consumer ~p still waiting on AMQP channel from ~s"
                       ,[Consumer, Broker]
                       ),
            ExistingAssignment
    end.

-spec reserve(kz_term:api_pid(), kz_term:api_binary(), atom(), kz_amqp_type()) -> kz_amqp_assignment().
reserve(Consumer, Broker, Application, 'sticky') when Broker =/= 'undefined' ->
    Ref = erlang:monitor('process', Consumer),
    Assignment = #kz_amqp_assignment{consumer=Consumer
                                    ,consumer_ref=Ref
                                    ,application=Application
                                    ,broker=Broker
                                    ,type='sticky'
                                    },
    ets:insert(?TAB, Assignment),
    lager:debug("consumer ~p waiting on sticky AMQP channel from ~s"
               ,[Consumer, Broker]
               ),
    Assignment;
reserve(Consumer, _, Application, 'float') ->
    Ref = erlang:monitor('process', Consumer),
    Assignment = #kz_amqp_assignment{consumer=Consumer
                                    ,consumer_ref=Ref
                                    ,application=Application
                                    ,type='float'
                                    },
    ets:insert(?TAB, Assignment),
    lager:debug("consumer ~p waiting on AMQP channel from current broker", [Consumer]),
    Assignment.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type down_match() :: {'channel' | 'consumer', kz_amqp_assignment()}.
-type down_matches() :: [down_match()].

-spec handle_down_msg(down_matches(), pid(), any()) -> 'ok'.
handle_down_msg([], _Pid, _Reason) -> lager:info("no matches for downed pid ~p: ~p", [_Pid, _Reason]);
handle_down_msg(Matches, _Pid, Reason) ->
    lists:foreach(fun(M) -> handle_down_match(M, Reason) end, Matches).

-spec handle_down_match(down_match(), any()) -> 'ok'.
handle_down_match({'consumer', _}
                 ,'shutdown'
                 ) -> 'ok';
handle_down_match({'consumer', #kz_amqp_assignment{consumer=Consumer}=Assignment}
                 ,_Reason
                 ) ->
    lager:debug("consumer ~p, went down without closing channel: ~p"
               ,[Consumer, _Reason]
               ),
    _ = log_short_lived(Assignment),
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    release_assignments(ets:match_object(?TAB, Pattern, 1));
handle_down_match({'channel', #kz_amqp_assignment{timestamp=Timestamp
                                                 ,consumer='undefined'
                                                 }
                  }
                 ,{shutdown,{connection_closing,{server_initiated_close, _, _}}}
                 ) ->
    ets:delete(?TAB, Timestamp);
handle_down_match({'channel', #kz_amqp_assignment{timestamp=Timestamp
                                                 ,channel=Channel
                                                 ,broker=Broker
                                                 ,consumer='undefined'
                                                 }
                  }
                 ,Reason
                 ) ->
    lager:debug("unused channel ~p on ~s went down: ~p"
               ,[Channel, Broker, Reason]
               ),
    ets:delete(?TAB, Timestamp);
handle_down_match({'channel', #kz_amqp_assignment{channel=Channel
                                                 ,type='float'
                                                 ,broker=Broker
                                                 ,consumer=Consumer
                                                 }=Assignment
                  }
                 ,Reason
                 ) ->
    lager:debug("floating channel ~p on ~s went down while still assigned to consumer ~p: ~p"
               ,[Channel, Broker, Consumer, Reason]
               ),
    Consumer ! {'kz_amqp_assignment', 'lost_channel'},
    maybe_defer_reassign(Assignment, Reason);
handle_down_match({'channel', #kz_amqp_assignment{channel=Channel
                                                 ,type='sticky'
                                                 ,broker=Broker
                                                 ,consumer=Consumer
                                                 }=Assignment
                  }
                 ,Reason
                 ) ->
    lager:debug("sticky channel ~p on ~s went down while still assigned to consumer ~p: ~p"
               ,[Channel, Broker, Consumer, Reason]
               ),
    Consumer ! {'kz_amqp_assignment', 'lost_channel'},
    maybe_defer_reassign(Assignment, Reason).

-spec maybe_defer_reassign(#kz_amqp_assignment{}, any()) -> 'ok'.
maybe_defer_reassign(#kz_amqp_assignment{}=Assignment
                    ,{'shutdown',{'server_initiated_close', 404, _Msg}}
                    ) ->
    lager:debug("defer channel reassign for ~p ms", [?SERVER_RETRY_PERIOD]),
    kz_process:spawn(
      fun() ->
              timer:sleep(?SERVER_RETRY_PERIOD),
              gen_server:cast(?SERVER, {'maybe_defer_reassign', Assignment})
      end);
maybe_defer_reassign(#kz_amqp_assignment{}=Assignment
                    ,{'shutdown',{'connection_closing', _}}
                    ) ->
    lager:debug("defer channel reassign for ~p ms", [?SERVER_RETRY_PERIOD]),
    kz_process:spawn(
      fun() ->
              timer:sleep(?SERVER_RETRY_PERIOD),
              gen_server:cast(?SERVER, {'maybe_defer_reassign', Assignment})
      end);
maybe_defer_reassign(#kz_amqp_assignment{timestamp=Timestamp
                                        ,consumer=Consumer
                                        ,type=Type
                                        }
                    ,_Reason
                    ) ->
    Props = reassign_props(Type),
    ets:update_element(?TAB, Timestamp, Props),
    gen_server:cast(?SERVER, {'maybe_reassign', Consumer}).

-spec reassign_props(kz_amqp_type()) -> [{integer(), 'undefined' | 'true'}].
reassign_props('float') ->
    [{#kz_amqp_assignment.channel, 'undefined'}
    ,{#kz_amqp_assignment.channel_ref, 'undefined'}
    ,{#kz_amqp_assignment.connection, 'undefined'}
    ,{#kz_amqp_assignment.reconnect, 'true'}
    ,{#kz_amqp_assignment.broker, 'undefined'}
    ];
reassign_props('sticky') ->
    [{#kz_amqp_assignment.channel, 'undefined'}
    ,{#kz_amqp_assignment.channel_ref, 'undefined'}
    ,{#kz_amqp_assignment.connection, 'undefined'}
    ,{#kz_amqp_assignment.reconnect, 'true'}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_watcher(pid() | kz_amqp_assignment(), kz_term:api_pid()) -> 'ok'.
add_watcher(_Consumer, 'undefined') -> 'ok';
add_watcher(Consumer, Watcher)
  when is_pid(Consumer) ->
    case find(Consumer) of
        #kz_amqp_assignment{}=Assignment -> add_watcher(Assignment, Watcher);
        {'error', 'no_channel'} -> 'ok'
    end;
add_watcher(#kz_amqp_assignment{channel=Channel}=Assignment, Watcher)
  when is_pid(Channel) ->
    Watcher ! Assignment,
    'ok';
add_watcher(#kz_amqp_assignment{watchers=Watchers
                               ,timestamp=Timestamp
                               }, Watcher) ->
    W = sets:add_element(Watcher, Watchers),
    Props = [{#kz_amqp_assignment.watchers, W}],
    ets:update_element(?TAB, Timestamp, Props),
    'ok'.

-spec wait_for_assignment('infinity') -> kz_amqp_assignment();
                         (non_neg_integer()) -> kz_amqp_assignment() |
                                                {'error', 'timeout'}.
wait_for_assignment(Timeout) ->
    receive
        #kz_amqp_assignment{channel=Channel}=Assignment
          when is_pid(Channel) -> Assignment
    after
        Timeout -> {'error', 'timeout'}
    end.

-spec request_and_wait(pid(), kz_term:api_ne_binary(), timeout()) ->
                              kz_amqp_assignment() |
                              {'error', 'timeout'}.
request_and_wait(Consumer, Broker, Timeout) when is_pid(Consumer) ->
    request_channel(Consumer, Broker, self()),
    wait_for_assignment(Timeout).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_reference(reference()) -> down_matches().
find_reference(Ref) ->
    MatchSpec = [{#kz_amqp_assignment{channel_ref=Ref
                                     ,_='_'
                                     }
                 ,[]
                 ,[{{'channel', '$_'}}]
                 }
                ,{#kz_amqp_assignment{consumer_ref=Ref
                                     ,_='_'
                                     }
                 ,[]
                 ,[{{'consumer', '$_'}}]
                 }],
    ets:select(?TAB, MatchSpec).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec log_short_lived(kz_amqp_assignment()) -> 'ok'.
log_short_lived(#kz_amqp_assignment{assigned='undefined'}) -> 'ok';
log_short_lived(#kz_amqp_assignment{assigned=Timestamp}=Assignment) ->
    log_short_lived(Assignment, kz_time:elapsed_s(Timestamp)).

log_short_lived(#kz_amqp_assignment{consumer=Consumer
                                   ,type=Type
                                   ,channel=Channel
                                   ,broker=Broker
                                   }
               ,DurationS
               ) when DurationS < 5 ->
    lager:warning("short lived assignment (~ps) for ~p (channel ~p type ~p broker ~p)"
                 ,[DurationS, Consumer, Channel, Type, Broker]
                 );
log_short_lived(_Assignment, _Duration) -> 'ok'.

-spec register_channel_handlers(pid(), pid()) -> 'ok'.
register_channel_handlers(Channel, Consumer) ->
    amqp_channel:register_return_handler(Channel, Consumer),
    amqp_channel:register_confirm_handler(Channel, Consumer),
    amqp_channel:register_flow_handler(Channel, Consumer),

    lager:debug("registered handlers for channel ~p to ~p", [Channel, Consumer]).

-spec unregister_channel_handlers(pid()) -> 'ok'.
unregister_channel_handlers(Channel) ->
    _ = (catch amqp_channel:unregister_return_handler(Channel)),
    _ = (catch amqp_channel:unregister_confirm_handler(Channel)),
    _ = (catch amqp_channel:unregister_flow_handler(Channel)),
    lager:debug("unregistered handlers for channel ~p", [Channel]).

-spec release_handlers({kz_amqp_assignments(), ets:continuation()} | '$end_of_table' | pid()) -> 'ok'.
release_handlers(Consumer)
  when is_pid(Consumer) ->
    Pattern = #kz_amqp_assignment{consumer=Consumer, _='_'},
    release_handlers(ets:match_object(?TAB, Pattern, 1));
release_handlers('$end_of_table') -> 'ok';
release_handlers({[#kz_amqp_assignment{channel=Channel}]
                 ,Continuation
                 }
                )
  when is_pid(Channel) ->
    case is_process_alive(Channel) of
        'true' -> unregister_channel_handlers(Channel);
        'false' -> 'ok'
    end,
    release_handlers(ets:match(Continuation));
release_handlers({[#kz_amqp_assignment{}], Continuation}) ->
    release_handlers(ets:match(Continuation)).
