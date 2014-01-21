%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_assignments).

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
-export([add_broker/1]).
-export([remove_broker/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(TAB, ?MODULE).

-record(state, {brokers = ordsets:new()}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec find() -> wh_amqp_assignment() | {'error', 'no_channel'}.
find() -> find(wh_amqp_channel:consumer_pid()).

-spec find(pid()) -> wh_amqp_assignment() | {'error', 'no_channel'}.
find(Consumer) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} -> Assignment;
        _Else -> {'error', 'no_channel'}
    end.

-spec get_channel() -> wh_amqp_assignment().
get_channel() -> get_channel(wh_amqp_channel:consumer_pid()).

-spec get_channel(non_neg_integer()) -> wh_amqp_assignment() | {'error', 'timeout'};
                 ('infinity') -> wh_amqp_assignment();
                 (pid()) -> wh_amqp_assignment().
get_channel(Consumer) when is_pid(Consumer) ->
    get_channel(Consumer, 'infinity');
get_channel(Timeout) when is_integer(Timeout); Timeout =:= 'infinity' ->
    get_channel(wh_amqp_channel:consumer_pid(), Timeout).

-spec get_channel(pid(), non_neg_integer()) -> wh_amqp_assignment() | {'error', 'timeout'};
                 (pid(), 'infinity') -> wh_amqp_assignment().
get_channel(Consumer, Timeout) when is_pid(Consumer) ->
    case find(Consumer) of
        #wh_amqp_assignment{channel=Channel}=Assignment 
          when is_pid(Channel) -> Assignment;
        #wh_amqp_assignment{} ->
            gen_server:cast(?MODULE, {'add_watcher', Consumer, self()}),
            wait_for_assignment(Timeout);
        {'error', 'no_channel'} ->
            request_and_wait(Consumer, wh_amqp_channel:consumer_broker(), Timeout)
    end.

-spec request_channel(pid(), api_binary()) -> wh_amqp_assignment().
request_channel(Consumer, 'undefined') when is_pid(Consumer) ->
    gen_server:call(?MODULE, {'request_float', Consumer});
request_channel(Consumer, Broker) when is_pid(Consumer) ->
    gen_server:call(?MODULE, {'request_sticky', Consumer, Broker}).

-spec add_channel(ne_binary(), pid(), pid()) -> 'ok'.
add_channel(Broker, Connection, Channel) when is_pid(Channel) ->
    gen_server:cast(?MODULE, {'add_channel', Broker, Connection, Channel}).

-spec release(pid()) -> 'ok'.    
release(Consumer) ->
    gen_server:cast(?MODULE, {'release_assignments', Consumer}).

-spec add_broker(ne_binary()) -> 'ok'.
add_broker(Broker) ->
    gen_server:cast(?MODULE, {'add_broker', Broker}).

-spec remove_broker(ne_binary()) -> 'ok'.
remove_broker(Broker) ->
    gen_server:cast(?MODULE, {'remove_broker', Broker}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {'stop', Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    put(callid, ?LOG_SYSTEM_ID),
    _ = ets:new(?TAB, ['named_table'
                       ,{'keypos', #wh_amqp_assignment.timestamp}
                       ,'protected'
                       ,'ordered_set'
                      ]),
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, Reply, State} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({'request_float', Consumer}, _, State) ->
    {'reply', assign_or_reserve(Consumer, primary_broker(State), 'float'), State};
handle_call({'request_sticky', Consumer, Broker}, _, State) ->
    {'reply', assign_or_reserve(Consumer, Broker, 'sticky'), State};
handle_call(_Msg, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'add_broker', Broker}, #state{brokers=Brokers}=State) ->    
    {'noreply', State#state{brokers=ordsets:add_element(Broker, Brokers)}};
handle_cast({'remove_broker', Broker}, #state{brokers=Brokers}=State) ->
    {'noreply', State#state{brokers=ordsets:del_element(Broker, Brokers)}};
handle_cast({'add_channel', Broker, Connection, Channel}, State) ->
    _ = case primary_broker(State) =:= Broker of
            'true' -> add_channel_primary_broker(Broker, Connection, Channel);
            'false' -> add_channel_alternate_broker(Broker, Connection, Channel)
        end,
    {'noreply', State};
handle_cast({'release_assignments', Consumer}, State) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    _ = release_assignments(ets:match_object(?TAB, MatchSpec, 1)),
    {'noreply', State};
handle_cast({'add_watcher', Consumer, Watcher}, State) ->
    _ = add_watcher(Consumer, Watcher),
    {'noreply', State};
handle_cast({'maybe_reassign', Consumer}, State) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    _ = case ets:match_object(?TAB, MatchSpec, 1) of
            {[#wh_amqp_assignment{channel=Channel}], _}
              when is_pid(Channel) -> 'ok';
            {[#wh_amqp_assignment{type='sticky'
                                  ,broker=Broker}=Assignment
             ], _} -> maybe_reassign(Assignment, Broker);
            {[#wh_amqp_assignment{type='float'}=Assignment], _} ->
                maybe_reassign(Assignment, primary_broker(State))
        end,
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, 'process', _Pid, Reason}, State) ->
    erlang:demonitor(Ref, ['flush']),
    handle_down_msg(find_reference(Ref), Reason),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("AMQP assignments terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec release_assignments({wh_amqp_assignments(), ets:continuation()} | '$end_of_table') -> 'ok'.
release_assignments('$end_of_table') -> 'ok';
release_assignments({[#wh_amqp_assignment{consumer_ref=Ref}=Assignment]
                     ,Continuation})
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    release_assignments({[Assignment#wh_amqp_assignment{consumer_ref='undefined'}]
                         ,Continuation});
release_assignments({[#wh_amqp_assignment{channel_ref=Ref}=Assignment]
                     ,Continuation})
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    release_assignments({[Assignment#wh_amqp_assignment{channel_ref='undefined'}]
                         ,Continuation});
release_assignments({[#wh_amqp_assignment{channel=Channel}=Assignment]
                     ,Continuation})
  when is_pid(Channel) ->
    _ = (catch wh_amqp_channel:close(Channel)),
    release_assignments({[Assignment#wh_amqp_assignment{channel='undefined'}]
                         ,Continuation});
release_assignments({[#wh_amqp_assignment{timestamp=Timestamp
                                         ,consumer=Consumer}
                     ], Continuation}) ->
    lager:debug("removed assignment for consumer ~p", [Consumer]),
    _ = ets:delete(?TAB, Timestamp),
    release_assignments(ets:match(Continuation)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec primary_broker(#state{}) -> api_binary().
primary_broker(#state{brokers=Brokers}) ->
    case ordsets:to_list(Brokers) of
        [Broker|_] -> Broker;
        _Else -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_reassign(wh_amqp_assignment(), api_binary()) -> 'undefined' | wh_amqp_assignment().
maybe_reassign(_, 'undefined') -> 'undefined';
maybe_reassign(#wh_amqp_assignment{}=ConsumerAssignment, Broker) ->
    %% This will attempt to find a matching channel that
    %% does not already have a consumer (a "prechannel")
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='undefined'
                                      ,broker=Broker
                                      ,_='_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$_']}
                ],
    case ets:select(?TAB, MatchSpec, 1) of
        '$end_of_table' -> ConsumerAssignment;
        {[#wh_amqp_assignment{}=ChannelAssignment], _} ->
            move_channel_to_consumer(ChannelAssignment, ConsumerAssignment)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec assign_or_reserve(pid(), api_binary(), wh_amqp_type()) -> wh_amqp_assignment().
assign_or_reserve(Consumer, 'undefined', Type) ->
    %% When there is no primary broker we will not be able to 
    %% find a channel so just make a reservation for the
    %% next avaialable
    maybe_reserve(Consumer, 'undefined', Type);
assign_or_reserve(Consumer, Broker, Type) ->
    %% This will attempt to find a matching channel that
    %% does not already have a consumer (a "prechannel")
    MatchSpec = [{#wh_amqp_assignment{channel='$1'
                                      ,consumer='undefined'
                                      ,broker=Broker
                                      ,_='_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$_']}
                ],
    case ets:select(?TAB, MatchSpec, 1) of
        '$end_of_table' ->             
            %% No channel available, reserve assignment
            %% which will be assigned when an appropriate
            %% connection adds a prechannel
            maybe_reserve(Consumer, Broker, Type);
        {[#wh_amqp_assignment{}=Assignment], _} ->
            %% Attempt to assign the consumer to the channel (or
            %% maybe vice-versa, see assign_consumer)
            assign_consumer(Assignment, Consumer, Type)
    end.

-spec assign_consumer(wh_amqp_assignment(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
assign_consumer(#wh_amqp_assignment{}=ChannelAssignment, Consumer, Type) ->
    %% This will attempt to find an existing entry for the consumer...
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        '$end_of_table' ->
            %% When a new consumer requests a channel, and a prechannel
            %% is available, assign the channel to the consumer.
            add_consumer_to_channel(ChannelAssignment, Consumer, Type);
        {[#wh_amqp_assignment{channel=Channel}=ConsumerAssignment], _}
          when is_pid(Channel) ->
            %% When the initial search (outside the gen_server process)
            %% did not have a channel but ahead of the request in the
            %% mailbox was a prechannel that got assigned to the consumer
            %% just return the updated assignment
            lager:debug("consumer ~p already has a valid channel ~p"
                        ,[Consumer, Channel]),
            ConsumerAssignment;
        {[#wh_amqp_assignment{}=ConsumerAssignment], _} ->
            %% When an existing consumer requests a channel, and a
            %% prechannel is available, move the channel to the consumer.
            %% This occurs when the consumer attempts an operation after
            %% loosing a valid channel but before a prechannel assignment.
            move_channel_to_consumer(ChannelAssignment, ConsumerAssignment)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec move_channel_to_consumer(wh_amqp_assignment(), wh_amqp_assignment()) -> wh_amqp_assignment().
move_channel_to_consumer(#wh_amqp_assignment{channel=Channel
                                             ,channel_ref=ChannelRef
                                             ,broker=_Broker
                                             ,connection=Connection}=ChannelAssignment
                         ,#wh_amqp_assignment{consumer=Consumer}=ConsumerAssignment) ->
    Assigned = now(),
    Props = [{#wh_amqp_assignment.channel, Channel}
             ,{#wh_amqp_assignment.channel_ref, ChannelRef}
             ,{#wh_amqp_assignment.connection, Connection}
             ,{#wh_amqp_assignment.assigned, Assigned}
             %% The next updates are for future fetches
             ,{#wh_amqp_assignment.reconnect, 'false'}
             ,{#wh_amqp_assignment.watchers, sets:new()}
            ],
    ets:update_element(?TAB, ConsumerAssignment#wh_amqp_assignment.timestamp, Props),
    ets:delete(?TAB, ChannelAssignment#wh_amqp_assignment.timestamp),
    %% TODO: make sure this channel is rebuilt when required....
    lager:debug("assigned existing consumer ~p an available channel ~p on ~s"
                ,[Consumer, Channel, _Broker]),
    send_notifications(
      ConsumerAssignment#wh_amqp_assignment{channel=Channel                                                  
                                            ,channel_ref=ChannelRef
                                            ,connection=Connection
                                            ,assigned=Assigned}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_consumer_to_channel(wh_amqp_assignment(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
add_consumer_to_channel(#wh_amqp_assignment{timestamp=Timestamp
                                            ,channel=_Channel
                                            ,broker=_Broker}=Assignment
                        ,Consumer, Type) ->
    Assigned = now(),
    Ref = erlang:monitor('process', Consumer),
    Props = [{#wh_amqp_assignment.consumer, Consumer}
             ,{#wh_amqp_assignment.consumer_ref, Ref}
             ,{#wh_amqp_assignment.assigned, Assigned}
             ,{#wh_amqp_assignment.type, Type}
             %% The next updates are for future fetches
             ,{#wh_amqp_assignment.reconnect, 'false'}
             ,{#wh_amqp_assignment.watchers, sets:new()}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    lager:debug("assigned existing channel ~p on ~s to new consumer ~p"
                ,[_Channel, _Broker, Consumer]),
    send_notifications(
      Assignment#wh_amqp_assignment{consumer=Consumer
                                    ,consumer_ref=Ref
                                    ,assigned=Assigned
                                    ,type=Type}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_channel_primary_broker(ne_binary(), pid(), pid()) -> 'ok'.
add_channel_primary_broker(Broker, Connection, Channel) ->
    Routines = [fun maybe_assign_float/3
                ,fun maybe_assign_sticky/3
                ,fun cache/3
               ],
    add_channel(Routines, Broker, Connection, Channel).

-spec add_channel_alternate_broker(ne_binary(), pid(), pid()) -> 'ok'.
add_channel_alternate_broker(Broker, Connection, Channel) ->
    Routines = [fun maybe_assign_sticky/3
                ,fun cache/3
               ],
    add_channel(Routines, Broker, Connection, Channel).    

-spec add_channel(function(), ne_binary(), pid(), pid()) -> 'ok'.
add_channel([], _, _, _) -> 'ok';
add_channel([Routine|Routines], Broker, Connection, Channel) ->
    case Routine(Broker, Connection, Channel) of
        'true' -> 'ok';
        'false' -> add_channel(Routines, Broker, Connection, Channel)
    end.

-spec maybe_assign_float(ne_binary(), pid(), pid()) -> boolean().
maybe_assign_float(Broker, Connection, Channel) ->
    %% This will find any floating consumers that are currently
    %% connected to another broker and re-assign them to the
    %% new channel (on the new broker)
    MatchSpec = [{#wh_amqp_assignment{type='float'
                                      ,broker='$1'
                                      ,consumer='$2'
                                      ,_='_'},
                  [{'andalso'
                    ,{'=/=', '$1', Broker}
                    ,{'=/=', '$2', 'undefined'}
                   }],
                  ['$_']
                 }],
    case ets:select(?TAB, MatchSpec, 1) of
        '$end_of_table' -> 'false';
        {[#wh_amqp_assignment{}=Assignment], _} ->
            assign_channel(Assignment, Broker, Connection, Channel)
    end.

-spec maybe_assign_sticky(ne_binary(), pid(), pid()) -> boolean().
maybe_assign_sticky(Broker, Connection, Channel) ->
    %% This will find any sticky consumers for this broker
    %% without a channel and assign this new channel to them
    MatchSpec = [{#wh_amqp_assignment{type='sticky'
                                      ,broker=Broker
                                      ,channel='undefined'
                                      ,consumer='$1'
                                      ,_='_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$_']
                 }],
    case ets:select(?TAB, MatchSpec, 1) of
        '$end_of_table' -> 'false';
        {[#wh_amqp_assignment{}=Assignment], _} ->
            assign_channel(Assignment, Broker, Connection, Channel)
    end.

-spec assign_channel(wh_amqp_assignment(), ne_binary(), pid(), pid()) -> 'true'.
assign_channel(#wh_amqp_assignment{timestamp=Timestamp
                                   ,consumer=Consumer}=Assignment
               ,Broker, Connection, Channel) ->
    Assigned = now(),
    Ref = erlang:monitor('process', Channel),
    Props = [{#wh_amqp_assignment.channel, Channel}
             ,{#wh_amqp_assignment.channel_ref, Ref}
             ,{#wh_amqp_assignment.broker, Broker}
             ,{#wh_amqp_assignment.connection, Connection}
             ,{#wh_amqp_assignment.assigned, Assigned}
             ,{#wh_amqp_assignment.reconnect, 'false'}
             ,{#wh_amqp_assignment.watchers, sets:new()}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    ReconnectedAssigment 
        = Assignment#wh_amqp_assignment{channel=Channel
                                        ,channel_ref=Ref
                                        ,broker=Broker
                                        ,connection=Connection
                                        ,assigned=Assigned},
    _ = maybe_reconnect(ReconnectedAssigment),
    send_notifications(ReconnectedAssigment),
    lager:debug("assigned new channel ~p on ~s to consumer ~p"
                ,[Channel, Broker, Consumer]),
    'true'.

-spec cache(ne_binary(), pid(), pid()) -> 'true'.
cache(Broker, Connection, Channel) ->
    Ref = erlang:monitor('process', Channel),
    lager:debug("added new channel ~p on ~s to available pool"
                ,[Channel, Broker]),
    ets:insert(?TAB, #wh_amqp_assignment{channel=Channel
                                         ,channel_ref=Ref
                                         ,broker=Broker
                                         ,connection=Connection
                                        }).

-spec maybe_reconnect(wh_amqp_assignment()) -> 'ok'.
maybe_reconnect(#wh_amqp_assignment{reconnect='false'}) -> 'ok';
maybe_reconnect(#wh_amqp_assignment{consumer=Consumer}=Assignment) ->
    reconnect(Assignment, wh_amqp_history:get(Consumer)).

-spec reconnect(wh_amqp_assignment(), wh_amqp_commands()) -> 'ok'.
reconnect(_, []) -> 'ok';
reconnect(Assignment, [Command|Commands]) ->
    _ = wh_amqp_channel:command(Assignment, Command),
    reconnect(Assignment, Commands).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_notifications(wh_amqp_assignment()) -> wh_amqp_assignment().
send_notifications(#wh_amqp_assignment{}=Assignment) ->
    notify_connection(Assignment),
    notify_consumer(Assignment),
    notify_watchers(Assignment).

-spec notify_connection(wh_amqp_assignment()) -> 'ok'.
notify_connection(#wh_amqp_assignment{connection=Connection}) ->
    %% Trigger the connection to primitively open a new channel (prechannel)
    wh_amqp_connection:create_prechannel(Connection).

-spec notify_consumer(wh_amqp_assignment()) -> 'ok'.
notify_consumer(#wh_amqp_assignment{consumer=Consumer
                                    ,reconnect=Reconnect}) ->
    %% Trigger gen_server to continue with AMQP initialization
    gen_server:cast(Consumer, {'wh_amqp_assignment', {'new_channel', Reconnect}}).
                             

-spec notify_watchers(wh_amqp_assignment()) -> wh_amqp_assignment().
notify_watchers(#wh_amqp_assignment{watchers=Watchers}=Assignment) ->
    notify_watchers(Assignment, sets:to_list(Watchers)).

-spec notify_watchers(wh_amqp_assignment(), [pid(),...] | []) -> wh_amqp_assignment().
notify_watchers(#wh_amqp_assignment{}=Assignment, []) ->
    Assignment#wh_amqp_assignment{watchers=sets:new(), reconnect='false'};
notify_watchers(#wh_amqp_assignment{}=Assignment, [Watcher|Watchers]) ->
    %% Notify consumer processes waiting on a channel that they where just
    %% assigned one
    Watcher ! Assignment,
    notify_watchers(Assignment, Watchers).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_reserve(api_binary(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
maybe_reserve(Consumer, Broker, Type) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        '$end_of_table' -> 
            %% This handles the condition when a consumer requested a channel but
            %% the broker was not avaiable (or there where none for a float)...
            %% the first time.  Add a reservation for matching prechannels.
            reserve(Consumer, Broker, Type);
        {[#wh_amqp_assignment{}=ExistingAssignment], _} ->
            %% This handles the condition when a consumer requested a channel,
            %% but the broker was not available (or there where none for a float);
            %% however, this is not the first request so ignore the impatient bastard.
            lager:debug("consumer ~p still waiting on AMQP channel from ~s"
                        ,[Consumer, Broker]),
            ExistingAssignment
    end.

-spec reserve(api_binary(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
reserve(Consumer, Broker, 'sticky') when Broker =/= 'undefined' ->
    Ref = erlang:monitor('process', Consumer),
    Assignment = #wh_amqp_assignment{consumer=Consumer
                                     ,consumer_ref=Ref
                                     ,broker=Broker
                                     ,type='sticky'
                                    },
    ets:insert(?TAB, Assignment),
    lager:debug("consumer ~p waiting on sticky AMQP channel from ~s"
                ,[Consumer, Broker]),
    Assignment;
reserve(Consumer, _, 'float') ->
    Ref = erlang:monitor('process', Consumer),
    Assignment = #wh_amqp_assignment{consumer=Consumer
                                     ,consumer_ref=Ref
                                     ,type='float'
                                    },
    ets:insert(?TAB, Assignment),
    lager:debug("consumer ~p waiting on AMQP channel from current broker"
                ,[Consumer]),
    Assignment.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type down_match() :: {'channel' | 'consumer', wh_amqp_assignment()}.
-type down_matches() :: [down_match(),...] | [].

-spec handle_down_msg(down_matches(), _) -> 'ok'.
handle_down_msg([], _) -> 'ok';
handle_down_msg([Match|Matches], Reason) ->    
    _ = handle_down_match(Match, Reason),
    handle_down_msg(Matches, Reason).

-spec handle_down_match(down_match(), _) -> any().
handle_down_match({'consumer', #wh_amqp_assignment{consumer=Consumer}=Assignment}
                  ,_Reason) ->
    lager:debug("consumer ~p, went down without closing channel: ~p"
                ,[Consumer, _Reason]),
    _ = log_short_lived(Assignment),
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    release_assignments(ets:match_object(?TAB, MatchSpec, 1));
handle_down_match({'channel', #wh_amqp_assignment{timestamp=Timestamp
                                                  ,channel=Channel
                                                  ,broker=Broker
                                                  ,consumer='undefined'}}
                  ,Reason) ->
    lager:debug("unused channel ~p on ~s went down: ~p"
                ,[Channel, Broker, Reason]),
    ets:delete(?TAB, Timestamp);
handle_down_match({'channel', #wh_amqp_assignment{timestamp=Timestamp
                                                  ,channel=Channel
                                                  ,type='float'
                                                  ,broker=Broker
                                                  ,consumer=Consumer}}
                  ,Reason) ->
    Props = [{#wh_amqp_assignment.channel, 'undefined'}
             ,{#wh_amqp_assignment.channel_ref, 'undefined'}
             ,{#wh_amqp_assignment.connection, 'undefined'}
             ,{#wh_amqp_assignment.broker, 'undefined'}
             ,{#wh_amqp_assignment.reconnect, 'true'}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    %% TODO: Attempt to assign...
    gen_server:cast(?MODULE, {'maybe_reassign', Consumer}),
    lager:debug("floating channel ~p on ~s went down: ~p"
                ,[Channel, Broker, Reason]);
handle_down_match({'channel', #wh_amqp_assignment{timestamp=Timestamp
                                                  ,channel=Channel
                                                  ,type='sticky'
                                                  ,broker=Broker
                                                  ,consumer=Consumer}}
                  ,Reason) ->
    Props = [{#wh_amqp_assignment.channel, 'undefined'}
             ,{#wh_amqp_assignment.channel_ref, 'undefined'}
             ,{#wh_amqp_assignment.connection, 'undefined'}
             ,{#wh_amqp_assignment.reconnect, 'true'}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    %% TODO: Attempt to assign...
    gen_server:cast(?MODULE, {'maybe_reassign', Consumer}),
    lager:debug("sticky channel ~p on ~s went down: ~p"
                ,[Channel, Broker, Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
add_watcher(Consumer, Watcher) ->
    case find(Consumer) of
        #wh_amqp_assignment{channel=Channel}=Assignment 
          when is_pid(Channel) ->
            Watcher ! {'wh_amqp_assignment', Assignment};
        #wh_amqp_assignment{watchers=Watchers
                            ,timestamp=Timestamp} ->
            W = sets:add_element(Watcher, Watchers),
            Props = [{#wh_amqp_assignment.watchers, W}],
            ets:update_element(?TAB, Timestamp, Props)
    end.

-spec wait_for_assignment('infinity') -> wh_amqp_assignment();
                         (non_neg_integer()) -> wh_amqp_assignment() | 
                                                {'error', 'timeout'}.
wait_for_assignment(Timeout) ->
    receive
        #wh_amqp_assignment{channel=Channel}=Assignment
          when is_pid(Channel) -> Assignment
    after
        Timeout -> {'error', 'timeout'}
    end.

-spec request_and_wait(pid(), api_binary(), 'infinity') -> wh_amqp_assignment();
                      (pid(), api_binary(), non_neg_integer()) -> wh_amqp_assignment() | 
                                                                  {'error', 'timeout'}.
request_and_wait(Consumer, Broker, Timeout) when is_pid(Consumer) ->
    case request_channel(Consumer, Broker) of
        #wh_amqp_assignment{channel=Channel}=Assignment 
          when is_pid(Channel) -> Assignment;
        #wh_amqp_assignment{} ->
            gen_server:cast(?MODULE, {'add_watcher', Consumer, self()}),
            wait_for_assignment(Timeout)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_reference(reference()) -> down_matches().
find_reference(Ref) ->
    MatchSpec = [{#wh_amqp_assignment{channel_ref='$1'
                                      ,_='_'}
                  ,[{'=:=', '$1', Ref}]
                  ,[{{'channel', '$_'}}]
                 }
                 ,{#wh_amqp_assignment{consumer_ref='$1'
                                       ,_='_'}
                   ,[{'=:=', '$1', Ref}]
                   ,[{{'consumer', '$_'}}]
                  }],
    ets:select(?TAB, MatchSpec).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec log_short_lived(wh_amqp_assignment()) -> 'ok'.
log_short_lived(#wh_amqp_assignment{assigned=Timestamp}=Assignment) ->
    Duration = wh_util:elapsed_s(Timestamp),
    case Duration < 5 of
        'false' -> 'ok';
        'true' -> 
            lager:warning("short lived assignment (~ps): ~p"
                          ,[Duration, Assignment])
    end.
