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
-export([new_channel/3]).
-export([request_float/0
         ,request_float/1
        ]).
-export([request_sticky/1
         ,request_sticky/2
        ]).
-export([add_broker/1]).
-export([remove_broker/1]).
-export([remove/1]).
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

-spec new_channel(ne_binary(), pid(), pid()) -> 'ok'.
new_channel(Broker, Connection, Channel) ->
    gen_server:cast(?MODULE, {'channel', Broker, Connection, Channel}).

-spec request_float() -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_float() -> request_float(wh_amqp_channel:consumer_pid()).

-spec request_float(pid()) -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_float(Pid) ->
    case find(Pid) of
        #wh_amqp_assignment{channel=Channel}=Assignment
          when is_pid(Channel) -> Assignment;
        _Else ->
            gen_server:call(?MODULE, {'request_float', Pid})
    end.

-spec request_sticky(_) -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_sticky(Broker) ->
    request_sticky(Broker, wh_amqp_channel:consumer_pid()).

-spec request_sticky(_, pid()) -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_sticky(Broker, Pid) ->
    case find(Pid) of
        #wh_amqp_assignment{channel=Channel}=Assignment 
          when is_pid(Channel) -> Assignment;
        _Else ->
            gen_server:call(?MODULE, {'request_sticky', Broker, Pid})
    end.

-spec add_broker(ne_binary()) -> 'ok'.
add_broker(Broker) ->
    gen_server:cast(?MODULE, {'add_broker', Broker}).

-spec remove_broker(ne_binary()) -> 'ok'.
remove_broker(Broker) ->
    gen_server:cast(?MODULE, {'remove_broker', Broker}).

-spec remove(pid()) -> 'ok'.    
remove(Consumer) ->
    gen_server:cast(?MODULE, {'remove', Consumer}).

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
    {'reply', assign_or_reserve(primary_broker(State), Consumer, 'float'), State};
handle_call({'request_sticky', Broker, Consumer}, _, State) ->
    {'reply', assign_or_reserve(Broker, Consumer, 'sticky'), State};
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
handle_cast({'channel', Broker, Connection, Channel}, State) ->
    _ = case primary_broker(State) =:= Broker of
            'true' -> new_channel_primary_broker(Broker, Connection, Channel);
            'false' -> new_channel_alternate_broker(Broker, Connection, Channel)
        end,
    {'noreply', State};
handle_cast({'remove', Consumer}, State) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    _ = remove_assignments(ets:match_object(?TAB, MatchSpec, 1)),
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
-spec remove_assignments({wh_amqp_assignments(), ets:continuation()} | '$end_of_table') -> 'ok'.
remove_assignments('$end_of_table') -> 'ok';
remove_assignments({[#wh_amqp_assignment{consumer_ref=Ref}=Assignment]
                    ,Continuation})
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    remove_assignments({[Assignment#wh_amqp_assignment{consumer_ref='undefined'}]
                       ,Continuation});
remove_assignments({[#wh_amqp_assignment{channel_ref=Ref}=Assignment]
                    ,Continuation})
  when is_reference(Ref) ->
    demonitor(Ref, ['flush']),
    remove_assignments({[Assignment#wh_amqp_assignment{channel_ref='undefined'}]
                       ,Continuation});
remove_assignments({[#wh_amqp_assignment{channel=Channel}=Assignment]
                    ,Continuation})
  when is_pid(Channel) ->
    _ = wh_amqp_channel:close(Channel),
    remove_assignments({[Assignment#wh_amqp_assignment{channel='undefined'}]
                        ,Continuation});
remove_assignments({[#wh_amqp_assignment{timestamp=Timestamp
                                         ,consumer=Consumer}
                    ], Continuation}) ->
    lager:debug("removed assignment for consumer ~p", [Consumer]),
    _ = ets:delete(?TAB, Timestamp),
    remove_assignments(ets:match(Continuation)).

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
-spec assign_or_reserve(api_binary(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
assign_or_reserve('undefined', Consumer, Type) ->
    %% When there is no primary broker we will not be able to 
    %% find a channel so just make a reservation for the
    %% next avaialable when a primary broker starts adding channels
    maybe_reserve(Consumer, Type);
assign_or_reserve(Broker, Consumer, Type) ->
    %% This will find a channel that does not already have
    %% a consumer
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
            %% connection adds a new channel
            maybe_reserve(Broker, Consumer, Type);
        {[#wh_amqp_assignment{}=Assignment], _} ->
            %% Attempt to assign the consumer to the channel (or
            %% maybe vice-versa, see assign_consumer)
            assign_consumer(Assignment, Consumer, Type)
    end.

-spec assign_consumer(wh_amqp_assignment(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
assign_consumer(#wh_amqp_assignment{channel=Channel
                                    ,channel_ref=ChannelRef
                                    ,broker=_Broker
                                    ,connection=Connection}=Assignment
                ,Consumer, Type) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        %% When the initial search (outside the gen_server process)
        %% did not have a channel but ahead of the request in the
        %% mailbox was a new channel that got assigned to the consumer
        %% just return the existing assignment
        {[#wh_amqp_assignment{channel=Channel}=ExistingAssignment], _}
          when is_pid(Channel) ->
            lager:debug("consumer ~p already has a valid channel ~p"
                        ,[Consumer, Channel]),
            ExistingAssignment;
        %% When an existing consumer requests a channel, and one is
        %% available, give the channel to the consumer.  This occurs
        %% when the consumer makes a request prior to a channel added
        %% a connection being assigned (meaning the connection's
        %% channels are currently being assigned to other consumers).
        %% TODO: make sure this channel is rebuilt when required....
        {[#wh_amqp_assignment{}=ExistingAssignment], _} ->
            Props = [{#wh_amqp_assignment.channel, Channel}
                     ,{#wh_amqp_assignment.channel_ref, ChannelRef}
                     ,{#wh_amqp_assignment.connection, Connection}
                     ,{#wh_amqp_assignment.assigned, now()}
                    ],
            ets:update_element(?TAB, ExistingAssignment#wh_amqp_assignment.timestamp, Props),
            ets:delete(?TAB, Assignment#wh_amqp_assignment.timestamp),
            gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', 'false'}}),
            gen_server:cast(Connection, 'create_prechannel'),
            lager:debug("assigned existing channel ~p on ~s to existing consumer ~p"
                        ,[Channel, _Broker, Consumer]),
            ExistingAssignment#wh_amqp_assignment{channel=Channel
                                                  ,channel_ref=ChannelRef
                                                  ,connection=Connection};
        %% When a new consumer requests a channel, and one is 
        %% available, give the consumer to the channel.
        _Else ->
            Ref = erlang:monitor('process', Consumer),
            Props = [{#wh_amqp_assignment.consumer, Consumer}
                     ,{#wh_amqp_assignment.consumer_ref, Ref}
                     ,{#wh_amqp_assignment.type, Type}
                     ,{#wh_amqp_assignment.assigned, now()}
                    ],
            ets:update_element(?TAB, Assignment#wh_amqp_assignment.timestamp, Props),
            gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', 'false'}}),
            gen_server:cast(Connection, 'create_prechannel'),
            lager:debug("assigned existing channel ~p on ~s to new consumer ~p"
                        ,[Channel, _Broker, Consumer]),
            Assignment#wh_amqp_assignment{consumer=Consumer
                                          ,consumer_ref=Ref
                                          ,type=Type}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new_channel_primary_broker(ne_binary(), pid(), pid()) -> 'ok'.
new_channel_primary_broker(Broker, Connection, Channel) ->
    Routines = [fun maybe_assign_float/3
                ,fun maybe_assign_sticky/3
                ,fun cache/3
               ],
    new_channel(Routines, Broker, Connection, Channel).

-spec new_channel_alternate_broker(ne_binary(), pid(), pid()) -> 'ok'.
new_channel_alternate_broker(Broker, Connection, Channel) ->
    Routines = [fun maybe_assign_sticky/3
                ,fun cache/3
               ],
    new_channel(Routines, Broker, Connection, Channel).    

-spec new_channel(function(), ne_binary(), pid(), pid()) -> 'ok'.
new_channel([], _, _, _) -> 'ok';
new_channel([Routine|Routines], Broker, Connection, Channel) ->
    case Routine(Broker, Connection, Channel) of
        'true' -> 'ok';
        'false' -> new_channel(Routines, Broker, Connection, Channel)
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
                                   ,consumer=Consumer
                                   ,reconnect=Reconnect}=Assignment
               ,Broker, Connection, Channel) ->
    Ref = erlang:monitor('process', Channel),
    Props = [{#wh_amqp_assignment.channel, Channel}
             ,{#wh_amqp_assignment.channel_ref, Ref}
             ,{#wh_amqp_assignment.broker, Broker}
             ,{#wh_amqp_assignment.connection, Connection}
             ,{#wh_amqp_assignment.assigned, now()}
             ,{#wh_amqp_assignment.reconnect, 'false'}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    gen_server:cast(Connection, 'create_prechannel'),
    _ = maybe_reconnect(Assignment#wh_amqp_assignment{channel=Channel
                                                      ,channel_ref=Ref
                                                      ,broker=Broker
                                                      ,connection=Connection}),
    gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', Reconnect}}),
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
-spec maybe_reserve(pid(), wh_amqp_type()) -> wh_amqp_assignment().
maybe_reserve(Consumer, Type) ->
    maybe_reserve('undefined', Consumer, Type).

-spec maybe_reserve(api_binary(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
maybe_reserve(Broker, Consumer, Type) ->
    MatchSpec = #wh_amqp_assignment{consumer=Consumer, _='_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        %% This handles the condition when a consumer requested a channel,
        %% but the broker was not available (or there where none for a float);
        %% however, this is not the first request so the reserve exists
        {[#wh_amqp_assignment{}=ExistingAssignment], _} ->
            lager:debug("consumer ~p still waiting on AMQP channel from ~s"
                        ,[Consumer, Broker]),
            ExistingAssignment;
        %% This handles the condition when a consumer requested a channel but
        %% the broker was not avaiable (or there where none for a float)...
        %% the first time ;)
        _Else -> reserve(Broker, Consumer, Type)
    end.

-spec reserve(api_binary(), pid(), wh_amqp_type()) -> wh_amqp_assignment().
reserve(Broker, Consumer, 'sticky') when Broker =/= 'undefined' ->
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
reserve(_, Consumer, 'float') ->
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
    remove_assignments(ets:match_object(?TAB, MatchSpec, 1));
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
                                                  ,broker=Broker}}
                  ,Reason) ->
    Props = [{#wh_amqp_assignment.channel, 'undefined'}
             ,{#wh_amqp_assignment.channel_ref, 'undefined'}
             ,{#wh_amqp_assignment.connection, 'undefined'}
             ,{#wh_amqp_assignment.broker, 'undefined'}
             ,{#wh_amqp_assignment.reconnect, 'true'}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    %% TODO: Attempt to assign...
    lager:debug("floating channel ~p on ~s went down: ~p"
                ,[Channel, Broker, Reason]);
handle_down_match({'channel', #wh_amqp_assignment{timestamp=Timestamp
                                                  ,channel=Channel
                                                  ,type='sticky'
                                                  ,broker=Broker}}
                  ,Reason) ->
    Props = [{#wh_amqp_assignment.channel, 'undefined'}
             ,{#wh_amqp_assignment.channel_ref, 'undefined'}
             ,{#wh_amqp_assignment.connection, 'undefined'}
             ,{#wh_amqp_assignment.reconnect, 'true'}
            ],
    ets:update_element(?TAB, Timestamp, Props),
    %% TODO: Attempt to assign...
    lager:debug("sticky channel ~p on ~s went down: ~p"
                ,[Channel, Broker, Reason]).

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

-spec log_short_lived(_) -> 'ok'.
log_short_lived(#wh_amqp_assignment{assigned=Timestamp}=Assignment) ->
    Duration = wh_util:elapsed_s(Timestamp),
    case Duration < 5 of
        'false' -> 'ok';
        'true' -> 
            lager:warning("short lived assignment (~ps): ~p"
                          ,[Duration, Assignment])
    end.
