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
    MatchSpec = #wh_amqp_assignment{consumer = Consumer, _ = '_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} -> Assignment;
        _Else -> {'error', 'no_channel'}
    end.

new_channel(Broker, Connection, Channel) ->
    gen_server:cast(?MODULE, {'channel', Broker, Connection, Channel}).

-spec request_float() -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_float() -> request_float(wh_amqp_channel:consumer_pid()).

-spec request_float(pid()) -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_float(Pid) ->
    case find(Pid) of
        #wh_amqp_assignment{}=Assignment -> Assignment;
        {'error', 'no_channel'} ->
            gen_server:call(?MODULE, {'request_float', Pid})
    end.

-spec request_sticky(_) -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_sticky(Broker) ->
    request_sticky(Broker, wh_amqp_channel:consumer_pid()).

-spec request_sticky(_, pid()) -> wh_amqp_assignment() | {'error', 'no_channel'}.
request_sticky(Broker, Pid) ->
    case find(Pid) of
        #wh_amqp_assignment{}=Assignment -> Assignment;
        {'error', 'no_channel'} ->
            gen_server:call(?MODULE, {'request_sticky', Broker, Pid})
    end.

add_broker(Broker) ->
    gen_server:cast(?MODULE, {'add_broker', Broker}).
    
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
                       ,{'keypos', #wh_amqp_assignment.created}
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
assign_or_reserve('undefined', Consumer, Type) -> reserve(Consumer, Type);
assign_or_reserve(Broker, Consumer, Type) ->
    %% This will find a channel that does not already have
    %% a consumer and assign this consumer to it (setting
    %% the assignment type as it does so) or reserve an 
    %% assignment for the next channel
    MatchSpec = [{#wh_amqp_assignment{channel = '$1'
                                      ,consumer = 'undefined'
                                      ,broker = Broker
                                      ,_ = '_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$_']}
                ],
    case ets:select(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} -> 
            assign_consumer(Assignment, Consumer, Type);
        '$end_of_table' -> reserve(Broker, Consumer, Type)
    end.

assign_consumer(#wh_amqp_assignment{created=Created
                                    ,connection=Connection}=Assignment
                ,Consumer, Type) ->
    MatchSpec = #wh_amqp_assignment{consumer = Consumer, _ = '_'},
    case ets:match_object(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} -> 
            io:format("MARKER 1~n", []),
            Assignment;
        _Else ->
            Ref = erlang:monitor('process', Consumer),
            ets:update_element(?TAB, Created, [{#wh_amqp_assignment.consumer, Consumer}
                                               ,{#wh_amqp_assignment.consumer_ref, Ref}
                                               ,{#wh_amqp_assignment.type, Type}
                                              ]),
            gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', 'false'}}),
            gen_server:cast(Connection, {'wh_amqp_channel', 'channel_assigned'}),
            Assignment#wh_amqp_assignment{consumer = Consumer
                                          ,consumer_ref = Ref
                                          ,type = Type}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
new_channel_primary_broker(Broker, Connection, Channel) ->
    Routines = [fun maybe_reassign/3
                ,fun maybe_assign_sticky/3
                ,fun maybe_assign/3
                ,fun cache/3
               ],
    new_channel(Routines, Broker, Connection, Channel).

new_channel_alternate_broker(Broker, Connection, Channel) ->
    Routines = [fun maybe_assign_sticky/3
                ,fun cache/3
               ],
    new_channel(Routines, Broker, Connection, Channel).    

new_channel([], _, _, _) -> 'ok';
new_channel([Routine|Routines], Broker, Connection, Channel) ->
    case Routine(Broker, Connection, Channel) of
        'true' -> 'ok';
        'false' -> new_channel(Routines, Broker, Connection, Channel)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
maybe_reassign(Broker, Connection, Channel) ->
    %% This will find any floating consumers that are currently
    %% connected to another broker and re-assign them to the
    %% new channel (on the new broker)
    MatchSpec = [{#wh_amqp_assignment{type = 'float'
                                      ,broker = '$1'
                                      ,consumer = '$2'
                                      ,_ = '_'},
                  [{'andalso'
                    ,{'=/=', '$1', Broker}
                    ,{'=/=', '$2', 'undefined'}
                   }],
                  ['$_']}
                ],
    case ets:select(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} ->
            io:format("MARKER 2~n", []),
            assign_channel(Assignment, Broker, Connection, Channel);
        '$end_of_table' -> 'false'

    end.

maybe_assign_sticky(Broker, Connection, Channel) ->
    %% This will find any sticky consumers for this broker
    %% without a channel and assign this new channel to them
    MatchSpec = [{#wh_amqp_assignment{type = 'sticky'
                                      ,broker = Broker
                                      ,channel = 'undefined'
                                      ,consumer = '$1'
                                      ,_ = '_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$_']}
                ],
    case ets:select(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} ->
            io:format("MARKER 3~n", []),
            assign_channel(Assignment, Broker, Connection, Channel);
        '$end_of_table' -> 'false'

    end.   

maybe_assign(Broker, Connection, Channel) ->
    %% This will find any consumer without a channel and 
    %% assign this new channel to it
    MatchSpec = [{#wh_amqp_assignment{channel = 'undefined'
                                      ,consumer = '$1'
                                      ,_ = '_'},
                  [{'=/=', '$1', 'undefined'}],
                  ['$_']}
                ],
    case ets:select(?TAB, MatchSpec, 1) of
        {[#wh_amqp_assignment{}=Assignment], _} ->
            io:format("MARKER 4 ~p~n", [Assignment]),
            assign_channel(Assignment, Broker, Connection, Channel);
        '$end_of_table' -> 'false'

    end.

assign_channel(#wh_amqp_assignment{created=Created
                                   ,consumer=Consumer}
               ,Broker, Connection, Channel) ->
    Ref = erlang:monitor('process', Channel),
    ets:update_element(?TAB, Created, [{#wh_amqp_assignment.channel, Channel}
                                       ,{#wh_amqp_assignment.channel_ref, Ref}
                                       ,{#wh_amqp_assignment.broker, Broker}
                                       ,{#wh_amqp_assignment.connection, Connection}
                                      ]),
%%    gen_server:cast(Consumer, {'wh_amqp_channel', {'new_channel', 'false'}}),
    gen_server:cast(Connection, {'wh_amqp_channel', 'channel_assigned'}),
    'true'.

cache(Broker, Connection, Channel) ->
    Ref = erlang:monitor('process', Channel),
    ets:insert(?TAB, #wh_amqp_assignment{channel = Channel
                                         ,channel_ref = Ref
                                         ,broker = Broker
                                         ,connection = Connection
                                        }).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
reserve(Consumer, Type) ->
    reserve('undefined', Consumer, Type).

reserve(Broker, Consumer, Type) ->
    Ref = erlang:monitor('process', Consumer),
    Assignment = #wh_amqp_assignment{consumer = Consumer
                                     ,consumer_ref = Ref
                                     ,broker = Broker
                                     ,type = Type
                                    },
    ets:insert(?TAB, Assignment),
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
handle_down_msg([_|_]=Matches, Reason) ->
    _ = [handle_down_match(Match, Reason) || Match <- Matches],
    'ok'.

-spec handle_down_match(down_match(), _) -> any().
handle_down_match({'consumer', #wh_amqp_assignment{created=Created}=Assignment}, Reason) ->    
    lager:notice("consumer went down without closing channel: ~p", [Reason]),
    ets:delete(?TAB, Created),
    wh_amqp_channel:close(Assignment),
    _ = log_short_lived(Assignment);
handle_down_match({'channel', #wh_amqp_assignment{broker=Broker}}
                  ,{'shutdown',{'connection_closing', Reason}}) ->
    %% TODO: disconnect broker
    lager:critical("channel died when connection to '~s' was lost: ~p"
                   ,[Broker, Reason]);
handle_down_match({'channel', #wh_amqp_assignment{broker=Broker}=Assignment}, Reason) ->
    %% TODO: re-assign channel
    lager:notice("channel went down while still connected to '~s': ~p"
                 ,[Broker, Reason]).

-spec find_reference(reference()) -> down_matches().
find_reference(Ref) ->
    MatchSpec = [{#wh_amqp_assignment{channel_ref = '$1'
                                      ,_ = '_'}
                  ,[{'=:=', '$1', Ref}]
                  ,[{{'channel', '$_'}}]
                 }
                 ,{#wh_amqp_assignment{consumer_ref = '$1'
                                       ,_ = '_'}
                   ,[{'=:=', '$1', Ref}]
                   ,[{{'consumer', '$_'}}]
                  }],
    ets:select(?TAB, MatchSpec).

-spec log_short_lived(wh_amqp_assignment()) -> 'ok'.
log_short_lived(#wh_amqp_assignment{created=Created}=Channel) ->
    Duration = wh_util:elapsed_s(Created),
    case Duration < 5 of
        'false' -> 'ok';
        'true' -> lager:info("short lived channel (~ps): ~p", [Duration, Channel])
    end.
