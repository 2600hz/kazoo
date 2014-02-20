%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_connections).

-behaviour(gen_server).

-export([new/1
         ,new/2
        ]).
-export([add/1
         ,add/2
        ]).
-export([remove/1]).
-export([broker_connections/1]).
-export([broker_available_connections/1]).
-export([primary_broker/0]).
-export([federated_brokers/0]).
-export([broker_zone/1]).
-export([available/1]).
-export([unavailable/1]).
-export([is_available/0]).
-export([wait_for_available/0]).
-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(TAB, ?MODULE).

-include("amqp_util.hrl").

-record(state, {watchers=sets:new()}).
-type state() :: #state{}.

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

-spec new(wh_amqp_connection() | text()) -> wh_amqp_connection() | {'error', _}.
new(Broker) -> new(Broker, 'local').

-spec new(wh_amqp_connection() | text(), text()) -> wh_amqp_connection() | {'error', _}.
new(Broker, Zone) ->
    case broker_connections(Broker) =:= 0 of
        'false' -> {'error', 'exists'};
        'true' -> wh_amqp_connections:add(Broker, Zone)
    end.

-spec add(wh_amqp_connection() | text()) -> wh_amqp_connection() | {'error', _}.
add(Broker) -> add(Broker, 'local').

-spec add(wh_amqp_connection() | text(), text()) -> wh_amqp_connection() | {'error', _}.
add(#wh_amqp_connection{broker=Broker}=Connection, Zone) ->
    case wh_amqp_connection_sup:add(Connection) of
        {'ok', Pid} ->
            gen_server:cast(?MODULE, {'new_connection', Pid, Broker, Zone}),
            Connection;
        {'error', Reason} ->
            lager:warning("unable to start amqp connection to '~s': ~p"
                          ,[Broker, Reason]),
            {'error', Reason}
    end;
add(Broker, Zone) when not is_binary(Broker) ->
    add(wh_util:to_binary(Broker), Zone);
add(Broker, Zone) when not is_atom(Zone) ->
    add(Broker, wh_util:to_atom(Zone, 'true'));
add(Broker, Zone) ->
    case catch amqp_uri:parse(wh_util:to_list(Broker)) of
        {'EXIT', _R} ->
            lager:error("failed to parse AMQP URI '~s': ~p", [Broker, _R]),
            {'error', 'invalid_uri'};
        {'error', {Info, _}} ->
            lager:error("failed to parse AMQP URI '~s': ~p", [Broker, Info]),
            {'error', 'invalid_uri'};
        {'ok', #amqp_params_network{}=Params} ->
            add(#wh_amqp_connection{broker=Broker
                                    ,params=Params#amqp_params_network{connection_timeout=500}
                                   }
                ,Zone);
        {'ok', Params} ->
            add(#wh_amqp_connection{broker=Broker
                                    ,params=Params
                                   }
                ,Zone)
    end.

-spec remove(pids() | pid() | text()) -> 'ok'.
remove([]) -> 'ok';
remove([Connection|Connections]) when is_pid(Connection) ->
    _ = wh_amqp_connection_sup:remove(Connection),
    remove(Connections);
remove(Connection) when is_pid(Connection) ->
    wh_amqp_connection_sup:remove(Connection);
remove(Broker) when not is_binary(Broker) ->
    remove(wh_util:to_binary(Broker));
remove(Broker) ->
    Pattern = #wh_amqp_connections{broker=Broker
                                   ,connection='$1'
                                   ,_='_'
                                  },
    remove([Connection || [Connection] <- ets:match(?TAB, Pattern)]).

-spec available(pid()) -> 'ok'.
available(Connection) when is_pid(Connection) ->
    gen_server:cast(?MODULE, {'connection_available', Connection}).

-spec unavailable(pid()) -> 'ok'.
unavailable(Connection) when is_pid(Connection) ->
    gen_server:cast(?MODULE, {'connection_unavailable', Connection}).


-spec broker_connections(ne_binary()) -> non_neg_integer().
broker_connections(Broker) ->
    MatchSpec = [{#wh_amqp_connections{broker=Broker
                                       ,_='_'
                                      },
                  [],
                  ['true']}
                ],
    ets:select_count(?TAB, MatchSpec).

-spec broker_available_connections(ne_binary()) -> non_neg_integer().
broker_available_connections(Broker) ->
    MatchSpec = [{#wh_amqp_connections{broker=Broker
                                       ,available='true'
                                       ,_='_'
                                      },
                  [],
                  ['true']}
                ],
    ets:select_count(?TAB, MatchSpec).

-spec primary_broker() -> api_binary().
primary_broker() ->
    Pattern = #wh_amqp_connections{available='true'
                                   ,zone='local'
                                   ,broker='$1'
                                   ,_='_'
                                  },
    case lists:sort([Broker
                     || [Broker] <- ets:match(?TAB, Pattern)
                    ])
    of
        [] -> 'undefined';
        [Broker|_] -> Broker
    end.

-spec federated_brokers() -> ne_binaries().
federated_brokers() ->
    MatchSpec = [{#wh_amqp_connections{zone='$1'
                                       ,broker='$2'
                                       ,_='_'
                                      },
                  [{'=/=', '$1', 'local'}],
                  ['$2']
                 }
                ],
    sets:to_list(
      sets:from_list(
        ets:select(?TAB, MatchSpec)
       )
     ).

-spec broker_zone(ne_binary()) -> atom().
broker_zone(Broker) ->
    Pattern = #wh_amqp_connections{broker=Broker
                                   ,zone='$1'
                                   ,_='_'
                                  },
    case ets:match(?TAB, Pattern) of
        [[Zone]|_] -> Zone;
        _Else -> 'unknown'
    end.

-spec is_available() -> boolean().
is_available() -> primary_broker() =/= 'undefined'.

-spec wait_for_available() -> 'ok'.
wait_for_available() -> wait_for_available('infinity').

-spec wait_for_available('infinity') -> 'ok';
                        (non_neg_integer()) -> 'ok' | {'error', 'timeout'}.
wait_for_available(Timeout) ->
    case is_available() of
        'true' -> 'ok';
        'false' ->
            gen_server:cast(?MODULE, {'add_watcher', self()}),
            wait_for_notification(Timeout)
    end.

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
    wh_util:put_callid(?LOG_SYSTEM_ID),
    _ = ets:new(?TAB, ['named_table'
                       ,{'keypos', #wh_amqp_connections.connection}
                       ,'protected'
                       ,{'read_concurrency', 'true'}
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
handle_cast({'new_connection', Connection, Broker, Zone}, State) ->
    Ref = erlang:monitor('process', Connection),
    _ = ets:insert(?TAB, #wh_amqp_connections{connection=Connection
                                              ,connection_ref=Ref
                                              ,broker=Broker
                                              ,zone=Zone
                                             }),
    {'noreply', State, 'hibernate'};
handle_cast({'connection_available', Connection}, State) ->
    lager:debug("connection ~p is now available", [Connection]),
    Props = [{#wh_amqp_connections.available, 'true'}],
    _ = ets:update_element(?TAB, Connection, Props),
    {'noreply', notify_watchers(State), 'hibernate'};
handle_cast({'connection_unavailable', Connection}, State) ->
    lager:warning("connection ~p is no longer available", [Connection]),
    Props = [{#wh_amqp_connections.available, 'false'}],
    _ = ets:update_element(?TAB, Connection, Props),
    {'noreply', State, 'hibernate'};
handle_cast({'add_watcher', Watcher}, State) ->
    case is_available() of
        'false' -> {'noreply', add_watcher(Watcher, State), 'hibernate'};
        'true' ->
            _ = notify_watcher(Watcher),
            {'noreply', State, 'hibernate'}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State, 'hibernate'}.

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
handle_info({'DOWN', Ref, 'process', Connection, _Reason}, State) ->
    lager:warning("connection ~p went down: ~p"
                  ,[Connection, _Reason]),
    erlang:demonitor(Ref, ['flush']),
    _ = ets:delete(?TAB, Connection),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

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
    lager:debug("AMQP connections terminating: ~p", [_Reason]).

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
-spec add_watcher(pid(), state()) -> state().
add_watcher(Watcher, #state{watchers=Watchers}=State) ->
    State#state{watchers=sets:add_element(Watcher, Watchers)}.

-spec notify_watchers(state()) -> state().
notify_watchers(#state{watchers=[]}=State) ->
    State#state{watchers=sets:new()};
notify_watchers(#state{watchers=[Watcher|Watchers]}=State) ->
    _ = notify_watcher(Watcher),
    notify_watchers(State#state{watchers=Watchers});
notify_watchers(#state{watchers=Watchers}=State) ->
    notify_watchers(State#state{watchers=sets:to_list(Watchers)}).

-spec notify_watcher(pid()) -> any().
notify_watcher(Watcher) ->
    Watcher ! {'wh_amqp_connections', 'connection_available'}.

-spec wait_for_notification(wh_timeout()) ->
                                   'ok' |
                                   {'error', 'timeout'}.
wait_for_notification(Timeout) ->
    receive
        {'wh_amqp_connections', 'connection_available'} -> 'ok'
    after
        Timeout -> {'error', 'timeout'}
    end.
