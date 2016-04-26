%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Simple cache server
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_cache).

-behaviour(gen_listener).

-export([start_link/0, start_link/1
         ,start_link/2, start_link/3
         ,stop_local/1
        ]).
-export([store/2, store/3]).
-export([peek/1]).
-export([fetch/1, fetch_keys/0]).
-export([erase/1]).
-export([flush/0]).
-export([filter/1]).
-export([dump/0, dump/1]).
-export([wait_for_key/1, wait_for_key/2]).

-export([store_local/3, store_local/4]).
-export([peek_local/2]).
-export([fetch_local/2, fetch_keys_local/1]).
-export([erase_local/2]).
-export([flush_local/1]).
-export([filter_local/2, filter_erase_local/2]).
-export([dump_local/1, dump_local/2]).
-export([wait_for_key_local/2
         ,wait_for_key_local/3
        ]).
-export([handle_document_change/2]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("kz_caches.hrl").

-define(SERVER, ?MODULE).
-define(EXPIRES, ?SECONDS_IN_HOUR). %% an hour
-define(EXPIRE_PERIOD, 10 * ?MILLISECONDS_IN_SECOND).
-define(EXPIRE_PERIOD_MSG, 'expire_cache_objects').
-define(DEFAULT_WAIT_TIMEOUT, 5).

-define(NOTIFY_KEY(Key), {'monitor_key', Key}).

-define(BINDINGS, [{'self', []}]).
%% -define(RESPONDERS, [{{?MODULE, 'handle_document_change'}
%%                       ,[{<<"configuration">>, <<"*">>}]
%%                      }]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DATABASE_BINDING, [{'type', <<"database">>}]).

-type store_options() :: [{'origin', origin_tuple() | origin_tuples()} |
                          {'expires', wh_timeout()} |
                          {'callback', 'undefined' | callback_fun()}
                         ] | [].
-export_type([store_options/0]).

-record(state, {name :: atom()
                ,tab :: ets:tid()
                ,pointer_tab :: ets:tid()
                ,monitor_tab :: ets:tid()
                ,new_channel_flush = 'false' :: boolean()
                ,channel_reconnect_flush = 'false' :: boolean()
                ,new_node_flush = 'false' :: boolean()
                ,expire_node_flush = 'false' :: boolean()
                ,expire_period = ?EXPIRE_PERIOD :: wh_timeout()
                ,expire_period_ref :: reference()
                ,props = [] :: list()
                ,has_monitors = 'false' :: boolean()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), list() | wh_timeout()) -> startlink_ret().
-spec start_link(atom(), wh_timeout(), list()) -> startlink_ret().

start_link() -> start_link(?SERVER).
start_link(Name) -> start_link(Name, ?EXPIRE_PERIOD).

start_link(Name, Props) when is_list(Props) ->
    start_link(Name, ?EXPIRE_PERIOD, Props);
start_link(Name, ExpirePeriod) ->
    start_link(Name, ExpirePeriod, []).

start_link(Name, ExpirePeriod, Props) ->
    case props:get_value('origin_bindings', Props) of
        'undefined' ->
            lager:debug("started new cache process (gen_server): ~s", [Name]),
            gen_server:start_link({'local', Name}, ?MODULE, [Name, ExpirePeriod, Props], []);
        BindingProps ->
            lager:debug("started new cache process (gen_listener): ~s", [Name]),
            Bindings = [{'conf', ['federate' | P]} || P <- maybe_add_db_binding(BindingProps)],
            gen_listener:start_link({'local', Name}, ?MODULE
                                    ,[{'bindings', Bindings}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ]
                                    ,[Name, ExpirePeriod, Props]
                                   )
    end.

-spec stop_local(pid()) -> 'ok'.
stop_local(Srv) ->
    catch gen_server:call(Srv, 'stop'),
    'ok'.

-spec maybe_add_db_binding(wh_proplists()) -> wh_proplists().
maybe_add_db_binding([]) -> [];
maybe_add_db_binding([[]]) -> [[]];
maybe_add_db_binding(BindingProps) ->
    [?DATABASE_BINDING | BindingProps].

-spec store(any(), any()) -> 'ok'.
-spec store(any(), any(), wh_proplist()) -> 'ok'.

store(K, V) -> store(K, V, []).

store(K, V, Props) -> store_local(?SERVER, K, V, Props).

-spec peek(any()) -> {'ok', any()} |
                     {'error', 'not_found'}.
peek(K) -> peek_local(?SERVER, K).

-spec fetch(any()) -> {'ok', any()} |
                      {'error', 'not_found'}.
fetch(K) -> fetch_local(?SERVER, K).

-spec erase(any()) -> 'ok'.
erase(K) -> erase_local(?SERVER, K).

-spec flush() -> 'ok'.
flush() -> flush_local(?SERVER).

-spec fetch_keys() -> [any()].
fetch_keys() -> fetch_keys_local(?SERVER).

-spec filter(fun((any(), any()) -> boolean())) -> [{any(), any()}].
filter(Pred) when is_function(Pred, 2) -> filter_local(?SERVER, Pred).

-spec dump() -> 'ok'.
dump() -> dump('false').

-spec dump(text()) -> 'ok'.
dump(ShowValue) -> dump_local(?SERVER, ShowValue).

-spec wait_for_key(any()) -> {'ok', any()} |
                             {'error', 'timeout'}.
-spec wait_for_key(any(), wh_timeout()) -> {'ok', any()} |
                                           {'error', 'timeout'}.

wait_for_key(Key) -> wait_for_key(Key, ?DEFAULT_WAIT_TIMEOUT).

wait_for_key(Key, Timeout) -> wait_for_key_local(?SERVER, Key, Timeout).

%% Local cache API
-spec store_local(server_ref(), any(), any()) -> 'ok'.
-spec store_local(server_ref(), any(), any(), wh_proplist()) -> 'ok'.

store_local(Srv, K, V) -> store_local(Srv, K, V, []).

store_local(Srv, K, V, Props) when is_atom(Srv) ->
    case whereis(Srv) of
        'undefined' ->
            throw({'error', 'unknown_cache', Srv});
        Pid -> store_local(Pid, K, V, Props)
    end;
store_local(Srv, K, V, Props) when is_pid(Srv) ->
    gen_server:cast(Srv, {'store', #cache_obj{key=K
                                              ,value=V
                                              ,expires=get_props_expires(Props)
                                              ,callback=get_props_callback(Props)
                                              ,origin=get_props_origin(Props)
                                             }}).

-spec peek_local(atom(), any()) -> {'ok', any()} |
                                   {'error', 'not_found'}.
peek_local(Srv, K) ->
    try ets:lookup_element(Srv, K, #cache_obj.value) of
        Value -> {'ok', Value}
    catch
        'error':'badarg' ->
            {'error', 'not_found'}
    end.

-spec fetch_local(atom(), any()) -> {'ok', any()} |
                                    {'error', 'not_found'}.
fetch_local(Srv, K) ->
    case peek_local(Srv, K) of
        {'error', 'not_found'}=E -> E;
        {'ok', Value} ->
            ets:update_element(Srv, K, {#cache_obj.timestamp, wh_util:current_tstamp()}),
%            gen_server:cast(Srv, {'update_timestamp', K, }),
            {'ok', Value}
    end.

-spec erase_local(atom(), any()) -> 'ok'.
erase_local(Srv, K) ->
    case peek_local(Srv, K) of
        {'error', 'not_found'} -> 'ok';
        {'ok', _} -> gen_server:cast(Srv, {'erase', K})
    end.

-spec flush_local(atom()) -> 'ok'.
flush_local(Srv) ->
    gen_server:cast(Srv, {'flush'}).

-spec fetch_keys_local(atom()) -> list().
fetch_keys_local(Srv) ->
    MatchSpec = [{#cache_obj{key = '$1'
                            ,_ = '_'
                            }
                 ,[]
                 ,['$1']
                 }],
    ets:select(Srv, MatchSpec).

-spec filter_erase_local(atom(), fun((any(), any()) -> boolean())) ->
                                non_neg_integer().
filter_erase_local(Srv, Pred) when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Count) ->
                      case Pred(K, V) of
                          'true' -> erase_local(Srv, K), Count+1;
                          'false' -> Count
                      end;
                 (_, Count) -> Count
              end
              ,0
              ,Srv
             ).

-spec filter_local(atom(), fun((any(), any()) -> boolean())) -> [{any(), any()}].
filter_local(Srv, Pred) when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Acc) ->
                      case Pred(K, V) of
                          'true' -> [{K, V}|Acc];
                          'false' -> Acc
                      end;
                 (_, Acc) -> Acc
              end
             ,[]
             ,Srv
             ).

-spec dump_local(text()) -> 'ok'.
dump_local(Srv) -> dump_local(Srv, 'false').

-spec dump_local(text(), text() | boolean()) -> 'ok'.
dump_local(Srv, ShowValue) when not is_atom(Srv) ->
    dump_local(wh_util:to_atom(Srv), ShowValue);
dump_local(Srv, ShowValue) when not is_boolean(ShowValue) ->
    dump_local(Srv, wh_util:to_boolean(ShowValue));
dump_local(Srv, ShowValue) ->
    {PointerTab, MonitorTab} = gen_listener:call(Srv, {'tables'}),

    _ = [dump_table(Tab, ShowValue)
         || Tab <- [Srv, PointerTab, MonitorTab]
        ],
    'ok'.

-spec dump_table(ets:tid(), boolean()) -> 'ok'.
dump_table(Tab, ShowValue) ->
    Now = wh_util:current_tstamp(),
    io:format("Table ~p~n", [ets:info(Tab, 'name')]),
    _ = [display_cache_obj(CacheObj, ShowValue, Now)
         || CacheObj <- ets:match_object(Tab, #cache_obj{_ = '_'})
        ],
    'ok'.

-spec display_cache_obj(cache_obj(), boolean(), gregorian_seconds()) -> 'ok'.
display_cache_obj(#cache_obj{key=Key
                            ,value=Value
                            ,timestamp=Timestamp
                            ,expires=Expires
                            ,origin=Origin
                            ,callback=Callback
                            }
                 ,ShowValue
                 ,Now
                 ) ->
    io:format("Key: ~300p~n", [Key]),
    io:format("Expires: ~30p~n", [Expires]),
    case is_number(Expires) of
        'true' ->
            io:format("Remaining: ~30p~n", [(Timestamp
                                             + Expires)
                                            - Now
                                           ]);
        'false' -> 'ok'
    end,
    io:format("Origin: ~300p~n", [Origin]),
    io:format("Callback: ~s~n", [Callback =/= 'undefined']),
    case ShowValue of
        'true' -> io:format("Value: ~p~n", [Value]);
        'false' -> 'ok'
    end,
    io:format("~n", []).

-spec wait_for_key_local(atom(), any()) -> {'ok', any()} |
                                           {'error', 'timeout'}.
-spec wait_for_key_local(atom(), any(), wh_timeout()) ->
                                {'ok', any()} |
                                {'error', 'timeout'}.
wait_for_key_local(Srv, Key) ->
    wait_for_key_local(Srv, Key, ?DEFAULT_WAIT_TIMEOUT).

wait_for_key_local(Srv, Key, Timeout) ->
    {'ok', Ref} = gen_server:call(Srv, {'wait_for_key', Key, Timeout}),
    lager:debug("waiting for message with ref ~p", [Ref]),
    receive
        {'exists', Ref, Value} -> {'ok', Value};
        {'store', Ref, Value} -> {'ok', Value};
        {_, Ref, _} -> {'error', 'timeout'}
    end.

-spec handle_document_change(wh_json:object(), state()) -> 'ok' | 'false'.
handle_document_change(JObj, #state{}=State) ->
    Db = wh_json:get_value(<<"Database">>, JObj),
    Type = wh_json:get_value(<<"Type">>, JObj),
    Id = wh_json:get_value(<<"ID">>, JObj),

    _Keys = handle_document_change(Db, Type, Id, State),
    _Keys =/= [] andalso
        lager:debug("removed ~p keys for ~s/~s/~s", [length(_Keys), Db, Id, Type]).

-spec handle_document_change(ne_binary(), ne_binary(), ne_binary(), state()) -> list().

handle_document_change(Db, <<"database">>, _Id, #state{pointer_tab=PointerTab}=State) ->
    MatchSpec = match_db_changed(Db),
    lists:foldl(fun(Obj, Removed) ->
                        erase_changed(Obj, Removed, State)
                end
               ,[]
               ,ets:select(PointerTab, MatchSpec)
               );
handle_document_change(Db, Type, Id, #state{pointer_tab=PointerTab, tab=Tab}=State) ->
    lager:debug("search for ~p : ~p : ~p : ~p", [ets:info(Tab, 'name'), Db, Type, Id]),
    MatchSpec = match_doc_changed(Db, Type, Id),
    Objects = ets:select(PointerTab, MatchSpec),
    lager:debug("select for ~p returned ~b objects", [ets:info(Tab, 'name'), length(Objects)]),
    lists:foldl(fun(Obj, Removed) ->
                        erase_changed(Obj, Removed, State)
                end
               ,[]
               ,ets:select(PointerTab, MatchSpec)
               ).

-spec match_db_changed(ne_binary()) -> ets:match_spec().
match_db_changed(Db) ->
    [{#cache_obj{origin = {'db', Db}, _ = '_'}
      ,[]
      ,['$_']
     }
     ,{#cache_obj{origin = {'db', Db, '_'}, _ = '_'}
       ,[]
       ,['$_']
      }
     ,{#cache_obj{origin = {'type', <<"database">>, Db}, _ = '_'}
       ,[]
       ,['$_']
      }
    ].

-spec match_doc_changed(ne_binary(), ne_binary(), ne_binary()) -> ets:match_spec().
match_doc_changed(Db, Type, Id) ->
    [{#cache_obj{origin = {'db', Db}, _ = '_'}
      ,[]
      ,['$_']
     }
     ,{#cache_obj{origin = {'db', Db, Type}, _ = '_'}
       ,[]
       ,['$_']
      }
     ,{#cache_obj{origin = {'db', Db, Id}, _ = '_'}
       ,[]
       ,['$_']
      }
     ,{#cache_obj{origin = {'type', Type, Id}, _ = '_'}
       ,[]
       ,['$_']
      }
     ,{#cache_obj{origin = {'type', Type}, _ = '_'}
       ,[]
       ,['$_']
      }
    ].

-spec erase_changed(cache_obj(), list(), state()) -> list().
erase_changed(#cache_obj{key=Key}, Removed, #state{tab=Tab
                                                   ,pointer_tab=PointerTab
                                                   ,monitor_tab=MonitorTab
                                                  }) ->
    case lists:member(Key, Removed) of
        'true' -> Removed;
        'false' ->
            lager:debug("removing updated cache object ~-300p", [Key]),
            erase_changed(Key, Tab, PointerTab, MonitorTab),
            [Key | Removed]
    end.

-spec erase_changed(any(), ets:tid(), ets:tid(), ets:tid()) -> 'true'.
erase_changed(Key, Tab, PointerTab, MonitorTab) ->
    maybe_exec_erase_callbacks(Tab, Key),
    maybe_remove_object(Tab, Key),
    maybe_remove_object(PointerTab, Key),
    maybe_remove_object(MonitorTab, Key).

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
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, ExpirePeriod, Props]) ->
    wh_util:put_callid(Name),
    wapi_conf:declare_exchanges(),

    Tab = ets:new(Name
                 ,['set', 'named_table'
                  , {'keypos', #cache_obj.key}
%%                  , 'protected'
                  , 'public'
                  , {write_concurrency, 'true'}
                  , {read_concurrency, 'true'}
                  ]
                 ),
    PointerTab = ets:new(pointer_tab(Name)
                         ,['bag', 'protected', {'keypos', #cache_obj.key}]
                        ),
    MonitorTab = ets:new(monitor_tab(Name)
                         ,['bag', 'protected', {'keypos', #cache_obj.key}]
                        ),

    _ = case props:get_value('new_node_flush', Props) of
            'true' -> wh_nodes:notify_new();
            _ -> 'ok'
        end,
    _ = case props:get_value('expire_node_flush', Props) of
            'true' -> wh_nodes:notify_expire();
            _ -> 'ok'
        end,
    {'ok', #state{name=Name
                  ,tab=Tab
                  ,pointer_tab=PointerTab
                  ,monitor_tab=MonitorTab
                  ,new_channel_flush=props:get_value('new_channel_flush', Props)
                  ,channel_reconnect_flush=props:get_value('channel_reconnect_flush', Props)
                  ,new_node_flush=props:get_value('new_node_flush', Props)
                  ,expire_node_flush=props:get_value('expire_node_flush', Props)
                  ,expire_period=ExpirePeriod
                  ,expire_period_ref=start_expire_period_timer(ExpirePeriod)
                  ,props=Props
                 }}.

-spec pointer_tab(atom()) -> atom().
pointer_tab(Tab) ->
    to_tab(Tab, "_pointers").

-spec monitor_tab(atom()) -> atom().
monitor_tab(Tab) ->
    to_tab(Tab, "_monitors").

-spec to_tab(atom(), list()) -> atom().
to_tab(Tab, Suffix) ->
    wh_util:to_atom(wh_util:to_list(Tab) ++ Suffix, 'true').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec monitor_response_fun(pid(), reference()) -> fun().
monitor_response_fun(Pid, Ref) ->
    fun(_, Value, Reason) -> Pid ! {Reason, Ref, Value} end.

handle_call({'tables'}, _From, #state{pointer_tab=PointerTab
                                      ,monitor_tab=MonitorTab
                                     }=State) ->
    {'reply', {PointerTab, MonitorTab}, State};
handle_call({'wait_for_key', Key, Timeout}
           ,{Pid, _}
           ,#state{tab=Tab
                  ,monitor_tab=MonitorTab
                  }=State
           ) ->
    Ref = make_ref(),
    try ets:lookup_element(Tab, Key, #cache_obj.value) of
        Value ->
            Pid ! {'exists', Ref, Value},
            {'reply', {'ok', Ref}, State}
    catch
        'error':'badarg' ->
            CacheObj = #cache_obj{key=Key
                                 ,value=Ref
                                 ,expires=Timeout
                                 ,callback=monitor_response_fun(Pid, Ref)
                                 },
            ets:insert(MonitorTab, CacheObj),
            {'reply', {'ok', Ref}, State#state{has_monitors='true'}}
    end;
handle_call('stop', _From, State) ->
    lager:debug("recv stop from ~p", [_From]),
    {'stop', 'normal', State};
handle_call({'store', CacheObj}, _From, State) ->
    State1 = handle_store(CacheObj, State),
    {'reply', 'ok', State1};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'store', #cache_obj{key=Key
                                ,value=Value
                                ,origin=Origins
                                }=CacheObj
            }
           ,#state{tab=Tab
                  ,pointer_tab=PointerTab
                  }=State
           ) ->
    ets:insert(Tab, CacheObj#cache_obj{origin='undefined'}),
    insert_origin_pointers(Origins, CacheObj, PointerTab),
    {'noreply', maybe_exec_store_callbacks(State, Key, Value)};

handle_cast({'update_timestamp', Key, Timestamp}, #state{tab=Tab}=State) ->
    ets:update_element(Tab, Key, {#cache_obj.timestamp, Timestamp}),
    {'noreply', State};

handle_cast({'erase', Key}, #state{tab=Tab
                                  ,pointer_tab=PointerTab
                                  ,monitor_tab=MonitorTab
                                  }=State) ->
    erase_changed(Key, Tab, PointerTab, MonitorTab),
    {'noreply', State};

handle_cast({'flush'}, #state{tab=Tab
                             ,pointer_tab=PointerTab
                             ,monitor_tab=MonitorTab
                             }=State) ->
    maybe_exec_flush_callbacks(Tab),
    maybe_exec_flush_callbacks(PointerTab),
    maybe_exec_flush_callbacks(MonitorTab),

    ets:delete_all_objects(Tab),
    ets:delete_all_objects(PointerTab),
    ets:delete_all_objects(MonitorTab),

    {'noreply', State};

handle_cast({'wh_amqp_channel', {'new_channel', 'false'}}
           ,#state{name=Name
                  ,tab=Tab
                  ,pointer_tab=PointerTab
                  ,monitor_tab=MonitorTab
                  ,new_channel_flush='true'
                  }=State
           ) ->
    ets:delete_all_objects(Tab),
    ets:delete_all_objects(PointerTab),
    ets:delete_all_objects(MonitorTab),
    lager:debug("new channel, flush everything from ~s", [Name]),
    {'noreply', State};
handle_cast({'wh_amqp_channel', {'new_channel', 'true'}}
           ,#state{name=Name
                  ,tab=Tab
                  ,pointer_tab=PointerTab
                  ,monitor_tab=MonitorTab
                  ,channel_reconnect_flush='true'
                  }=State
           ) ->
    ets:delete_all_objects(Tab),
    ets:delete_all_objects(PointerTab),
    ets:delete_all_objects(MonitorTab),

    lager:debug("reconnected channel, flush everything from ~s", [Name]),
    {'noreply', State};
handle_cast({'wh_nodes', {'expire', #wh_node{node=Node}}}
           ,#state{name=Name
                  ,tab=Tab
                  ,pointer_tab=PointerTab
                  ,monitor_tab=MonitorTab
                  ,expire_node_flush='true'
                  }=State
           ) ->
    ets:delete_all_objects(Tab),
    ets:delete_all_objects(PointerTab),
    ets:delete_all_objects(MonitorTab),

    lager:debug("node ~s has expired, flush everything from ~s", [Node, Name]),
    {'noreply', State};
handle_cast({'wh_nodes', {'new', #wh_node{node=Node}}}
           ,#state{name=Name
                  ,tab=Tab
                  ,pointer_tab=PointerTab
                  ,monitor_tab=MonitorTab
                  ,new_node_flush='true'
                  }=State) ->
    ets:delete_all_objects(Tab),
    ets:delete_all_objects(PointerTab),
    ets:delete_all_objects(MonitorTab),

    lager:debug("new node ~s, flush everything from ~s", [Node, Name]),
    {'noreply', State};
handle_cast(_, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'timeout', Ref, ?EXPIRE_PERIOD_MSG}
            ,#state{expire_period_ref=Ref
                    ,expire_period=Period
                    ,tab=Tab
                    ,pointer_tab=PointerTab
                    ,monitor_tab=MonitorTab
                   }=State
           ) ->
   _ = expire_objects(Tab, [PointerTab, MonitorTab]),
    {'noreply', State#state{expire_period_ref=start_expire_period_timer(Period)}};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(JObj, #state{tab=Tab}=State) ->
    case (V=wapi_conf:doc_update_v(JObj)) andalso
             (wh_api:node(JObj) =/= wh_util:to_binary(node()) orelse
              wh_json:get_atom_value(<<"Origin-Cache">>, JObj) =/= ets:info(Tab, 'name')
             )
    of
        'true' -> handle_document_change(JObj, State);
        'false' when V -> 'ok';
        'false' -> lager:error("payload invalid for wapi_conf : ~p", [JObj])
    end,
    'ignore'.

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
terminate(_Reason, #state{tab=Tab}) ->
    ets:delete(Tab).

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
-spec get_props_expires(wh_proplist()) -> wh_timeout().
get_props_expires(Props) ->
    case props:get_value('expires', Props) of
        'undefined' -> ?EXPIRES;
        'infinity' -> 'infinity';
        Expires when is_integer(Expires)
                     andalso Expires > 0 ->
            Expires
    end.

-spec get_props_callback(wh_proplist()) -> 'undefined' | callback_fun().
get_props_callback(Props) ->
    case props:get_value('callback', Props) of
        'undefined' -> 'undefined';
        Fun when is_function(Fun, 3) -> Fun
    end.

-spec get_props_origin(wh_proplist()) -> 'undefined' | origin_tuple() | origin_tuples().
get_props_origin(Props) -> props:get_value('origin', Props).

-spec expire_objects(ets:tid(), [ets:tid()]) -> non_neg_integer().
expire_objects(Tab, AuxTables) ->
    Now = wh_util:current_tstamp(),
    FindSpec = [{#cache_obj{key = '$1'
                            ,value = '$2'
                            ,expires = '$3'
                            ,timestamp = '$4'
                            ,callback = '$5'
                            ,_ = '_'
                           }
                 ,[{'=/=', '$3', 'infinity'}
                   ,{'>', {'const', Now}, {'+', '$4', '$3'}}
                  ]
                 ,[{{'$5', '$1', '$2'}}]
                }],
    Objects = ets:select(Tab, FindSpec),
    _ = maybe_exec_expired_callbacks(Objects),
    Keys = [K || {_, K, _} <- Objects],
    maybe_remove_objects(Tab, Keys),
    lists:foreach(fun(Aux) -> maybe_remove_objects(Aux, Keys) end, AuxTables),
    length(Objects).

-spec maybe_exec_expired_callbacks([{callback_fun(), any(), any()}]) -> 'ok'.
maybe_exec_expired_callbacks(Objects) ->
    [exec_expired_callback(Fun, K, V)
     || {Fun, K, V} <- Objects,
        is_function(Fun, 3)
    ],
    'ok'.

-spec exec_expired_callback(callback_fun(), any(), any()) -> 'ok'.
exec_expired_callback(Fun, K, V) ->
    _ = wh_util:spawn(Fun, [K, V, 'expire']),
    'ok'.

-spec maybe_remove_objects(ets:tid(), list()) -> 'ok'.
maybe_remove_objects(Tab, Objects) ->
    _ = [maybe_remove_object(Tab, Object) || Object <- Objects],
    'ok'.

-spec maybe_remove_object(ets:tid(), cache_obj() | any()) -> 'true'.
maybe_remove_object(Tab, #cache_obj{key = Key}) ->
    maybe_remove_object(Tab, Key);
maybe_remove_object(Tab, Key) ->
    ets:delete(Tab, Key).

-spec maybe_exec_erase_callbacks(ets:tid(), cache_obj() | any()) -> 'ok'.
maybe_exec_erase_callbacks(_Tab
                          ,#cache_obj{callback=Fun
                                     ,value=Value
                                     ,key=Key
                                     }
                          ) when is_function(Fun, 3) ->
    wh_util:spawn(Fun, [Key, Value, 'erase']),
    'ok';
maybe_exec_erase_callbacks(_Tab, #cache_obj{}) -> 'ok';
maybe_exec_erase_callbacks(Tab, Key) ->
    try ets:lookup_element(Tab, Key, #cache_obj.callback) of
        Fun when is_function(Fun, 3) ->
            wh_util:spawn(fun exec_erase_callbacks/3, [Tab, Key, Fun]),
            'ok';
        _Else -> 'ok'
    catch
        'error':'badarg' -> 'ok'
    end.

-spec exec_erase_callbacks(ets:tid(), any(), callback_fun()) ->
                                  any().
exec_erase_callbacks(Tab, Key, Fun) ->
    Value = ets:lookup_element(Tab, Key, #cache_obj.value),
    Fun(Key, Value, 'erase').

-spec maybe_exec_flush_callbacks(ets:tid()) -> 'ok'.
maybe_exec_flush_callbacks(Tab) ->
    MatchSpec =
        [{#cache_obj{key = '$1'
                     ,value = '$2'
                     ,callback = '$3'
                     , _ = '_'
                    }
          ,[{'=/=', '$3', 'undefined'}]
          ,[{{'$3', '$1', '$2'}}]
         }],
    _ = [wh_util:spawn(Callback, [K, V, 'flush'])
         || {Callback, K, V} <- ets:select(Tab, MatchSpec),
            is_function(Callback, 3)
        ],
    'ok'.

-spec maybe_exec_store_callbacks(state(), any(), any()) -> state().
maybe_exec_store_callbacks(#state{has_monitors='false'}=State, _, _) -> State;
maybe_exec_store_callbacks(#state{monitor_tab=MonitorTab}=State, Key, Value) ->
    MatchSpec = [{#cache_obj{key = Key
                             ,callback = '$2'
                             ,_ = '_'
                            }
                  ,[]
                  ,['$2']
                 }],
    _ = case ets:select(MonitorTab, MatchSpec) of
            [] -> 'ok';
            Callbacks ->
                exec_store_callback(Callbacks, Key, Value),
                delete_monitor_callbacks(MonitorTab, Key)
        end,
    State#state{has_monitors=has_monitors(MonitorTab)}.

-spec has_monitors(ets:tid()) -> boolean().
has_monitors(MonitorTab) ->
    ets:info(MonitorTab, 'size') > 0.

-spec exec_store_callback(callback_funs(), any(), any()) -> 'ok'.
exec_store_callback(Callbacks, Key, Value) ->
    Args = [Key, Value, 'store'],
    _Pids = [wh_util:spawn(Callback, Args) || Callback <- Callbacks],
    'ok'.

-spec delete_monitor_callbacks(ets:tid(), any()) -> 'true'.
delete_monitor_callbacks(MonitorTab, Key) ->
    ets:delete(MonitorTab, Key).

-spec start_expire_period_timer(pos_integer()) -> reference().
start_expire_period_timer(ExpirePeriod) ->
    erlang:start_timer(ExpirePeriod, self(), ?EXPIRE_PERIOD_MSG).

-spec insert_origin_pointers('undefined' | origin_tuple() | origin_tuples()
                            ,cache_obj(), ets:tid()) -> 'ok'.
insert_origin_pointers('undefined', _CacheObj, _PointerTab) -> 'ok';
insert_origin_pointers(Origin, CacheObj, PointerTab) when is_tuple(Origin) ->
    insert_origin_pointer(Origin, CacheObj, PointerTab);
insert_origin_pointers(Origins, CacheObj, PointerTab) when is_list(Origins) ->
    [insert_origin_pointer(Origin, CacheObj, PointerTab) || Origin <- Origins],
    'ok'.

-spec insert_origin_pointer(origin_tuple(), cache_obj(), ets:tid()) -> 'true'.
insert_origin_pointer(Origin, #cache_obj{key=Key}=CacheObj, PointerTab) ->
    ets:insert(PointerTab
              ,CacheObj#cache_obj{key=Key
                                 ,value=Key
                                 ,origin=Origin
                                 ,callback='undefined'
                                 ,expires='infinity'
                                 }
              ).
