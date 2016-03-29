%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Simple cache server
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kzc_ets_listener).

-include("kz_caches.hrl").

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([handle_document_change/2]).

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
                ,props = [] :: wh_proplist()
                ,has_monitors = 'false' :: boolean()
               }).
-type state() :: #state{}.


%%% API

-spec handle_document_change(wh_json:object(), wh_proplist()) -> 'ok' | 'false'.
handle_document_change(JObj, Props) ->
    'true' = wapi_conf:doc_update_v(JObj),
    Srv = props:get_value('server', Props),
    Db = wh_json:get_value(<<"Database">>, JObj),
    Type = wh_json:get_value(<<"Type">>, JObj),
    Id = wh_json:get_value(<<"ID">>, JObj),

    _Keys = maybe_erase_changed(Srv, Db, Type, Id
                               ,props:get_value('pointer_tab', Props)
                               ),
    _Keys =/= [] andalso
        lager:debug("removed ~p keys for ~s/~s/~s", [length(_Keys), Db, Id, Type]).

-spec maybe_erase_changed(pid(), ne_binary(), ne_binary(), ne_binary(), ets:tid()) -> list().
maybe_erase_changed(Srv, Db, Type, Id, PointerTab) ->
    MatchSpec = [{#cache_obj{origin = {'db', Db}, _ = '_'}
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
                ],

    lists:foldl(fun(Obj, Removed) ->
                        erase_changed(Obj, Removed, Srv)
                end
               ,[]
               ,ets:select(PointerTab, MatchSpec)
               ).

-spec erase_changed(cache_obj(), list(), pid()) -> list().
erase_changed(#cache_obj{key=Key}, Removed, Srv) ->
    case lists:member(Key, Removed) of
        'true' -> Removed;
        'false' ->
            lager:debug("removing updated cache object ~-300p", [Key]),
            gen_listener:cast(Srv, {'erase', Key}),
            [Key | Removed]
    end.


%%%===================================================================
%%% gen_listener callbacks
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
                 ,['set', 'protected', 'named_table', {'keypos', #cache_obj.key}]
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

-spec to_tab(atom(), string()) -> atom().
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
    maybe_exec_erase_callbacks(Tab, Key),

    maybe_remove_object(Tab, Key),
    maybe_remove_object(PointerTab, Key),
    maybe_remove_object(MonitorTab, Key),

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
    _ = [expire_objects(T) || T <- [Tab, PointerTab, MonitorTab]],
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
handle_event(_JObj, #state{tab=Tab
                           ,pointer_tab=PointerTab
                           ,monitor_tab=MonitorTab
                          }) ->
    {'reply', [{'object_tab', Tab}
              ,{'pointer_tab', PointerTab}
              ,{'monitor_tab', MonitorTab}
              ]}.

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
terminate(_Reason, #state{tab = Tab
                          ,pointer_tab = PointerTab
                          ,monitor_tab = MonitorTab
                         }) ->
    [ets:delete(T) || T <- [Tab, PointerTab, MonitorTab]].

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


%%% Internals


-spec expire_objects(ets:tid()) -> non_neg_integer().
expire_objects(Tab) ->
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
    maybe_remove_objects(Tab, [K || {_, K, _} <- Objects]),
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

%%% End of Module
