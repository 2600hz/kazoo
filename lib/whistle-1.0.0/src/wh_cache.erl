%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Simple cache server
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_cache).

-behaviour(gen_listener).

-export([start_link/0, start_link/1
         ,start_link/2, start_link/3
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
-export([filter_local/2]).
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

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(SERVER, ?MODULE).
-define(EXPIRES, 3600). %% an hour
-define(EXPIRE_PERIOD, 10000).
-define(DEFAULT_WAIT_TIMEOUT, 5).

-define(NOTIFY_KEY(Key), {monitor_key, Key}).

-define(BINDINGS, [{self, []}]).
-define(RESPONDERS, [{{?MODULE, handle_document_change}
                      ,[{<<"configuration">>, <<"*">>}]
                     }]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-type callback_fun() :: fun((_, _, 'flush' | 'erase' | 'expire') -> _).
-type origin_tuple() :: {'db', ne_binary(), ne_binary()}.
-record(cache_obj, {key :: term()
                    ,value :: term()
                    ,expires :: pos_integer() | 'infinity'
                    ,timestamp = wh_util:current_tstamp() :: pos_integer()
                    ,callback :: callback_fun()
                    ,origin :: origin_tuple() | [origin_tuple(),...]
                    ,type = normal :: 'normal' | 'monitor' | 'pointer'           
                   }).

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
start_link() ->
    start_link(?SERVER).

start_link(Name) ->
    start_link(Name, ?EXPIRE_PERIOD).

start_link(Name, Props) when is_list(Props) ->
    start_link(Name, ?EXPIRE_PERIOD, Props);
start_link(Name, ExpirePeriod) ->
    start_link(Name, ExpirePeriod, []).

start_link(Name, ExpirePeriod, Props) ->
    case props:get_value(origin_bindings, Props) of
        undefined ->
            lager:debug("started new cache process (gen_server): ~s", [Name]),
            gen_server:start_link({local, Name}, ?MODULE, [Name, ExpirePeriod], []);
        BindingProps ->
            lager:debug("started new cache process (gen_listener): ~s", [Name]),
            Bindings = [{conf, P} || P <- BindingProps],
            gen_listener:start_link({local, Name}, ?MODULE, [{bindings, Bindings}
                                                             ,{responders, ?RESPONDERS}
                                                             ,{queue_name, ?QUEUE_NAME}
                                                             ,{queue_options, ?QUEUE_OPTIONS}
                                                             ,{consume_options, ?CONSUME_OPTIONS}
                                                            ], [Name, ExpirePeriod])
    end.

-spec store/2 :: (term(), term()) -> 'ok'.
-spec store/3 :: (term(), term(), proplist()) -> 'ok'.

store(K, V) ->
    store(K, V, []).

store(K, V, Props) ->
    store_local(?SERVER, K, V, Props).

-spec peek/1 :: (term()) -> {'ok', term()} | {'error', 'not_found'}.
peek(K) ->
    peek_local(?SERVER, K).

-spec fetch/1 :: (term()) -> {'ok', term()} | {'error', 'not_found'}.
fetch(K) ->
    fetch_local(?SERVER, K).

-spec erase/1 :: (term()) -> 'ok'.
erase(K) ->
    erase_local(?SERVER, K).

-spec flush/0 :: () -> 'ok'.
flush() ->
    flush_local(?SERVER).

-spec fetch_keys/0 :: () -> [term(),...] | [].
fetch_keys() ->
    fetch_keys_local(?SERVER).

-spec filter/1 :: (fun((term(), term()) -> boolean())) -> proplist().
filter(Pred) when is_function(Pred, 2) ->
    filter_local(?SERVER, Pred).

-spec dump/0 :: () -> 'ok'.
dump() ->
    dump(false).

-spec dump/1 :: (text()) -> 'ok'.
dump(ShowValue) ->
    dump_local(?SERVER, ShowValue).

-spec wait_for_key/1 :: (term()) -> {'ok', term()} | {'error', 'timeout'}.
-spec wait_for_key/2 :: (term(), 'infinity' | non_neg_integer()) -> {'ok', term()} | {'error', 'timeout'}.

wait_for_key(Key) ->
    wait_for_key(Key, ?DEFAULT_WAIT_TIMEOUT).

wait_for_key(Key, Timeout) ->
    wait_for_key_local(?SERVER, Key, Timeout).

%% Local cache API
-spec store_local/3 :: (atom(), term(), term()) -> 'ok'.
-spec store_local/4 :: (atom(), term(), term(), proplist()) -> 'ok'.

store_local(Srv, K, V) ->
    store_local(Srv, K, V, []).

store_local(Srv, K, V, Props) ->
    gen_server:cast(Srv, {store, #cache_obj{key=K
                                            ,value=V
                                            ,expires=get_props_expires(Props)
                                            ,callback=get_props_callback(Props)
                                            ,origin=get_props_origin(Props)
                                           }}).

-spec peek_local/2 :: (atom(), term()) -> {'ok', term()} | {'error', 'not_found'}.
peek_local(Srv, K) ->
    try ets:lookup_element(Srv, K, #cache_obj.value) of
        Value -> {ok, Value}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec fetch_local/2 :: (atom(), term()) -> {'ok', term()} | {'error', 'not_found'}.
fetch_local(Srv, K) ->
    try ets:lookup_element(Srv, K, #cache_obj.value) of
        Value ->
            gen_server:cast(Srv, {update_timestamp, K, wh_util:current_tstamp()}),
            {ok, Value}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec erase_local/2 :: (atom(), term()) -> 'ok'.
erase_local(Srv, K) ->
    gen_server:cast(Srv, {erase, K}).

-spec flush_local/1 :: (atom()) -> 'ok'.
flush_local(Srv) ->
    gen_server:cast(Srv, {flush}).

-spec fetch_keys_local/1 :: (atom()) -> list().
fetch_keys_local(Srv) ->
    MatchSpec = [{#cache_obj{key = '$1', type = normal, _ = '_'}
                  ,[]
                  ,['$1']
                 }],
    ets:select(Srv, MatchSpec).

-spec filter_local/2 :: (atom(), fun((term(), term()) -> boolean())) -> wh_proplist().
filter_local(Srv, Pred)  when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V, type = normal}, Acc) ->
                      case Pred(K, V) of
                          true -> [{K, V}|Acc];
                          false -> Acc
                      end;
                 (_, Acc) -> Acc
              end, [], Srv).

-spec dump_local/1 :: (text()) -> 'ok'.
dump_local(Srv) ->
    dump_local(Srv, false).

-spec dump_local/2 :: (text(), text()) -> 'ok'.
dump_local(Srv, ShowValue) when not is_atom(Srv) ->
    dump_local(wh_util:to_atom(Srv), ShowValue);
dump_local(Srv, ShowValue) when not is_atom(ShowValue) ->
    dump_local(Srv, wh_util:to_atom(ShowValue));
dump_local(Srv, ShowValue) ->
    Now = wh_util:current_tstamp(),
    _ = [begin             
             io:format("Key: ~300p~n", [Obj#cache_obj.key]),
             case Obj#cache_obj.type =/= normal of
                 true -> io:format("Value: ~300p~n", [Obj#cache_obj.value]);
                 false -> ok
             end,
             io:format("Type: ~s~n", [Obj#cache_obj.type]),
             io:format("Expires: ~30p~n", [Obj#cache_obj.expires]),
             case is_number(Obj#cache_obj.expires) of
                 true ->
                     io:format("Remaining: ~30p~n", [(Obj#cache_obj.timestamp 
                                                      + Obj#cache_obj.expires)
                                                     - Now
                                                    ]);
                 false -> ok
             end,
             io:format("Origin: ~300p~n", [Obj#cache_obj.origin]),
             io:format("Callback: ~s~n", [Obj#cache_obj.callback =/= undefined]),
             case Obj#cache_obj.type =:= normal  andalso ShowValue of
                 true -> io:format("Value: ~p~n", [Obj#cache_obj.value]);
                 false -> ok
             end,
             io:format("~n", [])
         end
         || Obj <- ets:match_object(Srv, #cache_obj{_ = '_'})
        ],
    ok.

-spec wait_for_key_local/2 :: (atom(), term()) -> {'ok', term()} | {'error', 'timeout'}.
-spec wait_for_key_local/3 :: (atom(), term(), 'infinity' | non_neg_integer()) -> {'ok', term()} | {'error', 'timeout'}.

wait_for_key_local(Srv, Key) ->
    wait_for_key_local(Srv, Key, ?DEFAULT_WAIT_TIMEOUT).

wait_for_key_local(Srv, Key, Timeout) ->
    {ok, Ref} = gen_server:call(Srv, {wait_for_key, Key, Timeout}),
    lager:debug("waiting for message with ref ~p", [Ref]),
    receive
        {exists, Ref, Value} -> {ok, Value};
        {store, Ref, Value} -> {ok, Value};
        {_, Ref, _} -> {error, timeout}
    end.

-spec handle_document_change/2 :: (wh_json:object(), proplist()) -> 'ok'.
handle_document_change(JObj, Props) ->
    true = wapi_conf:doc_update_v(JObj),
    Srv = props:get_value(server, Props),
    Id = wh_json:get_value(<<"ID">>, JObj),
    Db = wh_json:get_value(<<"Database">>, JObj),
    gen_listener:cast(Srv, {change, {db, Db, Id}}).

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
init([Name, ExpirePeriod]) ->
    put(callid, Name),
    _ = erlang:send_after(ExpirePeriod, self(), {expire, ExpirePeriod}),
    {ok, ets:new(Name, [set, protected, named_table, {keypos, #cache_obj.key}])}.

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
handle_call({wait_for_key, Key, Timeout}, {Pid, _}, Cache) ->
    Ref = make_ref(),
    _ = try ets:lookup_element(Cache, Key, #cache_obj.value) of
            Value ->  Pid ! {exists, Ref, Value}
        catch
            error:badarg ->
                Fun = fun(_, V, Reason) ->  Pid ! {Reason, Ref, V} end,
                CacheObj = #cache_obj{key=Ref
                                      ,value=Key
                                      ,expires=Timeout
                                      ,callback=Fun
                                      ,type=monitor
                                     },
                ets:insert(Cache, CacheObj)
        end,
    {reply, {ok, Ref}, Cache};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast({store, #cache_obj{key=Key, value=Value, origin=[Origin|Origins]}=CacheObj}, Cache) ->
    ets:insert(Cache, CacheObj#cache_obj{origin=Origin}),
    _ = [begin
             Ref = make_ref(),
             ets:insert(Cache, CacheObj#cache_obj{key=Ref, value=Key                                                  
                                                  ,origin=O, type=pointer
                                                  ,callback=undefined
                                                  ,expires=infinity})
         end
         || O <- Origins
        ],
    _ = maybe_exec_store_callbacks(Key, Value, Cache),
    {noreply, Cache, hibernate};
handle_cast({store, #cache_obj{key=Key, value=Value}=CacheObj}, Cache) ->
    ets:insert(Cache, CacheObj),
    _ = maybe_exec_store_callbacks(Key, Value, Cache),
    {noreply, Cache, hibernate};
handle_cast({update_timestamp, Key, Timestamp}, Cache) ->
    ets:update_element(Cache, Key, {#cache_obj.timestamp, Timestamp}),
    {noreply, Cache, hibernate};
handle_cast({erase, Key}, Cache) ->
    _ = maybe_exec_erase_callbacks(Key, Cache),
    _ = maybe_remove_object(Key, Cache),
    {noreply, Cache, hibernate};
handle_cast({flush}, Cache) ->
    _ = maybe_exec_flush_callbacks(Cache),
    ets:delete_all_objects(Cache),
    {noreply, Cache, hibernate};
handle_cast({change, {db, _, _}=Change}, Cache) ->
    _ = maybe_erase_changed(Change, Cache),
    {noreply, Cache};
handle_cast(_, Cache) ->
    {noreply, Cache, hibernate}.

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
handle_info({expire, ExpirePeriod}, Cache) ->
    _ = expire_objects(Cache),
    _ = erlang:send_after(ExpirePeriod, self(), {expire, ExpirePeriod}),
    {noreply, Cache, hibernate};
handle_info(_Info, Cache) ->
    {noreply, Cache, hibernate}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {reply, []}.

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
terminate(_Reason, Cache) ->
    ets:delete(Cache).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_props_expires/1 :: (proplist()) -> 'undefined' | 'infinity' | integer().
get_props_expires(Props) ->
    case props:get_value(expires, Props) of
        undefined -> ?EXPIRES;
        infinity -> infinity;
        Expires when is_integer(Expires) 
                     andalso Expires > 0 ->
            Expires
    end.

-spec get_props_callback/1 :: (proplist()) -> 'undefined' | callback_fun().
get_props_callback(Props) ->
    case props:get_value(callback, Props) of
        undefined -> undefined;
        Fun when is_function(Fun, 3) -> Fun
    end.
             
-spec get_props_origin/1 :: (proplist()) -> 'undefined' | term().
get_props_origin(Props) ->
    props:get_value(origin, Props).

-spec maybe_erase_changed/2 :: (origin_tuple(), atom()) -> 'ok'.
maybe_erase_changed(Change, Cache) ->
    MatchSpec = [{#cache_obj{origin = '$1', _ = '_'}
                 ,[{'=:=', {const, Change}, '$1'}]
                 ,['$_']
                }],
    Objects = ets:select(Cache, MatchSpec),
    _ = erase_changed(Objects, [], Cache),
    ok.

-spec erase_changed/3 :: ([] | [#cache_obj{},...], list(), atom()) -> 'ok'.
erase_changed([], _, _) -> ok;
erase_changed([#cache_obj{type=pointer, value=Key}|Objects], Removed, Cache) ->
    _ = case lists:member(Key, Removed) of
            true -> ok;
            false ->
                lager:debug("removing updated cache object ~-300p", [Key]),
                _ = maybe_exec_erase_callbacks(Key, Cache),
                maybe_remove_object(Key, Cache)
        end,
    erase_changed(Objects, [Key|Removed], Cache);
erase_changed([#cache_obj{type=normal, key=Key}|Objects], Removed, Cache) ->
    _ = case lists:member(Key, Removed) of
            true -> ok;
            false ->
                lager:debug("removing updated cache object ~-300p", [Key]),
                _ = maybe_exec_erase_callbacks(Key, Cache),
                maybe_remove_object(Key, Cache)
        end,    
    erase_changed(Objects, [Key|Removed], Cache).

-spec expire_objects/1 :: (atom()) -> 'ok'.
expire_objects(Cache) ->
    Now = wh_util:current_tstamp(),
    FindSpec = [{#cache_obj{key = '$1', value = '$2', expires = '$3'
                            ,timestamp = '$4', callback = '$5',  _ = '_'}
                 ,[{'=/=', '$3', infinity}
                   ,{'>', {const, Now}, {'+', '$4', '$3'}}
                  ]
                 ,[{{'$5', '$1', '$2'}}]
                }],
    Objects = ets:select(Cache, FindSpec),
    _ = maybe_exec_expired_callbacks(Objects, Cache),
    maybe_remove_objects([K || {_, K, _} <- Objects], Cache).

-spec maybe_exec_expired_callbacks/2 :: (list(), atom()) -> 'ok'.
maybe_exec_expired_callbacks([], _) -> ok; 
maybe_exec_expired_callbacks([{Fun, K, V}|Objects], Cache) when is_function(Fun, 3) ->
    _ = spawn(fun() -> Fun(K, V, expire) end),
    maybe_exec_expired_callbacks(Objects, Cache);
maybe_exec_expired_callbacks([_|Objects], Cache) ->
    maybe_exec_expired_callbacks(Objects, Cache).

-spec maybe_remove_objects/2 :: (list(), atom()) -> 'ok'.
maybe_remove_objects([], _) -> ok;
maybe_remove_objects([Object|Objects], Cache) -> 
    _ = maybe_remove_object(Object, Cache),
    maybe_remove_objects(Objects, Cache).

-spec maybe_remove_object/2 :: (term(), atom()) -> 'ok'.
maybe_remove_object(#cache_obj{key = Key}, Cache) ->
    maybe_remove_object(Key, Cache);
maybe_remove_object(Key, Cache) -> 
    DeleteSpec = [{#cache_obj{value = '$1'
                              ,type = pointer
                              ,_ = '_'}
                   ,[{'=:=', {const, Key}, '$1'}]
                   ,[true]
                  }
                  ,{#cache_obj{key = '$1'
                               ,type = normal
                               ,_ = '_'}
                    ,[{'=:=', {const, Key}, '$1'}]
                    ,[true]
                   }],
    ets:select_delete(Cache, DeleteSpec).

-spec maybe_exec_erase_callbacks/2 :: (term(), atom()) -> 'ok'.
maybe_exec_erase_callbacks(Key, Cache) ->
    case ets:lookup(Cache, Key) of
        [#cache_obj{callback=Fun
                    ,value=Value}] when is_function(Fun, 3) ->
            _ = spawn(fun() -> Fun(Key, Value, erase) end),
            ok;
        _Else -> ok
    end.

-spec maybe_exec_flush_callbacks/1 :: (atom()) -> 'ok'.
maybe_exec_flush_callbacks(Cache) ->
    MatchSpec = [{#cache_obj{key = '$1'
                             ,value = '$2'
                             ,callback = '$3'
                             , _ = '_'}
                  ,[{'=/=', '$3', undefined}]
                  ,[{{'$3', '$1', '$2'}}]
                 }],
    _ = [spawn(fun() -> Callback(K, V, flush) end)
         || {Callback, K, V} <- ets:select(Cache, MatchSpec)
        ],
    ok.

-spec maybe_exec_store_callbacks/3 :: (term(), term(), atom()) -> 'ok'.
maybe_exec_store_callbacks(Key, Value, Cache) ->
    MatchSpec = [{#cache_obj{value = '$1'
                             ,callback = '$2'
                             ,type = monitor
                             ,_ = '_'
                            }
                  ,[{'=:=', '$1', {const, Key}}]
                  ,['$2']
                 }],
    Monitors = ets:select(Cache, MatchSpec),
    _ = [spawn(fun() -> Callback(Key, Value, store) end)
         || Callback <- Monitors
        ],
    _ = delete_monitor_callbacks(Key, Cache),
    ok.

-spec delete_monitor_callbacks/2 :: (term(), atom()) -> 'ok'.
delete_monitor_callbacks(Key, Cache) ->
    DeleteSpec = [{#cache_obj{value = '$1'
                              ,type = monitor
                              ,_ = '_'
                             }
                   ,[{'=:=', '$1', {const, Key}}]
                   ,[true]
                  }
                 ],
    ets:select_delete(Cache, DeleteSpec).
