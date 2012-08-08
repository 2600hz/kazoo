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

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([store/2, store/3, store/4]).
-export([peek/1]).
-export([fetch/1, fetch_keys/0]).
-export([erase/1]).
-export([flush/0]).
-export([filter/1]).
-export([wait_for_key/1, wait_for_key/2]).

-export([store_local/3, store_local/4, store_local/5]).
-export([peek_local/2]).
-export([fetch_local/2, fetch_keys_local/1]).
-export([erase_local/2]).
-export([flush_local/1]).
-export([filter_local/2]).
-export([wait_for_key_local/2, wait_for_key_local/3]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
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

%% NOTE: the use of atom() in the specs are for ets match specs and not otherwise valid types
-type callback_fun() :: fun((_, _, 'flush' | 'erase' | 'expire') -> _).
-record(cache_obj, {
          key :: term()
         ,value :: term()
         ,expires=?EXPIRES :: pos_integer() | 'infinity' | atom()
         ,timestamp=wh_util:current_tstamp() :: pos_integer() | atom()
         ,callback :: callback_fun()  | atom()
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

start_link(Name, ExpirePeriod) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, ExpirePeriod], []).

%% T - seconds to store the pair
-spec store/2 :: (term(), term()) -> 'ok'.
-spec store/3 :: (term(), term(), pos_integer() | 'infinity' | function()) -> 'ok'.
-spec store/4 :: (term(), term(), pos_integer() | 'infinity', function()) -> 'ok'.

store(K, V) ->
    store(K, V, ?EXPIRES).

store(K, V, Fun) when is_function(Fun, 3) ->
    store(K, V, ?EXPIRES, Fun);
store(K, V, T) ->
    store_local(?SERVER, K, V, T).

store(K, V, T, Fun) when is_function(Fun, 3) ->
    store_local(?SERVER, K, V, T, Fun).

-spec peek/1 :: (term()) ->
                        {'ok', term()} |
                        {'error', 'not_found'}.
peek(K) ->
    peek_local(?SERVER, K).

-spec fetch/1 :: (term()) ->
                         {'ok', term()} |
                         {'error', 'not_found'}.
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

-spec wait_for_key/1 :: (term()) -> 
                                {'ok', term()} |
                                {'error', 'timeout'}.
-spec wait_for_key/2 :: (term(), 'infinity' | non_neg_integer()) ->
                                {'ok', term()} |
                                {'error', 'timeout'}.
wait_for_key(Key) ->
    wait_for_key(Key, ?DEFAULT_WAIT_TIMEOUT).
wait_for_key(Key, Timeout) ->
    wait_for_key_local(?SERVER, Key, Timeout).

%% Local cache API
-spec store_local/3 :: (atom(), term(), term()) -> 'ok'.
-spec store_local/4 :: (atom(), term(), term(), pos_integer() | 'infinity' | function() | {atom(), atom()}) -> 'ok'.
-spec store_local/5 :: (atom(), term(), term(), pos_integer() | 'infinity', function() | {atom(), atom()}) -> 'ok'.

store_local(Srv, K, V) ->
    store_local(Srv, K, V, ?EXPIRES).

store_local(Srv, K, V, Fun) when is_function(Fun, 3) ->
    store_local(Srv, K, V, ?EXPIRES, Fun);
store_local(Srv, K, V, T) ->
    gen_server:cast(Srv, {store, #cache_obj{key=K
                                            ,value=V
                                            ,expires=T
                                           }}).

store_local(Srv, K, V, T, Fun) when is_function(Fun, 3) ->
    gen_server:cast(Srv, {store, #cache_obj{key=K
                                            ,value=V
                                            ,expires=T
                                            ,callback=Fun
                                           }}).

-spec peek_local/2 :: (atom(), term()) ->
                              {'ok', term()} |
                              {'error', 'not_found'}.
peek_local(Srv, K) ->
    try ets:lookup_element(Srv, K, #cache_obj.value) of
        Value -> {ok, Value}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec fetch_local/2 :: (atom(), term()) ->
                               {'ok', term()} |
                               {'error', 'not_found'}.
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
    MatchSpec = [{#cache_obj{key = '$1', _ = '_'}
                  ,[{'orelse', {'not', {is_tuple, '$1'}},
                    {'=/=',{element, 1, '$1'}, monitor_key}}
                  ]
                  ,['$1']
                 }],
    ets:select(Srv, MatchSpec).

-spec filter_local/2 :: (atom(), fun((term(), term()) -> boolean())) -> wh_proplist().
filter_local(Srv, Pred)  when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key={monitor_key, _}}, Acc) -> Acc;
                 (#cache_obj{key=K, value=V}, Acc) ->
                      case Pred(K, V) of
                          true -> [{K, V}|Acc];
                          false -> Acc
                      end;
                 (_, Acc) -> Acc
              end, [], Srv).

-spec wait_for_key_local/2 :: (atom(), term()) ->
                                      {'ok', term()} |
                                      {'error', 'timeout'}.
-spec wait_for_key_local/3 :: (atom(), term(), 'infinity' | non_neg_integer()) ->
                                      {'ok', term()} |
                                      {'error', 'timeout'}.
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
    lager:debug("started new cache proc: ~s", [Name]),
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
                CacheObj = #cache_obj{key=?NOTIFY_KEY(Ref)
                                      ,value=Key
                                      ,expires=Timeout
                                      ,callback=Fun
                                     },
                ets:insert(Cache, CacheObj)
        end,
    {reply, {ok, Ref}, Cache}.

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
handle_cast({store, #cache_obj{key=Key, value=Value}=CacheObj}, Cache) ->
    ets:insert(Cache, CacheObj),
    MatchSpec = [{#cache_obj{key = {monitor_key, '_'}
                             ,value = '$1'
                             ,callback = '$2'
                             ,_ = '_'
                            }
                  ,[{'=:=', '$1', {const, Key}}]
                  ,['$2']
                 }],
    _ = case ets:select(Cache, MatchSpec) of
            [] -> ok;
            Monitors ->
                _ = [spawn(fun() -> Callback(Key, Value, store) end)
                     || Callback <- Monitors
                    ],
                DeleteSpec = [{#cache_obj{key = {monitor_key, '_'}
                                          ,value = '$1'
                                          ,callback = '$1'
                                          ,_ = '_'
                                         }
                               ,[{'=:=', '$1', {const, Key}}]
                               ,[true]
                              }
                             ],
                ets:select_delete(Cache, DeleteSpec)
        end,
    {noreply, Cache, hibernate};
handle_cast({update_timestamp, K, Timestamp}, Cache) ->
    ets:update_element(Cache, K, {#cache_obj.timestamp, Timestamp}),
    {noreply, Cache, hibernate};
handle_cast({erase, K}, Cache) ->
    _ = try ets:lookup_element(Cache, K, #cache_obj.callback) of
            undefined -> ok;
            Callback ->
                V = ets:lookup_element(Cache, K, #cache_obj.value),
                spawn(fun() -> Callback(K, V, erase) end)
        catch
            error:badarg -> ok
        end,
    ets:delete(Cache, K),
    {noreply, Cache, hibernate};
handle_cast({flush}, Cache) ->
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
    ets:delete_all_objects(Cache),
    {noreply, Cache, hibernate};
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
    Now = wh_util:current_tstamp(),
    FindSpec = [{#cache_obj{key = '$1', value = '$2', expires = '$3',
                            timestamp = '$4', callback = '$5'}
                 ,[{'=/=', '$3', infinity}
                   ,{'>', {const, Now}, {'+', '$4', '$3'}}
                  ]
                 ,[{{'$5', '$1', '$2'}}]
                }],
    _ = case ets:select(Cache, FindSpec) of
            [] -> ok;
            Expired ->
                _ = [spawn(fun() -> Callback(K, V, expire) end)
                     || {Callback, K, V} <- Expired
                            ,is_function(Callback)
                    ],
                DeleteSpec = [{#cache_obj{expires = '$3'
                                          ,timestamp = '$4'
                                          ,_ = '_'}
                               ,[{'=/=', '$3', infinity},
                                 {'>', {const, Now}, {'+', '$4', '$3'}}
                                ]
                               ,[true]
                              }],
                ets:select_delete(Cache, DeleteSpec)
        end,
    _ = erlang:send_after(ExpirePeriod, self(), {expire, ExpirePeriod}),
    {noreply, Cache, hibernate};
handle_info(_Info, Cache) ->
    {noreply, Cache, hibernate}.

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
