%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Simple cache server
%%% @end
%%% Created : 30 Mar 2011 by James Aimonetti <james@2600hz.org>
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

-export([store_local/3, store_local/4, store_local/5]).
-export([peek_local/2]).
-export([fetch_local/2, fetch_keys_local/1]).
-export([erase_local/2]).
-export([flush_local/1]). 
-export([filter_local/2]).

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
-define(EXPIRE_CHECK, 10000).

-record(cache_obj, {key
                    ,value
                    ,expires=?EXPIRES
                    ,timestamp=wh_util:current_tstamp()
                    ,callback=undefined
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?MODULE], []).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

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

-spec peek_local/2 :: (atom(), term()) -> {'ok', term()} | {'error', 'not_found'}.
peek_local(Srv, K) ->
    try ets:lookup_element(Srv, K, #cache_obj.value) of
        Value -> 
            {ok, Value}
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

-spec fetch_keys_local/1 :: (atom()) -> [term(),...] | [].
fetch_keys_local(Srv) ->
    lists:concat(ets:match(Srv, #cache_obj{key='$1'
                                           ,value='_'
                                           ,expires='_'
                                           ,timestamp='_'
                                           ,callback='_'
                                          })).

-spec filter_local/2 :: (atom(), fun((term(), term()) -> boolean())) -> proplist().
filter_local(Srv, Pred)  when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Acc) ->
                      case Pred(K, V) of
                          true -> [{K, V}|Acc];
                          false -> Acc
                      end
              end, [], Srv).

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
init([Name]) ->
    put(callid, ?LOG_SYSTEM_ID),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), expire),
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
handle_call(_, _, Cache) ->
    {reply, {error, not_implemented}, Cache}.

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
handle_cast({store, CacheObj}, Cache) ->
    ets:insert(Cache, CacheObj),
    {noreply, Cache, hibernate};
handle_cast({update_timestamp, K, Timestamp}, Cache) ->
    ets:update_element(Cache, K, {#cache_obj.timestamp, Timestamp}),
    {noreply, Cache, hibernate};
handle_cast({erase, K}, Cache) ->
    try ets:lookup_element(Cache, K, #cache_obj.callback) of
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
    MatchSpec = [{#cache_obj{key = '$1', value = '$2', expires = '_',
                             timestamp = '_', callback = '$3'},
                  [{'=/=', '$3', undefined}],
                  [{{'$3', '$1', '$2'}}]}
                ],
    [spawn(fun() -> Callback(K, V, flush) end)
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
handle_info(expire, Cache) ->
    Now = wh_util:current_tstamp(),
    FindSpec = [{#cache_obj{key = '$1', value = '$2', expires = '$3',
                            timestamp = '$4', callback = '$5'},
                 [{'=/=', '$3', infinity},
                  {'>', {const, Now}, {'+', '$4', '$3'}}],
                 [{{'$5', '$1', '$2'}}]}
                ],
    [spawn(fun() -> Callback(K, V, expire) end)
     || {Callback, K, V} <- ets:select(Cache, FindSpec)
            ,is_function(Callback)
    ],
    DeleteSpec = [{#cache_obj{key = '_', value = '_', expires = '$3',
                              timestamp = '$4', callback = '_'},
                   [{'=/=', '$3', infinity},
                    {'>', {const, Now}, {'+', '$4', '$3'}}],
                   [true]}
                 ],
    ets:select_delete(Cache, DeleteSpec),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), expire),
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
    ets:delete(Cache),
    ok.

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
