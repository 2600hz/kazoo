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
-module(kzc_cache).

-export([start_link/1, start_link/2]).

-export([store/3, store/4]).
-export([peek/2]).
-export([fetch/2, fetch_keys/1]).
-export([erase/2]).
-export([flush/1]).
-export([filter/2, filter_erase/2]).
-export([dump/1, dump/2]).
-export([wait_for_key/2, wait_for_key/3]).

-include("kz_caches.hrl").

-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS(CBModule), [{{CBModule, 'handle_document_change'}
                                ,[{<<"configuration">>, <<"*">>}]
                               }
                              ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DATABASE_BINDING, [{'type', <<"database">>}]).


-type predicate2() :: fun((any(), any()) -> boolean()).

-export_type([predicate2/0]).


%%% API

%%--------------------------------------------------------------------
%% @doc Starts a cache server
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Name) when is_atom(Name) ->
    start_link(Name, []).

start_link(Name, Props)
  when is_atom(Name), is_list(Props) ->
    CBModule = get_props_datastore(Props),
    case props:get_value('origin_bindings', Props) of
        'undefined' ->
            lager:debug("started new cache process (gen_server): ~s", [Name]),
            gen_server:start_link({'local', Name}, CBModule, [Name, ?EXPIRE_PERIOD, Props], []);
        BindingProps ->
            lager:debug("started new cache process (gen_listener): ~s", [Name]),
            Bindings = [{'conf', ['federate' | P]} || P <- maybe_add_db_binding(BindingProps)],
            gen_listener:start_link({'local', Name}, CBModule
                                    ,[{'bindings', Bindings}
                                      ,{'responders', ?RESPONDERS(CBModule)}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ]
                                    ,[Name, ?EXPIRE_PERIOD, Props]
                                   )
    end.

-spec maybe_add_db_binding(wh_proplists()) -> wh_proplists().
maybe_add_db_binding([]) -> [];
maybe_add_db_binding([[]]) -> [[]];
maybe_add_db_binding(BindingProps) ->
    [?DATABASE_BINDING | BindingProps].

%% Local cache API
-spec store(server_ref(), any(), any()) -> 'ok'.
-spec store(server_ref(), any(), any(), wh_proplist()) -> 'ok'.

store(Srv, K, V) ->
    store(Srv, K, V, []).

store(Srv, K, V, Props) when is_atom(Srv) ->
    case whereis(Srv) of
        'undefined' -> throw({'error', 'unknown_cache', Srv});
        Pid -> store(Pid, K, V, Props)
    end;
store(Srv, K, V, Props) when is_pid(Srv) ->
    Obj = #cache_obj{key = K
                     ,value = V
                     ,expires = get_props_expires(Props)
                     ,callback = get_props_callback(Props)
                     ,origin = get_props_origin(Props)
                    },
    gen_server:cast(Srv, {'store', Obj}).

-spec peek(atom(), any()) -> {'ok', any()} | {'error', 'not_found'}.
peek(Srv, K) ->
    try ets:lookup_element(Srv, K, #cache_obj.value) of
        Value -> {'ok', Value}
    catch
        'error':'badarg' ->
            {'error', 'not_found'}
    end.

-spec fetch(atom(), any()) -> {'ok', any()} | {'error', 'not_found'}.
fetch(Srv, K) ->
    case peek(Srv, K) of
        {'error', 'not_found'}=E -> E;
        {'ok', _V}=Ok ->
            gen_server:cast(Srv, {'update_timestamp', K, wh_util:current_tstamp()}),
            Ok
    end.

-spec erase(atom(), any()) -> 'ok'.
erase(Srv, K) ->
    case peek(Srv, K) of
        {'error', 'not_found'} -> 'ok';
        {'ok', _} ->
            gen_server:cast(Srv, {'erase', K})
    end.

-spec flush(atom()) -> 'ok'.
flush(Srv) ->
    gen_server:cast(Srv, {'flush'}).

-spec fetch_keys(atom()) -> list().
fetch_keys(Srv) ->
    MatchSpec = [{#cache_obj{key = '$1'
                            ,_ = '_'
                            }
                 ,[]
                 ,['$1']
                 }],
    ets:select(Srv, MatchSpec).

-spec filter_erase(atom(), predicate2()) -> non_neg_integer().
filter_erase(Srv, Pred) when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Count) ->
                      case Pred(K, V) of
                          'false' -> Count;
                          'true' ->
                              ?MODULE:erase(Srv, K),
                              Count+1
                      end;
                 (_, Count) -> Count
              end
              ,0
              ,Srv
             ).

-spec filter(atom(), predicate2()) -> [{any(), any()}].
filter(Srv, Pred) when is_function(Pred, 2) ->
    ets:foldl(fun(#cache_obj{key=K, value=V}, Acc) ->
                      case Pred(K, V) of
                          'true' -> [{K, V} | Acc];
                          'false' -> Acc
                      end;
                 (_, Acc) -> Acc
              end
             ,[]
             ,Srv
             ).

-spec dump(text()) -> 'ok'.
dump(Srv) ->
    dump(Srv, 'false').

-spec dump(atom(), boolean()) -> 'ok'.
dump(Srv, ShowValue)
  when is_atom(Srv), is_boolean(ShowValue) ->
    {PointerTab, MonitorTab} = gen_listener:call(Srv, {'tables'}),
    _ = [dump_table(Tab, ShowValue)
         || Tab <- [Srv, PointerTab, MonitorTab]
        ],
    'ok';
dump(Srv, ShowValue) ->
    dump(wh_util:to_atom(Srv), wh_util:to_boolean(ShowValue)).

-spec dump_table(ets:tid(), boolean()) -> 'ok'.
dump_table(Tab, ShowValue) ->
    Now = wh_util:current_tstamp(),
    io:format("Table ~p~n", [Tab]),
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

-spec wait_for_key(atom(), any()) -> {'ok', any()} | {'error', 'timeout'}.
-spec wait_for_key(atom(), any(), wh_timeout()) -> {'ok', any()} | {'error', 'timeout'}.
wait_for_key(Srv, Key) ->
    wait_for_key(Srv, Key, ?DEFAULT_WAIT_TIMEOUT).

wait_for_key(Srv, Key, Timeout) ->
    {'ok', Ref} = gen_server:call(Srv, {'wait_for_key', Key, Timeout}),
    lager:debug("waiting for message with ref ~p", [Ref]),
    receive
        {'exists', Ref, Value} -> {'ok', Value};
        {'store', Ref, Value} -> {'ok', Value};
        {_, Ref, _} -> {'error', 'timeout'}
    end.


%%% Internals

-spec get_props_datastore(wh_proplist()) -> module().
get_props_datastore(Props) ->
    props:get_value('kzc_datastore', Props, 'kzc_ets_listener').

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
get_props_origin(Props) ->
    maybe_add_db_origin(props:get_value('origin', Props)).

-spec maybe_add_db_origin(wh_proplist()) -> 'undefined' | origin_tuple() | origin_tuples().
maybe_add_db_origin(Props) when is_list(Props) -> maybe_add_db_origin(Props, []);
maybe_add_db_origin({'db', Db}) ->
    [{'db',Db}, {'type', <<"database">>, Db}];
maybe_add_db_origin({'db', Db, Id}) ->
    [{'db',Db,Id}, {'type', <<"database">>, Db}];
maybe_add_db_origin(Props) -> Props.

-spec maybe_add_db_origin(wh_proplist(), wh_proplist()) -> 'undefined' | origin_tuple() | origin_tuples().
maybe_add_db_origin([], Acc) -> lists:reverse(props:unique(Acc));
maybe_add_db_origin([{'db', Db} | Props], Acc) ->
    maybe_add_db_origin(Props, [{'type', <<"database">>, Db}, {'db',Db} |Acc]);
maybe_add_db_origin([{'db', Db, Id} | Props], Acc) ->
    maybe_add_db_origin(Props, [{'type', <<"database">>, Db}, {'db',Db,Id} |Acc]);
maybe_add_db_origin([P | Props], Acc) ->
    maybe_add_db_origin(Props, [P |Acc]).

%%% End of Module
