%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(ecallmgr_config).

-export([flush/0, flush/1, flush/2
        ,flush_default/0, flush_default/1
        ]).
-export([get/1, get/2, get/3
        ,get_json/1, get_json/2, get_json/3
        ,get_jsons/1, get_jsons/2, get_jsons/3
        ,get_integer/1, get_integer/2, get_integer/3
        ,get_boolean/1, get_boolean/2, get_boolean/3
        ,is_true/1, is_true/2, is_true/3
        ,get_ne_binary/1, get_ne_binary/2, get_ne_binary/3
        ,get_ne_binaries/1, get_ne_binaries/2, get_ne_binaries/3
        ,get_default/1, get_default/2
        ]).
-export([fetch/1, fetch/2, fetch/3, fetch/4
        ,set/2
        ,set_default/2
        ,set_node/2, set_node/3
        ]).

-compile([{'no_auto_import', [get/1]}]).

-include("ecallmgr.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-type node_specific() :: node() | ne_binary().
-type api_node_specific() :: 'undefined' | node_specific().

-spec flush() -> 'ok'.
-spec flush(kz_json:path()) -> 'ok' | {'error', any()}.

flush() ->
    kz_cache:flush_local(?ECALLMGR_UTIL_CACHE),
    flush('undefined').

flush(Key) ->
    flush(Key, 'undefined').

-spec flush(api_binary(), api_node_specific()) -> 'ok'.
flush(Key, 'undefined') ->
    flush(Key, <<"undefined">>);
flush(Key, Node) ->
    CacheKey = cache_key(Key, Node),
    kz_cache:erase_local(?ECALLMGR_UTIL_CACHE, CacheKey),
    Req = [{<<"Category">>, <<"ecallmgr">>}
          ,{<<"Key">>, Key}
          ,{<<"Node">>, Node}
          ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("flushing ~s from sysconf", [Key]),
    kz_amqp_worker:cast(props:filter_undefined(Req)
                       ,fun kapi_sysconf:publish_flush_req/1
                       ).

-spec flush_default() -> 'ok'.
-spec flush_default(api_binary()) -> 'ok'.
flush_default() ->
    flush('undefined', <<"default">>).
flush_default(Key) ->
    flush(Key, <<"default">>).

-spec get(kz_json:path()) -> kz_json:api_json_term().
-spec get(kz_json:path(), Default) ->
                 kz_json:json_term() | Default.
-spec get(kz_json:path(), Default, api_node_specific()) ->
                 kz_json:json_term() | Default.

-ifdef(TEST).
get(_) -> undefined.
get(_, Default) -> Default.
get(_, Default, _) -> Default.
-else.
get(Key) ->
    get(Key, 'undefined').

get(Key, Default) ->
    get(Key, Default, kz_term:to_binary(node())).

get(Key, Default, 'undefined') ->
    get(Key, Default);
get(Key, Default, Node) when not is_binary(Node) ->
    get(Key, Default, kz_term:to_binary(Node));
get(Key, Default, Node) ->
    case kz_cache:fetch_local(?ECALLMGR_UTIL_CACHE, cache_key(Key, Node)) of
        {'ok', V} -> V;
        {'error', E} when E =:= 'not_found';
                          E =:= 'undefined' ->
            fetch(Key, Default, Node)
    end.
-endif.

-spec get_default(kz_json:path()) -> kz_json:api_json_term().
-spec get_default(kz_json:path(), Default) -> kz_json:json_term() | Default.
get_default(Key) ->
    get(Key, 'undefined', <<"default">>).
get_default(Key, Default) ->
    get(Key, Default, <<"default">>).

-spec get_json(kz_json:path()) -> api_object().
-spec get_json(kz_json:path(), Default) -> kz_json:object() | Default.
-spec get_json(kz_json:path(), Default, api_node_specific()) -> kz_json:object() | Default.
get_json(Key) ->
    get_json(Key, 'undefined').
get_json(Key, Default) ->
    as_json_value(get(Key, Default), Default).
get_json(Key, Default, Node) ->
    as_json_value(get(Key, Default, Node), Default).

-spec as_json_value(any(), api_object()) -> api_object().
as_json_value('undefined', Default) -> Default;
as_json_value(V, Default) ->
    case kz_json:is_json_object(V) of
        'true' -> V;
        'false' -> Default
    end.

-spec get_jsons(kz_json:path()) -> kz_json:objects().
-spec get_jsons(kz_json:path(), Default) -> kz_json:objects() | Default.
-spec get_jsons(kz_json:path(), Default, api_node_specific()) -> kz_json:objects() | Default.
get_jsons(Key) ->
    get_jsons(Key, []).
get_jsons(Key, Default) ->
    as_jsons_value(get(Key, Default), Default).
get_jsons(Key, Default, Node) ->
    as_jsons_value(get(Key, Default, Node), Default).

-spec as_jsons_value(any(), api_objects()) -> api_objects().
as_jsons_value(V, Default) when is_list(V) ->
    case lists:all(fun kz_json:is_json_object/1, V) of
        'false' -> Default;
        'true' -> V
    end;
as_jsons_value(_, Default) -> Default.

-spec get_integer(kz_json:path()) -> api_integer().
-spec get_integer(kz_json:path(), Default) -> integer() | Default.
-spec get_integer(kz_json:path(), Default, api_node_specific()) -> integer() | Default.
get_integer(Key) ->
    get_integer(Key, 'undefined').

get_integer(Key, Default) ->
    case get(Key, Default) of
        Default -> Default;
        N -> kz_term:to_integer(N)
    end.

get_integer(Key, Default, Node) ->
    case get(Key, Default, Node) of
        Default -> Default;
        N -> kz_term:to_integer(N)
    end.

-spec get_boolean(kz_json:path()) -> api_boolean().
-spec get_boolean(kz_json:path(), Default) -> boolean() | Default.
-spec get_boolean(kz_json:path(), Default, api_node_specific()) -> boolean() | Default.
get_boolean(Key) ->
    get_boolean(Key, 'undefined').

get_boolean(Key, Default) ->
    case get(Key, Default) of
        Default -> Default;
        N -> kz_term:to_boolean(N)
    end.

get_boolean(Key, Default, Node) ->
    case get(Key, Default, Node) of
        Default -> Default;
        N -> kz_term:to_boolean(N)
    end.

-spec is_true(kz_json:path()) -> boolean().
-spec is_true(kz_json:path(), Default) -> boolean() | Default.
-spec is_true(kz_json:path(), Default, api_node_specific()) -> boolean() | Default.
is_true(Key) ->
    kz_term:is_true(?MODULE:get(Key)).

is_true(Key, Default) ->
    case get(Key, Default) of
        Default -> Default;
        N -> kz_term:is_true(N)
    end.

is_true(Key, Default, Node) ->
    case get(Key, Default, Node) of
        Default -> Default;
        N -> kz_term:is_true(N)
    end.

-spec get_ne_binary(kz_json:path()) -> api_ne_binary().
-spec get_ne_binary(kz_json:path(), Default) -> ne_binary() | Default.
-spec get_ne_binary(kz_json:path(), Default, api_node_specific()) -> ne_binary() | Default.
get_ne_binary(Key) ->
    get_ne_binary(Key, 'undefined').

get_ne_binary(Key, Default) ->
    case get(Key, Default) of
        V=?NE_BINARY -> V;
        _ -> Default
    end.

get_ne_binary(Key, Default, Node) ->
    case get(Key, Default, Node) of
        V=?NE_BINARY -> V;
        _ -> Default
    end.

-spec get_ne_binaries(kz_json:path()) -> ne_binaries().
-spec get_ne_binaries(kz_json:path(), Default) -> ne_binaries() | Default.
-spec get_ne_binaries(kz_json:path(), Default, api_node_specific()) -> ne_binaries() | Default.
get_ne_binaries(Key) ->
    get_ne_binaries(Key, []).

get_ne_binaries(Key, Default) ->
    case get(Key, Default) of
        NeBinaries when is_list(NeBinaries) ->
            [kz_term:to_binary(NeBinary)
             || NeBinary <- NeBinaries,
                kz_term:is_not_empty(NeBinaries)
            ];
        _ -> Default
    end.

get_ne_binaries(Key, Default, Node) ->
    case get(Key, Default, Node) of
        NeBinaries when is_list(NeBinaries) ->
            [kz_term:to_binary(NeBinary)
             || NeBinary <- NeBinaries,
                kz_term:is_not_empty(NeBinaries)
            ];
        _ -> Default
    end.

-spec fetch(kz_json:path()) -> kz_json:api_json_term().
-spec fetch(kz_json:path(), Default) ->
                   kz_json:json_term() | Default.
-spec fetch(kz_json:path(), Default, api_node_specific() | pos_integer()) ->
                   kz_json:json_term() | Default.
-spec fetch(kz_json:path(), Default, api_node_specific(), pos_integer()) ->
                   kz_json:json_term() | Default.

fetch(Key) ->
    fetch(Key, 'undefined').

fetch(Key, Default) ->
    fetch(Key, Default, kz_term:to_binary(node())).

fetch(Key, Default, 'undefined') ->
    fetch(Key, Default);
fetch(Key, Default, Timeout) when is_integer(Timeout) ->
    fetch(Key, Default, kz_term:to_binary(node()), Timeout);
fetch(Key, Default, Node) when not is_binary(Node) ->
    fetch(Key, Default, kz_term:to_binary(Node));
fetch(Key, Default, <<_/binary>> = Node) ->
    fetch(Key, Default, Node, ?DEFAULT_FETCH_TIMEOUT).

fetch(Key, Default, Node, RequestTimeout) ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
          ,{<<"Key">>, Key}
          ,{<<"Default">>, Default}
          ,{<<"Node">>, Node}
          ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("looking up '~p' from sysconf", [Key]),
    ReqResp = kz_amqp_worker:call(props:filter_undefined(Req)
                                 ,fun kapi_sysconf:publish_get_req/1
                                 ,fun kapi_sysconf:get_resp_v/1
                                 ,RequestTimeout - 100
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:debug("unable to get config for key '~s' failed: ~p", [Key, _R]),
            Default;
        {'ok', JObj} ->
            Value = get_response_value(JObj, Default),
            _ = maybe_cache_resp(Key, Node, Value),
            Value
    end.

-spec maybe_cache_resp(ne_binary(), ne_binary(), any()) -> 'ok'.
maybe_cache_resp(_, _ , 'null') -> 'ok';
maybe_cache_resp(_, _ , <<"null">>) -> 'ok';
maybe_cache_resp(Key, Node, Value) ->
    CacheProps = [{'origin', {'db', ?KZ_CONFIG_DB, <<"ecallmgr">>}}],
    kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                        ,cache_key(Key, Node)
                        ,Value
                        ,CacheProps
                        ).

-spec set(kz_json:path(), kz_json:json_term()) -> 'ok'.
set(Key, Value) ->
    set(Key, Value, kz_term:to_binary(node()), []).

-spec set_default(kz_json:path(), kz_json:json_term()) -> 'ok'.
set_default(Key, Value) ->
    set(Key, Value, <<"default">>, []).

-spec set_node(kz_json:path(), kz_json:json_term()) -> 'ok'.
set_node(Key, Value) ->
    set_node(Key, Value, node()).

-spec set_node(kz_json:path(), kz_json:json_term(), node_specific()) -> 'ok'.
set_node(Key, Value, Node) when is_atom(Node) ->
    set_node(Key, Value, kz_term:to_binary(Node));
set_node(Key, Value, Node) ->
    set(Key, Value, Node, [{'node_specific', 'true'}]).

-spec set(kz_json:path(), kz_json:json_term(), node_specific(), kz_proplist()) -> 'ok'.
set(Key, Value, Node, Opt) ->
    Props = [{<<"Category">>, <<"ecallmgr">>}
            ,{<<"Key">>, Key}
            ,{<<"Value">>, Value}
            ,{<<"Node-Specific">>, props:is_true('node_specific', Opt, 'false')}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    Req = [{<<"Node">>, Node}
           | lists:keydelete(<<"Node">>, 1, Props)
          ],

    lager:info("setting ecallmgr_config value: ~p", [Props]),

    ReqResp = kz_amqp_worker:call(props:filter_undefined(Req)
                                 ,fun kapi_sysconf:publish_set_req/1
                                 ,fun kz_term:always_true/1
                                 ),
    case ReqResp of
        {'ok', _} -> maybe_cache_resp(Key, Node, Value);
        {'error', _R} ->
            lager:debug("set config for key '~s' failed: ~p", [Key, _R])
    end.

-spec get_response_value(kz_json:object(), any()) -> any().
get_response_value(JObj, Default) ->
    case kz_json:get_value(<<"Value">>, JObj) of
        'undefined' -> Default;
        'null' -> Default;
        <<"undefined">> -> Default;
        <<"null">> -> Default;
        Value -> Value
    end.

-spec cache_key(Key, Node) -> {?MODULE, Key, Node}.
cache_key(K, Node) ->
    {?MODULE, K, Node}.
