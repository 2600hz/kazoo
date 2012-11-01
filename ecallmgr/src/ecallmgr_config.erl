%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(ecallmgr_config).

-export([flush/0, flush/1]).
-export([get/1, get/2, get/3]).
-export([fetch/1, fetch/2, fetch/3]).
-export([set/2, set/3]).

-compile([{no_auto_import, [get/1]}]).

-include("ecallmgr.hrl").

-spec flush/0 :: () -> 'ok'.
-spec flush/1 :: (wh_json:json_string()) -> 'ok' | {'error', _}.

flush() ->
    wh_cache:flush_local(?ECALLMGR_UTIL_CACHE).

flush(Key) ->
    flush(Key, node()).

flush(Key, Node) when not is_binary(Key) ->
    flush(wh_util:to_binary(Key), Node);
flush(Key, Node) when not is_binary(Node) ->
    flush(Key, wh_util:to_binary(Node));
flush(Key, Node) ->
    CacheKey = cache_key(Key, Node),
    wh_cache:erase_local(?ECALLMGR_UTIL_CACHE, CacheKey),
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, Key}
           ,{<<"Node">>, Node}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("flushing ~s from sysconf", [Key]),
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,props:filter_undefined(Req)
                        ,fun wapi_sysconf:publish_flush_req/1
                       ).
    
-spec get/1 :: (wh_json:json_string()) -> wh_json:json_term() | 'undefined'.
-spec get/2 :: (wh_json:json_string(), Default) -> wh_json:json_term() | Default.
-spec get/3 :: (wh_json:json_string(), Default, wh_json:json_string() | atom()) -> wh_json:json_term() | Default.

get(Key) ->
    get(Key, undefined).

get(Key, Default) ->
    get(Key, Default, wh_util:to_binary(node())).

get(Key, Default, Node) when not is_binary(Key) ->
    get(wh_util:to_binary(Key), Default, Node);
get(Key, Default, Node) when not is_binary(Node) ->
    get(Key, Default, wh_util:to_binary(Node));
get(Key, Default, Node) ->
    case wh_cache:fetch_local(?ECALLMGR_UTIL_CACHE, cache_key(Key, Node)) of
        {ok, V} -> V;
        {error, E} when E =:= not_found orelse E =:= undefined ->
            Value = fetch(Key, Default, Node),
            wh_cache:store_local(?ECALLMGR_UTIL_CACHE, cache_key(Key, Node), Value),
            Value
    end.

-spec fetch/1 :: (wh_json:json_string()) -> wh_json:json_term() | 'undefined'.
-spec fetch/2 :: (wh_json:json_string(), Default) -> wh_json:json_term() | Default.
-spec fetch/3 :: (wh_json:json_string(), Default, wh_json:json_string() | atom()) -> wh_json:json_term() | Default.

fetch(Key) ->
    fetch(Key, undefined).

fetch(Key, Default) ->
    fetch(Key, Default, wh_util:to_binary(node())).

fetch(Key, Default, Node) when not is_binary(Key) ->
    fetch(wh_util:to_binary(Key), Default, Node);
fetch(Key, Default, Node) when not is_binary(Node) ->
    fetch(Key, Default, wh_util:to_binary(Node));
fetch(Key, Default, Node) ->
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, Key}
           ,{<<"Default">>, Default}
           ,{<<"Node">>, Node}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("looking up ~s from sysconf", [Key]),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,props:filter_undefined(Req)
                                  ,fun wapi_sysconf:publish_get_req/1
                                  ,fun wapi_sysconf:get_resp_v/1
                                 ),
    case ReqResp of
        {error, _R} -> 
            lager:debug("unable to get config for key '~s' failed: ~p", [Key, _R]),
            Default;
        {ok, JObj} ->
            get_response_value(JObj, Default)
    end.

-spec set/2 :: (wh_json:json_string(), wh_json:json_term()) -> 'ok'.
-spec set/3 :: (wh_json:json_string(), wh_json:json_term(), wh_json:json_string() | atom()) -> 'ok'.

set(Key, Value) ->
    set(Key, Value, wh_util:to_binary(node())).

set(Key, Value, Node) when not is_binary(Key) -> 
    set(wh_util:to_binary(Key), Value, Node);
set(Key, Value, Node) when not is_binary(Node) ->
    set(Key, Value, wh_util:to_binary(Node));
set(Key, Value, Node) ->
    wh_cache:store_local(?ECALLMGR_UTIL_CACHE, cache_key(Key, Node), Value),
    Req = [{<<"Category">>, <<"ecallmgr">>}
           ,{<<"Key">>, Key}
           ,{<<"Value">>, Value}
           ,{<<"Node">>, Node}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,props:filter_undefined(Req)
                                  ,fun wapi_sysconf:publish_set_req/1
                                  ,fun wh_amqp_worker:any_resp/1),
    case ReqResp of
        {error, _R} -> 
            lager:debug("set config for key '~s' failed: ~p", [Key, _R]);
        {ok, _} ->
            lager:debug("set config for key '~s' to new value: ~p", [Key, Value])
    end.

-spec get_response_value/2 :: (wh_json:json_object(), term()) -> term().
get_response_value(JObj, Default) ->
    case wh_json:get_value(<<"Value">>, JObj) of
        undefined -> Default;
        null -> Default;
        <<"undefined">> -> Default;
        <<"null">> -> Default;
        Value -> Value
    end. 

cache_key(K, Node) ->
    {?MODULE, K, Node}.
