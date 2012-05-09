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

-export([flush/0]).
-export([get/1, get/2, get/3]).
-export([set/2, set/3]).

-compile([{no_auto_import, [get/1]}]).

-include("ecallmgr.hrl").

-spec flush/0 :: () -> 'ok'.
flush() ->
    {ok, Cache} = ecallmgr_util_sup:cache_proc(),
    wh_cache:flush_local(Cache).

-spec get/1 :: (wh_json:json_string()) -> wh_json:json_term() | 'undefined'.
-spec get/2 :: (wh_json:json_string(), Default) -> wh_json:json_term() | Default.
-spec get/3 :: (wh_json:json_string(), Default, wh_json:json_string() | atom()) -> wh_json:json_term() | Default.
get(Key) ->
    get(Key, undefined).
get(Key, Default) ->
    get(Key, Default, wh_util:to_binary(node())).
get(Key0, Default, Node0) ->
    Key = wh_util:to_binary(Key0),
    Node = wh_util:to_binary(Node0),

    CacheKey = cache_key(Key, Node),

    {ok, Cache} = ecallmgr_util_sup:cache_proc(),
    case wh_cache:fetch_local(Cache, CacheKey) of
        {ok, V} -> V;
        {error, E} when E =:= not_found orelse E =:= undefined ->
            Req = [KV ||
                      {_, V} = KV <- [{<<"Category">>, <<"ecallmgr">>}
                                      ,{<<"Key">>, Key}
                                      ,{<<"Default">>, Default}
                                      ,{<<"Node">>, Node}
                                      ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
                                      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                     ],
                      V =/= undefined],

            lager:debug("looking up ~s from sysconf", [Key]),
            ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                          ,Req
                                          ,fun wapi_sysconf:publish_get_req/1
                                          ,fun wapi_sysconf:get_resp_v/1
                                         ),
            case ReqResp of
                {error, _R} -> 
                    lager:debug("unable to get config for key '~s' failed: ~p", [Key0, _R]),
                    Default;
                {ok, RespJObj} ->
                    V = case wh_json:get_value(<<"Value">>, RespJObj) of
                            undefined -> Default;
                            null -> Default;
                            <<"undefined">> -> Default;
                            <<"null">> -> Default;
                            Value ->
                                wh_cache:store_local(Cache, CacheKey, Value),
                                Value
                        end,
                    V
            end
    end.

-spec set/2 :: (wh_json:json_string(), wh_json:json_term()) -> 'ok'.
-spec set/3 :: (wh_json:json_string(), wh_json:json_term(), wh_json:json_string() | atom()) -> 'ok'.
set(Key, Value) ->
    set(Key, Value, wh_util:to_binary(node())).
set(Key0, Value, Node0) ->
    Key = wh_util:to_binary(Key0),
    Node = wh_util:to_binary(Node0),

    {ok, Cache} = ecallmgr_util_sup:cache_proc(),
    wh_cache:store_local(Cache, cache_key(Key, Node), Value),

    Req = [KV ||
              {_, V} = KV <- [{<<"Category">>, <<"ecallmgr">>}
                              ,{<<"Key">>, Key}
                              ,{<<"Value">>, Value}
                              ,{<<"Node">>, Node}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ],
              V =/= undefined],
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,Req
                                  ,fun wapi_sysconf:publish_set_req/1
                                  ,fun wh_amqp_worker:any_resp/1),
    case ReqResp of
        {error, _R} -> 
            lager:debug("set config for key '~s' failed: ~p", [Key0, _R]);
        {ok, _} ->
            lager:debug("set config for key '~s' to new value: ~p", [Key0, Value])
    end.
 
cache_key(K, Node) ->
    {?MODULE, K, Node}.
