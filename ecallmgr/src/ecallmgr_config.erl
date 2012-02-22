%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% 
%%% @end
%%% Created : 28 Oct 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_config).

-export([flush/0]).
-export([get/1, get/2, get/3]).
-export([set/2, set/3]).

-include("ecallmgr.hrl").

-spec flush/0 :: () -> 'ok'.
flush() ->
    {ok, Cache} = ecallmgr_sup:cache_proc(),
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

    {ok, Cache} = ecallmgr_sup:cache_proc(),
    case wh_cache:fetch_local(Cache, cache_key(Key, Node)) of
        {ok, V} -> V;
        {error, not_found} ->
            Req = [KV ||
                      {_, V} = KV <- [{<<"Category">>, <<"ecallmgr">>}
                                      ,{<<"Key">>, Key}
                                      ,{<<"Default">>, Default}
                                      ,{<<"Node">>, Node}
                                      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                     ],
                      V =/= undefined],
            case catch ecallmgr_amqp_pool:get_req(Req) of
                {ok, RespJObj} ->
                    true = wapi_sysconf:get_resp_v(RespJObj),
                    V = case wh_json:get_value(<<"Value">>, RespJObj) of
                            undefined -> Default;
                            null -> Default;
                            <<"undefined">> -> Default;
                            <<"null">> -> Default;
                            Value ->
                                wh_cache:store_local(Cache, cache_key(Key, Node), Value),
                                Value
                        end,
                    V;
                {'EXIT', _} ->
                    Default
            end
    end.

-spec set/2 :: (wh_json:json_string(), wh_json:json_term()) -> 'ok'.
-spec set/3 :: (wh_json:json_string(), wh_json:json_term(), wh_json:json_string() | atom()) -> 'ok'.
set(Key, Value) ->
    set(Key, Value, wh_util:to_binary(node())).
set(Key0, Value, Node0) ->
    Key = wh_util:to_binary(Key0),
    Node = wh_util:to_binary(Node0),

    {ok, Cache} = ecallmgr_sup:cache_proc(),
    wh_cache:store_local(Cache, cache_key(Key, Node), Value),

    Req = [KV ||
              {_, V} = KV <- [{<<"Category">>, <<"ecallmgr">>}
                              ,{<<"Key">>, Key}
                              ,{<<"Value">>, Value}
                              ,{<<"Node">>, Node}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ],
              V =/= undefined],
    case catch ecallmgr_amqp_pool:set_req(Req) of
        {'EXIT', _} ->
            ?LOG("failed to recv resp for setting ~s to ~p", [Key, Value]);
        _ ->
            ?LOG("recv resp for setting ~s to ~p", [Key, Value])
    end.

cache_key(K, Node) ->
    {?MODULE, K, Node}.
