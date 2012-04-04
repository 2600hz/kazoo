%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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
-define(DO_NOT_CACHE, [<<"ecallmgr.acls">>]).

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

            case ecallmgr_amqp_pool:get_req(Req) of
                {ok, RespJObj} ->
                    true = wapi_sysconf:get_resp_v(RespJObj),
                    V = case wh_json:get_value(<<"Value">>, RespJObj) of
                            undefined -> Default;
                            null -> Default;
                            <<"undefined">> -> Default;
                            <<"null">> -> Default;
                            Value ->
                                case lists:member(Key, ?DO_NOT_CACHE) of
                                    false -> wh_cache:store_local(Cache, cache_key(Key, Node), Value);
                                    true -> nothing
                                end,
                                Value
                        end,
                    V;
                {error, timeout} -> Default
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
    case lists:member(Key, ?DO_NOT_CACHE) of
        false -> wh_cache:store_local(Cache, cache_key(Key, Node), Value);
        true -> nothing
    end,

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
            lager:debug("failed to recv resp for setting ~s to ~p", [Key, Value]);
        _ ->
            lager:debug("recv resp for setting ~s to ~p", [Key, Value])
    end.

cache_key(K, Node) ->
    {?MODULE, K, Node}.
