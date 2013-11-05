%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Handle requests to read configuration data
%%% Support nested keys a la wh_json, with a #
%%% as a separator i.e key#subkey#subsubkey
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(sysconf_get).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = wapi_sysconf:get_req_v(ApiJObj),
    Category = wh_json:get_binary_value(<<"Category">>, ApiJObj),
    Key = wh_json:get_binary_value(<<"Key">>, ApiJObj),
    Default = wh_json:get_value(<<"Default">>, ApiJObj),
    Node = wh_json:get_binary_value(<<"Node">>, ApiJObj),
    lager:debug("received sysconf get for ~s:~s from ~s", [Category, Key, Node]),
    MsgID = wh_json:get_value(<<"Msg-ID">>, ApiJObj),
    RespQ =  wh_json:get_value(<<"Server-ID">>, ApiJObj),
    case get_value(Category, Key, Default, Node) of
        'undefined' -> 'ok';
        Value ->
            lager:debug("sending reply for ~s.~s(~s): ~p"
                        ,[Category, Key, Node, Value]),
            Resp = [{<<"Category">>, Category}
                    ,{<<"Key">>, Key}
                    ,{<<"Value">>, Value}
                    ,{<<"Msg-ID">>, MsgID}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_sysconf:publish_get_resp(RespQ, Resp)
    end.

-spec get_value(ne_binary(), ne_binary(), term(), ne_binary()) -> term().
get_value(_, <<"acls">>, _, Node) ->
    sysconf_acls:build(Node);
get_value(_, <<"gateways">>, _, Node) ->
    sysconf_gateways:build(Node);
get_value(Category, Key, Default, Node) ->
    whapps_config:get(Category, Key, Default, Node).
