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
    Default = wh_json:get_value(<<"Default">>, ApiJObj, null),
    Node = wh_json:get_binary_value(<<"Node">>, ApiJObj),
    MsgID = wh_json:get_value(<<"Msg-ID">>, ApiJObj),

    lager:debug("received sysconf get for ~s:~s from ~s", [Category, Key, Node]),

    RespQ =  wh_json:get_value(<<"Server-ID">>, ApiJObj),
    Resp = [{<<"Category">>, Category}
            ,{<<"Key">>, Key}
            ,{<<"Value">>, get_value(Category, Key, Default, Node)}
            ,{<<"Msg-ID">>, MsgID}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_sysconf:publish_get_resp(RespQ, Resp).

-spec get_value(ne_binary(), ne_binary(), term(), ne_binary()) -> term().
get_value(_, <<"acls">>, _, Node) ->
    sysconf_acls:build(Node);
get_value(_, <<"gateways">>, _, Node) ->
    sysconf_gateways:build(Node);
get_value(Category, Key, Default, Node) ->
    whapps_config:get(Category, Key, Default, Node).
