%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Handle AMQP requests to write system configuration data.
%%% Support nested keys like key#subkey#subsubkey
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(sysconf_set).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = wapi_sysconf:set_req_v(ApiJObj),

    Category = wh_json:get_value(<<"Category">>, ApiJObj),
    Key = wh_json:get_value(<<"Key">>, ApiJObj),
    Value = wh_json:get_value(<<"Value">>, ApiJObj),
    MsgID = wh_json:get_value(<<"Msg-ID">>, ApiJObj),
    Node = wh_json:get_value(<<"Node">>, ApiJObj),

    lager:debug("received sysconf set for ~s[~s.~s]", [Category, Node, Key]),

    {'ok', _} = whapps_config:set(Category, Key, Value, Node),

    RespQ =  wh_json:get_value(<<"Server-ID">>, ApiJObj),

    Resp = [{<<"Category">>, Category}
            ,{<<"Key">>, Key}
            ,{<<"Value">>, Value}
            ,{<<"Msg-ID">>, MsgID}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_sysconf:publish_set_resp(RespQ, Resp).
