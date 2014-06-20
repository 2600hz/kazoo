%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Handle AMQP requests to write system configuration data.
%%% Support nested keys like key#subkey#subsubkey
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(sysconf_set).

-export([init/0
         ,handle_req/2
        ]).

-include("sysconf.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = wapi_sysconf:set_req_v(ApiJObj),
    wh_util:put_callid(ApiJObj),

    Category = wh_json:get_value(<<"Category">>, ApiJObj),
    Key = wh_json:get_value(<<"Key">>, ApiJObj),
    Value = wh_json:get_value(<<"Value">>, ApiJObj),

    {'ok', _} = case wh_json:is_true(<<"Node-Specific">>, ApiJObj) of
                    'true' ->
                        Node = wh_json:get_value(<<"Node">>, ApiJObj),
                        lager:debug("received sysconf node specific setting for ~s[~s.~s]"
                                    ,[Category, Node, Key]),
                        whapps_config:set_node(Category, Key, Value, Node);
                    'false' ->
                        lager:debug("received sysconf setting for ~s[~s.~s]"
                                    ,[Category, <<"default">>, Key]),
                        whapps_config:set(Category, Key, Value, <<"default">>)
                end,

    RespQ =  wh_json:get_value(<<"Server-ID">>, ApiJObj),
    Resp = [{<<"Category">>, Category}
            ,{<<"Key">>, Key}
            ,{<<"Value">>, Value}
            ,{<<"Msg-ID">>,  wh_json:get_value(<<"Msg-ID">>, ApiJObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_sysconf:publish_set_resp(RespQ, Resp).
