%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle AMQP requests to write system configuration data.
%%% Support nested keys like key#subkey#subsubkey
%%%
%%% @author Edouard Swiac
%%% @end
%%%-----------------------------------------------------------------------------
-module(sysconf_set).

-export([init/0
        ,handle_req/2
        ]).

-include("sysconf.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = kapi_sysconf:set_req_v(ApiJObj),
    kz_util:put_callid(ApiJObj),

    Category = kz_json:get_value(<<"Category">>, ApiJObj),
    Key = kz_json:get_value(<<"Key">>, ApiJObj),
    Value = kz_json:get_value(<<"Value">>, ApiJObj),

    {'ok', _} = case kz_json:is_true(<<"Node-Specific">>, ApiJObj) of
                    'true' ->
                        Node = kz_json:get_value(<<"Node">>, ApiJObj),
                        lager:debug("received sysconf node specific setting for ~s[~s.~s]"
                                   ,[Category, Node, Key]),
                        kapps_config:set_node(Category, Key, Value, Node);
                    'false' ->
                        lager:debug("received sysconf setting for ~s[~s.~s]"
                                   ,[Category, <<"default">>, Key]),
                        kapps_config:set(Category, Key, Value, <<"default">>)
                end,

    RespQ =  kz_json:get_value(<<"Server-ID">>, ApiJObj),
    Resp = [{<<"Category">>, Category}
           ,{<<"Key">>, Key}
           ,{<<"Value">>, Value}
           ,{<<"Msg-ID">>,  kz_json:get_value(<<"Msg-ID">>, ApiJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_sysconf:publish_set_resp(RespQ, Resp).
