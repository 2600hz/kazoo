-ifndef(KAPI_ROUTE_HRL).

-define(KEY_CALL_ID, <<"Call-ID">>).
-define(KEY_CONTROL_QUEUE, <<"Control-Queue">>).
-define(KEY_FETCH_ID, [<<"Custom-Channel-Vars">>, <<"Fetch-ID">>]).

%% routing keys to use in the callmgr exchange
-define(KEY_ROUTE_REQ, <<"route.req">>). %% corresponds to the route_req/1 api call

-define(EVENT_CATEGORY, <<"dialplan">>).
-define(ROUTE_REQ_EVENT_NAME, <<"route_req">>).

%% Route Requests
-define(ROUTE_REQ_HEADERS, [<<"From">>
                           ,<<"Request">>
                           ,<<"To">>
                           ,?KEY_CALL_ID
                           ]).
-define(OPTIONAL_ROUTE_REQ_HEADERS, [<<"Body">>
                                    ,<<"Call-Direction">>
                                    ,<<"Caller-ID-Name">>
                                    ,<<"Caller-ID-Number">>
                                    ,<<"Codecs">>
                                    ,<<"Cost-Parameters">>
                                    ,<<"Custom-Application-Vars">>
                                    ,<<"Custom-Channel-Vars">>
                                    ,<<"Custom-Routing-Headers">>
                                    ,<<"Custom-SIP-Headers">>
                                    ,<<"Context">>
                                    ,<<"From-Network-Addr">>
                                    ,<<"From-Network-Port">>
                                    ,<<"From-Tag">>
                                    ,<<"Geo-Location">>
                                    ,<<"Max-Call-Length">>
                                    ,<<"Media">>
                                    ,<<"Message-ID">>
                                    ,<<"Orig-IP">>
                                    ,<<"Orig-Port">>
                                    ,<<"Origination-Call-ID">>
                                    ,<<"Prepend-CID-Name">>
                                    ,<<"Resource-Type">>
                                    ,<<"Ringback-Media">>
                                    ,<<"SIP-Request-Host">>
                                    ,<<"Switch-Hostname">>
                                    ,<<"Switch-Nodename">>
                                    ,<<"Switch-URI">>
                                    ,<<"Switch-URL">>
                                    ,<<"To-Tag">>
                                    ,<<"Transcode">>
                                    ,<<"Transfer-Media">>
                                    ,<<"User-Agent">>
                                    ]).
-define(ROUTE_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                          ,{<<"Event-Name">>, ?ROUTE_REQ_EVENT_NAME}
                          ,{<<"Resource-Type">>, [<<"mms">>, <<"sms">>
                                                 ,<<"audio">>, <<"video">>
                                                 ,<<"chat">>, <<"metaflow">>
                                                 ]}
                          ,{<<"Media">>, [<<"process">>, <<"proxy">>, <<"bypass">>]}
                          ]).
-define(ROUTE_REQ_COST_PARAMS, [<<"Max-Incremental-Cost">>
                               ,<<"Max-Setup-Cost">>
                               ,<<"Min-Increment-Cost">>
                               ,<<"Min-Setup-Cost">>
                               ]).
-define(ROUTE_REQ_TYPES, [{<<"Caller-ID-Name">>, fun erlang:is_binary/1}
                         ,{<<"Caller-ID-Number">>, fun erlang:is_binary/1}
                         ,{<<"Cost-Parameters">>, fun has_cost_parameters/1}
                         ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                         ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                         ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                         ,{<<"Event-Queue">>, fun erlang:is_binary/1}
                         ,{<<"From">>, fun erlang:is_binary/1}
                         ,{<<"Request">>, fun erlang:is_binary/1}
                         ,{<<"To">>, fun erlang:is_binary/1}
                         ,{?KEY_CALL_ID, fun erlang:is_binary/1}
                         ]).

%% Route Responses
-define(ROUTE_RESP_ROUTE_HEADERS, [<<"Invite-Format">>]).
-define(OPTIONAL_ROUTE_RESP_ROUTE_HEADERS, [<<"Auth-Password">>
                                           ,<<"Auth-User">>
                                           ,<<"Caller-ID-Name">>
                                           ,<<"Caller-ID-Number">>
                                           ,<<"Caller-ID-Type">>
                                           ,<<"Codecs">>
                                           ,<<"Context">>
                                           ,<<"Custom-Application-Vars">>
                                           ,<<"Custom-Channel-Vars">>
                                           ,<<"Custom-SIP-Headers">>
                                           ,<<"Media">>
                                           ,<<"Progress-Timeout">>
                                           ,<<"Proxy-Via">>
                                           ,<<"Rate">>
                                           ,<<"Rate-Increment">>
                                           ,<<"Rate-Minimum">>
                                           ,<<"Rate-NoCharge-Time">>
                                           ,<<"Route">>
                                           ,<<"Surcharge">>
                                           ,<<"To-DID">>
                                           ,<<"To-Realm">>
                                           ,<<"To-User">>
                                           ,<<"Weight-Cost">>
                                           ,<<"Weight-Location">>
                                           ]).
-define(ROUTE_RESP_ROUTE_VALUES, [{<<"Media">>, [<<"process">>, <<"bypass">>, <<"auto">>]}
                                 ,{<<"Caller-ID-Type">>, [<<"from">>, <<"rpid">>, <<"pid">>]}
                                 ,?INVITE_FORMAT_TUPLE
                                 ]).
-define(ROUTE_RESP_ROUTE_TYPES, [{<<"Codecs">>, fun erlang:is_list/1}
                                ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                                ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                                ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                                ,{<<"Route">>, fun erlang:is_binary/1}
                                ,{<<"To-Realm">>, fun erlang:is_binary/1}
                                ,{<<"To-User">>, fun erlang:is_binary/1}
                                ]).

%% Route Responses
-define(ROUTE_RESP_HEADERS, [<<"Method">>]).
-define(OPTIONAL_ROUTE_RESP_HEADERS, [<<"Application-Data">>
                                     ,<<"Custom-Application-Vars">>
                                     ,<<"Custom-Channel-Vars">>
                                     ,<<"From-Realm">>
                                     ,<<"From-URI">>
                                     ,<<"From-User">>
                                     ,<<"Plan-Data">>
                                     ,<<"Pre-Park">>
                                     ,<<"Ringback-Media">>
                                     ,<<"Route-Error-Code">>
                                     ,<<"Route-Error-Message">>
                                     ,<<"Routes">>
                                     ,<<"Transfer-Media">>
                                     ]).
-define(ROUTE_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                           ,{<<"Event-Name">>, <<"route_resp">>}
                           ,{<<"Method">>, [<<"bridge">>, <<"park">>, <<"error">>, <<"sms">>
                                           ,<<"plan">>, <<"application">>
                                           ]
                            }
                           ,{<<"Pre-Park">>, [<<"none">>, <<"ring_ready">>, <<"answer">>]}
                           ]).
-define(ROUTE_RESP_TYPES, [{<<"Route-Error-Code">>, fun erlang:is_binary/1}
                          ,{<<"Route-Error-Message">>, fun erlang:is_binary/1}
                          ,{<<"Routes">>, fun erlang:is_list/1}
                          ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                          ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                          ]).

%% Route Winner
-define(ROUTE_WIN_HEADERS, [?KEY_CALL_ID, ?KEY_CONTROL_QUEUE]).
-define(OPTIONAL_ROUTE_WIN_HEADERS, [<<"Custom-Channel-Vars">>, <<"Switch-Hostname">>]).
-define(ROUTE_WIN_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                          ,{<<"Event-Name">>, <<"route_win">>}
                          ]).
-define(ROUTE_WIN_TYPES, [{?KEY_CALL_ID, fun erlang:is_binary/1}
                         ,{?KEY_CONTROL_QUEUE, fun erlang:is_binary/1}
                         ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                         ]).


-define(KAPI_ROUTE_HRL, 'true').
-endif.
