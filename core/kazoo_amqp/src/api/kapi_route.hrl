-ifndef(KAPI_ROUTE_HRL).

%% routing keys to use in the callmgr exchange
-define(KEY_ROUTE_REQ, <<"route.req">>). %% corresponds to the route_req/1 api call

-define(EVENT_CATEGORY, <<"dialplan">>).
-define(KEY_CALL_ID, <<"Call-ID">>).
-define(KEY_CONTROL_QUEUE, <<"Control-Queue">>).
-define(KEY_FETCH_ID, [<<"Custom-Channel-Vars">>, <<"Fetch-ID">>]).

%% Route Requests (Used on kapi_schemas.erl module)
-define(ROUTE_REQ_COST_PARAMS, [<<"Max-Incremental-Cost">>
                               ,<<"Max-Setup-Cost">>
                               ,<<"Min-Increment-Cost">>
                               ,<<"Min-Setup-Cost">>
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


-define(KAPI_ROUTE_HRL, 'true').
-endif.
