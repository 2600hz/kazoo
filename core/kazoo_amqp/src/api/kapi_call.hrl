-ifndef(KAPI_CALL_HRL).

%% Routing key prefix for rating
-define(KEY_RATING_REQ, <<"call.rating">>).

%% Call Events
-define(CALL_EVENT_ROUTING_KEY(Event, CallId)
       ,list_to_binary(["call."
                       ,kz_term:to_binary(Event)
                       ,"."
                       ,kz_amqp_util:encode(CallId)
                       ])
       ).
-define(CALL_EVENT_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CALL_EVENT_HEADERS
       ,[<<"Application-Data">>
        ,<<"Application-Event">>
        ,<<"Application-Name">>
        ,<<"Application-Response">>
        ,<<"Billing-Seconds">>
        ,<<"Bridge-Hangup-Cause">>
        ,<<"Caller-Destination-Number">>
        ,<<"Call-Direction">>
        ,<<"Callee-ID-Name">>
        ,<<"Callee-ID-Number">>
        ,<<"Caller-ID-Name">>
        ,<<"Caller-ID-Number">>
        ,<<"Channel-Answer-State">>
        ,<<"Channel-Call-State">>
        ,<<"Channel-Created-Time">>
        ,<<"Channel-Is-Loopback">>
        ,<<"Channel-Debug">>
        ,<<"Channel-Loopback-Bowout">>
        ,<<"Channel-Loopback-Bowout-Execute">>
        ,<<"Channel-Loopback-Leg">>
        ,<<"Channel-Loopback-Other-Leg-ID">>
        ,<<"Channel-Moving">>
        ,<<"Channel-Name">>
        ,<<"Channel-State">>
        ,<<"Conference-Config">>
        ,<<"Conference-Name">>
        ,<<"Control-Queue">>
        ,<<"Custom-Application-Vars">>
        ,<<"Custom-Channel-Vars">>
        ,<<"Custom-SIP-Headers">>
        ,<<"Detected-Tone">>
        ,<<"Digits-Dialed">>
        ,<<"Disposition">>
        ,<<"DTMF-Digit">> %% DTMF and Tones
        ,<<"DTMF-Duration">>
        ,<<"Duration-Seconds">>
        ,<<"Fax-Info">>
        ,<<"From">>
        ,<<"From-Tag">>
        ,<<"From-Uri">>
        ,<<"Hangup-Cause">>
        ,<<"Hangup-Code">> %% Hangup
        ,<<"Interaction-ID">>
        ,<<"Intercepted-By">>
        ,<<"Length">>
        ,<<"Local-SDP">>
        ,<<"Media-Recordings">>
        ,<<"Media-Server">>
        ,<<"Origination-Call-ID">>
        ,<<"Other-Leg-Call-ID">> %% BRIDGE
        ,<<"Other-Leg-Caller-ID-Name">>
        ,<<"Other-Leg-Caller-ID-Number">>
        ,<<"Other-Leg-Destination-Number">>
        ,<<"Other-Leg-Direction">>
        ,<<"Parking-Slot">>
        ,<<"Presence-ID">>
        ,<<"Raw-Application-Data">>
        ,<<"Raw-Application-Name">>
        ,<<"Recording">>
        ,<<"Remote-SDP">>
        ,<<"Replaced-By">>
        ,<<"Request">>
        ,<<"Ringing-Seconds">>
        ,<<"Silence-Terminated">> %% Record-related
        ,<<"Switch-Hostname">>
        ,<<"Switch-Nodename">>
        ,<<"Switch-URI">>
        ,<<"Switch-URL">>
        ,<<"Target-Call-ID">> %% TRANSFEREE
        ,<<"Terminator">>
        ,<<"Timestamp">>
        ,<<"To">>
        ,<<"To-Tag">>
        ,<<"To-Uri">>
        ,<<"Transfer-Type">>
        ,<<"Transfer-To">>
        ,<<"Transfer-History">>
        ,<<"Transfer-Source">>
        ,<<"User-Agent">>
        ,<<"Call-Debug">>
        ,<<"Root-Call-Interaction-ID">>
        ,<<"Resigning-UUID">>
        ,<<"Resigning-Peer-UUID">>
        ,<<"Connecting-Leg-A-UUID">>
        ,<<"Connecting-Leg-B-UUID">>
        ,<<"Endpoint-Disposition">>
        ,<<"Transfer-Disposition">>
        ,<<"Bridge-B-Unique-ID">>
        ]).
-define(CALL_EVENT_VALUES, [{<<"Event-Category">>, <<"call_event">>}]).
-define(CALL_EVENT_TYPES, [{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                          ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                          ,{<<"Custom-SIP-Headers">>, fun kz_json:is_json_object/1}
                          ,{<<"Fax-Info">>, fun kz_json:is_json_object/1}
                          ]).

%% Channel Status Request
-define(CHANNEL_STATUS_REQ_HEADERS, [<<"Call-ID">>]).
-define(OPTIONAL_CHANNEL_STATUS_REQ_HEADERS, [<<"Active-Only">>]).
-define(CHANNEL_STATUS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                   ,{<<"Event-Name">>, <<"channel_status_req">>}
                                   ]).
-define(CHANNEL_STATUS_REQ_TYPES, []).

%% Channel Status Response
-define(CHANNEL_STATUS_RESP_HEADERS, [<<"Call-ID">>, <<"Status">>]).
-define(OPTIONAL_CHANNEL_STATUS_RESP_HEADERS
       ,[<<"Custom-Application-Vars">>
        ,<<"Custom-Channel-Vars">>
        ,<<"Error-Msg">>
        ,<<"From-Tag">>
        ,<<"Other-Leg-Call-ID">>
        ,<<"Realm">>
        ,<<"Switch-Hostname">>
        ,<<"Switch-Nodename">>
        ,<<"Switch-URL">>
        ,<<"To-Tag">>
        ,<<"Username">>
        ]).
-define(CHANNEL_STATUS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"channel_status_resp">>}
                                    ,{<<"Status">>, [<<"active">>, <<"tmpdown">>, <<"terminated">>]}
                                    ]).
-define(CHANNEL_STATUS_RESP_TYPES, [{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                                   ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                                   ]).

%% Query Auth ID Req
-define(QUERY_AUTH_ID_REQ_HEADERS, [<<"Auth-ID">>]).
-define(OPTIONAL_QUERY_AUTH_ID_REQ_HEADERS, []).
-define(QUERY_AUTH_ID_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                  ,{<<"Event-Name">>, <<"query_auth_id_req">>}
                                  ]).
-define(QUERY_AUTH_ID_REQ_TYPES, []).

%% Query Auth ID Resp
-define(QUERY_AUTH_ID_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_AUTH_ID_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_AUTH_ID_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                   ,{<<"Event-Name">>, <<"query_auth_id_resp">>}
                                   ]).
-define(QUERY_AUTH_ID_RESP_TYPES, []).

%% Query User Channels Req
-define(QUERY_USER_CHANNELS_REQ_HEADERS, []).
-define(OPTIONAL_QUERY_USER_CHANNELS_REQ_HEADERS
       ,[<<"Active-Only">>
        ,<<"Authorizing-IDs">>
        ,<<"Realm">>
        ,<<"Username">>
        ,<<"Usernames">>
        ]).
-define(QUERY_USER_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                        ,{<<"Event-Name">>, <<"query_user_channels_req">>}
                                        ]).
-define(QUERY_USER_CHANNELS_REQ_TYPES, [{<<"Usernames">>, fun erlang:is_list/1}
                                       ,{<<"Username">>, fun erlang:is_binary/1}
                                       ,{<<"Authorizing-IDs">>, fun erlang:is_list/1}
                                       ,{<<"Active-Only">>, fun kz_term:is_boolean/1}
                                       ]).

%% Query User Channels Resp
-define(QUERY_USER_CHANNELS_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_USER_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_USER_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                         ,{<<"Event-Name">>, <<"query_user_channels_resp">>}
                                         ]).
-define(QUERY_USER_CHANNELS_RESP_TYPES, []).

%% Query Account Channels Req
-define(QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_QUERY_ACCOUNT_CHANNELS_REQ_HEADERS, [<<"Active-Only">>, <<"Username">>, <<"Usernames">>]).
-define(QUERY_ACCOUNT_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                           ,{<<"Event-Name">>, <<"query_account_channels_req">>}
                                           ]).
-define(QUERY_ACCOUNT_CHANNELS_REQ_TYPES, [{<<"Usernames">>, fun erlang:is_list/1}
                                          ,{<<"Username">>, fun erlang:is_binary/1}
                                          ,{<<"Active-Only">>, fun kz_term:is_boolean/1}
                                          ]).

%% Query Account Channels Resp
-define(QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, []).
-define(OPTIONAL_QUERY_ACCOUNT_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(QUERY_ACCOUNT_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                            ,{<<"Event-Name">>, <<"query_account_channels_resp">>}
                                            ]).
-define(QUERY_ACCOUNT_CHANNELS_RESP_TYPES, []).

%% Query Channels Req
-define(QUERY_CHANNELS_REQ_HEADERS, []).
-define(OPTIONAL_QUERY_CHANNELS_REQ_HEADERS, [<<"Fields">>, <<"Call-ID">>
                                             ,<<"Active-Only">>
                                             ]).
-define(QUERY_CHANNELS_REQ_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                   ,{<<"Event-Name">>, <<"query_channels_req">>}
                                   ]).
-define(QUERY_CHANNELS_REQ_TYPES, [{<<"Active-Only">>, fun kz_term:is_boolean/1}]).

%% Query Channels Resp
-define(QUERY_CHANNELS_RESP_HEADERS, [<<"Channels">>]).
-define(OPTIONAL_QUERY_CHANNELS_RESP_HEADERS, []).
-define(QUERY_CHANNELS_RESP_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                    ,{<<"Event-Name">>, <<"query_channels_resp">>}
                                    ]).
-define(QUERY_CHANNELS_RESP_TYPES, [{<<"Channels">>, fun kz_json:is_json_object/1}]).

%% Call Usurp Control
-define(CALL_USURP_CONTROL_HEADERS, [<<"Call-ID">>, <<"Fetch-ID">>]).
-define(OPTIONAL_CALL_USURP_CONTROL_HEADERS, [<<"Reason">>, <<"Media-Node">>]).
-define(CALL_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                   ,{<<"Event-Name">>, <<"usurp_control">>}
                                   ]).
-define(CALL_USURP_CONTROL_TYPES, []).

%% Usurp Call Event Publisher
-define(PUBLISHER_USURP_CONTROL_HEADERS, [<<"Call-ID">>, <<"Reference">>]).
-define(OPTIONAL_PUBLISHER_USURP_CONTROL_HEADERS, [<<"Reason">>, <<"Media-Node">>]).
-define(PUBLISHER_USURP_CONTROL_VALUES, [{<<"Event-Category">>, <<"call_event">>}
                                        ,{<<"Event-Name">>, <<"usurp_publisher">>}
                                        ]).
-define(PUBLISHER_USURP_CONTROL_TYPES, []).

-define(KAPI_CALL_HRL, 'true').
-endif.
