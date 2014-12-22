%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_local_sms).

-export([local_message_handling/2]).

-include("stepswitch.hrl").

-spec local_message_handling(wh_proplist(), wh_json:object() ) -> 'ok'.
local_message_handling(Props, JObj) ->
    FetchId = wh_util:rand_hex_binary(16),
    CallId = wh_util:rand_hex_binary(16),
    ServerID = wh_json:get_ne_value(<<"Server-ID">>, JObj),
    ReqResp = wh_amqp_worker:call(route_req(CallId, FetchId, Props, JObj)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]),
            Delivery = delivery_from_req(JObj, <<"Error">>, <<"500">>, 'true'),
            send_sms_response(sms_error(Delivery, JObj), ServerID);
        {'ok', JObjResp} ->
            'true' = wapi_route:resp_v(JObjResp),
            Delivery = delivery_from_req(JObj, <<"Success">>, <<"200">>, 'undefined'),
            send_sms_response(sms_success(Delivery, JObj), ServerID),
            send_route_win(FetchId, CallId, JObjResp)
    end,
    'ok'.

-spec sms_error(wh_json:object(), wh_json:object()) -> wh_proplist().
sms_error(JObj, Request) ->
    lager:debug("error during outbound request: ~s", [wh_util:to_binary(wh_json:encode(JObj))]),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
     ,{<<"To-DID">>, wh_json:get_value(<<"To-DID">>, Request)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_success(wh_json:object(), wh_json:object()) -> wh_proplist().
sms_success(JObj, Request) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, Request)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, Request, <<>>)}
     ,{<<"Response-Message">>, <<"SUCCESS">>}
     ,{<<"Response-Code">>, <<"sip:200">>}
     ,{<<"Resource-Response">>, JObj}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec send_sms_response(wh_json:object() | wh_proplist(), ne_binary()) -> 'ok'. 
send_sms_response(JObj, ServerID) ->
    wh_amqp_worker:cast(JObj, fun(A) -> wapi_offnet_resource:publish_resp(ServerID, A) end).

-spec send_route_win(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
send_route_win(_FetchId, CallId, JObj) ->
    ServerQ = wh_json:get_value(<<"Server-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Win = [{<<"Msg-ID">>, CallId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
           ,{<<"Custom-Channel-Vars">>, CCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ServerQ]),
    wh_amqp_worker:cast(Win, fun(Payload)-> wapi_route:publish_win(ServerQ, Payload) end).

-spec delivery_from_req(wh_json:object(), binary(), api_binary(), api_boolean()) -> wh_json:object().
delivery_from_req(JObj, Status, DeliveryCode, DeliveryFailure) ->
    Keys = [<<"Event-Category">>
            ,<<"Event-Name">>
            ,<<"App-Name">>
            ,<<"App-Version">>
            ,<<"Node">>
           ],    
    Props = props:filter_empty(props:filter_undefined(
        [{<<"Delivery-Result-Code">>, DeliveryCode }
         ,{<<"Delivery-Failure">>, DeliveryFailure}
         ,{<<"Status">>, Status}
             | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
        ])),
    wh_json:set_values(Props, wh_json:delete_keys(Keys, JObj)).

-spec route_req(ne_binary(), ne_binary(), wh_proplist(), wh_json:object() ) -> wh_proplist().
route_req(CallId, FetchId, Props, JObj) ->
    TargetAccountId = wh_number_properties:account_id(Props),
    TargetAccountRealm = wh_util:get_account_realm(TargetAccountId),
    RequestAccountRealm = wh_json:get_value(<<"Account-Realm">>, JObj),
    ToDID = wh_json:get_value(<<"To-DID">>, JObj),
    To = <<ToDID/binary, "@", TargetAccountRealm/binary>>,
    FromNumber = wh_json:get_first_defined([<<"Caller-ID-Number">>
                                            ,<<"Outbound-Caller-ID-Number">>
                                           ], JObj, <<"0000000000">>),
    From = <<FromNumber/binary, "@", RequestAccountRealm/binary>>,        
    [{<<"Msg-ID">>, FetchId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Message-ID">>, wh_json:get_value(<<"Message-ID">>, JObj)}
     ,{<<"Caller-ID-Name">>, wh_json:get_first_defined([<<"Caller-ID-Name">>
                                                        ,<<"Outbound-Caller-ID-Name">>
                                                       ], JObj, <<"0000000000">>)}
     ,{<<"Caller-ID-Number">>, wh_json:get_first_defined([<<"Caller-ID-Number">>
                                                          ,<<"Outbound-Caller-ID-Number">>
                                                         ], JObj, <<"0000000000">>)}
     ,{<<"To">>, To}
     ,{<<"From">>, From}
     ,{<<"Request">>, To}
     ,{<<"Body">>, wh_json:get_value(<<"Body">>, JObj)}
     ,{<<"Custom-Channel-Vars">>, 
       wh_json:from_list(
         props:filter_undefined(
           [{<<"Fetch-ID">>, FetchId}
            ,{<<"Account-ID">>, TargetAccountId}
            ,{<<"Account-Realm">>, TargetAccountRealm}
            ,{<<"Inception">>, From}
           ]))}
     ,{<<"Resource-Type">>, <<"sms">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
