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

-spec local_message_handling(wh_proplist(), wapi_offnet_resource:req()) -> 'ok'.
local_message_handling(Props, OffnetReq) ->
    FetchId = wh_util:rand_hex_binary(16),
    CallId = wh_util:rand_hex_binary(16),
    ServerID = wapi_offnet_resource:server_id(OffnetReq),
    ReqResp = wh_amqp_worker:call(route_req(CallId, FetchId, Props, OffnetReq)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]),
            Delivery = delivery_from_req(OffnetReq, <<"Error">>, <<"500">>, 'true'),
            send_sms_response(sms_error(Delivery, OffnetReq), ServerID);
        {'ok', JObjResp} ->
            'true' = wapi_route:resp_v(JObjResp),
            Delivery = delivery_from_req(OffnetReq, <<"Success">>, <<"200">>, 'undefined'),
            send_sms_response(sms_success(Delivery, OffnetReq), ServerID),
            send_route_win(FetchId, CallId, JObjResp)
    end.

-spec sms_error(wh_json:object(), wapi_offnet_resource:req()) -> wh_proplist().
sms_error(JObj, OffnetReq) ->
    lager:debug("error during outbound request: ~s"
                ,[wh_json:encode(wapi_offnet_resource:req_to_jobj(OffnetReq))]
               ),
    [{<<"Call-ID">>, wapi_offnet_resource:call_id(OffnetReq)}
     ,{<<"Msg-ID">>, wapi_offnet_resource:msg_id(OffnetReq)}
     ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
     ,{<<"Response-Code">>, <<"sip:500">>}
     ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
     ,{<<"To-DID">>, wapi_offnet_resource:to_did(OffnetReq)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_success(wh_json:object(), wapi_offnet_resource:req()) -> wh_proplist().
sms_success(JObj, OffnetReq) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, wapi_offnet_resource:call_id(OffnetReq)}
     ,{<<"Msg-ID">>, wapi_offnet_resource:msg_id(OffnetReq)}
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
    ServerQ = wh_api:server_id(JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Win = [{<<"Msg-ID">>, CallId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
           ,{<<"Custom-Channel-Vars">>, CCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ServerQ]),
    wh_amqp_worker:cast(Win, fun(Payload)-> wapi_route:publish_win(ServerQ, Payload) end).

-spec delivery_from_req(wapi_offnet_resource:req(), binary(), api_binary(), api_boolean()) ->
                               wh_json:object().
delivery_from_req(OffnetReq, Status, DeliveryCode, DeliveryFailure) ->
    OffnetJObj = wapi_offnet_resource:req_to_jobj(OffnetReq),
    Keys = [<<"Event-Category">>
            ,<<"Event-Name">>
            ,<<"App-Name">>
            ,<<"App-Version">>
            ,<<"Node">>
           ],
    Props = props:filter_empty(
              [{<<"Delivery-Result-Code">>, DeliveryCode}
               ,{<<"Delivery-Failure">>, DeliveryFailure}
               ,{<<"Status">>, Status}
               | wh_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
              ]),

    wh_json:set_values(
      Props
      ,wh_json:delete_keys(Keys, OffnetJObj)
     ).

-spec request_caller_id(wapi_offnet_resource:req()) -> {ne_binary(), ne_binary()}.
request_caller_id(OffnetReq) ->
    {wapi_offnet_resource:outbound_caller_id_number(OffnetReq
                                                    ,wh_util:anonymous_caller_id_number()
                                                   )
     ,wapi_offnet_resource:outbound_caller_id_name(OffnetReq
                                                     ,wh_util:anonymous_caller_id_name()
                                                    )
    }.

-spec route_req(ne_binary(), ne_binary(), wh_proplist(), wapi_offnet_resource:req()) -> wh_proplist().
route_req(CallId, FetchId, Props, OffnetReq) ->
    TargetAccountId = knm_number:account_id(Props),
    TargetAccountRealm = wh_util:get_account_realm(TargetAccountId),
    OffnetReqAccountRealm = wapi_offnet_resource:account_realm(OffnetReq),
    ToDID = wapi_offnet_resource:to_did(OffnetReq),
    To = <<ToDID/binary, "@", TargetAccountRealm/binary>>,
    {FromNumber, FromName} = request_caller_id(OffnetReq),
    From = <<FromNumber/binary, "@", OffnetReqAccountRealm/binary>>,

    CCVs =
        wh_json:from_list(
          props:filter_undefined(
            [{<<"Fetch-ID">>, FetchId}
             ,{<<"Account-ID">>, TargetAccountId}
             ,{<<"Account-Realm">>, TargetAccountRealm}
             ,{<<"Inception">>, From}
            ]
           )
         ),

    [{<<"Msg-ID">>, FetchId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Message-ID">>, wapi_offnet_resource:message_id(OffnetReq)}
     ,{<<"Caller-ID-Name">>, FromName}
     ,{<<"Caller-ID-Number">>, FromNumber}
     ,{<<"To">>, To}
     ,{<<"From">>, From}
     ,{<<"Request">>, To}
     ,{<<"Body">>, wapi_offnet_resource:body(OffnetReq)}
     ,{<<"Custom-Channel-Vars">>, CCVs}
     ,{<<"Resource-Type">>, <<"sms">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
