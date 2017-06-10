%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_local_sms).

-export([local_message_handling/2]).

-include("stepswitch.hrl").

-spec local_message_handling(knm_number_options:extra_options(), kapi_offnet_resource:req()) -> 'ok'.
local_message_handling(Props, OffnetReq) ->
    FetchId = kz_binary:rand_hex(16),
    CallId = kz_binary:rand_hex(16),
    ServerID = kapi_offnet_resource:server_id(OffnetReq),
    ReqResp = kz_amqp_worker:call(route_req(CallId, FetchId, Props, OffnetReq)
                                 ,fun kapi_route:publish_req/1
                                 ,fun kapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]),
            Delivery = delivery_from_req(OffnetReq, <<"Error">>, <<"500">>, 'true'),
            send_sms_response(sms_error(Delivery, OffnetReq), ServerID);
        {'ok', JObjResp} ->
            'true' = kapi_route:resp_v(JObjResp),
            Delivery = delivery_from_req(OffnetReq, <<"Success">>, <<"200">>, 'undefined'),
            send_sms_response(sms_success(Delivery, OffnetReq), ServerID),
            send_route_win(FetchId, CallId, JObjResp)
    end.

-spec sms_error(kz_json:object(), kapi_offnet_resource:req()) -> kz_proplist().
sms_error(JObj, OffnetReq) ->
    lager:debug("error during outbound request: ~s"
               ,[kz_json:encode(kapi_offnet_resource:req_to_jobj(OffnetReq))]
               ),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"NORMAL_TEMPORARY_FAILURE">>}
    ,{<<"Response-Code">>, <<"sip:500">>}
    ,{<<"Error-Message">>, kz_json:get_value(<<"Error-Message">>, JObj, <<"failed to process request">>)}
    ,{<<"To-DID">>, kapi_offnet_resource:to_did(OffnetReq)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec sms_success(kz_json:object(), kapi_offnet_resource:req()) -> kz_proplist().
sms_success(JObj, OffnetReq) ->
    lager:debug("outbound request successfully completed"),
    [{<<"Call-ID">>, kapi_offnet_resource:call_id(OffnetReq)}
    ,{<<"Msg-ID">>, kapi_offnet_resource:msg_id(OffnetReq)}
    ,{<<"Response-Message">>, <<"SUCCESS">>}
    ,{<<"Response-Code">>, <<"sip:200">>}
    ,{<<"Resource-Response">>, JObj}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec send_sms_response(kz_json:object() | kz_proplist(), ne_binary()) -> 'ok'.
send_sms_response(JObj, ServerID) ->
    kz_amqp_worker:cast(JObj, fun(A) -> kapi_offnet_resource:publish_resp(ServerID, A) end).

-spec send_route_win(ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
send_route_win(_FetchId, CallId, JObj) ->
    ServerQ = kz_api:server_id(JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    Win = [{<<"Msg-ID">>, CallId}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending route_win to ~s", [ServerQ]),
    kz_amqp_worker:cast(Win, fun(Payload)-> kapi_route:publish_win(ServerQ, Payload) end).

-spec delivery_from_req(kapi_offnet_resource:req(), binary(), api_binary(), api_boolean()) ->
                               kz_json:object().
delivery_from_req(OffnetReq, Status, DeliveryCode, DeliveryFailure) ->
    OffnetJObj = kapi_offnet_resource:req_to_jobj(OffnetReq),
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
               | kz_api:default_headers(<<"message">>, <<"delivery">>, ?APP_NAME, ?APP_VERSION)
              ]),

    kz_json:set_values(
      Props
                      ,kz_json:delete_keys(Keys, OffnetJObj)
     ).

-spec request_caller_id(kapi_offnet_resource:req()) -> {ne_binary(), ne_binary()}.
request_caller_id(OffnetReq) ->
    AccountId = kapi_offnet_resource:account_id(OffnetReq),
    {kapi_offnet_resource:outbound_caller_id_number(OffnetReq
                                                   ,kz_privacy:anonymous_caller_id_number(AccountId)
                                                   )
    ,kapi_offnet_resource:outbound_caller_id_name(OffnetReq
                                                 ,kz_privacy:anonymous_caller_id_name(AccountId)
                                                 )
    }.

-spec route_req(ne_binary(), ne_binary(), knm_number_options:extra_options(), kapi_offnet_resource:req()) -> kz_proplist().
route_req(CallId, FetchId, Props, OffnetReq) ->
    TargetAccountId = knm_number_options:account_id(Props),
    TargetAccountRealm = kz_util:get_account_realm(TargetAccountId),
    OffnetReqAccountRealm = kapi_offnet_resource:account_realm(OffnetReq),
    ToDID = kapi_offnet_resource:to_did(OffnetReq),
    To = <<ToDID/binary, "@", TargetAccountRealm/binary>>,
    {FromNumber, FromName} = request_caller_id(OffnetReq),
    From = <<FromNumber/binary, "@", OffnetReqAccountRealm/binary>>,

    CCVs = kz_json:from_list(
             [{<<"Fetch-ID">>, FetchId}
             ,{<<"Account-ID">>, TargetAccountId}
             ,{<<"Account-Realm">>, TargetAccountRealm}
             ,{<<"Inception">>, From}
             ]),

    [{<<"Msg-ID">>, FetchId}
    ,{<<"Call-ID">>, CallId}
    ,{<<"Message-ID">>, kapi_offnet_resource:message_id(OffnetReq)}
    ,{<<"Caller-ID-Name">>, FromName}
    ,{<<"Caller-ID-Number">>, FromNumber}
    ,{<<"To">>, To}
    ,{<<"From">>, From}
    ,{<<"Request">>, To}
    ,{<<"Body">>, kapi_offnet_resource:body(OffnetReq)}
    ,{<<"Custom-Channel-Vars">>, CCVs}
    ,{<<"Resource-Type">>, <<"sms">>}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
