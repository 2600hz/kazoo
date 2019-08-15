%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handle sms api docs
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_api).

-export([handle_api_sms/2]).

-include("doodle.hrl").

-spec handle_api_sms(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_api_sms(Db, Id) ->
    {'ok', Doc} = kz_datamgr:open_doc(Db, Id),
    Status = kz_json:get_value(<<"pvt_status">>, Doc),
    Origin = kz_json:get_value(<<"pvt_origin">>, Doc),
    FetchId = kz_binary:rand_hex(16),
    maybe_handle_sms_document(Status, Origin, FetchId, Id, Doc).

-spec maybe_handle_sms_document(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_handle_sms_document(<<"queued">>, <<"api">>, FetchId, Id, JObj) ->
    process_sms_api_document(FetchId, Id, JObj);
maybe_handle_sms_document(_Status, _Origin, _FetchId, _Id, _JObj) -> 'ok'.

-spec process_sms_api_document(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
process_sms_api_document(FetchId, <<_:7/binary, CallId/binary>> = _Id, APIJObj) ->
    ReqResp = kz_amqp_worker:call(route_req(FetchId, CallId, APIJObj)
                                 ,fun kapi_route:publish_req/1
                                 ,fun kapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', RespJObj} ->
            'true' = kapi_route:resp_v(RespJObj),
            send_route_win(FetchId, CallId, RespJObj)
    end.

-spec send_route_win(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_route_win(FetchId, CallId, JObj) ->
    ServerQ = kz_json:get_value(<<"Server-ID">>, JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    Win = [{<<"Msg-ID">>, FetchId}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sms api handler sending route_win to ~s", [ServerQ]),
    kz_amqp_worker:cast(Win, fun(Payload) -> kapi_route:publish_win(ServerQ, Payload) end).

-spec route_req(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
route_req(FetchId, CallId, JObj) ->
    [{<<"Msg-ID">>, FetchId}
    ,{<<"Call-ID">>, CallId}
    ,{<<"Message-ID">>, kz_json:get_value(<<"Message-ID">>, JObj, kz_binary:rand_hex(16))}
    ,{<<"Caller-ID-Name">>, kz_json:get_value(<<"from_user">>, JObj)}
    ,{<<"Caller-ID-Number">>, kz_json:get_value(<<"from_user">>, JObj)}
    ,{<<"To">>, kz_json:get_value(<<"to">>, JObj)}
    ,{<<"From">>, kz_json:get_value(<<"from">>, JObj)}
    ,{<<"Request">>, kz_json:get_value(<<"request">>, JObj)}
    ,{<<"Body">>, kz_json:get_value(<<"body">>, JObj)}
    ,{<<"Custom-Channel-Vars">>, kz_json:from_list(route_req_ccvs(FetchId, JObj))}
    ,{<<"Resource-Type">>, <<"sms">>}
    ,{<<"Call-Direction">>, <<"inbound">>}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec route_req_ccvs(kz_term:ne_binary(), kz_json:object()) -> kz_term:proplist().
route_req_ccvs(FetchId, JObj) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
      ,{<<"Account-ID">>, kz_doc:account_id(JObj)}
      ,{<<"Reseller-ID">>, kzd_services:reseller_id(JObj)}
      ,{<<"Authorizing-Type">>, kz_json:get_value(<<"pvt_authorization_type">>, JObj)}
      ,{<<"Authorizing-ID">>, kz_json:get_value(<<"pvt_authorization">>, JObj)}
      ,{<<"Owner-ID">>, kz_json:get_value(<<"pvt_owner_id">>, JObj)}
      ,{<<"Channel-Authorized">>, 'true'}
      ,{<<"Doc-Revision">>, kz_doc:revision(JObj)}
      ,{<<"Doc-ID">>, kz_doc:id(JObj)}
      ,{<<"Scheduled-Delivery">>, kz_json:get_value(<<"scheduled">>, JObj)}
      ,{<<"API-Call">>, 'true'}
       | kz_json:to_proplist(<<"pvt_address_options">>, JObj)
      ]).
