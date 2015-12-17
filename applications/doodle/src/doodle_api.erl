%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%% Handle sms api docs
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(doodle_api).

-export([handle_api_sms/2]).

-include("doodle.hrl").

-spec handle_api_sms(ne_binary(), ne_binary()) -> 'ok'.
handle_api_sms(Db, Id) ->
    {'ok', Doc} = couch_mgr:open_doc(Db, Id),
    Status = wh_json:get_value(<<"pvt_status">>, Doc),
    Origin = wh_json:get_value(<<"pvt_origin">>, Doc),
    FetchId = wh_util:rand_hex_binary(16),
    maybe_handle_sms_document(Status, Origin, FetchId, Id, Doc).

-spec maybe_handle_sms_document(ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_handle_sms_document(<<"queued">>, <<"api">>, FetchId, Id, JObj) ->
    process_sms_api_document(FetchId, Id, JObj);
maybe_handle_sms_document(_Status, _Origin, _FetchId, _Id, _JObj) -> 'ok'.

-spec process_sms_api_document(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
process_sms_api_document(FetchId, <<_:7/binary, CallId/binary>> = _Id, APIJObj) ->
    ReqResp = wh_amqp_worker:call(route_req(FetchId, CallId, APIJObj)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', RespJObj} ->
            'true' = wapi_route:resp_v(RespJObj),
            send_route_win(FetchId, CallId, RespJObj)
    end.

-spec send_route_win(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
send_route_win(FetchId, CallId, JObj) ->
    ServerQ = wh_json:get_value(<<"Server-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Win = [{<<"Msg-ID">>, FetchId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
           ,{<<"Custom-Channel-Vars">>, CCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sms api handler sending route_win to ~s", [ServerQ]),
    wh_amqp_worker:cast(Win, fun(Payload) -> wapi_route:publish_win(ServerQ, Payload) end).

-spec route_req(ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().
route_req(FetchId, CallId, JObj) ->
    [{<<"Msg-ID">>, FetchId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Message-ID">>, wh_json:get_value(<<"Message-ID">>, JObj, wh_util:rand_hex_binary(16))}
     ,{<<"Caller-ID-Name">>, wh_json:get_value(<<"from_user">>, JObj)}
     ,{<<"Caller-ID-Number">>, wh_json:get_value(<<"from_user">>, JObj)}
     ,{<<"To">>, wh_json:get_value(<<"to">>, JObj)}
     ,{<<"From">>, wh_json:get_value(<<"from">>, JObj)}
     ,{<<"Request">>, wh_json:get_value(<<"request">>, JObj)}
     ,{<<"Body">>, wh_json:get_value(<<"body">>, JObj)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(route_req_ccvs(FetchId, JObj))}
     ,{<<"Resource-Type">>, <<"sms">>}
     ,{<<"Call-Direction">>, <<"inbound">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec route_req_ccvs(ne_binary(), wh_json:object()) -> wh_proplist().
route_req_ccvs(FetchId, JObj) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
       ,{<<"Account-ID">>, wh_doc:account_id(JObj)}
       ,{<<"Reseller-ID">>, kzd_services:reseller_id(JObj)}
       ,{<<"Authorizing-Type">>, wh_json:get_value(<<"pvt_authorization_type">>, JObj)}
       ,{<<"Authorizing-ID">>, wh_json:get_value(<<"pvt_authorization">>, JObj)}
       ,{<<"Owner-ID">>, wh_json:get_value(<<"pvt_owner_id">>, JObj)}
       ,{<<"Channel-Authorized">>, 'true'}
       ,{<<"Doc-Revision">>, wh_doc:revision(JObj)}
       ,{<<"Doc-ID">>, wh_doc:id(JObj)}
       ,{<<"Scheduled-Delivery">>, wh_json:get_value(<<"scheduled">>, JObj)}
       ,{<<"API-Call">>, 'true'}
       | wh_json:to_proplist(<<"pvt_address_options">>, JObj)
      ]).
