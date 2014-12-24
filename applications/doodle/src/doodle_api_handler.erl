%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   
%%%-------------------------------------------------------------------
-module(doodle_api_handler).

-export([handle_req/2]).

-include("doodle.hrl").


-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok' | pid().
handle_req(JObj, _Props) ->
    'true' = wapi_conf:doc_update_v(JObj),
    Id = wh_json:get_value(<<"ID">>, JObj),
    Db = wh_json:get_value(<<"Database">>, JObj),
    {'ok', Doc} = couch_mgr:open_doc(Db, Id),
    Status = wh_json:get_value(<<"pvt_status">>, Doc),
    Origin = wh_json:get_value(<<"pvt_origin">>, Doc),
    FetchId = wh_util:rand_hex_binary(16),
    maybe_handle_sms_document(Status, Origin, FetchId, Id, Doc).

-spec maybe_handle_sms_document(binary(), binary(), binary(), binary(), wh_json:object()) -> 'ok'.
maybe_handle_sms_document(<<"queued">>, <<"api">>, FetchId, Id, Doc) ->
    process_sms_api_document(FetchId, Id, Doc);
maybe_handle_sms_document(_Status, _Origin, _FetchId, _Id, _Doc) -> 'ok'.   
                                                   
    

-spec process_sms_api_document(binary(), binary(), wh_json:object()) -> 'ok'.
process_sms_api_document(FetchId, Id, Doc) when not is_list(Doc) ->
    process_sms_api_document(FetchId, Id, wh_json:to_proplist(Doc));
process_sms_api_document(FetchId, <<_:7/binary, CallId/binary>> = _Id, Props) ->
    ReqResp = wh_amqp_worker:call(route_req(FetchId, CallId, Props)
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]);
        {'ok', JObj} ->
            'true' = wapi_route:resp_v(JObj),
            send_route_win(FetchId, CallId, JObj)
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

-spec route_req(ne_binary(), ne_binary(), wh_proplist()) -> wh_proplist().
route_req(FetchId, CallId, Props) ->
    [{<<"Msg-ID">>, FetchId}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Message-ID">>, props:get_value(<<"Message-ID">>, Props, wh_util:rand_hex_binary(16))}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"from_user">>, Props)}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"from_user">>, Props)}
     ,{<<"To">>, props:get_value(<<"to">>, Props)}
     ,{<<"From">>, props:get_value(<<"from">>, Props)}
     ,{<<"Request">>, props:get_value(<<"request">>, Props)}
     ,{<<"Body">>, props:get_value(<<"body">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(route_req_ccvs(FetchId, Props))}
     ,{<<"Resource-Type">>, <<"sms">>}
     ,{<<"Call-Direction">>, <<"inbound">>}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec route_req_ccvs(ne_binary(), wh_proplist()) -> wh_proplist().
route_req_ccvs(FetchId, Props) ->
    props:filter_undefined(
      [{<<"Fetch-ID">>, FetchId}
       ,{<<"Account-ID">>, props:get_value(<<"pvt_account_id">>, Props)}
       ,{<<"Reseller-ID">>, props:get_value(<<"pvt_reseller_id">>, Props)}
       ,{<<"Authorizing-Type">>, props:get_value(<<"pvt_authorization_type">>, Props)}
       ,{<<"Authorizing-ID">>, props:get_value(<<"pvt_authorization">>, Props)}
       ,{<<"Owner-ID">>, props:get_value(<<"pvt_owner_id">>, Props)}
       ,{<<"Channel-Authorized">>, 'true'}
       ,{<<"Doc-Revision">>, props:get_value(<<"_rev">>, Props)}
      ]
     ).
