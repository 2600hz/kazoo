%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Handler for sms inbound AMQP payload
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(doodle_inbound_handler).

-export([handle_req/3]).

-include("doodle.hrl").

-spec handle_req(wh_json:object(), wh_proplist(), basic_deliver()) -> 'ok'.
handle_req(JObj, Props, Deliver) ->
    Srv = props:get_value('server', Props),
    case wapi_sms:inbound_v(JObj) of
        'true' ->
            case maybe_relay_request(JObj) of
                'ack' -> gen_listener:ack(Srv, Deliver);
                'nack' -> gen_listener:ack(Srv, Deliver)
            end;
        'false' ->
            lager:debug("error validating inbound message : ~p", [JObj]),
             gen_listener:ack(Srv, Deliver)
    end.

-spec maybe_relay_request(wh_json:object()) -> 'ack' | 'nack'.
maybe_relay_request(JObj) ->
    lager:debug("DOODLE INBOUND ~p", [JObj]),
    FetchId = wh_util:rand_hex_binary(16),
    CallId =  wh_util:rand_hex_binary(16),
    {Number, Inception} = doodle_util:get_inbound_destination(JObj),
    case doodle_util:lookup_number(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for ~s: ~p", [Number, _R]),
            'nack';
        {'ok', _, NumberProps} ->
            Routines = [fun set_account_id/3
                        ,fun set_inception/3
                        ,fun set_mdn/3
                        ,fun set_static/3
                        ,fun delete_headers/3
                        ,fun set_realm/3
                       ],
            Fun = fun(F, J) -> F(Inception, NumberProps, J) end,
            JObjReq = lists:foldl(Fun, JObj, Routines),
            process_sms_req(FetchId, CallId, JObjReq)
    end.

-spec process_sms_req(binary(), binary(), wh_json:object()) -> 'ack' | 'nack'.
process_sms_req(FetchId, CallId, JObj) when not is_list(JObj) ->
    Req = wh_json:set_values([{<<"Msg-ID">>, FetchId}
                              ,{<<"Call-ID">>, CallId}
                              ,{<<"Channel-Authorized">>, 'true'}
                              ,{?CCV(<<"Fetch-ID">>), FetchId}
                                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ], JObj),
    process_sms_req(FetchId, CallId, wh_json:to_proplist(Req));
process_sms_req(FetchId, CallId, Req) ->
    ReqResp = wh_amqp_worker:call(Req
                                  ,fun wapi_route:publish_req/1
                                  ,fun wapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]),
            'nack';
        {'ok', JObj} ->
            'true' = wapi_route:resp_v(JObj),
            send_route_win(FetchId, CallId, JObj)
    end.


-spec send_route_win(ne_binary(), ne_binary(), wh_json:object()) -> 'ack'.
send_route_win(FetchId, CallId, JObj) ->
    ServerQ = wh_json:get_value(<<"Server-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Win = [{<<"Msg-ID">>, FetchId}
           ,{<<"Call-ID">>, CallId}
           ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
           ,{<<"Custom-Channel-Vars">>, CCVs}
           | wh_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sms inbound handler sending route_win to ~s", [ServerQ]),
    wh_amqp_worker:cast(Win, fun(Payload) -> wapi_route:publish_win(ServerQ, Payload) end),
    'ack'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec set_account_id(ne_binary(), wh_proplist(), wh_json:object()) ->
                            wh_json:object().
set_account_id(_Inception, NumberProps, JObj) ->
    AccountId = wh_number_properties:account_id(NumberProps),
    AccountRealm = wh_util:get_account_realm(AccountId),
    wh_json:set_values([{?CCV(<<"Account-ID">>), AccountId}
                        ,{?CCV(<<"Account-Realm">>), AccountRealm}
                       ], JObj).

-spec set_inception(ne_binary(), wh_proplist(), wh_json:object()) ->
                           wh_json:object().
set_inception(Inception, _, JObj) ->
    case Inception of
        <<"off-net">> ->
            Request = wh_json:get_value(<<"From">>, JObj),
            wh_json:set_value(?CCV(<<"Inception">>), Request, JObj);
        _ -> wh_json:delete_keys([<<"Inception">>, ?CCV(<<"Inception">>)], JObj)
    end.

-spec set_mdn(ne_binary(), wh_proplist(), wh_json:object()) ->
          wh_json:object().
set_mdn(Inception, NumberProps, JObj) ->
    Number = wh_number_properties:number(NumberProps),
    case Inception of
        <<"on-net">> ->
            case doodle_util:lookup_mdn(Number) of
                {'ok', Id, OwnerId} ->
                    wh_json:set_values( props:filter_undefined(
                                          [{?CCV(<<"Authorizing-Type">>), <<"device">>}
                                           ,{?CCV(<<"Authorizing-ID">>), Id}
                                           ,{?CCV(<<"Owner-ID">>), OwnerId}
                                          ]), JObj);
                _ -> JObj
            end;
        _ -> JObj
    end.

-spec set_static(ne_binary(), wh_proplist(), wh_json:object()) ->
                           wh_json:object().
set_static(_Inception, _, JObj) ->
    wh_json:set_values([{<<"Resource-Type">>, <<"sms">>}
                        ,{<<"Call-Direction">>, <<"inbound">>}
                        ,{?CCV(<<"Channel-Authorized">>), 'true'}
                       ], JObj).

-spec delete_headers(ne_binary(), wh_proplist(), wh_json:object()) ->
                           wh_json:object().
delete_headers(_, _, JObj) ->
    Keys = [<<"Server-ID">>
            ,<<"App-Name">>
            ,<<"App-Version">>
            ,<<"Event-Category">>
            ,<<"Event-Name">>
            ,<<"Node">>
           ],
    wh_json:delete_keys(Keys, JObj).

-spec set_realm(ne_binary(), wh_proplist(), wh_json:object()) ->
                           wh_json:object().
set_realm(_, _, JObj) ->
    Realm = wh_json:get_value(?CCV(<<"Account-Realm">>), JObj),
    Keys = [<<"To">>, <<"From">>, {<<"To">>, <<"Request">>}],
    KVs = lists:foldl(fun({K1, K2}, Acc) ->
                              lager:debug("~p , ~p", [K1, K2]),
                              V = wh_json:get_value(K1, JObj),
                              [ set_realm_value(K2, V, Realm) | Acc];
                         (K, Acc) ->
                              V = wh_json:get_value(K, JObj),
                              [ set_realm_value(K, V, Realm) | Acc]
                      end, [], Keys),
    wh_json:set_values(KVs, JObj).

-spec set_realm_value(K, ne_binary(), ne_binary()) -> {K, ne_binary()}.
set_realm_value(K, Value, Realm) ->
    {K, <<Value/binary, "@", Realm/binary>>}.
