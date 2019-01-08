%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handler for sms inbound AMQP payload
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_inbound_handler).

-export([handle_req/3]).

-include("doodle.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist(), gen_listener:basic_deliver()) -> 'ok'.
handle_req(JObj, Props, Deliver) ->
    Srv = props:get_value('server', Props),
    case kapi_sms:inbound_v(JObj) of
        'true' ->
            handle_inbound_sms(JObj, Srv, Deliver);
        'false' ->
            lager:debug("error validating inbound message : ~p", [JObj]),
            gen_listener:ack(Srv, Deliver)
    end.

-spec handle_inbound_sms(kz_json:object(), pid(), gen_listener:basic_deliver()) -> 'ok'.
handle_inbound_sms(JObj, Srv, Deliver) ->
    case maybe_relay_request(JObj) of
        'ack' -> gen_listener:ack(Srv, Deliver);
        'nack' -> gen_listener:nack(Srv, Deliver)
    end.

-spec maybe_relay_request(kz_json:object()) -> 'ack' | 'nack'.
maybe_relay_request(JObj) ->
    {Number, Inception} = doodle_util:get_inbound_destination(JObj),
    case knm_number:lookup_account(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for ~s: ~p", [Number, _R]),
            %% TODO send system notify ?
            'ack';
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

            FetchId = kz_binary:rand_hex(16),
            CallId =  kz_binary:rand_hex(16),

            process_sms_req(FetchId, CallId, JObjReq)
    end.

-spec process_sms_req(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ack' | 'nack'.
process_sms_req(FetchId, CallId, JObj) ->
    Req = kz_json:set_values([{<<"Msg-ID">>, FetchId}
                             ,{<<"Call-ID">>, CallId}
                             ,{<<"Channel-Authorized">>, 'true'}
                             ,{?CCV(<<"Fetch-ID">>), FetchId}
                              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ], JObj),

    ReqResp = kz_amqp_worker:call(Req
                                 ,fun kapi_route:publish_req/1
                                 ,fun kapi_route:is_actionable_resp/1
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:info("did not receive route response for request ~s: ~p", [FetchId, _R]),
            'nack';
        {'ok', RespJObj} ->
            'true' = kapi_route:resp_v(RespJObj),
            send_route_win(FetchId, CallId, RespJObj)
    end.

-spec send_route_win(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ack'.
send_route_win(FetchId, CallId, JObj) ->
    ServerQ = kz_json:get_value(<<"Server-ID">>, JObj),
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    Win = [{<<"Msg-ID">>, FetchId}
          ,{<<"Call-ID">>, CallId}
          ,{<<"Control-Queue">>, <<"chatplan_ignored">>}
          ,{<<"Custom-Channel-Vars">>, CCVs}
           | kz_api:default_headers(<<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sms inbound handler sending route_win to ~s", [ServerQ]),
    kz_amqp_worker:cast(Win, fun(Payload) -> kapi_route:publish_win(ServerQ, Payload) end),
    'ack'.

%%------------------------------------------------------------------------------
%% @doc determine the e164 format of the inbound number
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(kz_term:ne_binary(), knm_number_options:extra_options(), kz_json:object()) ->
                            kz_json:object().
set_account_id(_Inception, NumberProps, JObj) ->
    AccountId = knm_number_options:account_id(NumberProps),
    AccountRealm = kzd_accounts:fetch_realm(AccountId),
    kz_json:set_values(props:filter_undefined([{?CCV(<<"Account-ID">>), AccountId}
                                              ,{?CCV(<<"Account-Realm">>), AccountRealm}
                                              ,{?CCV(<<"Authorizing-Type">>), <<"resource">>}
                                              ])
                      ,JObj
                      ).

-spec set_inception(kz_term:ne_binary(), knm_number_options:extra_options(), kz_json:object()) ->
                           kz_json:object().
set_inception(<<"off-net">>, _, JObj) ->
    Request = kz_json:get_value(<<"From">>, JObj),
    kz_json:set_value(?CCV(<<"Inception">>), Request, JObj);
set_inception(_Inception, _, JObj) ->
    kz_json:delete_keys([<<"Inception">>, ?CCV(<<"Inception">>)], JObj).

-spec set_mdn(kz_term:ne_binary(), knm_number_options:extra_options(), kz_json:object()) ->
                     kz_json:object().
set_mdn(<<"on-net">>, NumberProps, JObj) ->
    Number = knm_number_options:number(NumberProps),
    case doodle_util:lookup_mdn(Number) of
        {'ok', Id, OwnerId} ->
            kz_json:set_values(props:filter_undefined([{?CCV(<<"Authorizing-Type">>), <<"device">>}
                                                      ,{?CCV(<<"Authorizing-ID">>), Id}
                                                      ,{?CCV(<<"Owner-ID">>), OwnerId}
                                                      ])
                              ,kz_json:delete_keys([?CCV(<<"Authorizing-Type">>)
                                                   ,?CCV(<<"Authorizing-ID">>)
                                                   ]
                                                  ,JObj
                                                  )
                              );
        {'error', _} -> JObj
    end;
set_mdn(_Inception, _NumberProps, JObj) -> JObj.

-spec set_static(kz_term:ne_binary(), knm_number_options:extra_options(), kz_json:object()) ->
                        kz_json:object().
set_static(_Inception, _, JObj) ->
    kz_json:set_values([{<<"Resource-Type">>, <<"sms">>}
                       ,{<<"Call-Direction">>, <<"inbound">>}
                       ,{?CCV(<<"Channel-Authorized">>), 'true'}
                       ]
                      ,JObj
                      ).

-spec delete_headers(kz_term:ne_binary(), knm_number_options:extra_options(), kz_json:object()) ->
                            kz_json:object().
delete_headers(_, _, JObj) ->
    kz_api:remove_defaults(JObj).

-spec set_realm(kz_term:ne_binary(), knm_number_options:extra_options(), kz_json:object()) ->
                       kz_json:object().
set_realm(_, _, JObj) ->
    Realm = kz_json:get_value(?CCV(<<"Account-Realm">>), JObj),
    Keys = [<<"To">>, <<"From">>, {<<"To">>, <<"Request">>}],
    KVs = lists:foldl(fun({K1, K2}, Acc) ->
                              V = kz_json:get_value(K1, JObj),
                              [ set_realm_value(K2, V, Realm) | Acc];
                         (K, Acc) ->
                              V = kz_json:get_value(K, JObj),
                              [ set_realm_value(K, V, Realm) | Acc]
                      end, [], Keys),
    kz_json:set_values(KVs, JObj).

-spec set_realm_value(K, kz_term:ne_binary(), kz_term:ne_binary()) -> {K, kz_term:ne_binary()}.
set_realm_value(K, Value, Realm) ->
    {K, <<Value/binary, "@", Realm/binary>>}.
