%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handler for sms inbound AMQP payload
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    Map = #{number => Number
           ,inception => Inception
           ,request => JObj
           },
    Routines = [fun custom_header_token/1
               ,fun lookup_number/1
               ,fun account_from_number/1
               ,fun set_inception/1
               ,fun lookup_mdn/1
               ,fun set_static/1
               ,fun delete_headers/1
               ,fun set_realm/1
               ],
    case kz_maps:exec(Routines, Map) of
        #{account_id := _AccountId, used_by := <<"callflow">>} = M ->
            lager:info("processing inbound sms request ~s in account ~s", [Number, _AccountId]),
            process_sms_req(M);
        #{account_id := _AccountId, used_by := _UsedBy} ->
            lager:info("inbound sms request ~s in account ~s handled by ~s", [Number, _AccountId, _UsedBy]),
            'ack';
        M ->
            lager:info("unable to determine account for ~s => ~p", [Number, M]),
            %% TODO send system notify ?
            'ack'
    end.

-spec process_sms_req(map()) -> 'ack' | 'nack'.
process_sms_req(#{fetch_id := FetchId, call_id := CallId, request := JObj}) ->
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
    _ = kz_amqp_worker:cast(Win, fun(Payload) -> kapi_route:publish_win(ServerQ, Payload) end),
    'ack'.

custom_header_token(#{request := JObj} = Map) ->
    case kz_json:get_ne_binary_value([<<"Custom-SIP-Headers">>, <<"X-AUTH-Token">>], JObj) of
        'undefined' -> Map;
        Token ->
            custom_header_token(Map, JObj, Token)
    end.

custom_header_token(Map, JObj, Token) ->
    case binary:split(Token, <<"@">>, ['global']) of
        [AuthorizingId, AccountId | _] ->
            AccountRealm = kzd_accounts:fetch_realm(AccountId),
            AccountDb = kz_util:format_account_db(AccountId),
            case kz_datamgr:open_cache_doc(AccountDb, AuthorizingId) of
                {'ok', Doc} ->
                    Props = props:filter_undefined([{?CCV(<<"Authorizing-Type">>), kz_doc:type(Doc)}
                                                   ,{?CCV(<<"Authorizing-ID">>), AuthorizingId}
                                                   ,{?CCV(<<"Owner-ID">>), kzd_devices:owner_id(Doc)}
                                                   ,{?CCV(<<"Account-ID">>), AccountId}
                                                   ,{?CCV(<<"Account-Realm">>), AccountRealm}
                                                   ]),
                    Map#{authorizing_id => AuthorizingId
                        ,account_id => AccountId
                        ,request => kz_json:set_values(Props, JObj)
                        };
                _Else ->
                    lager:warning("unexpected result reading doc ~s/~s => ~p", [AuthorizingId, AccountId, _Else]),
                    Map
            end;
        _Else ->
            lager:warning("unexpected result spliting Token => ~p", [_Else]),
            Map
    end.




lookup_number(#{account_id := _AccountId} = Map) -> Map;
lookup_number(#{number := Number} = Map) ->
    case knm_phone_number:fetch(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for ~s: ~p", [Number, _R]),
            Map;
        {'ok', KNumber} ->
            Map#{phone_number => KNumber, used_by => knm_phone_number:used_by(KNumber)}
    end;
lookup_number(Map) ->
    Map.

account_from_number(#{account_id := _AccountId} = Map) -> Map;
account_from_number(#{phone_number := KNumber, request := JObj} = Map) ->
    case knm_phone_number:assigned_to(KNumber) of
        'undefined' -> Map;
        AccountId ->
            AccountRealm = kzd_accounts:fetch_realm(AccountId),
            Props = [{?CCV(<<"Account-ID">>), AccountId}
                    ,{?CCV(<<"Account-Realm">>), AccountRealm}
                    ,{?CCV(<<"Authorizing-Type">>), <<"resource">>}
                    ],
            Map#{account_id => AccountId
                ,request => kz_json:set_values(Props, JObj)
                }
    end;
account_from_number(Map) ->
    Map.

-spec set_inception(map()) -> map().
set_inception(#{inception := <<"offnet">>, request := JObj} = Map) ->
    Request = kz_json:get_value(<<"From">>, JObj),
    Map#{request => kz_json:set_value(?CCV(<<"Inception">>), Request, JObj)};
set_inception(#{request := JObj} = Map) ->
    Map#{request => kz_json:delete_keys([<<"Inception">>, ?CCV(<<"Inception">>)], JObj)}.

-spec lookup_mdn(map()) -> map().
lookup_mdn(#{authorizing_id := _AuthorizingId} = Map) -> Map;
lookup_mdn(#{phone_number := KNumber, request := JObj} = Map) ->
    Number = knm_phone_number:number(KNumber),
    case doodle_util:lookup_mdn(Number) of
        {'ok', Id, OwnerId} ->
            Props = props:filter_undefined([{?CCV(<<"Authorizing-Type">>), <<"device">>}
                                           ,{?CCV(<<"Authorizing-ID">>), Id}
                                           ,{?CCV(<<"Owner-ID">>), OwnerId}
                                           ]),
            JObj1 = kz_json:delete_keys([?CCV(<<"Authorizing-Type">>)
                                        ,?CCV(<<"Authorizing-ID">>)
                                        ], JObj),
            Map#{request => kz_json:set_values(Props, JObj1)};
        {'error', _} -> Map
    end;
lookup_mdn(Map) -> Map.

-spec set_static(map()) -> map().
set_static(#{request := JObj} = Map) ->
    FetchId = kz_api:msg_id(JObj, kz_binary:rand_hex(16)),
    CallId =  kz_api:call_id(JObj, kz_binary:rand_hex(16)),
    Props = [{<<"Resource-Type">>, <<"sms">>}
            ,{<<"Call-Direction">>, <<"inbound">>}
            ,{?CCV(<<"Channel-Authorized">>), 'true'}
            ],
    Map#{fetch_id => FetchId
        ,call_id => CallId
        ,request => kz_json:set_values(Props, JObj)
        }.

-spec delete_headers(map()) -> map().
delete_headers(#{request := JObj} = Map) ->
    Map#{request => kz_api:remove_defaults(JObj)}.

-spec set_realm(map()) -> map().
set_realm(#{request:= JObj, account_id := _AccountId} = Map) ->
    Realm = kz_json:get_value(?CCV(<<"Account-Realm">>), JObj),
    Keys = [<<"To">>, <<"From">>, {<<"To">>, <<"Request">>}],
    KVs = lists:foldl(fun({K1, K2}, Acc) ->
                              V = kz_json:get_value(K1, JObj),
                              [ set_realm_value(K2, V, Realm) | Acc];
                         (K, Acc) ->
                              V = kz_json:get_value(K, JObj),
                              [ set_realm_value(K, V, Realm) | Acc]
                      end, [], Keys),
    Map#{request => kz_json:set_values(KVs, JObj)};
set_realm(Map) -> Map.

-spec set_realm_value(K, kz_term:ne_binary(), kz_term:ne_binary()) -> {K, kz_term:ne_binary()}.
set_realm_value(K, Value, Realm) ->
    {K, <<Value/binary, "@", Realm/binary>>}.
