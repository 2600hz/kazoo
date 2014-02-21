%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_authz).

-export([authorize/3]).
-export([rate_channel/2]).
-export([kill_channel/2]).

-include("ecallmgr.hrl").

-define(RATE_VARS, [<<"Rate">>, <<"Rate-Increment">>
                    ,<<"Rate-Minimum">>, <<"Surcharge">>
                    ,<<"Rate-Name">>, <<"Base-Cost">>
                    ,<<"Discount-Percentage">>
                   ]).

-spec authorize(wh_proplist(), ne_binary(), atom()) -> boolean().
authorize(Props, CallId, Node) ->
    put('callid', CallId),
    case maybe_authorize_channel(Props, Node) of
        'true' ->
            lager:debug("channel is authorized", []),
            'true';
        'false' ->
            lager:debug("channel is not authorized", []),
            'false'
    end.

-spec kill_channel(wh_proplist(), atom()) -> 'ok'.
-spec kill_channel(ne_binary(), ne_binary(), atom()) -> 'ok'.

kill_channel(Props, Node) ->
    Direction = props:get_value(<<"Call-Direction">>, Props),
    CallId = props:get_value(<<"Unique-ID">>, Props),
    lager:debug("killing unauthorized channel", []),
    kill_channel(Direction, CallId, Node).

kill_channel(<<"inbound">>, CallId, Node) ->
    %% Give any pending route requests a chance to cleanly terminate this call,
    %% if it has not been processed yet.  Then chop its head off....
    _ = freeswitch:api(Node, 'uuid_kill', wh_util:to_list(<<CallId/binary, " INCOMING_CALL_BARRED">>)),
    'ok';
kill_channel(<<"outbound">>, CallId, Node) ->
    _ = freeswitch:api(Node, 'uuid_kill', wh_util:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    'ok'.

-spec maybe_authorize_channel(wh_proplist(), atom()) -> boolean().
maybe_authorize_channel(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    case props:get_value(?GET_CCV(<<"Channel-Authorized">>), Props) of
        <<"true">> ->
            wh_cache:store_local(?ECALLMGR_UTIL_CACHE
                                 ,?AUTHZ_RESPONSE_KEY(CallId)
                                 ,{'true', wh_json:new()}),
            'true';
        <<"false">> ->
            wh_cache:store_local(?ECALLMGR_UTIL_CACHE
                                 ,?AUTHZ_RESPONSE_KEY(CallId)
                                 ,'false'),
            'false';
        _Else ->
            case props:get_value(<<"Hunt-Destination-Number">>, Props) of
                <<"conference">> ->
                    wh_cache:store_local(?ECALLMGR_UTIL_CACHE
                                 ,?AUTHZ_RESPONSE_KEY(CallId)
                                 ,{'true', wh_json:new()}),
                    'true';
                _ -> maybe_channel_recovering(Props, CallId, Node)
            end
    end.

-spec maybe_channel_recovering(wh_proplist(), ne_binary(), atom()) -> boolean().
maybe_channel_recovering(Props, CallId, Node) ->
    case props:is_true(<<"variable_recovered">>, Props, 'false') of
        'true' -> allow_call(Props, CallId, Node);
        'false' -> is_authz_enabled(Props, CallId, Node)
    end.

-spec is_authz_enabled(wh_proplist(), ne_binary(), atom()) -> boolean().
is_authz_enabled(Props, CallId, Node) ->
    case ecallmgr_config:is_true(<<"authz_enabled">>, 'false') of
        'true' -> is_global_resource(Props, CallId, Node);
        'false' ->
            lager:debug("config ecallmgr.authz is disabled"),
            allow_call(Props, CallId, Node)
    end.

-spec is_global_resource(wh_proplist(), ne_binary(), atom()) -> boolean().
is_global_resource(Props, CallId, Node) ->
    case props:is_true(?GET_CCV(<<"Global-Resource">>), Props, 'true')
        orelse ecallmgr_config:is_true(<<"authz_local_resources">>, 'false')
    of
        'true' -> is_consuming_resource(Props, CallId, Node);
        'false' ->
            lager:debug("channel is a local resource"),
            allow_call(Props, CallId, Node)
    end.

-spec is_consuming_resource(wh_proplist(), ne_binary(), atom()) -> boolean().
is_consuming_resource(Props, CallId, Node) ->
    case props:get_value(<<"Call-Direction">>, Props) of
        <<"outbound">> ->
            case props:get_value(?GET_CCV(<<"Resource-ID">>), Props) =/= 'undefined' of
                'true' -> request_channel_authorization(Props, CallId, Node);
                'false' ->
                    lager:debug("outbound channel is not consuming a resource"),
                    allow_call(Props, CallId, Node)
            end;
        <<"inbound">> ->
            case props:get_value(?GET_CCV(<<"Authorizing-ID">>), Props) =:= 'undefined'
                orelse props:get_value(?GET_CCV(<<"Authorizing-Type">>), Props) =:= <<"resource">>
            of
                'true' -> request_channel_authorization(Props, CallId, Node);
                'false' ->
                    lager:debug("inbound channel is not consuming a resource"),
                    allow_call(Props, CallId, Node)
            end
    end.

-spec request_channel_authorization(wh_proplist(), ne_binary(), atom()) -> boolean().
request_channel_authorization(Props, CallId, Node) ->
    lager:debug("channel authorization request started"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,authz_req(Props)
                                  ,fun wapi_authz:publish_authz_req/1
                                  ,fun wapi_authz:authz_resp_v/1
                                  ,5000),
    case ReqResp of
        {'error', _R} ->
            lager:debug("authz request lookup failed: ~p", [_R]),
            authz_default(Props, CallId, Node);
        {'ok', JObj} -> authz_response(JObj, Props, CallId, Node)
    end.

-spec authz_response(wh_json:object(), wh_proplist(), ne_binary(), atom()) -> boolean().
authz_response(JObj, Props, CallId, Node) ->
    case wh_json:is_true(<<"Is-Authorized">>, JObj)
        orelse wh_json:is_true(<<"Soft-Limit">>, JObj)
    of
        'true' -> authorize_account(JObj, Props, CallId, Node);
        'false' ->
            lager:info("channel is unauthorized: ~s/~s"
                       ,[wh_json:get_value(<<"Account-Billing">>, JObj)
                         ,wh_json:get_value(<<"Reseller-Billing">>, JObj)
                        ]),
            case maybe_deny_call(Props, CallId, Node) of
                'true' -> authorize_account(JObj, Props, CallId, Node);
                'false' -> 'false'
            end
    end.

-spec authorize_account(wh_json:object(), wh_proplist(), ne_binary(), atom()) -> boolean().
authorize_account(JObj, Props, CallId, Node) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Type = wh_json:get_value(<<"Account-Billing">>, JObj),
    lager:debug("call authorized by account ~s as ~s", [AccountId, Type]),
    P = props:set_values([{?GET_CCV(<<"Account-ID">>), AccountId}
                          ,{?GET_CCV(<<"Account-Billing">>), Type}
                         ], Props),
    authorize_reseller(JObj, P, CallId, Node).

-spec authorize_reseller(wh_json:object(), wh_proplist(), ne_binary(), atom()) -> boolean().
authorize_reseller(JObj, Props, CallId, Node) ->
    AccountId = props:get_value(?GET_CCV(<<"Account-ID">>), Props),
    case wh_json:get_value(<<"Reseller-ID">>, JObj, AccountId) of
        AccountId -> rate_call(Props, CallId, Node);
        ResellerId ->
            Type = wh_json:get_value(<<"Reseller-Billing">>, JObj),
            lager:debug("call authorized by reseller ~s as ~s", [ResellerId, Type]),
            P = props:set_values([{?GET_CCV(<<"Reseller-ID">>), ResellerId}
                                  ,{?GET_CCV(<<"Reseller-Billing">>), Type}
                                 ], Props),
            rate_call(P, CallId, Node)
    end.

-spec rate_call(wh_proplist(), ne_binary(), atom()) -> 'true'.
rate_call(Props, CallId, Node) ->
    spawn(?MODULE, 'rate_channel', [Props, Node]),
    allow_call(Props, CallId, Node).

-spec allow_call(wh_proplist(), ne_binary(), atom()) -> 'true'.
allow_call(Props, CallId, Node) ->
    lager:debug("channel authorization succeeded, allowing call"),
    Vars = props:filter_undefined(
             [{<<"Account-ID">>, props:get_value(?GET_CCV(<<"Account-ID">>), Props)}
              ,{<<"Account-Billing">>, props:get_value(?GET_CCV(<<"Account-Billing">>), Props)}
              ,{<<"Reseller-ID">>, props:get_value(?GET_CCV(<<"Reseller-ID">>), Props)}
              ,{<<"Reseller-Billing">>, props:get_value(?GET_CCV(<<"Reseller-Billing">>), Props)}
              ,{<<"Global-Resource">>, props:get_value(?GET_CCV(<<"Global-Resource">>), Props)}
              ,{<<"Channel-Authorized">>, <<"true">>}
             ]),
    wh_cache:store_local(?ECALLMGR_UTIL_CACHE
                         ,?AUTHZ_RESPONSE_KEY(CallId)
                         ,{'true', wh_json:from_list(Vars)}),
    _ = case props:is_true(<<"Call-Setup">>, Props, 'false') of
            'false' -> ecallmgr_util:set(Node, CallId, Vars);
            'true' -> 'ok'
        end,
    'true'.

-spec maybe_deny_call(wh_proplist(), api_binary(), atom()) -> boolean().
maybe_deny_call(Props, CallId, Node) ->
    case ecallmgr_config:get_boolean(<<"authz_dry_run">>, 'false') of
        'true' -> rate_call(Props, CallId, Node);
        'false' ->
            wh_cache:store_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId), 'false'),
            spawn(?MODULE, 'kill_channel', [Props, Node]),
            'false'
    end.

-spec rate_channel(wh_proplist(), atom()) -> 'ok'.
rate_channel(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put('callid', CallId),
    lager:debug("sending rate request"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,rating_req(CallId, Props)
                                  ,fun wapi_rate:publish_req/1
                                  ,fun wapi_rate:resp_v/1
                                  ,10000
                                 ),
    case ReqResp of
        {'error', _R} -> lager:debug("rate request lookup failed: ~p", [_R]);
        {'ok', RespJObj} -> set_rating_ccvs(RespJObj, Node)
    end.

-spec authz_default(wh_proplist(), ne_binary(), atom()) -> {'ok', ne_binary()} | boolean().
%% TODO: fix use of authz_default
authz_default(Props, CallId, Node) ->
    case ecallmgr_config:get(<<"authz_default_action">>, <<"deny">>) of
        <<"deny">> -> maybe_deny_call(Props, CallId, Node);
        _Else -> allow_call(Props, CallId, Node)
    end.

-spec set_rating_ccvs(wh_json:object(), atom()) -> 'ok'.
set_rating_ccvs(JObj, Node) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    lager:debug("setting rating information"),
    Props = lists:foldl(fun(<<"Rate">>, Acc) ->
                                maybe_update_callee_id(JObj, Acc);
                           (Key, Acc) ->
                                case wh_json:get_binary_value(Key, JObj) of
                                    'undefined' -> Acc;
                                    Value ->
                                        [{Key, Value}|Acc]
                                   end
                           end, [], ?RATE_VARS),
    ecallmgr_util:set(Node, CallId, props:filter_undefined(Props)).

-spec maybe_update_callee_id(wh_json:object(), wh_proplist()) -> wh_proplist().
maybe_update_callee_id(JObj, Acc) ->
    Rate = wh_json:get_binary_value(<<"Rate">>, JObj, <<"0.00">>),
    case wh_json:is_true(<<"Update-Callee-ID">>, JObj, 'false') of
        'true' ->
            ConvertedRate = wh_util:to_binary(wht_util:units_to_dollars(wh_util:to_number(Rate))),
            [{<<"ignore_display_updates">>, <<"false">>}
             ,{<<"effective_callee_id_name">>, <<"$", ConvertedRate/binary
                                                 ," per min ${effective_callee_id_name}">>}
             ,{<<"Rate">>, Rate}
             | Acc
            ];
        'false' -> [{<<"Rate">>, Rate}|Acc]
    end.

-spec authz_req(wh_proplist()) -> wh_proplist().
authz_req(Props) ->
    props:filter_undefined(
      [{<<"To">>, ecallmgr_util:get_sip_to(Props)}
       ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
       ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
       ,{<<"Call-ID">>, props:get_value(<<"Unique-ID">>, Props)}
       ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
       ,{<<"Other-Leg-Call-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props)}
       ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec rating_req(ne_binary(), wh_proplist()) -> wh_proplist().
rating_req(CallId, Props) ->
    AccountId = props:get_value(?GET_CCV(<<"Account-ID">>), Props),
    [{<<"To-DID">>, props:get_value(<<"Caller-Destination-Number">>, Props)}
     ,{<<"From-DID">>, props:get_value(<<"variable_effective_caller_id_number">>, Props
                                       ,props:get_value(<<"Caller-Caller-ID-Number">>, Props))}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Account-ID">>, AccountId}
     ,{<<"Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
