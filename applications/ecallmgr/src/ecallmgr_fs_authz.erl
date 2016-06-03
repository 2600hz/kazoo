%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz, INC
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
                    ,<<"Rate-Name">>, <<"Base-Cost">>, <<"Pvt-Cost">>
                    ,<<"Discount-Percentage">>, <<"Rate-NoCharge-Time">>
                   ]).

-spec authorize(kz_proplist(), ne_binary(), atom()) -> boolean().
authorize(Props, CallId, Node) ->
    kz_util:put_callid(CallId),
    case maybe_authorize_channel(Props, Node) of
        'true' ->
            lager:debug("channel is authorized"),
            'true';
        'false' ->
            lager:debug("channel is not authorized"),
            'false'
    end.

-spec kill_channel(kz_proplist(), atom()) -> 'ok'.
-spec kill_channel(ne_binary(), ne_binary(), ne_binary(), atom()) -> 'ok'.

kill_channel(Props, Node) ->
    Direction = kzd_freeswitch:call_direction(Props),
    ResourceType = kzd_freeswitch:resource_type(Props, <<"audio">>),
    CallId = kzd_freeswitch:call_id(Props),
    lager:debug("killing unauthorized channel"),
    kill_channel(Direction, ResourceType, CallId, Node).

kill_channel(_, <<"sms">>, _CallId, _Node) -> 'ok';
kill_channel(<<"inbound">>, _, CallId, Node) ->
    %% Give any pending route requests a chance to cleanly terminate this call,
    %% if it has not been processed yet.  Then chop its head off....
    _ = freeswitch:api(Node, 'uuid_kill', kz_util:to_list(<<CallId/binary, " USER_BUSY">>)),
    'ok';
kill_channel(<<"outbound">>, _, CallId, Node) ->
    _ = freeswitch:api(Node, 'uuid_kill', kz_util:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    'ok'.

-spec maybe_authorize_channel(kz_proplist(), atom()) -> boolean().
maybe_authorize_channel(Props, Node) ->
    CallId = kzd_freeswitch:call_id(Props),
    case kzd_freeswitch:channel_authorized(Props) of
        <<"true">> ->
            kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                                 ,?AUTHZ_RESPONSE_KEY(CallId)
                                 ,{'true', kz_json:new()}
                                ),
            'true';
        <<"false">> ->
            kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                                 ,?AUTHZ_RESPONSE_KEY(CallId)
                                 ,'false'
                                ),
            'false';
        _Else ->
            case kzd_freeswitch:hunt_destination_number(Props) of
                <<"conference">> ->
                    kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                                         ,?AUTHZ_RESPONSE_KEY(CallId)
                                         ,{'true', kz_json:new()}
                                        ),
                    'true';
                _Hunt ->
                    maybe_channel_recovering(Props, CallId, Node)
            end
    end.

-spec maybe_channel_recovering(kz_proplist(), ne_binary(), atom()) -> boolean().
maybe_channel_recovering(Props, CallId, Node) ->
    case kzd_freeswitch:is_channel_recovering(Props, 'false') of
        'true' ->
            allow_call(Props, CallId, Node);
        'false' ->
            is_authz_enabled(Props, CallId, Node)
    end.

-spec is_authz_enabled(kz_proplist(), ne_binary(), atom()) -> boolean().
is_authz_enabled(Props, CallId, Node) ->
    case ecallmgr_config:is_true(<<"authz_enabled">>, 'false') of
        'true' -> is_global_resource(Props, CallId, Node);
        'false' ->
            lager:debug("config ecallmgr.authz is disabled"),
            allow_call(Props, CallId, Node)
    end.

-spec is_global_resource(kz_proplist(), ne_binary(), atom()) -> boolean().
is_global_resource(Props, CallId, Node) ->
    case kzd_freeswitch:is_consuming_global_resource(Props, 'true')
        orelse ecallmgr_config:is_true(<<"authz_local_resources">>, 'false')
    of
        'true' -> is_consuming_resource(Props, CallId, Node);
        'false' ->
            lager:debug("channel is a local resource"),
            allow_call(Props, CallId, Node)
    end.

-spec is_consuming_resource(kz_proplist(), ne_binary(), atom()) -> boolean().
is_consuming_resource(Props, CallId, Node) ->
    case kzd_freeswitch:call_direction(Props) of
        <<"outbound">> ->
            is_consuming_outbound_resource(Props, CallId, Node);
        <<"inbound">> ->
            is_consuming_inbound_resource(Props, CallId, Node)
    end.

-spec is_consuming_outbound_resource(kz_proplist(), ne_binary(), atom()) -> boolean().
is_consuming_outbound_resource(Props, CallId, Node) ->
    case kzd_freeswitch:resource_id(Props) of
        'undefined' ->
            lager:debug("outbound channel is not consuming a resource"),
            allow_call(Props, CallId, Node);
        _ResourceId -> request_channel_authorization(Props, CallId, Node)
    end.

-spec is_consuming_inbound_resource(kz_proplist(), ne_binary(), atom()) -> boolean().
is_consuming_inbound_resource(Props, CallId, Node) ->
    case kzd_freeswitch:authorizing_id(Props) =:= 'undefined'
        orelse kzd_freeswitch:authorizing_type(Props) =:= <<"resource">>
    of
        'true' -> request_channel_authorization(Props, CallId, Node);
        'false' ->
            lager:debug("inbound channel is not consuming a resource"),
            allow_call(Props, CallId, Node)
    end.

-spec request_channel_authorization(kz_proplist(), ne_binary(), atom()) ->
                                           boolean().
request_channel_authorization(Props, CallId, Node) ->
    lager:debug("channel authorization request started"),
    ReqResp = kz_amqp_worker:call(authz_req(Props)
                                  ,fun kapi_authz:publish_authz_req/1
                                  ,fun kapi_authz:authz_resp_v/1
                                  ,ecallmgr_fs_node:fetch_timeout(Node)
                                 ),
    case ReqResp of
        {'error', _R} ->
            lager:debug("authz request lookup failed: ~p", [_R]),
            authz_default(Props, CallId, Node);
        {'ok', JObj} -> authz_response(JObj, Props, CallId, Node)
    end.

-spec authz_response(kz_json:object(), kz_proplist(), ne_binary(), atom()) -> boolean().
authz_response(JObj, Props, CallId, Node) ->
    'ok' = set_ccv_trunk_usage(JObj, CallId, Node),
    case kz_json:is_true(<<"Is-Authorized">>, JObj)
        orelse kz_json:is_true(<<"Soft-Limit">>, JObj)
    of
        'true' -> authorize_account(JObj, Props, CallId, Node);
        'false' ->
            lager:info("channel is unauthorized: ~s/~s"
                       ,[kz_json:get_value(<<"Account-Billing">>, JObj)
                         ,kz_json:get_value(<<"Reseller-Billing">>, JObj)
                        ]),
            case maybe_deny_call(Props, CallId, Node) of
                'true' -> authorize_account(JObj, Props, CallId, Node);
                'false' -> 'false'
            end
    end.

-spec set_ccv_trunk_usage(kz_json:object(), ne_binary(), atom()) -> 'ok'.
set_ccv_trunk_usage(JObj, CallId, Node) ->
    ecallmgr_fs_command:set(Node
                           ,CallId
                           ,[{Key, TrunkUsage}
                             || Key <- [<<"Account-Trunk-Usage">>
                                       ,<<"Reseller-Trunk-Usage">>
                                       ],
                                (TrunkUsage = kz_call_event:custom_channel_var(JObj, Key)) =/= 'undefined'
                            ]
                           ),
    'ok'.

-spec authorize_account(kz_json:object(), kz_proplist(), ne_binary(), atom()) ->
                               boolean().
authorize_account(JObj, Props, CallId, Node) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    Type      = kz_json:get_value(<<"Account-Billing">>, JObj),
    ChanVars  = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),

    lager:debug("call authorized by account ~s as ~s", [AccountId, Type]),
    P = props:set_values(
          [{?GET_CCV(<<"Account-ID">>), AccountId}
           ,{?GET_CCV(<<"Account-Billing">>), Type}
           ,{<<"Outbound-Flags">>, kz_json:get_value(<<"Outbound-Flags">>, ChanVars)}
          ]
          ,Props
         ),

    authorize_reseller(JObj, P, CallId, Node).

-spec authorize_reseller(kz_json:object(), kz_proplist(), ne_binary(), atom()) ->
                                boolean().
authorize_reseller(JObj, Props, CallId, Node) ->
    AccountId = props:get_value(?GET_CCV(<<"Account-ID">>), Props),
    case kz_json:get_value(<<"Reseller-ID">>, JObj, AccountId) of
        AccountId -> rate_call(Props, CallId, Node);
        ResellerId ->
            Type = kz_json:get_value(<<"Reseller-Billing">>, JObj),
            lager:debug("call authorized by reseller ~s as ~s", [ResellerId, Type]),
            P = props:set_values([{?GET_CCV(<<"Reseller-ID">>), ResellerId}
                                  ,{?GET_CCV(<<"Reseller-Billing">>), Type}
                                 ]
                                 ,Props
                                ),
            rate_call(P, CallId, Node)
    end.

-spec rate_call(kz_proplist(), ne_binary(), atom()) -> 'true'.
rate_call(Props, CallId, Node) ->
    _P = kz_util:spawn(fun rate_channel/2, [Props, Node]),
    lager:debug("rating call in ~p", [_P]),
    allow_call(Props, CallId, Node).

-spec allow_call(kz_proplist(), ne_binary(), atom()) -> 'true'.
allow_call(Props, CallId, Node) ->
    lager:debug("channel authorization succeeded, allowing call"),
    Vars = props:filter_undefined(
             [{<<"Account-ID">>, kzd_freeswitch:account_id(Props)}
              ,{<<"Account-Billing">>, kzd_freeswitch:account_billing(Props)}
              ,{<<"Reseller-ID">>, kzd_freeswitch:reseller_id(Props)}
              ,{<<"Reseller-Billing">>, kzd_freeswitch:reseller_billing(Props)}
              ,{<<"Global-Resource">>, kzd_freeswitch:is_consuming_global_resource(Props)}
              ,{<<"Channel-Authorized">>, <<"true">>}
             ]),
    kz_cache:store_local(?ECALLMGR_UTIL_CACHE
                         ,?AUTHZ_RESPONSE_KEY(CallId)
                         ,{'true', kz_json:from_list(Vars)}
                        ),
    _ = case props:is_true(<<"Call-Setup">>, Props, 'false') of
            'false' -> ecallmgr_fs_command:set(Node, CallId, Vars);
            'true' -> 'ok'
        end,
    'true'.

-spec maybe_deny_call(kz_proplist(), api_binary(), atom()) -> boolean().
maybe_deny_call(Props, CallId, Node) ->
    case ecallmgr_config:get_boolean(<<"authz_dry_run">>, 'false') of
        'true' -> rate_call(Props, CallId, Node);
        'false' ->
            kz_cache:store_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId), 'false'),
            kz_util:spawn(fun kill_channel/2, [Props, Node]),
            'false'
    end.

-spec rate_channel(kz_proplist(), atom()) -> 'ok'.
rate_channel(Props, Node) ->
    CallId = kzd_freeswitch:call_id(Props),
    kz_util:put_callid(CallId),
    Direction = kzd_freeswitch:call_direction(Props),
    ReqResp = kz_amqp_worker:call(rating_req(CallId, Props)
                                  ,fun kapi_rate:publish_req/1
                                  ,fun kapi_rate:resp_v/1
                                  %% get inbound_rate_resp_timeout or outbound_rate_resp_timeout
                                  ,ecallmgr_config:get_integer(<<Direction/binary, "_rate_resp_timeout">>, 10 * ?MILLISECONDS_IN_SECOND)
                                 ),
    rate_channel_resp(Props, Node, ReqResp).

-spec rate_channel_resp(kz_proplist(), atom(), kz_amqp_worker:request_return()) -> 'ok'.
rate_channel_resp(Props, Node, {'ok', RespJObj}) ->
    maybe_set_rating_ccvs(Props, RespJObj, Node);
rate_channel_resp(Props, Node, {'error', _R}) ->
    lager:debug("rate request lookup failed: ~p", [_R]),

    %% disconnect only per_minute channels
    case kzd_freeswitch:account_billing(Props) =:= <<"per_minute">>
        orelse kzd_freeswitch:reseller_billing(Props) =:= <<"per_minute">>
    of
        'true' -> maybe_kill_unrated_channel(Props, Node);
        'false' -> 'ok'
    end.

-spec maybe_kill_unrated_channel(kz_proplist(), atom()) -> 'ok'.
maybe_kill_unrated_channel(Props, Node) ->
    Direction = kzd_freeswitch:call_direction(Props),

    case ecallmgr_config:is_true(<<Direction/binary, "_rate_required">>, 'false') of
        'false' -> 'ok';
        'true' ->
            lager:debug("no rate returned for ~s call, killing this channel", [Direction]),
            kill_channel(Props, Node)
    end.

-spec authz_default(kz_proplist(), ne_binary(), atom()) -> {'ok', ne_binary()} | boolean().
%% TODO: fix use of authz_default
authz_default(Props, CallId, Node) ->
    case ecallmgr_config:get(<<"authz_default_action">>, <<"deny">>) of
        <<"deny">> -> maybe_deny_call(Props, CallId, Node);
        _Else -> allow_call(Props, CallId, Node)
    end.

-spec maybe_set_rating_ccvs(kz_proplist(), kz_json:object(), atom()) -> 'ok'.
maybe_set_rating_ccvs(Props, JObj, Node) ->
    case kz_json:get_integer_value(<<"Rate">>, JObj) of
        'undefined' -> maybe_kill_unrated_channel(Props, Node);
        _Rate -> set_rating_ccvs(JObj, Node)
    end.

-spec set_rating_ccvs(kz_json:object(), atom()) -> 'ok'.
set_rating_ccvs(JObj, Node) ->
    lager:debug("setting rating information"),
    ecallmgr_fs_command:set(Node
                      ,kz_json:get_value(<<"Call-ID">>, JObj)
                      ,get_rating_ccvs(JObj)
                     ).

-spec get_rating_ccvs(kz_json:object()) -> kz_proplist().
get_rating_ccvs(JObj) ->
    lists:foldl(fun(Key, Acc) ->
                        rating_ccv(Key, Acc, JObj)
                end
                ,[]
                ,?RATE_VARS
               ).

-spec rating_ccv(ne_binary(), kz_proplist(), kz_json:object()) ->
                        kz_proplist().
rating_ccv(<<"Rate">>, Acc, JObj) ->
    maybe_update_callee_id(JObj, Acc);
rating_ccv(Key, Acc, JObj) ->
    case kz_json:get_binary_value(Key, JObj) of
        'undefined' -> Acc;
        Value -> [{Key, Value}|Acc]
    end.

-spec maybe_update_callee_id(kz_json:object(), kz_proplist()) -> kz_proplist().
maybe_update_callee_id(JObj, Acc) ->
    Rate = kz_json:get_binary_value(<<"Rate">>, JObj, <<"0.00">>),

    case kz_json:is_true(<<"Update-Callee-ID">>, JObj, 'false') of
        'true' ->
            ConvertedRate = kz_util:to_binary(wht_util:units_to_dollars(kz_util:to_number(Rate))),
            [{<<"ignore_display_updates">>, <<"false">>}
             ,{<<"effective_callee_id_name">>, <<"$", ConvertedRate/binary
                                                 ," per min ${effective_callee_id_name}"
                                               >>
              }
             ,{<<"Rate">>, Rate}
             | Acc
            ];
        'false' -> [{<<"Rate">>, Rate}|Acc]
    end.

-spec authz_req(kz_proplist()) -> kz_proplist().
authz_req(Props) ->
    props:filter_undefined(
      [{<<"To">>, ecallmgr_util:get_sip_to(Props)}
       ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
       ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
       ,{<<"Call-ID">>, kzd_freeswitch:call_id(Props)}
       ,{<<"Call-Direction">>, kzd_freeswitch:call_direction(Props)}
       ,{<<"Other-Leg-Call-ID">>, kzd_freeswitch:other_leg_call_id(Props)}
       ,{<<"Caller-ID-Name">>, kzd_freeswitch:caller_id_name(Props, kz_util:anonymous_caller_id_name())}
       ,{<<"Caller-ID-Number">>, kzd_freeswitch:caller_id_number(Props, kz_util:anonymous_caller_id_number())}
       ,{<<"From-Network-Addr">>, kzd_freeswitch:from_network_ip(Props)}
       ,{<<"From-Network-Port">>, kzd_freeswitch:from_network_port(Props)}
       ,{<<"Custom-Channel-Vars">>, kz_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec rating_req(ne_binary(), kz_proplist()) -> kz_proplist().
rating_req(CallId, Props) ->
    [{<<"To-DID">>, kzd_freeswitch:to_did(Props)}
     ,{<<"From-DID">>, kzd_freeswitch:caller_id_number(Props)}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Account-ID">>, kzd_freeswitch:account_id(Props)}
     ,{<<"Direction">>, kzd_freeswitch:call_direction(Props)}
     ,{<<"Send-Empty">>, 'true'}
     ,{<<"Outbound-Flags">>, props:get_value(<<"Outbound-Flags">>, Props)}
     | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
