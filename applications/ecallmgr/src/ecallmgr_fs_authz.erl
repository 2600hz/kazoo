%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Make a request for authorization, and answer queries about the CallID
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_authz).

-export([authorize/3]).
-export([rate_channel/2]).
-export([kill_channel/2]).

-include("ecallmgr.hrl").

-define(RATE_VARS, [<<"Base-Cost">>
                   ,<<"Discount-Percentage">>
                   ,<<"Pvt-Cost">>
                   ,<<"Rate">>
                   ,<<"Rate-Description">>
                   ,<<"Rate-Increment">>
                   ,<<"Rate-Minimum">>
                   ,<<"Rate-Name">>
                   ,<<"Rate-NoCharge-Time">>
                   ,<<"Surcharge">>
                   ]).

-type authz_reply() :: boolean() | {'true', kz_json:object()}.

-export_type([authz_reply/0]).

-spec authorize(kzd_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
authorize(Props, CallId, Node) ->
    kz_util:put_callid(CallId),
    AuthorizeReply = is_emergency_number(Props)
        orelse is_mobile_device(Props)
        orelse maybe_authorized_channel(Props, Node),
    lager:info("channel is~s authorized", [authorized_log(AuthorizeReply)]),
    _ = ecallmgr_fs_channel:set_authorized(CallId, was_authorized(AuthorizeReply)),
    AuthorizeReply.

-spec was_authorized(authz_reply()) -> boolean().
was_authorized({'true', _}) -> 'true';
was_authorized('true') -> 'true';
was_authorized('false') -> 'false'.

-spec authorized_log(boolean()) -> string().
authorized_log({'true', _}) -> "";
authorized_log('true') -> "";
authorized_log('false') -> " not".

-spec kill_channel(kz_evt_freeswitch:data(), atom()) -> 'ok'.
kill_channel(Props, Node) ->
    Direction = kz_evt_freeswitch:call_direction(Props),
    ResourceType = kz_evt_freeswitch:resource_type(Props, <<"audio">>),
    CallId = kz_evt_freeswitch:call_id(Props),
    lager:debug("killing unauthorized channel"),
    kill_channel(Direction, ResourceType, CallId, Node).

-spec kill_channel(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), atom()) -> 'ok'.
kill_channel(_, <<"sms">>, _CallId, _Node) -> 'ok';
kill_channel(<<"inbound">>, _, CallId, Node) ->
    %% Give any pending route requests a chance to cleanly terminate this call,
    %% if it has not been processed yet.  Then chop its head off....
    _ = freeswitch:api(Node, 'uuid_kill', kz_term:to_list(<<CallId/binary, " USER_BUSY">>)),
    'ok';
kill_channel(<<"outbound">>, _, CallId, Node) ->
    _ = freeswitch:api(Node, 'uuid_kill', kz_term:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    'ok'.

-spec is_mobile_device(kz_evt_freeswitch:data()) -> boolean().
is_mobile_device(Props, Node) ->
    <<"mobile">> =:=  kz_evt_freeswitch:authorizing_type(Props).

-spec maybe_authorized_channel(kz_evt_freeswitch:data(), atom()) -> authz_reply().
maybe_authorized_channel(Props, Node) ->
    case kz_evt_freeswitch:channel_authorized(Props) of
        <<"true">> ->
            lager:debug("channel is already authorized"),
            'true';
        <<"false">> ->
            lager:debug("channel is already denied authorization"),
            'false';
        _Else ->
            maybe_authorize_conference_number(Props)
                orelse maybe_channel_recovering(Props, kz_evt_freeswitch:call_id(Props), Node)
    end.

-spec maybe_authorize_conference_number(kz_evt_freeswitch:data()) -> boolean().
maybe_authorize_conference_number(Props) ->
    lager:debug("is destination number 'conference': ~s"
               ,[kz_evt_freeswitch:hunt_destination_number(Props)]
               ),
    <<"conference">> =:= kz_evt_freeswitch:hunt_destination_number(Props).

-spec maybe_channel_recovering(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
maybe_channel_recovering(Props, CallId, Node) ->
    case kz_evt_freeswitch:is_channel_recovering(Props, 'false') of
        'false' -> is_authz_enabled(Props, CallId, Node);
        'true' ->
            lager:info("channel is authorized because it is recovering"),
            allow_call(Props, CallId, Node)
    end.

-spec is_authz_enabled(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_authz_enabled(Props, CallId, Node) ->
    case kapps_config:is_true(?APP_NAME, <<"authz_enabled">>, 'false') of
        'true' -> is_global_resource(Props, CallId, Node);
        'false' ->
            lager:info("channel is authorized because config ecallmgr.authz is disabled"),
            allow_call(Props, CallId, Node)
    end.

-spec is_global_resource(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_global_resource(Props, CallId, Node) ->
    case kz_evt_freeswitch:is_consuming_global_resource(Props, 'true')
        orelse kapps_config:is_true(?APP_NAME, <<"authz_local_resources">>, 'false')
    of
        'true' -> is_consuming_resource(Props, CallId, Node);
        'false' ->
            lager:debug("channel is authorized because it is a local resource"),
            allow_call(Props, CallId, Node)
    end.

-spec is_consuming_resource(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_consuming_resource(Props, CallId, Node) ->
    case kz_evt_freeswitch:call_direction(Props) of
        <<"outbound">> ->
            is_consuming_outbound_resource(Props, CallId, Node);
        <<"inbound">> ->
            is_consuming_inbound_resource(Props, CallId, Node)
    end.

-spec is_consuming_outbound_resource(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_consuming_outbound_resource(Props, CallId, Node) ->
    case kz_evt_freeswitch:resource_id(Props) of
        'undefined' ->
            lager:debug("outbound channel is authorized because it is not consuming a resource"),
            allow_call(Props, CallId, Node);
        _ResourceId -> request_channel_authorization(Props, CallId, Node)
    end.

-spec is_consuming_inbound_resource(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
is_consuming_inbound_resource(Props, CallId, Node) ->
    case kz_evt_freeswitch:authorizing_id(Props) =:= 'undefined'
        orelse kz_evt_freeswitch:authorizing_type(Props) =:= <<"resource">>
    of
        'true' -> request_channel_authorization(Props, CallId, Node);
        'false' ->
            lager:debug("inbound channel is authorized because it is not consuming a resource"),
            allow_call(Props, CallId, Node)
    end.

-spec request_channel_authorization(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                                           authz_reply().
request_channel_authorization(Props, CallId, Node) ->
    lager:debug("channel authorization request started"),
    ReqResp = kz_amqp_worker:call(authz_req(Props)
                                 ,fun kapi_authz:publish_authz_req/1
                                 ,fun kapi_authz:authz_resp_v/1
                                 ,ecallmgr_fs_node:fetch_timeout(Node)
                                 ),
    case ReqResp of
        {'ok', JObj} -> authz_response(JObj, Props, CallId, Node);
        {'error', _R} ->
            lager:notice("authz request lookup failed: ~p", [_R]),
            authz_default(Props, CallId, Node)
    end.

-spec authz_response(kz_json:object(), kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
authz_response(JObj, Props, CallId, Node) ->
    case kz_json:is_true(<<"Is-Authorized">>, JObj)
        orelse kz_json:is_true(<<"Soft-Limit">>, JObj)
    of
        'true' -> authorize_account(JObj, Props, CallId, Node);
        'false' ->
            AccountBilling = kz_json:get_value(<<"Account-Billing">>, JObj),
            ResellerBilling = kz_json:get_value(<<"Reseller-Billing">>, JObj),
            lager:info("channel is unauthorized: ~s/~s" ,[AccountBilling, ResellerBilling]),
            case kapps_config:get_boolean(?APP_NAME, <<"authz_dry_run">>, 'false') of
                'true' -> authorize_account(JObj, Props, CallId, Node);
                'false' ->
                    %% Set the following CCVs so that we can see why the call was barred in CDRs

                    %% set Account-ID and Reseller-ID so CDRs can be saved for this call
                    %% (in case the call is inbound from carrier)
                    AccountId = kz_json:get_value(<<"Account-ID">>
                                                 ,JObj
                                                 ,props:get_value(?GET_CCV(<<"Account-ID">>), Props)
                                                 ),
                    ResellerId = kz_json:get_value(<<"Reseller-ID">>
                                                  ,JObj
                                                  ,props:get_value(?GET_CCV(<<"Reseller-ID">>), Props)
                                                  ),
                    _ = ecallmgr_fs_command:set(Node, CallId, [{<<"Account-Billing">>, AccountBilling}
                                                              ,{<<"Account-ID">>, AccountId}
                                                              ,{<<"Reseller-Billing">>, ResellerBilling}
                                                              ,{<<"Reseller-ID">>, ResellerId}
                                                              ]),
                    _ = kz_util:spawn(fun kill_channel/2, [Props, Node]),
                    'false'
            end
    end.

-spec authorize_account(kz_json:object(), kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                               authz_reply().
authorize_account(JObj, Props, CallId, Node) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    Type      = kz_json:get_value(<<"Account-Billing">>, JObj),
    ChanVars  = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),

    lager:debug("channel is authorized by account ~s as ~s", [AccountId, Type]),
    CCVs = [{<<"Account-ID">>, AccountId}
           ,{<<"Account-Billing">>, Type}
            | maybe_add_outbound_flags(ChanVars)
           ],
    P = kz_evt_freeswitch:set_ccvs(Props, CCVs),

    authorize_reseller(JObj, P, CallId, Node).

-spec maybe_add_outbound_flags(kz_json:object()) -> kz_term:proplist().
maybe_add_outbound_flags(JObj) ->
    case kz_json:get_value(<<"Outbound-Flags">>, JObj) of
        'undefined' -> [];
        Flags -> [{<<"Outbound-Flags">>, Flags}]
    end.

-spec authorize_reseller(kz_json:object(), kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                                authz_reply().
authorize_reseller(JObj, Props, CallId, Node) ->
    AccountId = kz_evt_freeswitch:account_id(Props),
    case kz_json:get_value(<<"Reseller-ID">>, JObj, AccountId) of
        AccountId -> set_ccv_trunk_usage(JObj, Props, CallId, Node);
        ResellerId ->
            Type = kz_json:get_value(<<"Reseller-Billing">>, JObj),
            lager:debug("channel is authorized by reseller ~s as ~s", [ResellerId, Type]),
            P = kz_evt_freeswitch:set_ccvs(Props, [{<<"Reseller-ID">>, ResellerId}
                                               ,{<<"Reseller-Billing">>, Type}
                                               ]),
            set_ccv_trunk_usage(JObj, P, CallId, Node)
    end.

-spec set_ccv_trunk_usage(kz_json:object(), kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) ->
                                 authz_reply().
set_ccv_trunk_usage(JObj, Props, CallId, Node) ->
    Usage = [{Key, TrunkUsage}
             || Key <- [<<"Account-Trunk-Usage">>
                       ,<<"Reseller-Trunk-Usage">>
                       ],
                'undefined' =/= (TrunkUsage = kz_call_event:custom_channel_var(JObj, Key))
            ],
    P = kz_evt_freeswitch:set_ccvs(Props, props:filter_undefined(Usage)),
    rate_call(P, CallId, Node).

-spec rate_call(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
rate_call(Props, CallId, Node) ->
    _P = kz_util:spawn(fun rate_channel/2, [Props, Node]),
    lager:debug("rating call in ~p", [_P]),
    allow_call(Props, CallId, Node).

-spec allow_call(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> authz_reply().
allow_call(Props, _CallId, _Node) ->
    lager:debug("channel authorization succeeded, allowing call"),
    Vars = props:filter_undefined(
             [{<<"Account-ID">>, kz_evt_freeswitch:account_id(Props)}
             ,{<<"Account-Billing">>, kz_evt_freeswitch:account_billing(Props)}
             ,{<<"Account-Trunk-Usage">>, kz_evt_freeswitch:account_trunk_usage(Props)}
             ,{<<"Reseller-ID">>, kz_evt_freeswitch:reseller_id(Props)}
             ,{<<"Reseller-Billing">>, kz_evt_freeswitch:reseller_billing(Props)}
             ,{<<"Reseller-Trunk-Usage">>, kz_evt_freeswitch:reseller_trunk_usage(Props)}
             ,{<<"Global-Resource">>, kz_evt_freeswitch:is_consuming_global_resource(Props)}
             ,{<<"Channel-Authorized">>, <<"true">>}
             ]),
    case kz_evt_freeswitch:is_call_setup(Props) of
        'true' ->
            lager:info("channel is authorized (with channel vars)"),
            {'true', kz_json:from_list(Vars)};
        'false' ->
            lager:info("channel is authorized"),
            'true'
    end.

-spec rate_channel(kz_evt_freeswitch:data(), atom()) -> 'ok'.
rate_channel(Props, Node) ->
    CallId = kz_evt_freeswitch:call_id(Props),
    kz_util:put_callid(CallId),
    Direction = kz_evt_freeswitch:call_direction(Props),
    ReqResp = kz_amqp_worker:call(rating_req(CallId, Props)
                                 ,fun kapi_rate:publish_req/1
                                 ,fun kapi_rate:resp_v/1
                                  %% get inbound_rate_resp_timeout or outbound_rate_resp_timeout
                                 ,kapps_config:get_integer(?APP_NAME, <<Direction/binary, "_rate_resp_timeout">>, 10 * ?MILLISECONDS_IN_SECOND)
                                 ),
    rate_channel_resp(Props, Node, ReqResp).

-spec rate_channel_resp(kz_evt_freeswitch:data(), atom(), kz_amqp_worker:request_return()) -> 'ok'.
rate_channel_resp(Props, Node, {'ok', RespJObj}) ->
    maybe_set_rating_ccvs(Props, RespJObj, Node);
rate_channel_resp(Props, Node, {'error', _R}) ->
    lager:debug("rate request lookup failed: ~p", [_R]),

    %% disconnect only per_minute channels
    case <<"per_minute">> =:= kz_evt_freeswitch:account_billing(Props)
        orelse <<"per_minute">> =:= kz_evt_freeswitch:reseller_billing(Props)
    of
        'true' -> maybe_kill_unrated_channel(Props, Node);
        'false' -> 'ok'
    end.

-spec maybe_kill_unrated_channel(kz_evt_freeswitch:data(), atom()) -> 'ok'.
maybe_kill_unrated_channel(Props, Node) ->
    Direction = kz_evt_freeswitch:call_direction(Props),

    case kapps_config:is_true(?APP_NAME, <<Direction/binary, "_rate_required">>, 'false') of
        'false' -> 'ok';
        'true' ->
            lager:debug("no rate returned for ~s call, killing this channel", [Direction]),
            kill_channel(Props, Node)
    end.

-spec authz_default(kz_evt_freeswitch:data(), kz_term:ne_binary(), atom()) -> {'ok', ne_binary()} | boolean().
%% TODO: fix use of authz_default
authz_default(Props, CallId, Node) ->
    case kapps_config:get_ne_binary(?APP_NAME, <<"authz_default_action">>, <<"deny">>) =:= <<"deny">>
        andalso kapps_config:get_boolean(?APP_NAME, <<"authz_dry_run">>, 'false') =/= 'false'
    of
        'false' -> rate_call(Props, CallId, Node);
        'true' ->
            _ = kz_util:spawn(fun kill_channel/2, [Props, Node]),
            'false'
    end.

-spec maybe_set_rating_ccvs(kz_evt_freeswitch:data(), kz_json:object(), atom()) -> 'ok'.
maybe_set_rating_ccvs(Props, JObj, Node) ->
    case kz_json:get_value(<<"Rate">>, JObj) of
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

-spec get_rating_ccvs(kz_json:object()) -> kz_term:proplist().
get_rating_ccvs(JObj) ->
    lists:foldl(fun(Key, Acc) ->
                        rating_ccv(Key, Acc, JObj)
                end
               ,[]
               ,?RATE_VARS
               ).

-spec rating_ccv(kz_term:ne_binary(), kz_term:proplist(), kz_json:object()) ->
                        kz_term:proplist().
rating_ccv(<<"Rate">>, Acc, JObj) ->
    maybe_update_callee_id(JObj, Acc);
rating_ccv(Key, Acc, JObj) ->
    case kz_json:get_binary_value(Key, JObj) of
        'undefined' -> Acc;
        Value -> [{Key, Value}|Acc]
    end.

-spec maybe_update_callee_id(kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
maybe_update_callee_id(JObj, Acc) ->
    Rate = kz_json:get_binary_value(<<"Rate">>, JObj, <<"0.00">>),

    case kz_json:is_true(<<"Update-Callee-ID">>, JObj, 'false') of
        'true' ->
            ConvertedRate = kz_term:to_binary(kz_currency:units_to_dollars(kz_term:to_number(Rate))),
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

-spec authz_req(kz_evt_freeswitch:data()) -> kz_term:proplist().
authz_req(Props) ->
    AccountId = kz_evt_freeswitch:account_id(Props),
    props:filter_undefined(
      [{<<"Call-ID">>, kz_evt_freeswitch:call_id(Props)}
      ,{<<"To">>, kz_json:get_ne_binary_value(<<"To">>, Props)}
      ,{<<"From">>, kz_json:get_ne_binary_value(<<"From">>, Props)}
      ,{<<"Request">>, kz_json:get_ne_binary_value(<<"Request">>, Props)}
      ,{<<"Call-Direction">>, kz_evt_freeswitch:call_direction(Props)}
      ,{<<"Other-Leg-Call-ID">>, kz_evt_freeswitch:other_leg_call_id(Props)}
      ,{<<"Caller-ID-Name">>
       ,kz_evt_freeswitch:caller_id_name(Props, kapps_call:unknown_caller_id_name(AccountId))
       }
      ,{<<"Caller-ID-Number">>
       ,kz_evt_freeswitch:caller_id_number(Props, kz_privacy:anonymous_caller_id_number(AccountId))
       }
      ,{<<"From-Network-Addr">>, kz_evt_freeswitch:from_network_ip(Props)}
      ,{<<"From-Network-Port">>, kz_evt_freeswitch:from_network_port(Props)}
      ,{<<"Custom-Channel-Vars">>, kz_evt_freeswitch:ccvs(Props)}
      ,{<<"Custom-Channel-Vars">>, kz_evt_freeswitch:cavs(Props)}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec outbound_flags(kz_evt_freeswitch:data()) -> [binary()] | 'undefined'.
outbound_flags(Props) ->
    case kz_evt_freeswitch:outbound_flags(Props) of
        'undefined' -> 'undefined';
        <<_/binary>> = Flags -> binary:split(Flags, <<"|">>);
        Flags when is_list(Flags) -> Flags
    end.

-spec rating_req(kz_term:ne_binary(), kz_evt_freeswitch:data()) -> kz_term:proplist().
rating_req(CallId, Props) ->
    props:filter_undefined([{<<"To-DID">>, kz_evt_freeswitch:to_did(Props)}
                           ,{<<"From-DID">>, kz_evt_freeswitch:caller_id_number(Props)}
                           ,{<<"Call-ID">>, CallId}
                           ,{<<"Account-ID">>, kz_evt_freeswitch:account_id(Props)}
                           ,{<<"Direction">>, kz_evt_freeswitch:call_direction(Props)}
                           ,{<<"Send-Empty">>, 'true'}
                           ,{<<"Outbound-Flags">>, outbound_flags(Props)}
                           ,{<<"Resource-ID">>, kz_evt_freeswitch:ccv(Props, <<"Resource-ID">>)}
                           ,{<<"Authorizing-Type">>, kz_evt_freeswitch:authorizing_type(Props)}
                            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ]).

-spec is_emergency_number(kzd_freeswitch:data()) -> authz_reply().
is_emergency_number(Props) ->
    <<"emergency">> =:= knm_converters:classify(kzd_freeswitch:to_did(Props))
        andalso <<"outbound">> =:= kzd_freeswitch:call_direction(Props).
