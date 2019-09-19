%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_authz_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, _) ->
    'true' = kapi_authz:authz_req_v(JObj),
    kz_util:put_callid(JObj),
    maybe_determine_account_id(j5_request:from_jobj(JObj)).

-spec maybe_determine_account_id(j5_request:request()) -> 'ok'.
maybe_determine_account_id(Request) ->
    case j5_request:account_id(Request) of
        'undefined' -> determine_account_id(Request);
        _Else -> maybe_account_limited(Request)
    end.

-spec determine_account_id(j5_request:request()) -> 'ok'.
determine_account_id(Request) ->
    case j5_request:caller_network_address(Request) of
        'undefined' -> determine_account_id_from_number(Request);
        IP -> determine_account_id_from_ip(Request, IP)
    end.

-spec determine_account_id_from_ip(j5_request:request(), kz_term:ne_binary()) -> 'ok'.
determine_account_id_from_ip(Request, IP) ->
    case kapps_util:get_ccvs_by_ip(IP) of
        {'ok', AccountCCVs} ->
            maybe_inbound_account_by_ip(j5_request:from_ccvs(Request, AccountCCVs), IP);
        {'error', 'not_found'} ->
            lager:debug("auth for IP ~s not found, trying number", [IP]),
            determine_account_id_from_number(Request);
        {'error', {Type, TypeId}} ->
            send_disabled_deny(Request, Type, TypeId)
    end.

-spec maybe_inbound_account_by_ip(j5_request:request(), kz_term:ne_binary()) -> 'ok'.
maybe_inbound_account_by_ip(Request, IP) ->
    AuthorizingType =
        kz_json:get_value(<<"Authorizing-Type">>, j5_request:ccvs(Request)),
    case j5_request:call_direction(Request) =:= <<"inbound">>
        andalso lists:member(AuthorizingType, ?INBOUND_ACCOUNT_TYPES)
    of
        'true' -> inbound_account_by_ip(Request, IP);
        'false' ->
            lager:debug("source IP ~s authorizing type requires authorization", [IP]),
            maybe_account_limited(Request)
    end.

-spec inbound_account_by_ip(j5_request:request(), kz_term:ne_binary()) -> 'ok'.
inbound_account_by_ip(Request, IP) ->
    AccountId = j5_request:account_id(Request),
    lager:debug("source IP ~s belongs to account ~s, allowing"
               ,[IP, AccountId]
               ),
    Routines = [fun(R) ->
                        ResellerId = kz_services_reseller:get_id(AccountId),
                        j5_request:set_reseller_id(ResellerId, R)
                end
               ,fun(R) -> j5_request:authorize_account(<<"limits_disabled">>, R) end
               ,fun(R) -> j5_request:authorize_reseller(<<"limits_disabled">>, R) end
               ],
    send_response(lists:foldl(fun(F, R) -> F(R) end, Request, Routines)).

-spec determine_account_id_from_number(j5_request:request()) -> 'ok'.
determine_account_id_from_number(Request) ->
    Number = j5_request:number(Request),
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, Props} ->
            lager:debug("number ~s belongs to ~s", [Number, AccountId]),
            Routines = [fun(R) -> j5_request:set_account_id(AccountId, R) end
                       ,fun(R) ->
                                ResellerId = kz_services_reseller:get_id(AccountId),
                                j5_request:set_reseller_id(ResellerId, R)
                        end
                       ],
            maybe_local_resource(Props, lists:foldl(fun(F, R) -> F(R) end, Request, Routines));
        {'error', {'account_disabled', AccountId}} ->
            send_disabled_deny(Request, 'account_disabled', AccountId);
        {'error', _R} ->
            lager:debug("unable to determine account id for ~s: ~p"
                       ,[Number, _R]
                       ),
            'ok'
    end.

-spec send_disabled_deny(j5_request:request(), kapps_util:not_enabled_error(), kz_term:ne_binary()) -> 'ok'.
send_disabled_deny(Request, 'account_disabled', AccountId) ->
    lager:debug("account ~s is disabled, rejecting", [AccountId]),
    Routines = [fun(R) -> j5_request:set_account_id(AccountId, R) end
               ,fun(R) ->
                        ResellerId = kz_services_reseller:get_id(AccountId),
                        j5_request:set_reseller_id(ResellerId, R)
                end
               ,fun(R) -> j5_request:deny_account(<<"disabled">>, R) end
               ],
    send_response(lists:foldl(fun(F, R) -> F(R) end, Request, Routines));
send_disabled_deny(Request, 'owner_disabled', _OwnerId) ->
    lager:debug("user ~s is disabled, rejecting", [_OwnerId]),
    Routines = [fun(R) -> j5_request:deny_account(<<"disabled">>, R) end],
    send_response(lists:foldl(fun(F, R) -> F(R) end, Request, Routines));
send_disabled_deny(Request, 'device_disabled', _DeviceId) ->
    lager:debug("device ~s is disabled, rejecting", [_DeviceId]),
    Routines = [fun(R) -> j5_request:deny_account(<<"disabled">>, R) end],
    send_response(lists:foldl(fun(F, R) -> F(R) end, Request, Routines)).

-spec maybe_local_resource(knm_number_options:extra_options(), j5_request:request()) -> 'ok'.
maybe_local_resource( Props, Request) ->
    case knm_number_options:is_local_number(Props) of
        'true' -> maybe_authz_local_resource(Request);
        'false' ->
            maybe_account_limited(Request)
    end.

-spec maybe_authz_local_resource(j5_request:request()) -> 'ok'.
maybe_authz_local_resource(Request) ->
    case should_authz_local(Request) of
        'false' -> allow_local_resource(Request);
        'true' ->
            lager:debug("authz_local_resources enabled, applying limits for local numbers"),
            maybe_account_limited(Request)
    end.

-spec allow_local_resource(j5_request:request()) -> 'ok'.
allow_local_resource(Request) ->
    Number = j5_request:number(Request),
    lager:debug("number ~s is a local number for account ~s, allowing"
               ,[Number, j5_request:account_id(Request)]
               ),
    Routines = [fun(R) -> j5_request:authorize_account(<<"limits_disabled">>, R) end
               ,fun(R) -> j5_request:authorize_reseller(<<"limits_disabled">>, R) end
               ],
    send_response(lists:foldl(fun(F, R) -> F(R) end, Request, Routines)).

-spec should_authz_local(j5_request:request()) -> boolean().
should_authz_local(Request) ->
    Node = j5_request:node(Request),
    kapps_config:get_is_true(<<"ecallmgr">>, <<"authz_local_resources">>, 'false', Node).

-spec maybe_account_limited(j5_request:request()) -> 'ok'.
maybe_account_limited(Request) ->
    AccountId = j5_request:account_id(Request),
    Limits = j5_limits:get(AccountId),
    R = maybe_authorize(Request, Limits),
    case j5_request:is_authorized(R, Limits) of
        'true' -> maybe_determine_reseller_id(R);
        'false' ->
            lager:debug("account ~s is not authorized to create this channel"
                       ,[AccountId]
                       ),
            send_response(R)
    end.

-spec maybe_determine_reseller_id(j5_request:request()) -> 'ok'.
maybe_determine_reseller_id(Request) ->
    case j5_request:reseller_id(Request) of
        'undefined' -> determine_reseller_id(Request);
        _Else -> maybe_reseller_limited(Request)
    end.

-spec determine_reseller_id(j5_request:request()) -> 'ok'.
determine_reseller_id(Request) ->
    AccountId = j5_request:account_id(Request),
    ResellerId = kz_services_reseller:get_id(AccountId),
    maybe_reseller_limited(
      j5_request:set_reseller_id(ResellerId, Request)
     ).

-spec maybe_reseller_limited(j5_request:request()) -> 'ok'.
maybe_reseller_limited(Request) ->
    ResellerId = j5_request:reseller_id(Request),
    case j5_request:account_id(Request) =:= ResellerId of
        'true' ->
            lager:debug("channel belongs to reseller, ignoring reseller billing"),
            send_response(
              j5_request:authorize_reseller(<<"limits_disabled">>, Request)
             );
        'false' ->
            check_reseller_limits(Request, ResellerId)
    end.

-spec check_reseller_limits(j5_request:request(), kz_term:ne_binary()) -> 'ok'.
check_reseller_limits(Request, ResellerId) ->
    Limits = j5_limits:get(ResellerId),
    R = maybe_authorize(Request, Limits),
    (not j5_request:is_authorized(R, Limits))
        andalso lager:debug("reseller ~s is not authorized to create this channel"
                           ,[ResellerId]
                           ),
    send_response(R).

-spec maybe_authorize(j5_request:request(), j5_limits:limits()) ->
                             j5_request:request().
maybe_authorize(Request, Limits) ->
    case j5_limits:enabled(Limits) of
        'true' -> maybe_authorize_exception(Request, Limits);
        'false' ->
            lager:debug("limits are disabled for account ~s"
                       ,[j5_limits:account_id(Limits)]
                       ),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits)
    end.

-spec maybe_authorize_exception(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_authorize_exception(Request, Limits) ->
    CallDirection = j5_request:call_direction(Request),
    AuthType = kz_json:get_value(<<"Authorizing-Type">>, j5_request:ccvs(Request)),
    case not is_authorizing_mobile(AuthType)
        andalso j5_request:classification(Request)
    of
        'false' ->
            lager:debug("allowing mobile call"),
            j5_per_minute:authorize(Request, Limits);
        <<"emergency">> ->
            lager:debug("allowing emergency call"),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        <<"tollfree_us">> when CallDirection =:= <<"outbound">> ->
            lager:debug("allowing outbound tollfree call"),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        _Else -> maybe_hard_limit(Request, Limits)
    end.

-spec is_authorizing_mobile(kz_term:api_ne_binary()) -> boolean().
is_authorizing_mobile(<<"mobile">>) -> 'true';
is_authorizing_mobile(_) -> 'false'.

-spec maybe_hard_limit(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_hard_limit(Request, Limits) ->
    R = j5_hard_limit:authorize(Request, Limits),
    case j5_request:billing(R, Limits) of
        <<"hard_limit">> -> maybe_soft_limit(R, Limits);
        _Else -> authorize(R, Limits)
    end.

-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    Routines = [fun j5_allotments:authorize/2
               ,fun j5_flat_rate:authorize/2
               ,fun j5_per_minute:authorize/2
               ],
    Result = lists:foldl(fun(F, R) ->
                                 case j5_request:is_authorized(R, Limits) of
                                     'false' -> F(R, Limits);
                                     'true' -> R
                                 end
                         end
                        ,Request
                        ,Routines
                        ),
    maybe_soft_limit(Result, Limits).

-spec maybe_soft_limit(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_soft_limit(Request, Limits) ->
    case j5_request:is_authorized(Request) of
        'true' -> Request;
        'false' ->
            case j5_request:call_direction(Request) of
                <<"outbound">> -> maybe_outbound_soft_limit(Request, Limits);
                <<"inbound">> -> maybe_inbound_soft_limit(Request, Limits)
            end
    end.

-spec maybe_outbound_soft_limit(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_outbound_soft_limit(Request, Limits) ->
    case j5_limits:soft_limit_outbound(Limits) of
        'false' -> Request;
        'true' ->
            lager:debug("outbound channel authorization is not enforced (soft limit)"),
            j5_request:set_soft_limit(Request)
    end.

-spec maybe_inbound_soft_limit(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_inbound_soft_limit(Request, Limits) ->
    case j5_limits:soft_limit_inbound(Limits) of
        'false' -> Request;
        'true' ->
            lager:debug("inbound channel authorization is not enforced (soft limit)"),
            j5_request:set_soft_limit(Request)
    end.

-define(AUTZH_TYPES_FOR_OUTBOUND, [<<"account">>
                                  ,<<"user">>
                                  ,<<"device">>
                                  ,<<"mobile">>
                                  ]).

-spec maybe_get_outbound_flags(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
maybe_get_outbound_flags('undefined', _AuthId, _AccountDb) -> 'undefined';
maybe_get_outbound_flags(_AuthType, 'undefined', _AccountDb) -> 'undefined';
maybe_get_outbound_flags(AuthType, AuthId, AccountDb) ->
    case lists:member(AuthType, ?AUTZH_TYPES_FOR_OUTBOUND)
        andalso kz_endpoint:get(AuthId, AccountDb)
    of
        {'ok', Endpoint} -> get_outbound_flags(Endpoint);
        _ -> 'undefined'
    end.

-spec get_outbound_flags(kz_json:object()) -> kz_term:api_binary().
get_outbound_flags(Endpoint) ->
%%% TODO: without a kapps_call we can not support dynamic
%%%     flags yet
    case kzd_devices:outbound_static_flags(Endpoint) of
        [] -> 'undefined';
        Flags -> Flags
    end.

-spec send_response(j5_request:request()) -> 'ok'.
send_response(Request) ->
    ServerId  = j5_request:server_id(Request),
    AccountDb = kz_util:format_account_id(j5_request:account_id(Request), 'encoded'),
    AuthType  = kz_json:get_value(<<"Authorizing-Type">>, j5_request:ccvs(Request)),
    AuthId    = kz_json:get_value(<<"Authorizing-ID">>, j5_request:ccvs(Request)),

    OutboundFlags = maybe_get_outbound_flags(AuthType, AuthId, AccountDb),

    CCVs = kz_json:from_list(
             [{<<"Account-Trunk-Usage">>, trunk_usage(j5_request:account_id(Request))}
             ,{<<"Reseller-Trunk-Usage">>, trunk_usage(j5_request:reseller_id(Request))}
             ,{<<"Outbound-Flags">>, OutboundFlags}
             ,{<<"To">>, j5_request:number(Request)}
             ]),

    Resp = props:filter_undefined(
             [{<<"Is-Authorized">>, kz_term:to_binary(j5_request:is_authorized(Request))}
             ,{<<"Account-ID">>, j5_request:account_id(Request)}
             ,{<<"Account-Billing">>, j5_request:account_billing(Request)}
             ,{<<"Reseller-ID">>, j5_request:reseller_id(Request)}
             ,{<<"Reseller-Billing">>, j5_request:reseller_billing(Request)}
             ,{<<"Call-Direction">>, j5_request:call_direction(Request)}
             ,{<<"Other-Leg-Call-ID">>, j5_request:other_leg_call_id(Request)}
             ,{<<"Soft-Limit">>, kz_term:to_binary(j5_request:soft_limit(Request))}
             ,{<<"Msg-ID">>, j5_request:message_id(Request)}
             ,{<<"Call-ID">>, j5_request:call_id(Request)}
             ,{<<"Custom-Channel-Vars">>, CCVs}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),

    maybe_publish_authz_resp(Request, ServerId, Resp).

-spec maybe_publish_authz_resp(j5_request:request(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
maybe_publish_authz_resp(Request, ServerId, Resp) ->
    maybe_publish_authz_resp(Request, ServerId, Resp, j5_channels:is_destroyed(j5_request:call_id(Request))).

-spec maybe_publish_authz_resp(j5_request:request(), kz_term:ne_binary(), kz_term:proplist(), boolean()) -> 'ok'.
maybe_publish_authz_resp(_Request, _ServerId, _Resp, 'true') ->
    lager:notice("the channel has already been destroyed, not sending authz response");
maybe_publish_authz_resp(Request, ServerId, Resp, 'false') ->
    kapi_authz:publish_authz_resp(ServerId, Resp),
    case j5_request:is_authorized(Request) of
        'false' -> j5_util:send_system_alert(Request);
        'true' ->
            kapi_authz:broadcast_authz_resp(Resp),
            j5_channels:authorized(kz_json:from_list(Resp))
    end.

-spec trunk_usage(kz_term:ne_binary()) -> kz_term:ne_binary().
trunk_usage(Id) ->
    Limits = j5_limits:get(Id),
    <<(kz_term:to_binary(j5_limits:inbound_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_limits:outbound_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_limits:twoway_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_limits:burst_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_channels:inbound_flat_rate(Id)))/binary, "/"
     ,(kz_term:to_binary(j5_channels:outbound_flat_rate(Id)))/binary
    >>.
