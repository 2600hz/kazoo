%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_authz_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req(kapi_authz:req(), kz_term:proplist()) -> any().
handle_req(JObj, _) ->
    kz_log:put_callid(JObj),
    'true' = kapi_authz:authz_req_v(JObj),
    Request = j5_request:from_jobj(JObj),
    maybe_account_limited(Request).

-spec maybe_account_limited(j5_request:request()) -> 'ok'.
maybe_account_limited(Request) ->
    AccountId = j5_request:account_id(Request),
    Limits = j5_limits:get(AccountId),
    R = maybe_authorize(Request, Limits),
    case j5_request:is_authorized(R, Limits) of
        'true' -> maybe_reseller_limited(R);
        'false' ->
            lager:debug("account ~s is not authorized to create this channel"
                       ,[AccountId]
                       ),
            send_response(R)
    end.

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
    Routines = [fun maybe_authorize_mobile/2
               ,fun maybe_authorize_resource_type/2
               ,fun maybe_authorize_classification/2
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
    maybe_hard_limit(Result, Limits).

-spec maybe_authorize_mobile(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_authorize_mobile(Request, Limits) ->
    AuthType = kz_json:get_value(<<"Authorizing-Type">>, j5_request:ccvs(Request)),

    case AuthType =:= <<"mobile">> of
        'true' ->
            lager:debug("allowing mobile call"),
            j5_per_minute:authorize(Request, Limits);
        'false' -> Request
    end.

-spec maybe_authorize_resource_type(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_authorize_resource_type(Request, Limits) ->
    ResourceType = kz_json:get_value(<<"Resource-Type">>, j5_request:ccvs(Request)),

    case lists:member(ResourceType, j5_limits:authz_resource_types(Limits)) of
        'true' ->
            lager:debug("allowing ~s call", [ResourceType]),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        'false' -> Request
    end.

-spec maybe_authorize_classification(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_authorize_classification(Request, Limits) ->
    Classification = j5_request:classification(Request),
    CallDirection = j5_request:call_direction(Request),

    case Classification of
        <<"emergency">> ->
            lager:debug("allowing emergency call"),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        <<"tollfree_us">> when CallDirection =:= <<"outbound">> ->
            lager:debug("allowing outbound tollfree call"),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        _Else -> Request
    end.

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
             [{<<"Is-Authorized">>, j5_request:is_authorized(Request)}
             ,{<<"Account-ID">>, j5_request:account_id(Request)}
             ,{<<"Account-Billing">>, j5_request:account_billing(Request)}
             ,{<<"Reseller-ID">>, j5_request:reseller_id(Request)}
             ,{<<"Reseller-Billing">>, j5_request:reseller_billing(Request)}
             ,{<<"Call-Direction">>, j5_request:call_direction(Request)}
             ,{<<"Other-Leg-Call-ID">>, j5_request:other_leg_call_id(Request)}
             ,{<<"Soft-Limit">>, j5_request:soft_limit(Request)}
             ,{<<"Msg-ID">>, j5_request:message_id(Request)}
             ,{<<"Call-ID">>, j5_request:call_id(Request)}
             ,{<<"Custom-Channel-Vars">>, CCVs}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_authz:publish_authz_resp(ServerId, Resp),
    j5_util:maybe_send_system_alert(Request),
    kapi_authz:broadcast_authz_resp(Resp).

-spec trunk_usage(kz_term:ne_binary()) -> kz_term:ne_binary().
trunk_usage(<<Id/binary>>) ->
    Limits = j5_limits:get(Id),
    <<(kz_term:to_binary(j5_limits:inbound_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_limits:outbound_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_limits:twoway_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_limits:burst_trunks(Limits)))/binary, "/"
     ,(kz_term:to_binary(j5_channels:inbound_flat_rate(Id)))/binary, "/"
     ,(kz_term:to_binary(j5_channels:outbound_flat_rate(Id)))/binary
    >>.
