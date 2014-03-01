%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _) ->
    'true' = wapi_authz:authz_req_v(JObj),
    wh_util:put_callid(JObj),
    maybe_determine_account_id(j5_request:from_jobj(JObj)).

-spec maybe_determine_account_id(j5_request:request()) -> 'ok'.
maybe_determine_account_id(Request) ->
    case j5_request:account_id(Request) of
        'undefined' -> determine_account_id(Request);
        _Else -> maybe_account_limited(Request)
    end.

-spec determine_account_id(j5_request:request()) -> 'ok'.
determine_account_id(Request) ->
    Number = j5_request:number(Request),
    case wh_number_manager:lookup_account_by_number(Number) of
        {'ok', AccountId, Props} ->
            maybe_local_resource(AccountId, Props, Request);
        {'error', {'account_disabled', AccountId}} ->
            lager:debug("account ~s is disabled", [AccountId]),
            R = j5_request:set_account_id(AccountId, Request),
            send_response(
              j5_request:deny_account(<<"disabled">>, R)
             );
        {'error', _R} ->
            lager:debug("unable to determine account id for ~s: ~p", [Number, _R]),
            'ok'
    end.

-spec maybe_local_resource(ne_binary(), wh_proplist(), j5_request:request()) -> 'ok'.
maybe_local_resource(AccountId, Props, Request) ->
    %% TODO: we need to check system_config to determine if we authz local
    case props:get_value('local', Props) of
        'false' ->
            maybe_account_limited(
              j5_request:set_account_id(AccountId, Request)
             );
        'true' ->
            Number = j5_request:number(Request),
            lager:debug("number ~s is a local number for account ~s, allowing"
                       ,[Number, AccountId]),
            Routines = [fun(R) -> j5_request:authorize_account(<<"limits_disabled">>, R) end
                       ,fun(R) -> j5_request:authorize_reseller(<<"limits_disabled">>, R) end
                       ,fun(R) -> j5_request:set_account_id(AccountId, R) end
                       ,fun(R) ->
                                ResellerId = wh_services:find_reseller_id(AccountId),
                                j5_request:set_reseller_id(ResellerId, R)
                        end
                       ],
            send_response(lists:foldl(fun(F, R) -> F(R) end, Request, Routines))
    end.

-spec maybe_account_limited(j5_request:request()) -> 'ok'.
maybe_account_limited(Request) ->
    AccountId = j5_request:account_id(Request),
    Limits = j5_limits:get(AccountId),
    R = maybe_authorize(Request, Limits),
    case j5_request:is_authorized(R, Limits) of
        'false' ->
            lager:debug("account ~s is not authorized to create this channel"
                        ,[AccountId]),
            send_response(R);
        'true' -> maybe_determine_reseller_id(R)
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
    ResellerId = wh_services:find_reseller_id(AccountId),
    maybe_reseller_limited(
      j5_request:set_reseller_id(ResellerId, Request)
     ).

-spec maybe_reseller_limited(j5_request:request()) -> 'ok'.
maybe_reseller_limited(Request) ->
    ResellerId = j5_request:reseller_id(Request),
    case j5_request:account_id(Request) =:= ResellerId of
        'true' ->
            lager:debug("channel belongs to reseller, ignoring reseller billing", []),
            send_response(
              j5_request:authorize_reseller(<<"limits_disabled">>, Request)
             );
        'false' ->
            Limits = j5_limits:get(ResellerId),
            R = maybe_authorize(Request, Limits),
            case j5_request:is_authorized(R, Limits) of
                'false' ->
                    lager:debug("reseller ~s is not authorized to create this channel"
                                ,[ResellerId]),
                    send_response(R);
                'true' -> send_response(R)
            end
    end.

-spec maybe_authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_authorize(Request, Limits) ->
    case j5_limits:enabled(Limits) of
        'true' -> maybe_authorize_exception(Request, Limits);
        'false' ->
            lager:debug("limits are disabled for account ~s"
                        ,[j5_limits:account_id(Limits)]),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits)
    end.

-spec maybe_authorize_exception(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_authorize_exception(Request, Limits) ->
    CallDirection = j5_request:call_direction(Request),
    case j5_request:classification(Request) of
        <<"emergency">> ->
            lager:debug("allowing emergency call", []),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        <<"tollfree_us">> when CallDirection =:= <<"outbound">> ->
            lager:debug("allowing outbound tollfree call", []),
            j5_request:authorize(<<"limits_disabled">>, Request, Limits);
        _Else -> maybe_hard_limit(Request, Limits)
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
    maybe_soft_limit(
      lists:foldl(fun(F, R) ->
                          case j5_request:is_authorized(R, Limits) of
                              'false' -> F(R, Limits);
                              'true' -> R
                          end
                  end, Request, Routines)
      ,Limits).

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
            lager:debug("outbound channel authorization is not enforced (soft limit)", []),
            j5_request:set_soft_limit(Request)
    end.

-spec maybe_inbound_soft_limit(j5_request:request(), j5_limits:limits()) -> j5_request:request().
maybe_inbound_soft_limit(Request, Limits) ->
    case j5_limits:soft_limit_inbound(Limits) of
        'false' -> Request;
        'true' ->
            lager:debug("inbound channel authorization is not enforced (soft limit)", []),
            j5_request:set_soft_limit(Request)
    end.

-spec send_response(j5_request:request()) -> 'ok'.
send_response(Request) ->
    ServerId = j5_request:server_id(Request),
    Resp = props:filter_undefined(
             [{<<"Is-Authorized">>, wh_util:to_binary(j5_request:is_authorized(Request))}
              ,{<<"Account-ID">>, j5_request:account_id(Request)}
              ,{<<"Account-Billing">>, j5_request:account_billing(Request)}
              ,{<<"Reseller-ID">>, j5_request:reseller_id(Request)}
              ,{<<"Reseller-Billing">>, j5_request:reseller_billing(Request)}
              ,{<<"Call-Direction">>, j5_request:call_direction(Request)}
              ,{<<"Other-Leg-Call-ID">>, j5_request:other_leg_call_id(Request)}
              ,{<<"Soft-Limit">>, wh_util:to_binary(j5_request:soft_limit(Request))}
              ,{<<"Msg-ID">>, j5_request:message_id(Request)}
              ,{<<"Call-ID">>, j5_request:call_id(Request)}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_authz:publish_authz_resp(ServerId, Resp),
    case j5_request:is_authorized(Request) of
        'true' ->
            wapi_authz:broadcast_authz_resp(Resp),
            j5_channels:authorized(wh_json:from_list(Resp));
        'false' -> j5_util:send_system_alert(Request)
    end.

