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
-module(j5_request).

-export([authorize/3
        ,authorize_account/2
        ,authorize_reseller/2
        ]).
-export([deny/3
        ,deny_account/2
        ,deny_reseller/2
        ]).
-export([is_authorized/1
        ,is_authorized/2
        ]).
-export([from_jobj/1
        ,from_ccvs/2
        ,to_jobj/1
        ]).

-export([set_account_id/2
        ,account_id/1
        ]).
-export([set_reseller_id/2
        ,reseller_id/1
        ]).
-export([set_soft_limit/1
        ,clear_soft_limit/1
        ,soft_limit/1
        ]).
-export([billing/2
        ,account_billing/1
        ,reseller_billing/1
        ]).
-export([call_direction/1]).
-export([call_id/1]).
-export([other_leg_call_id/1]).
-export([from/1]).
-export([to/1]).
-export([answered_time/1]).
-export([billing_seconds/1]).
-export([timestamp/1]).
-export([message_id/1]).
-export([server_id/1]).
-export([node/1]).
-export([classification/1]).
-export([number/1]).
-export([per_minute_cost/1]).
-export([call_cost/1, calculate_call/1]).
-export([ccvs/1]).
-export([caller_network_address/1]).
-export([rate/1
        ,rate_name/1
        ,rate_description/1
        ,rate_increment/1
        ,rate_minimum/1
        ,rate_nocharge_time/1
        ]).
-export([caller_id_number/1
        ,caller_id_name/1
        ]).
-export([callee_id_number/1
        ,callee_id_name/1
        ]).
-export([resource_type/1]).
-export([resource_id/1]).
-export([account_trunk_usage/1]).
-export([reseller_trunk_usage/1]).

-include("jonny5.hrl").

-record(request, {account_id :: kz_term:api_binary()
                 ,account_billing :: kz_term:api_binary()
                 ,account_authorized = 'false' :: boolean()
                 ,reseller_id :: kz_term:api_binary()
                 ,reseller_billing :: kz_term:api_binary()
                 ,reseller_authorized = 'false' :: boolean()
                 ,soft_limit = 'false' :: boolean()
                 ,call_id :: kz_term:api_binary()
                 ,call_direction :: kz_term:api_binary()
                 ,other_leg_call_id :: kz_term:api_binary()
                 ,sip_to :: kz_term:api_binary()
                 ,sip_from :: kz_term:api_binary()
                 ,sip_request :: kz_term:api_binary()
                 ,message_id :: kz_term:api_binary()
                 ,server_id :: kz_term:api_binary()
                 ,node :: kz_term:api_binary()
                 ,classification :: kz_term:api_binary()
                 ,number :: kz_term:api_binary()
                 ,billing_seconds = 0 :: non_neg_integer()
                 ,answered_time = 0 :: non_neg_integer()
                 ,timestamp = 0 :: kz_time:gregorian_seconds()
                 ,request_jobj :: kapi_authz:req()
                 ,request_ccvs = kz_json:new() :: kz_json:object()
                 }).
-opaque request() :: #request{}.
-export_type([request/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_jobj(kapi_authz:req()) -> request().
from_jobj(AuthzReq) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, AuthzReq, kz_json:new()),

    Request = kz_json:get_ne_binary_value(<<"Request">>, AuthzReq),
    [Num|_] = binary:split(Request, <<"@">>),
    Number = request_number(Num, CCVs),

    AccountId = kz_json:get_ne_binary_value(<<"Account-ID">>, CCVs),

    #request{account_id = AccountId
            ,account_billing = ccv_account_billing(CCVs)
            ,reseller_id = reseller_id(AccountId, CCVs)
            ,reseller_billing = ccv_reseller_billing(CCVs)
            ,call_id = kz_api:call_id(AuthzReq)
            ,call_direction = authz_call_direction(AuthzReq)
            ,other_leg_call_id = authz_other_call_leg(AuthzReq)
            ,sip_to = authz_to(AuthzReq)
            ,sip_from = authz_from(AuthzReq)
            ,sip_request = Request
            ,message_id = kz_api:msg_id(AuthzReq)
            ,server_id = kz_api:server_id(AuthzReq)
            ,node = kz_api:node(AuthzReq)
            ,billing_seconds = authz_billing_seconds(AuthzReq)
            ,answered_time = authz_answered_time(AuthzReq)
            ,timestamp = authz_timestamp(AuthzReq)
            ,classification = knm_converters:classify(Number)
            ,number = Number
            ,request_jobj = AuthzReq
            ,request_ccvs = CCVs
            }.

authz_timestamp(AuthzReq) ->
    case kz_json:get_integer_value(<<"Timestamp">>, AuthzReq) of
        'undefined' -> kz_time:now_s();
        TS -> TS
    end.

-spec authz_answered_time(kapi_authz:req()) -> non_neg_integer().
authz_answered_time(AuthzReq) ->
    kz_json:get_integer_value(<<"Answered-Seconds">>, AuthzReq, 0).

-spec authz_billing_seconds(kapi_authz:req()) -> non_neg_integer().
authz_billing_seconds(AuthzReq) ->
    kz_json:get_integer_value(<<"Billing-Seconds">>, AuthzReq, 0).

-spec authz_from(kapi_authz:req()) -> kz_term:api_ne_binary().
authz_from(AuthzReq) ->
    kz_json:get_ne_binary_value(<<"From">>, AuthzReq).

-spec authz_to(kapi_authz:req()) -> kz_term:api_ne_binary().
authz_to(AuthzReq) ->
    kz_json:get_first_defined([<<"To-URI">>, <<"To">>], AuthzReq).

-spec authz_call_direction(kapi_authz:req()) -> kz_term:api_ne_binary().
authz_call_direction(AuthzReq) ->
    kz_json:get_ne_binary_value(<<"Call-Direction">>, AuthzReq).

-spec authz_other_call_leg(kapi_authz:req()) -> kz_term:api_ne_binary().
authz_other_call_leg(AuthzReq) ->
    kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, AuthzReq).

-spec ccv_account_billing(kapi_authz:req()) -> kz_term:api_ne_binary().
ccv_account_billing(CCVs) ->
    kz_json:get_ne_binary_value(<<"Account-Billing">>, CCVs, <<"limits_enforced">>).

-spec ccv_reseller_billing(kapi_authz:req()) -> kz_term:api_ne_binary().
ccv_reseller_billing(CCVs) ->
    kz_json:get_ne_binary_value(<<"Reseller-Billing">>, CCVs, <<"limits_enforced">>).

-spec reseller_id(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_ne_binary().
reseller_id(<<AccountId/binary>>, CCVs) ->
    case kz_json:get_ne_binary_value(<<"Reseller-ID">>, CCVs) of
        'undefined' ->
            lager:debug("failed to find reseller on CCVs, checking account doc"),
            kzd_accounts:reseller_id(AccountId);
        ResellerId -> ResellerId
    end.

-spec from_ccvs(request(), kz_term:proplist()) -> request().
from_ccvs(#request{request_ccvs=ReqCCVs
                  ,request_jobj=ReqJObj
                  }=Request, CCVs) ->
    NewCCVs = kz_json:set_values(CCVs, ReqCCVs),

    Request#request{account_id=props:get_value(<<"Account-ID">>, CCVs)
                   ,request_ccvs=NewCCVs
                   ,request_jobj=kz_json:set_value(<<"Custom-Channel-Vars">>, NewCCVs, ReqJObj)
                   }.

-spec request_number(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
request_number(Number, CCVs) ->
    case kz_json:get_first_defined([<<"E164-Destination">>
                                   ,<<"Original-Number">>
                                   ]
                                  ,CCVs
                                  )
    of
        'undefined' -> Number;
        Original ->
            lager:debug("using original number ~s instead of ~s", [Original, Number]),
            Original
    end.

-spec ccvs(request()) -> kz_json:object().
ccvs(#request{request_ccvs=CCVs}) -> CCVs.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_jobj(request()) -> kz_json:object().
to_jobj(Request) ->
    kz_json:from_list([{<<"account_id">>, account_id(Request)}
                      ,{<<"reseller_id">>, reseller_id(Request)}
                      ,{<<"call_direction">>, call_direction(Request)}
                      ,{<<"call_id">>, call_id(Request)}
                      ,{<<"other_leg_call_id">>, other_leg_call_id(Request)}
                      ,{<<"answered_time">>, answered_time(Request)}
                      ,{<<"billing_seconds">>, billing_seconds(Request)}
                      ,{<<"from">>, from(Request)}
                      ,{<<"to">>, to(Request)}
                      ,{<<"number">>, ?MODULE:number(Request)}
                      ,{<<"classification">>, classification(Request)}
                      ]
                     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(kz_term:ne_binary(), request(), j5_limits:limits()) -> request().
authorize(Reason
         ,#request{reseller_id=AccountId
                  ,account_id=AccountId
                  }=Request
         ,_Limits
         ) ->
    authorized_by_account(Reason, Request);
authorize(Reason
         ,#request{reseller_id=ResellerId}=Request
         ,Limits
         ) ->
    case j5_limits:account_id(Limits) of
        ResellerId ->
            authorized_by_reseller(Reason, Request);
        _Id ->
            authorized_by_account(Reason, Request)
    end.

-spec authorized_by_reseller(kz_term:ne_binary(), request()) -> request().
authorized_by_reseller(Reason, #request{reseller_id=_ResellerId}=Request) ->
    lager:debug("reseller ~s authorized channel: ~s", [_ResellerId, Reason]),

    Request#request{reseller_billing=Reason
                   ,reseller_authorized='true'
                   }.

-spec authorized_by_account(kz_term:ne_binary(), request()) -> request().
authorized_by_account(Reason, #request{account_id=_AccountId}=Request) ->
    lager:debug("account ~s authorized channel: ~s", [_AccountId, Reason]),

    Request#request{account_billing=Reason
                   ,account_authorized='true'
                   }.

-spec authorize_account(kz_term:ne_binary(), request()) -> request().
authorize_account(Reason, Request) ->
    authorized_by_account(Reason, Request).

-spec authorize_reseller(kz_term:ne_binary(), request()) -> request().
authorize_reseller(Reason, Request) ->
    authorized_by_reseller(Reason, Request).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec deny(kz_term:ne_binary(), request(), j5_limits:limits()) -> request().
deny(Reason
    ,#request{reseller_id=ResellerId
             ,account_id=AccountId
             }=Request
    ,Limits
    ) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' ->
            lager:debug("reseller ~s denied channel: ~s", [ResellerId, Reason]),
            Request#request{reseller_billing=Reason
                           ,reseller_authorized='false'
                           };
        'false' ->
            lager:debug("account ~s denied channel: ~s", [AccountId, Reason]),
            Request#request{account_billing=Reason
                           ,account_authorized='false'
                           }
    end.

-spec deny_account(kz_term:ne_binary(), request()) -> request().
deny_account(Reason, #request{account_id=AccountId}=Request) ->
    lager:debug("account ~s denied channel: ~s", [AccountId, Reason]),
    Request#request{account_billing=Reason
                   ,account_authorized='false'
                   }.

-spec deny_reseller(kz_term:ne_binary(), request()) -> request().
deny_reseller(Reason, #request{reseller_id=ResellerId}=Request) ->
    lager:debug("reseller ~s denied channel: ~s", [ResellerId, Reason]),
    Request#request{reseller_billing=Reason
                   ,reseller_authorized='false'
                   }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_authorized(request()) -> boolean().
is_authorized(#request{account_id=AccountId
                      ,account_authorized=Authorized
                      ,reseller_id=AccountId
                      }) ->
    Authorized;
is_authorized(#request{account_authorized=AccountAuthorized
                      ,reseller_authorized=ResellerAuthorized
                      }) ->
    AccountAuthorized
        andalso ResellerAuthorized.

-spec is_authorized(request(), j5_limits:limits()) -> boolean().
is_authorized(#request{account_authorized=AccountAuthorized
                      ,reseller_id=ResellerId
                      ,reseller_authorized=ResellerAuthorized
                      }
             ,Limits
             ) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' -> ResellerAuthorized;
        'false' -> AccountAuthorized
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(kz_term:api_binary(), request()) -> request().
set_account_id(AccountId, Request) ->
    Request#request{account_id=AccountId}.

-spec account_id(request()) -> kz_term:api_binary().
account_id(#request{account_id=AccountId}) -> AccountId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_reseller_id(kz_term:api_binary(), request()) -> request().
set_reseller_id(ResellerId, Request) ->
    Request#request{reseller_id=ResellerId}.

-spec reseller_id(request()) -> kz_term:api_binary().
reseller_id(#request{reseller_id=ResellerId}) -> ResellerId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billing(request(), j5_limits:limits()) -> kz_term:api_binary().
billing(#request{account_billing=AccountBilling
                ,reseller_id=ResellerId
                ,reseller_billing=ResellerBilling
                }
       ,Limits
       ) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' -> ResellerBilling;
        'false' -> AccountBilling
    end.

-spec account_billing(request()) -> kz_term:api_binary().
account_billing(#request{account_billing=Billing}) -> Billing.

-spec reseller_billing(request()) -> kz_term:api_binary().
reseller_billing(#request{reseller_billing=Billing}) -> Billing.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_soft_limit(request()) -> request().
set_soft_limit(Request) ->
    Request#request{soft_limit='true'}.

-spec clear_soft_limit(request()) -> request().
clear_soft_limit(Request) ->
    Request#request{soft_limit='false'}.

-spec soft_limit(request()) -> boolean().
soft_limit(#request{soft_limit=SoftLimit}) -> SoftLimit.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec other_leg_call_id(request()) -> kz_term:api_binary().
other_leg_call_id(#request{other_leg_call_id=CallId}) -> CallId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec timestamp(request()) -> non_neg_integer().
timestamp(#request{timestamp=Timestamp}) -> Timestamp.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec answered_time(request()) -> non_neg_integer() | 'undefined'.
answered_time(#request{answered_time=AnsweredTime}) ->
    AnsweredTime.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billing_seconds(request()) -> non_neg_integer() | 'undefined'.
billing_seconds(#request{billing_seconds=BillingSeconds}) ->
    BillingSeconds.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call_direction(request()) -> kz_term:api_binary().
call_direction(#request{call_direction=CallDirection}) ->
    CallDirection.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call_id(request()) -> kz_term:api_binary().
call_id(#request{call_id=CallId}) -> CallId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from(request()) -> kz_term:api_binary().
from(#request{sip_from=From}) -> From.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to(request()) -> kz_term:api_binary().
to(#request{sip_to=To}) -> To.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec message_id(request()) -> kz_term:api_binary().
message_id(#request{message_id=MessageId}) -> MessageId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec server_id(request()) -> kz_term:api_binary().
server_id(#request{server_id=ServerId}) -> ServerId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec node(request()) -> kz_term:api_binary().
node(#request{node=NodeId}) -> NodeId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec classification(request()) -> kz_term:api_binary().
classification(#request{classification=Classification}) -> Classification.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec number(request()) -> kz_term:ne_binary().
number(#request{number=Number}) -> Number.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec per_minute_cost(request()) -> non_neg_integer().
per_minute_cost(#request{request_jobj=AuthzReq}) ->
    kapps_call_util:per_minute_cost(AuthzReq).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call_cost(request()) -> non_neg_integer().
call_cost(#request{request_jobj=AuthzReq}) ->
    kapps_call_util:call_cost(AuthzReq).

-spec calculate_call(request()) -> {non_neg_integer(), non_neg_integer()}.
calculate_call(#request{request_jobj=AuthzReq}) ->
    kapps_call_util:calculate_call(AuthzReq).

-spec caller_network_address(request()) -> kz_term:api_binary().
caller_network_address(#request{request_jobj=AuthzReq}) ->
    kz_json:get_value(<<"From-Network-Addr">>, AuthzReq).

-spec rate(request()) -> integer().
rate(#request{request_ccvs=CCVs}) ->
    kz_json:get_integer_value(<<"Rate">>, CCVs).

-spec rate_name(request()) -> kz_term:ne_binary().
rate_name(#request{request_ccvs=CCVs}) ->
    kz_json:get_ne_binary_value(<<"Rate-Name">>, CCVs).

-spec rate_description(request()) -> kz_term:ne_binary().
rate_description(#request{request_ccvs=CCVs}) ->
    kz_json:get_ne_binary_value(<<"Rate-Description">>, CCVs).

-spec rate_increment(request()) -> kz_term:ne_binary().
rate_increment(#request{request_ccvs=CCVs}) ->
    kz_json:get_integer_value(<<"Rate-Increment">>, CCVs, 0).

-spec rate_minimum(request()) -> kz_term:ne_binary().
rate_minimum(#request{request_ccvs=CCVs}) ->
    kz_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0).

-spec rate_nocharge_time(request()) -> kz_term:ne_binary().
rate_nocharge_time(#request{request_ccvs=CCVs}) ->
    kz_json:get_integer_value(<<"Rate-NoCharge-Time">>, CCVs, 0).

-spec caller_id_number(request()) -> kz_term:api_binary().
caller_id_number(#request{request_jobj=AuthzReq}) ->
    kz_json:get_value(<<"Caller-ID-Number">>, AuthzReq).

-spec caller_id_name(request()) -> kz_term:api_binary().
caller_id_name(#request{request_jobj=AuthzReq}) ->
    kz_json:get_value(<<"Caller-ID-Name">>, AuthzReq).

-spec callee_id_number(request()) -> kz_term:api_binary().
callee_id_number(#request{request_jobj=AuthzReq}) ->
    kz_json:get_value(<<"Callee-ID-Number">>, AuthzReq).

-spec callee_id_name(request()) -> kz_term:api_binary().
callee_id_name(#request{request_jobj=AuthzReq}) ->
    kz_json:get_value(<<"Callee-ID-Name">>, AuthzReq).

-spec resource_type(request()) -> kz_term:api_binary().
resource_type(#request{request_ccvs=CCVs}) ->
    kz_json:get_value(<<"Resource-Type">>, CCVs).

-spec resource_id(request()) -> kz_term:api_ne_binary().
resource_id(#request{request_ccvs=CCVs}) ->
    kz_json:get_ne_binary_value(<<"Resource-ID">>, CCVs).

-spec account_trunk_usage(request()) -> kz_term:api_binary().
account_trunk_usage(#request{request_ccvs=CCVs}) ->
    kz_json:get_value(<<"Account-Trunk-Usage">>, CCVs).

-spec reseller_trunk_usage(request()) -> kz_term:api_binary().
reseller_trunk_usage(#request{request_ccvs=CCVs}) ->
    kz_json:get_value(<<"Reseller-Trunk-Usage">>, CCVs).
