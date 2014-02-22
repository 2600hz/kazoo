%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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
-export([from_jobj/1]).

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
-export([classification/1]).
-export([number/1]).
-export([per_minute_cost/1]).
-export([call_cost/1]).

-include_lib("jonny5.hrl").

-record(request, {account_id :: api_binary()
                  ,account_billing :: api_binary()
                  ,account_authorized = 'false' :: boolean()
                  ,reseller_id :: api_binary()
                  ,reseller_billing :: api_binary()
                  ,reseller_authorized = 'false' :: boolean()
                  ,soft_limit = 'false' :: boolean()
                  ,call_id :: api_binary()
                  ,call_direction :: api_binary()
                  ,other_leg_call_id :: api_binary()
                  ,sip_to :: api_binary()
                  ,sip_from :: api_binary()
                  ,sip_request :: api_binary()
                  ,message_id :: api_binary()
                  ,server_id :: api_binary()
                  ,classification :: api_binary()
                  ,number :: api_binary()
                  ,billing_seconds = 0 :: non_neg_integer()
                  ,answered_time = 0 :: non_neg_integer()
                  ,timestamp = 0 :: non_neg_integer()
                  ,request_jobj = wh_json:new() :: wh_json:object()
                 }).
-opaque request() :: #request{}.
-export_type([request/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_jobj(wh_json:object()) -> request().
from_jobj(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    Request = wh_json:get_value(<<"Request">>, JObj),
    [Number|_] = binary:split(Request, <<"@">>),
    #request{account_id = wh_json:get_ne_value(<<"Account-ID">>, CCVs)
             ,account_billing = wh_json:get_ne_value(<<"Account-Billing">>, CCVs, <<"limits_enforced">>)
             ,reseller_id = wh_json:get_ne_value(<<"Reseller-ID">>, CCVs)
             ,reseller_billing = wh_json:get_ne_value(<<"Reseller-Billing">>, CCVs, <<"limits_enforced">>)
             ,call_id = wh_json:get_ne_value(<<"Call-ID">>, JObj)
             ,call_direction = wh_json:get_value(<<"Call-Direction">>, JObj)
             ,other_leg_call_id = wh_json:get_value(<<"Other-Leg-Call-ID">>, JObj)
             ,sip_to = wh_json:get_ne_value(<<"To">>, JObj)
             ,sip_from = wh_json:get_ne_value(<<"From">>, JObj)
             ,sip_request = Request
             ,message_id = wh_json:get_value(<<"Msg-ID">>, JObj)
             ,server_id = wh_json:get_value(<<"Server-ID">>, JObj)
             ,billing_seconds = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj, 0)
             ,answered_time = wh_json:get_integer_value(<<"Answered-Seconds">>, JObj, 0)
             ,timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp())
             ,classification = wnm_util:classify_number(Number)
             ,number = Number
             ,request_jobj = JObj}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(ne_binary(), request(), j5_limits:limits()) -> request().
authorize(Reason, #request{reseller_id=ResellerId
                           ,account_id=AccountId}=Request
          ,Limits) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' ->
            lager:debug("reseller ~s authorized channel: ~s"
                        ,[ResellerId, Reason]),
            Request#request{reseller_billing=Reason
                            ,reseller_authorized='true'};
        'false' ->
            lager:debug("account ~s authorized channel: ~s"
                        ,[AccountId, Reason]),
            Request#request{account_billing=Reason
                            ,account_authorized='true'}
    end.

-spec authorize_account(ne_binary(), request()) -> request().
authorize_account(Reason, #request{account_id=AccountId}=Request) ->
    lager:debug("account ~s authorized channel: ~s"
                ,[AccountId, Reason]),
    Request#request{account_billing=Reason
                    ,account_authorized='true'}.

-spec authorize_reseller(ne_binary(), request()) -> request().
authorize_reseller(Reason, #request{reseller_id=ResellerId}=Request) ->
    lager:debug("reseller ~s authorized channel: ~s"
                ,[ResellerId, Reason]),
    Request#request{reseller_billing=Reason
                    ,reseller_authorized='true'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec deny(ne_binary(), request(), j5_limits:limits()) -> request().
deny(Reason, #request{reseller_id=ResellerId
                     ,account_id=AccountId}=Request
     ,Limits) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' ->
            lager:debug("reseller ~s denied channel: ~s"
                        ,[ResellerId, Reason]),
            Request#request{reseller_billing=Reason
                            ,reseller_authorized='false'};
        'false' ->
            lager:debug("account ~s denied channel: ~s"
                        ,[AccountId, Reason]),
            Request#request{account_billing=Reason
                            ,account_authorized='false'}
    end.

-spec deny_account(ne_binary(), request()) -> request().
deny_account(Reason, #request{account_id=AccountId}=Request) ->
    lager:debug("account ~s denied channel: ~s"
                ,[AccountId, Reason]),
    Request#request{account_billing=Reason
                    ,account_authorized='false'}.

-spec deny_reseller(ne_binary(), request()) -> request().
deny_reseller(Reason, #request{reseller_id=ResellerId}=Request) ->
    lager:debug("reseller ~s denied channel: ~s"
                ,[ResellerId, Reason]),
    Request#request{reseller_billing=Reason
                    ,reseller_authorized='false'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(request()) -> boolean().
is_authorized(#request{account_id=AccountId
                       ,account_authorized=Authorized
                       ,reseller_id=AccountId}) -> Authorized;
is_authorized(#request{account_authorized=AccountAuthorized
                      ,reseller_authorized=ResellerAuthorized}) ->
    AccountAuthorized andalso ResellerAuthorized.

-spec is_authorized(request(), j5_limits:limits()) -> boolean().
is_authorized(#request{account_authorized=AccountAuthorized
                       ,reseller_id=ResellerId
                       ,reseller_authorized=ResellerAuthorized}
              ,Limits) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' -> ResellerAuthorized;
        'false' -> AccountAuthorized
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_account_id(api_binary(), request()) -> request().
set_account_id(AccountId, Request) -> Request#request{account_id=AccountId}.

-spec account_id(request()) -> api_binary().
account_id(#request{account_id=AccountId}) -> AccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id(api_binary(), request()) -> request().
set_reseller_id(ResellerId, Request) ->
    Request#request{reseller_id=ResellerId}.

-spec reseller_id(request()) -> api_binary().
reseller_id(#request{reseller_id=ResellerId}) -> ResellerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec billing(request(), j5_limits:limits()) -> api_binary().
billing(#request{account_billing=AccountBilling
                 ,reseller_id=ResellerId
                 ,reseller_billing=ResellerBilling}
        ,Limits) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' -> ResellerBilling;
        'false' -> AccountBilling
    end.

-spec account_billing(request()) -> api_binary().
account_billing(#request{account_billing=Billing}) -> Billing.

-spec reseller_billing(request()) -> api_binary().
reseller_billing(#request{reseller_billing=Billing}) -> Billing.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_soft_limit(request()) -> request().
set_soft_limit(Request) ->
    Request#request{soft_limit='true'}.

-spec clear_soft_limit(request()) -> request().
clear_soft_limit(Request) ->
    Request#request{soft_limit='false'}.

-spec soft_limit(request()) -> boolean().
soft_limit(#request{soft_limit=SoftLimit}) -> SoftLimit.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec other_leg_call_id(request()) -> api_binary().
other_leg_call_id(#request{other_leg_call_id=CallId}) -> CallId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp(request()) -> non_neg_integer().
timestamp(#request{timestamp=Timestamp}) -> Timestamp.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec answered_time(request()) -> non_neg_integer() | 'undefined'.
answered_time(#request{answered_time=AnsweredTime}) ->
    AnsweredTime.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec billing_seconds(request()) -> non_neg_integer() | 'undefined'.
billing_seconds(#request{billing_seconds=BillingSeconds}) ->
    BillingSeconds.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_direction(request()) -> api_binary().
call_direction(#request{call_direction=CallDirection}) ->
    CallDirection.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_id(request()) -> api_binary().
call_id(#request{call_id=CallId}) -> CallId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from(request()) -> api_binary().
from(#request{sip_from=From}) -> From.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to(request()) -> api_binary().
to(#request{sip_to=To}) -> To.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec message_id(request()) -> api_binary().
message_id(#request{message_id=MessageId}) -> MessageId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec server_id(request()) -> api_binary().
server_id(#request{server_id=ServerId}) -> ServerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec classification(request()) -> api_binary().
classification(#request{classification=Classification}) -> Classification.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec number(request()) -> ne_binary().
number(#request{number=Number}) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec per_minute_cost(request()) -> non_neg_integer().
per_minute_cost(#request{request_jobj=JObj}) ->
    wht_util:per_minute_cost(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_cost(request()) -> non_neg_integer().
call_cost(#request{request_jobj=JObj}) ->
    wht_util:call_cost(JObj).
