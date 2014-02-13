%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-export([billing/2
         ,account_billing/1
         ,reseller_billing/1
        ]).
-export([call_direction/1]).
-export([call_id/1]).
-export([from/1]).
-export([to/1]).
-export([answered_time/1]).
-export([billing_seconds/1]).
-export([timestamp/1]).
-export([message_id/1]).
-export([server_id/1]).
-export([number/1]).
-export([per_minute_cost/1]).
-export([call_cost/1]).

-include_lib("jonny5.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_jobj(wh_json:object()) -> j5_request().
from_jobj(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    #request{account_id = wh_json:get_ne_value(<<"Account-ID">>, CCVs)
             ,account_billing = wh_json:get_ne_value(<<"Account-Billing">>, CCVs, <<"limits_enforced">>)
             ,reseller_id = wh_json:get_ne_value(<<"Reseller-ID">>, CCVs)
             ,reseller_billing = wh_json:get_ne_value(<<"Reseller-Billing">>, CCVs, <<"limits_enforced">>)
             ,call_id = wh_json:get_ne_value(<<"Call-ID">>, JObj)
             ,call_direction = wh_json:get_value(<<"Call-Direction">>, JObj)
             ,sip_to = wh_json:get_ne_value(<<"To">>, JObj)
             ,sip_from = wh_json:get_ne_value(<<"From">>, JObj)
             ,sip_request = wh_json:get_value(<<"Request">>, JObj)
             ,message_id = wh_json:get_value(<<"Msg-ID">>, JObj)
             ,server_id = wh_json:get_value(<<"Server-ID">>, JObj)
             ,billing_seconds = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj, 0)
             ,answered_time = wh_json:get_integer_value(<<"Answered-Seconds">>, JObj, 0)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(ne_binary(), j5_request(), j5_limits()) -> j5_request().
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

-spec authorize_account(ne_binary(), j5_request()) -> j5_request().
authorize_account(Reason, #request{account_id=AccountId}=Request) ->
    lager:debug("account ~s authorized channel: ~s"
                ,[AccountId, Reason]),
    Request#request{account_billing=Reason
                    ,account_authorized='true'}.

-spec authorize_reseller(ne_binary(), j5_request()) -> j5_request().
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
-spec deny(ne_binary(), j5_request(), j5_limits()) -> j5_request().
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

-spec deny_account(ne_binary(), j5_request()) -> j5_request().
deny_account(Reason, #request{account_id=AccountId}=Request) ->
    lager:debug("account ~s denied channel: ~s"
                ,[AccountId, Reason]),
    Request#request{account_billing=Reason
                    ,account_authorized='false'}.

-spec deny_reseller(ne_binary(), j5_request()) -> j5_request().
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
-spec is_authorized(j5_request()) -> boolean().
is_authorized(#request{account_id=AccountId
                       ,account_authorized=Authorized
                       ,reseller_id=AccountId}) -> Authorized;
is_authorized(#request{account_authorized=AccountAuthorized
                      ,reseller_authorized=ResellerAuthorized}) ->
    AccountAuthorized andalso ResellerAuthorized.

-spec is_authorized(j5_request(), j5_limits()) -> boolean().
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
-spec set_account_id(api_binary(), j5_request()) -> j5_request().
set_account_id(AccountId, Request) -> Request#request{account_id=AccountId}.

-spec account_id(j5_request()) -> api_binary().
account_id(#request{account_id=AccountId}) -> AccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_reseller_id(api_binary(), j5_request()) -> j5_request().
set_reseller_id(ResellerId, Request) ->
    Request#request{reseller_id=ResellerId}.

-spec reseller_id(j5_request()) -> api_binary().
reseller_id(#request{reseller_id=ResellerId}) -> ResellerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec billing(j5_request(), j5_limits()) -> api_binary().
billing(#request{account_billing=AccountBilling
                 ,reseller_id=ResellerId
                 ,reseller_billing=ResellerBilling}
        ,Limits) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' -> ResellerBilling;
        'false' -> AccountBilling
    end.

-spec account_billing(j5_request()) -> api_binary().
account_billing(#request{account_billing=Billing}) -> Billing.

-spec reseller_billing(j5_request()) -> api_binary().
reseller_billing(#request{reseller_billing=Billing}) -> Billing.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp(j5_request()) -> api_binary().
timestamp(#request{timestamp=Timestamp}) -> Timestamp.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec answered_time(j5_request()) -> api_binary().
answered_time(#request{answered_time=AnsweredTime}) ->
    AnsweredTime.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec billing_seconds(j5_request()) -> api_binary().
billing_seconds(#request{billing_seconds=BillingSeconds}) ->
    BillingSeconds.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_direction(j5_request()) -> api_binary().
call_direction(#request{call_direction=CallDirection}) ->
    CallDirection.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_id(j5_request()) -> api_binary().
call_id(#request{call_id=CallId}) -> CallId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from(j5_request()) -> api_binary().
from(#request{sip_from=From}) -> From.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to(j5_request()) -> api_binary().
to(#request{sip_to=To}) -> To.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec message_id(j5_request()) -> api_binary().
message_id(#request{message_id=MessageId}) -> MessageId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec server_id(j5_request()) -> api_binary().
server_id(#request{server_id=ServerId}) -> ServerId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec number(j5_request()) -> ne_binary().
number(#request{sip_request=SIPRequest}) ->
    [Number|_] = binary:split(SIPRequest, <<"@">>),
    Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec per_minute_cost(j5_request()) -> non_neg_integer().
per_minute_cost(_Request) -> 0.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec call_cost(j5_request()) -> non_neg_integer().
call_cost(_Request) -> 0.
