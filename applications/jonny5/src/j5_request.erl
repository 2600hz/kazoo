%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
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
-export([call_cost/1]).
-export([ccvs/1]).
-export([caller_network_address/1]).

-include("jonny5.hrl").

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
                  ,node :: api_binary()
                  ,classification :: api_binary()
                  ,number :: api_binary()
                  ,billing_seconds = 0 :: non_neg_integer()
                  ,answered_time = 0 :: non_neg_integer()
                  ,timestamp = 0 :: gregorian_seconds()
                  ,request_jobj = kz_json:new() :: kz_json:object()
                  ,request_ccvs = kz_json:new() :: kz_json:object()
                 }).
-opaque request() :: #request{}.
-export_type([request/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_jobj(kz_json:object()) -> request().
from_jobj(JObj) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),

    Request = kz_json:get_value(<<"Request">>, JObj),
    [Num|_] = binary:split(Request, <<"@">>),
    Number = request_number(Num, CCVs),

    #request{account_id = kz_json:get_ne_value(<<"Account-ID">>, CCVs)
             ,account_billing = kz_json:get_ne_value(<<"Account-Billing">>, CCVs, <<"limits_enforced">>)
             ,reseller_id = kz_json:get_ne_value(<<"Reseller-ID">>, CCVs)
             ,reseller_billing = kz_json:get_ne_value(<<"Reseller-Billing">>, CCVs, <<"limits_enforced">>)
             ,call_id = kz_json:get_ne_value(<<"Call-ID">>, JObj)
             ,call_direction = kz_json:get_value(<<"Call-Direction">>, JObj)
             ,other_leg_call_id = kz_json:get_value(<<"Other-Leg-Call-ID">>, JObj)
             ,sip_to = kz_json:get_ne_value(<<"To">>, JObj)
             ,sip_from = kz_json:get_ne_value(<<"From">>, JObj)
             ,sip_request = Request
             ,message_id = kz_api:msg_id(JObj)
             ,server_id = kz_api:server_id(JObj)
             ,node = kz_api:node(JObj)
             ,billing_seconds = kz_json:get_integer_value(<<"Billing-Seconds">>, JObj, 0)
             ,answered_time = kz_json:get_integer_value(<<"Answered-Seconds">>, JObj, 0)
             ,timestamp = kz_json:get_integer_value(<<"Timestamp">>, JObj, kz_time:current_tstamp())
             ,classification = knm_converters:classify(Number)
             ,number = Number
             ,request_jobj = JObj
             ,request_ccvs = CCVs
            }.

-spec from_ccvs(request(), kz_proplist()) -> request().
from_ccvs(#request{request_ccvs=ReqCCVs
                   ,request_jobj=ReqJObj
                  }=Request, CCVs) ->
    NewCCVs = kz_json:set_values(CCVs, ReqCCVs),

    Request#request{account_id=props:get_value(<<"Account-ID">>, CCVs)
                    ,request_ccvs=NewCCVs
                    ,request_jobj=kz_json:set_value(<<"Custom-Channel-Vars">>, NewCCVs, ReqJObj)
                   }.


-spec request_number(ne_binary(), kz_json:object()) -> ne_binary().
request_number(Number, CCVs) ->
    case kz_json:get_first_defined([<<"E164-Destination">>
                                    ,<<"Original-Number">>
                                   ], CCVs
                                  ) of
        'undefined' -> Number;
        Original ->
            lager:debug("using original number ~s instead of ~s", [Original, Number]),
            Original
    end.

-spec ccvs(request()) -> kz_json:object().
ccvs(#request{request_ccvs=CCVs}) -> CCVs.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_jobj(request()) -> kz_json:object().
to_jobj(Request) ->
    Props =
        props:filter_undefined(
          [{<<"account_id">>, account_id(Request)}
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
         ),
    kz_json:from_list(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(ne_binary(), request(), j5_limits:limits()) -> request().
authorize(Reason
          ,#request{reseller_id=AccountId
                    ,account_id=AccountId
                   }=Request
          ,_Limits) ->
    authorized_by_account(Reason, Request);
authorize(Reason
          ,#request{reseller_id=ResellerId}=Request
          ,Limits) ->
    case j5_limits:account_id(Limits) of
        ResellerId ->
            authorized_by_reseller(Reason, Request);
        _Id ->
            authorized_by_account(Reason, Request)
    end.

-spec authorized_by_reseller(ne_binary(), request()) -> request().
authorized_by_reseller(Reason, #request{reseller_id=_ResellerId}=Request) ->
    lager:debug("reseller ~s authorized channel: ~s"
                ,[_ResellerId, Reason]
               ),
    Request#request{reseller_billing=Reason
                    ,reseller_authorized='true'
                   }.

-spec authorized_by_account(ne_binary(), request()) -> request().
authorized_by_account(Reason, #request{account_id=_AccountId}=Request) ->
    lager:debug("account ~s authorized channel: ~s"
                ,[_AccountId, Reason]
               ),
    Request#request{account_billing=Reason
                    ,account_authorized='true'
                   }.

-spec authorize_account(ne_binary(), request()) -> request().
authorize_account(Reason, Request) ->
    authorized_by_account(Reason, Request).

-spec authorize_reseller(ne_binary(), request()) -> request().
authorize_reseller(Reason, Request) ->
    authorized_by_reseller(Reason, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec deny(ne_binary(), request(), j5_limits:limits()) -> request().
deny(Reason
     ,#request{reseller_id=ResellerId
               ,account_id=AccountId
              }=Request
     ,Limits) ->
    case j5_limits:account_id(Limits) =:= ResellerId of
        'true' ->
            lager:debug("reseller ~s denied channel: ~s"
                        ,[ResellerId, Reason]
                       ),
            Request#request{reseller_billing=Reason
                            ,reseller_authorized='false'
                           };
        'false' ->
            lager:debug("account ~s denied channel: ~s"
                        ,[AccountId, Reason]
                       ),
            Request#request{account_billing=Reason
                            ,account_authorized='false'
                           }
    end.

-spec deny_account(ne_binary(), request()) -> request().
deny_account(Reason, #request{account_id=AccountId}=Request) ->
    lager:debug("account ~s denied channel: ~s"
                ,[AccountId, Reason]
               ),
    Request#request{account_billing=Reason
                    ,account_authorized='false'
                   }.

-spec deny_reseller(ne_binary(), request()) -> request().
deny_reseller(Reason, #request{reseller_id=ResellerId}=Request) ->
    lager:debug("reseller ~s denied channel: ~s"
                ,[ResellerId, Reason]
               ),
    Request#request{reseller_billing=Reason
                    ,reseller_authorized='false'
                   }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(request()) -> boolean().
is_authorized(#request{account_id=AccountId
                       ,account_authorized=Authorized
                       ,reseller_id=AccountId
                      }) -> Authorized;
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
set_account_id(AccountId, Request) ->
    Request#request{account_id=AccountId}.

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
                 ,reseller_billing=ResellerBilling
                }
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
-spec node(request()) -> api_binary().
node(#request{node=NodeId}) -> NodeId.

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

-spec caller_network_address(request()) -> api_binary().
caller_network_address(#request{request_jobj=JObj}) ->
    kz_json:get_value(<<"From-Network-Addr">>, JObj).
