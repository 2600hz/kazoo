%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_per_minute).

-export([authorize/2]).
-export([reconcile_cdr/2]).
-export([maybe_credit_available/2, maybe_credit_available/3]).

-include("jonny5.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    lager:debug("checking if account ~s has available per-minute credit"
               ,[j5_limits:account_id(Limits)]
               ),
    Amount = j5_limits:reserve_amount(Limits),
    case maybe_credit_available(Amount, Limits) of
        'false' -> Request;
        'true' -> j5_request:authorize(<<"per_minute">>, Request, Limits)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_cdr(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_cdr(Request, Limits) ->
    case j5_request:billing(Request, Limits) of
        <<"per_minute">> -> reconcile_call_cost(Request, Limits);
        _Else -> 'ok'
    end.

-spec reconcile_call_cost(j5_request:request(), j5_limits:limits()) -> 'ok'.
reconcile_call_cost(Request, Limits) ->
    case j5_request:calculate_call(Request) of
        {_, 0} -> 'ok';
        {Seconds, Amount} ->
            create_ledger_usage(Seconds, Amount, Request, Limits)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_credit_available(integer(), j5_limits:limits()) -> boolean().
maybe_credit_available(Amount, Limits) -> maybe_credit_available(Amount, Limits, 'false').

-spec maybe_credit_available(integer(), j5_limits:limits(), boolean()) -> boolean().
maybe_credit_available(Amount, Limits, IsReal) ->
    AccountId = j5_limits:account_id(Limits),
    Balance = case wht_util:current_balance(AccountId) of
                  {'ok', Bal} -> Bal;
                  {'error', _} -> 0
              end,
    PerMinuteCost = case kz_term:is_true(IsReal) of
                        'true' -> j5_channels:real_per_minute_cost(AccountId);
                        'false' -> j5_channels:per_minute_cost(AccountId)
                    end,
    maybe_prepay_credit_available(Balance - PerMinuteCost, Amount, Limits)
        orelse maybe_postpay_credit_available(Balance - PerMinuteCost, Amount, Limits).

-spec maybe_prepay_credit_available(integer(), integer(), j5_limits:limits()) -> boolean().
maybe_prepay_credit_available(Balance, Amount, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    Dbg = [AccountId
          ,wht_util:units_to_dollars(Amount)
          ,wht_util:units_to_dollars(Balance)
          ],
    case j5_limits:allow_prepay(Limits) of
        'false' ->
            lager:debug("account ~s is restricted from using prepay", [AccountId]),
            'false';
        'true' when (Balance - Amount) > 0 ->
            lager:debug("using prepay from account ~s $~w/$~w", Dbg),
            'true';
        'true' ->
            lager:debug("account ~s does not have enough prepay credit $~w/$~w", Dbg),
            'false'
    end.

-spec maybe_postpay_credit_available(integer(), integer(), j5_limits:limits()) -> boolean().
maybe_postpay_credit_available(Balance, Amount, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    MaxPostpay = j5_limits:max_postpay(Limits),
    case j5_limits:allow_postpay(Limits) of
        'false' ->
            lager:debug("account ~s is restricted from using postpay"
                       ,[AccountId]
                       ),
            'false';
        'true' when (Balance - Amount) > MaxPostpay ->
            lager:debug("using postpay from account ~s $~w/$~w"
                       ,[AccountId
                        ,wht_util:units_to_dollars(Amount)
                        ,wht_util:units_to_dollars(Balance)
                        ]
                       ),
            'true';
        'true' ->
            lager:debug("account ~s would exceed the maxium postpay amount $~w/$~w"
                       ,[AccountId
                        ,wht_util:units_to_dollars(Balance)
                        ,wht_util:units_to_dollars(MaxPostpay)
                        ]
                       ),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_ledger_usage(integer(), integer(), j5_request:request(), j5_limits:limits()) -> any().
create_ledger_usage(Seconds, Amount, Request, Limits) ->
    SrcService = <<"per-minute-voip">>,
    SrcId = j5_request:call_id(Request),
    LedgerId = j5_limits:account_id(Limits),
    AccountId = j5_request:account_id(Request),
    lager:debug("creating debit transaction in ledger ~s / ~s for $~w"
               ,[LedgerId, SrcService, wht_util:units_to_dollars(Amount)]
               ),
    Usage = [{<<"type">>, <<"voice">>}
            ,{<<"quantity">>, Seconds}
            ,{<<"unit">>, <<"sec">>}
            ],

    Extra = [{<<"amount">>, Amount}
            ,{<<"description">>, j5_request:rate_name(Request)}
            ,{<<"period_start">>, j5_request:timestamp(Request)}
            ,{<<"metadata">>, metadata(Request)}
            ],

    kz_ledger:debit(LedgerId, SrcService, SrcId, Usage, Extra, AccountId).

-spec metadata(j5_request:request()) -> kz_json:object().
metadata(Request) ->
    RateObj = kz_json:from_list(
                [{<<"name">>, j5_request:rate_name(Request)}
                ,{<<"description">>, j5_request:rate_description(Request)}
                ,{<<"value">>, j5_request:rate(Request)}
                ,{<<"increment">>, j5_request:rate_increment(Request)}
                ,{<<"minimum">>, j5_request:rate_minimum(Request)}
                ,{<<"nocharge_time">>, j5_request:rate_nocharge_time(Request)}
                ]),
    kz_json:from_list(
      [{<<"to">>, j5_request:to(Request)}
      ,{<<"from">>, j5_request:from(Request)}
      ,{<<"direction">>, j5_request:call_direction(Request)}
      ,{<<"caller_id_number">>, j5_request:caller_id_number(Request)}
      ,{<<"callee_id_number">>, j5_request:callee_id_number(Request)}
      ,{<<"rate">>, RateObj}
      ]).
