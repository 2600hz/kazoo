%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(j5_per_minute).

-export([authorize/2]).
-export([reconcile_cdr/2]).
-export([maybe_credit_available/2
        ,maybe_credit_available/3
        ]).

-include("jonny5.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(j5_request:request(), j5_limits:limits()) -> j5_request:request().
authorize(Request, Limits) ->
    lager:debug("checking if account ~s has available per-minute credit"
               ,[j5_limits:account_id(Limits)]
               ),
    ReserveUnits = j5_limits:reserve_amount(Limits),
    case maybe_credit_available(ReserveUnits, Limits) of
        'false' -> Request;
        'true' -> j5_request:authorize(<<"per_minute">>, Request, Limits)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_credit_available(kz_currency:units(), j5_limits:limits()) -> boolean().
maybe_credit_available(ReserveUnits, Limits) -> maybe_credit_available(ReserveUnits, Limits, 'false').

-spec maybe_credit_available(kz_currency:units(), j5_limits:limits(), boolean()) -> boolean().
maybe_credit_available(ReserveUnits, Limits, IsReal) ->
    AccountId = j5_limits:account_id(Limits),
    AvailableUnits = kz_currency:available_units(AccountId, 0),
    PerMinuteCost = case kz_term:is_true(IsReal) of
                        'true' -> j5_channels:real_per_minute_cost(AccountId);
                        'false' -> j5_channels:per_minute_cost(AccountId)
                    end,
    maybe_prepay_credit_available(AvailableUnits - PerMinuteCost, ReserveUnits, Limits)
        orelse maybe_postpay_credit_available(AvailableUnits - PerMinuteCost, ReserveUnits, Limits).

-spec maybe_prepay_credit_available(kz_currency:units(), kz_currency:units(), j5_limits:limits()) -> boolean().
maybe_prepay_credit_available(AvailableUnits, ReserveUnits, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    Dbg = [AccountId
          ,kz_currency:units_to_dollars(ReserveUnits)
          ,kz_currency:units_to_dollars(AvailableUnits)
          ],
    case j5_limits:allow_prepay(Limits) of
        'false' ->
            lager:debug("account ~s is restricted from using prepay", [AccountId]),
            'false';
        'true' when (AvailableUnits - ReserveUnits) > 0 ->
            lager:debug("using prepay from account ~s $~w/$~w", Dbg),
            'true';
        'true' ->
            lager:debug("account ~s does not have enough prepay credit $~w/$~w", Dbg),
            'false'
    end.

-spec maybe_postpay_credit_available(kz_currency:units(), kz_currency:units(), j5_limits:limits()) -> boolean().
maybe_postpay_credit_available(AvailableUnits, ReserveUnits, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    MaxPostpay = j5_limits:max_postpay(Limits),
    case j5_limits:allow_postpay(Limits) of
        'false' ->
            lager:debug("account ~s is restricted from using postpay"
                       ,[AccountId]
                       ),
            'false';
        'true' when (AvailableUnits - ReserveUnits) > MaxPostpay ->
            lager:debug("using postpay from account ~s $~w/$~w"
                       ,[AccountId
                        ,kz_currency:units_to_dollars(ReserveUnits)
                        ,kz_currency:units_to_dollars(AvailableUnits)
                        ]
                       ),
            'true';
        'true' ->
            lager:debug("account ~s would exceed the maximum postpay amount $~w/$~w"
                       ,[AccountId
                        ,kz_currency:units_to_dollars(AvailableUnits)
                        ,kz_currency:units_to_dollars(MaxPostpay)
                        ]
                       ),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_ledger_usage(kz_currency:units(), kz_currency:units(), j5_request:request(), j5_limits:limits()) -> any().
create_ledger_usage(Seconds, Amount, Request, Limits) ->
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, j5_request:account_id(Request)}
          ,{fun kz_ledger:set_source_service/2, <<"per-minute-voip">>}
          ,{fun kz_ledger:set_source_id/2, j5_request:call_id(Request)}
          ,{fun kz_ledger:set_description/2, j5_request:rate_name(Request)}
          ,{fun kz_ledger:set_usage_type/2, <<"voice">>}
          ,{fun kz_ledger:set_usage_quantity/2, Seconds}
          ,{fun kz_ledger:set_usage_unit/2, <<"sec">>}
          ,{fun kz_ledger:set_period_start/2, j5_request:timestamp(Request)}
          ,{fun kz_ledger:set_metadata/2, metadata(Request)}
          ,{fun kz_ledger:set_unit_amount/2, Amount}
          ]
         ),
    kz_ledger:debit(kz_ledger:setters(Setters), j5_limits:account_id(Limits)).

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
      ,{<<"caller_id_name">>, j5_request:caller_id_name(Request)}
      ,{<<"callee_id_number">>, j5_request:callee_id_number(Request)}
      ,{<<"callee_id_name">>, j5_request:callee_id_name(Request)}
      ,{<<"resource_type">>, j5_request:resource_type(Request)}
      ,{<<"account_trunk_usage">>, j5_request:account_trunk_usage(Request)}
      ,{<<"reseller_trunk_usage">>, j5_request:reseller_trunk_usage(Request)}
      ,{<<"rate">>, RateObj}
      ,{<<"billing_seconds">>, j5_request:billing_seconds(Request)}
      ]).
