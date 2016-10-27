%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_per_minute).

-export([authorize/2]).
-export([reconcile_cdr/2]).
-export([maybe_credit_available/2]).

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
maybe_credit_available(Amount, Limits) ->
    AccountId = j5_limits:account_id(Limits),
    Balance = wht_util:current_balance(AccountId),
    PerMinuteCost = j5_channels:per_minute_cost(AccountId),
    Routines = [fun maybe_prepay_credit_available/3
               ,fun maybe_postpay_credit_available/3
               ,fun maybe_promised_payment_available/3
               ],
    lager:debug("account ~s current balance: $~w, reserve amount: ~w, per_minute calls: ~w",
                [AccountId
                ,wht_util:units_to_dollars(Balance)
                ,wht_util:units_to_dollars(Amount)
                ,wht_util:units_to_dollars(PerMinuteCost)
                ]),
    {Result, ResultBalance} = lists:foldl(fun(F, Acc) -> F(AccountId, Acc, Limits) end
                                         ,{'false', Balance - Amount - PerMinuteCost}
                                         ,Routines
                                         ),
    lager:debug("account ~s result balance: $~w, enough credit: ~p",
                [AccountId
                ,wht_util:units_to_dollars(ResultBalance)
                ,Result
                ]),
    Result.

-spec maybe_prepay_credit_available(api_binary(), {boolean(), integer()}, j5_limits:limits()) -> {boolean(), integer()}.
maybe_prepay_credit_available(AccountId, {R, Balance}, Limits) ->
    Dbg = [AccountId
          ,wht_util:units_to_dollars(Balance)
          ],
    case j5_limits:allow_prepay(Limits) of
        'false' ->
            lager:debug("account ~s is restricted from using prepay", [AccountId]),
            {R, Balance};
        'true' when Balance > 0 ->
            lager:debug("using prepay from account ~s $~w", Dbg),
            {'true', Balance};
        'true' ->
            lager:debug("account ~s does not have enough prepay credit $~w", Dbg),
            {'false', Balance}
    end.

-spec maybe_postpay_credit_available(integer(), {boolean(), integer()}, j5_limits:limits()) -> {boolean(), integer()}.
maybe_postpay_credit_available(AccountId, {R, Balance}, Limits) ->
    MaxPostpay = j5_limits:max_postpay(Limits),
    NewBalance = Balance + MaxPostpay,
    Dbg = [AccountId
          ,wht_util:units_to_dollars(Balance)
          ,wht_util:units_to_dollars(MaxPostpay)
          ,wht_util:units_to_dollars(NewBalance)
          ],
    case j5_limits:allow_postpay(Limits) of
        'false' ->
            lager:debug("account ~s is restricted from using postpay"
                       ,[AccountId]
                       ),
            {R, Balance};
        'true' when NewBalance > 0 ->
            lager:debug("using postpay from account ~s $~w + $~w = $~w", Dbg),
            {'true', NewBalance};
        'true' ->
            lager:debug("account ~s would exceed the maxium postpay amount: $~w + $~w = $~w", Dbg),
            {'false', NewBalance}
    end.

-spec maybe_promised_payment_available(api_binary(), {boolean(), integer()}, j5_limits:limits()) -> {boolean(), integer()}.
maybe_promised_payment_available(AccountId, {R, Balance}, Limits) ->
    JObj = j5_limits:promised_payment(Limits),
    case kz_json:is_true(<<"enabled">>, JObj, 'false') of
        'false' ->
            lager:debug("account ~s doesn't allow to use promised payment",[AccountId]),
            {R, Balance};
        'true' ->
            IsArmed = kz_json:is_true(<<"armed">>, JObj, 'false'),
            Now = kz_util:current_tstamp(),
            Start = kz_json:get_integer_value(<<"start">>, JObj, 0),
            Duration = kz_json:get_integer_value(<<"duration">>, JObj, 0),
            Amount = wht_util:dollars_to_units(kz_json:get_float_value(<<"amount">>, JObj, 0.0)),
            NewBalance = Balance + Amount,
            Dbg = [AccountId
                  ,wht_util:units_to_dollars(Balance)
                  ,wht_util:units_to_dollars(Amount)
                  ,wht_util:units_to_dollars(NewBalance)
                  ],
            case IsArmed
                andalso
                Now > Start
                andalso
                Now < Start + Duration
            of
                'false' when IsArmed ->
                    lager:debug("account ~s has promised payment but can't use it: start ~p, stop ~p, current timestamp ~p, amount $~w"
                               ,[AccountId
                                ,Start
                                ,Start + Duration
                                ,Now
                                ,wht_util:units_to_dollars(Amount)
                                ]),
                    _ = try_disarm_promised_payment(AccountId, Balance),
                    {R, Balance};
                'false' ->
                    lager:debug("account ~s doesn't have armed promised payment",[AccountId]),
                    {R, Balance};
                'true' when NewBalance > 0 ->
                    lager:debug("using promised payment from account ~s $~w + $~w = $~w", Dbg),
                    {'true', NewBalance};
                'true' ->
                    lager:debug("account ~s exceeds the promised payment amount: $~w + $~w = $~w", Dbg),
                    {'false', NewBalance}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec try_disarm_promised_payment(api_binary(), integer()) -> 'ok'.
try_disarm_promised_payment(AccountId, Balance) when Balance > 0 ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    UpdateProps = [{[<<"pvt_promised_payment">>, <<"armed">>], 'false'}],
    case kz_datamgr:update_doc(AccountDb, <<"limits">>, UpdateProps) of
        {'ok', _JObj} ->
            lager:debug("disarming promised payment for account ~s", [AccountId]);
        {'error', E} ->
            lager:error("while trying to disarm promised payment for account ~s got error: ~p", [AccountId, E])
    end;
try_disarm_promised_payment(_AccountId, _Balance) -> 'ok'.

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
