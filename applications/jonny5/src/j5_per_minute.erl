%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
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
    case j5_request:call_cost(Request) of
        0 -> 'ok';
        Amount ->
            create_debit_transaction(<<"end">>, Amount, Request, Limits)
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
    Balance = wht_util:current_balance(AccountId),
    PerMinuteCost = case kz_util:is_true(IsReal) of
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
-spec create_debit_transaction(ne_binary(), integer(), j5_request:request(), j5_limits:limits()) -> any().
create_debit_transaction(Event, Amount, Request, Limits) ->
    LedgerId = j5_limits:account_id(Limits),
    lager:debug("creating debit transaction in ledger ~s for $~w"
                ,[LedgerId, wht_util:units_to_dollars(Amount)]
               ),
    Routines = [fun(T) ->
                        case j5_request:account_id(Request) of
                            LedgerId ->
                                kz_transaction:set_reason(<<"per_minute_call">>, T);
                            AccountId ->
                                T1 = kz_transaction:set_reason(<<"sub_account_per_minute_call">>, T),
                                kz_transaction:set_sub_account_info(AccountId, T1)
                        end
                end
                ,fun(T) -> kz_transaction:set_event(Event, T) end
                ,fun(T) -> kz_transaction:set_call_id(j5_request:call_id(Request), T) end
                ,fun(T) ->  kz_transaction:set_description(<<"per minute call">>, T) end
                ,fun(T) when Event =:= <<"end">> ->
                         kz_transaction:set_metadata(metadata(Request), T);
                    (T) -> T
                 end
               ],
    kz_transaction:save(
      lists:foldl(fun(F, T) -> F(T) end
                  ,kz_transaction:debit(LedgerId, Amount)
                  ,Routines
                 )
     ).

-spec metadata(j5_request:request()) -> kz_json:object().
metadata(Request) ->
    kz_json:from_list(
      [{<<"direction">>, j5_request:call_direction(Request)}
       ,{<<"duration">>, j5_request:billing_seconds(Request)}
       ,{<<"account_id">>, j5_request:account_id(Request)}
       ,{<<"to">>, j5_request:to(Request)}
       ,{<<"from">>, j5_request:from(Request)}
      ]).
