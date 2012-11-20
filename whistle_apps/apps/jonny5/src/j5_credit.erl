%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_credit).

-export([is_available/2]).
-export([reauthorize/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec is_available/2 :: (#limits{}, wh_json:json_object()) -> boolean().
is_available(Limits, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Balance = j5_util:current_balance(AccountId),
    prepay_is_available(Limits, Balance, JObj) 
        orelse postpay_is_available(Limits, Balance, JObj).

-spec reauthorize/2 :: (#limits{}, wh_json:json_object()) -> 'ok'.
reauthorize(Limits, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),    
    RateIncr = wh_json:get_integer_value([<<"Custom-Channel-Vars">>, <<"Rate-Increment">>]
                                         ,JObj, DefaultRateIncr),
    case (Timestamp - Answered) < RateIncr of
        true ->
            lager:debug("call has not exceeded the rate increment yet", []),
            j5_reauthz_req:send_allow_resp(JObj);
        false -> 
            maybe_debit_next_minute(AccountId, Limits, JObj)
    end.

-spec reconcile_cdr/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
reconcile_cdr(Account, JObj) ->
    Cost = extract_cost(JObj),
    SessionId = j5_util:get_session_id(JObj),
    Units = session_cost(SessionId, Account) - (-1 * wapi_money:dollars_to_units(Cost)),
    case end_per_minute(Units, j5_util:get_limits(Account), JObj) of
        {ok, _} -> 
            %% two side-effects 1) the view is rebuilt 2) the current balance is logged
            j5_util:current_balance(Account),
            ok;
        {error, conflict} -> ok;
        {error, _R} ->
            lager:debug("unable to update ledger ~s: ~p", [Account, _R]),
            reconcile_cdr(Account, JObj)
    end,
    ok.
    
-spec prepay_is_available/3 :: (#limits{}, integer(), wh_json:json_object()) -> boolean().
prepay_is_available(#limits{allow_prepay=false}, _, _) ->
    false;
prepay_is_available(#limits{allow_prepay=true, reserve_amount=ReserveAmount}=Limits, Balance, JObj) ->
    case (Balance - ReserveAmount) > 0 of
        false -> false;             
        true ->
            start_per_minute(ReserveAmount, Limits, JObj),
            true
    end.

-spec postpay_is_available/3 :: (#limits{}, integer(), wh_json:json_object()) -> boolean().
postpay_is_available(#limits{allow_postpay=false}, _, _) ->
    false;
postpay_is_available(#limits{allow_postpay=true, max_postpay_amount=MaxPostpay
                             ,reserve_amount=ReserveAmount}=Limits, Balance, JObj) ->
    case (Balance - ReserveAmount) > MaxPostpay of
        false -> false;             
        true -> 
            start_per_minute(ReserveAmount, Limits, JObj),
            true
    end.

-spec maybe_debit_next_minute/3 :: (ne_binary(), #limits{}, wh_json:json_object()) -> boolean().
maybe_debit_next_minute(Account, Limits, JObj) ->
    case debit_next_minute(Account, Limits, JObj) of
        true -> j5_reauthz_req:send_allow_resp(JObj);
        false -> j5_reauthz_req:send_deny_resp(JObj)
    end.

-spec debit_next_minute/3 :: (ne_binary(), #limits{}, wh_json:json_object()) -> boolean().
debit_next_minute(Account, Limits, JObj) ->
    Debit = extract_cost(wh_json:set_value(<<"Billing-Seconds">>, 60, JObj)),
    case is_credit_still_available(j5_util:current_balance(Account) - Debit, Limits) of
        false -> 
            lager:debug("account does not have the required credit to continue this call", []),
            false;
        true ->
            lager:debug("account has the required credit to continue this call for another minute", []),
            tick_per_minute(wapi_money:dollars_to_units(Debit), j5_util:get_limits(Account), JObj),
            true
    end.

-spec is_credit_still_available/2 :: (integer(), #limits{}) -> boolean().
is_credit_still_available(Balance, #limits{allow_prepay=true}) when Balance > 0 ->
    true;
is_credit_still_available(Balance, #limits{allow_postpay=true, max_postpay_amount=MaxPostpay}) ->
    Balance > MaxPostpay;
is_credit_still_available(_, _) ->
    false.

-spec extract_cost/1 :: (wh_json:json_object()) -> float().
extract_cost(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - wh_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    %% if we transition from allotment to per_minute the offset has a slight 
    %% fudge factor to allow accounts with no credit to terminate the call
    %% on the next re-authorization cycle (to allow for the in-flight time)
    case BillingSecs =< 0 of
        true -> 0;
        false ->
            DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),
            Rate = wh_json:get_float_value(<<"Rate">>, CCVs, 0.0),
            RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, DefaultRateIncr),
            RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
            Surcharge = wh_json:get_float_value(<<"Surcharge">>, CCVs, 0.0),
            Cost = whapps_util:calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
            SessionId = j5_util:get_session_id(JObj),
            Discount = (wh_json:get_integer_value(<<"Discount-Percentage">>, CCVs, 0) * 0.01) * Cost,
            lager:debug("session ~s, rate $~p/~ps, minumim ~ps, surcharge $~p, for ~ps, sub total $~p, discount $~p, total $~p"
                        ,[SessionId, Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost, Discount, (Cost - Discount)]),
            Cost - Discount
    end.

-spec start_per_minute/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
start_per_minute(Units, Limits, JObj) ->
    Props = [{<<"reason">>, <<"per_minute_channel">>}
             ,{<<"pvt_type">>, <<"debit">>}
            ], 
    j5_util:write_to_ledger(<<"start">>, Props, Units, Limits, JObj).

-spec tick_per_minute/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
tick_per_minute(Units, Limits, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp()),
    Props = [{<<"reason">>, <<"per_minute_channel">>}
             ,{<<"pvt_type">>, <<"debit">>}
            ], 
    j5_util:write_to_ledger(wh_util:to_binary(Timestamp), Props, Units, Limits, JObj).

-spec end_per_minute/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_jobj_return().
end_per_minute(Units, Limits, JObj) when Units > 0 ->
    Props = [{<<"reason">>, <<"per_minute_channel">>}
             ,{<<"pvt_type">>, <<"debit">>}
            ], 
    j5_util:write_to_ledger(<<"end">>, Props, Units, Limits, JObj);
end_per_minute(Units, Limits, JObj) when Units < 0 ->
    Props = [{<<"reason">>, <<"per_minute_channel">>}
             ,{<<"pvt_type">>, <<"credit">>}
            ], 
    j5_util:write_to_ledger(<<"end">>, Props, Units, Limits, JObj);
end_per_minute(_, _, _) ->
    {ok, wh_json:new()}.

-spec session_cost/2 :: (ne_binary(), ne_binary()) -> integer().
session_cost(SessionId, Ledger) ->
    LedgerDb = wh_util:format_account_id(Ledger, encoded),    
    ViewOptions = [reduce
                   ,group
                   ,{<<"key">>, SessionId}
                  ],
    case couch_mgr:get_results(LedgerDb, <<"transactions/session_cost">>, ViewOptions) of
        {ok, []} -> 0;
        {ok, [ViewRes|_]} -> wh_json:get_integer_value(<<"value">>, ViewRes, 0);
        {error, _R} -> 
            lager:debug("unable to get session cost for ~s: ~p", [SessionId, _R]),
            0
    end.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

postpay_limits() ->
    #limits{allow_postpay=true
            ,allow_prepay=false
            ,max_postpay_amount=-10000
            ,reserve_amount=5000
           }.

prepay_limits() ->
    #limits{allow_postpay=false
            ,allow_prepay=true
            ,max_postpay_amount=-10000
            ,reserve_amount=5000
           }.

request_jobj() ->
    Props = [{<<"Account-ID">>, <<"000000000000000000000000000000000">>}
             ,{<<"Call-ID">>, <<"000000000000000000000000000000000">>}
            ],
    wh_json:from_list(Props).

is_available_test() ->
    put(j5_test_balance, 0),
    ?assertEqual(false, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(true, is_available(postpay_limits(), request_jobj())),
    put(j5_test_balance, 10000),
    ?assertEqual(true, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(true, is_available(postpay_limits(), request_jobj())),
    put(j5_test_balance, -1000),
    ?assertEqual(false, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(true, is_available(postpay_limits(), request_jobj())),
    put(j5_test_balance, -10000),
    ?assertEqual(false, is_available(prepay_limits(), request_jobj())),
    ?assertEqual(false, is_available(postpay_limits(), request_jobj())),
    ok.

prepay_is_available_test() ->
    ?assertEqual(false, prepay_is_available(prepay_limits(), 0, request_jobj())),
    ?assertEqual(true, prepay_is_available(prepay_limits(), 10000, request_jobj())),
    ?assertEqual(false, prepay_is_available(prepay_limits(), -1000, request_jobj())),
    ?assertEqual(false, prepay_is_available(postpay_limits(), 0, request_jobj())),
    ok.

postpay_is_available_test() ->
    ?assertEqual(true, postpay_is_available(postpay_limits(), 0, request_jobj())),
    ?assertEqual(true, postpay_is_available(postpay_limits(), 10000, request_jobj())),
    ?assertEqual(false, postpay_is_available(postpay_limits(), -10000, request_jobj())),
    ?assertEqual(false, postpay_is_available(prepay_limits(), 0, request_jobj())),
    ok.

-endif.
