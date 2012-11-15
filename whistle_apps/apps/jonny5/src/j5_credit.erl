%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_credit).

-export([is_available/2]).
-export([session_heartbeat/2]).
-export([reconcile_cdr/2]).

-include("jonny5.hrl").

-spec is_available/2 :: (#limits{}, wh_json:json_object()) -> boolean().
is_available(Limits, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Balance = j5_util:current_balance(AccountId),
    case prepay_is_available(Limits, Balance, JObj) of
        false -> postpay_is_available(Limits, Balance, JObj);
        true -> true
    end.             

-spec session_heartbeat/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
session_heartbeat(Account, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),    
    RateIncr = wh_json:get_integer_value([<<"Custom-Channel-Vars">>, <<"Rate-Increment">>]
                                         ,JObj, DefaultRateIncr),
    _ = case (Timestamp - Answered) of
            Time when Time < RateIncr ->
                lager:debug("call has not exceeded the rate increment yet", []);
            Time -> debit_next_minute(Account, Time, JObj)
        end,
    ok.

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
postpay_is_available(#limits{max_postpay_amount=MaxPostpay}=Limits, Balance, JObj) when MaxPostpay > 0 ->
    postpay_is_available(Limits#limits{max_postpay_amount=MaxPostpay*-1}, Balance, JObj);
postpay_is_available(#limits{allow_postpay=true, max_postpay_amount=MaxPostpay
                             ,reserve_amount=ReserveAmount}=Limits, Balance, JObj) ->
    case (Balance - ReserveAmount) > MaxPostpay of
        false -> false;             
        true -> 
            start_per_minute(ReserveAmount, Limits, JObj),
            true
    end.

-spec debit_next_minute/3 :: (ne_binary(), integer(), wh_json:json_object()) -> wh_couch_return().
debit_next_minute(Account, Time, JObj) ->
    DefaultRate = whapps_config:get_float(<<"jonny5">>, <<"default_rate">>, ?DEFAULT_RATE),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),        
    Rate = wh_json:get_float_value([<<"Custom-Channel-Vars">>, <<"Rate">>], JObj, DefaultRate),
    RateIncr = wh_json:get_integer_value([<<"Custom-Channel-Vars">>, <<"Rate-Increment">>]
                                         ,JObj, DefaultRateIncr),
    Debit = whapps_util:calculate_cost(Rate, RateIncr, 0, 0.0, Time + 60)
        - whapps_util:calculate_cost(Rate, RateIncr, 0, 0.0, Time),
    tick_per_minute(wapi_money:dollars_to_units(Debit), j5_util:get_limits(Account), JObj).

-spec extract_cost/1 :: (wh_json:json_object()) -> float().
extract_cost(JObj) ->    
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),
    Rate = wh_json:get_float_value(<<"Rate">>, CCVs, 0.0),
    RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, DefaultRateIncr),
    RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
    Surcharge = wh_json:get_float_value(<<"Surcharge">>, CCVs, 0.0),
    Cost = whapps_util:calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
    SessionId = j5_util:get_session_id(JObj),
    lager:debug("final session ~s rate at $~p/~ps with minumim ~ps and surcharge $~p for ~p secs: $~p"
                ,[SessionId, Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    Cost.

-spec start_per_minute/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_couch_return().
start_per_minute(Units, Limits, JObj) ->
    Props = [{<<"reason">>, <<"per_minute_channel">>}
             ,{<<"pvt_type">>, <<"debit">>}
            ], 
    j5_util:write_to_ledger(<<"start">>, Props, Units, Limits, JObj).

-spec tick_per_minute/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_couch_return().
tick_per_minute(Units, Limits, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, wh_util:current_tstamp()),
    Props = [{<<"reason">>, <<"per_minute_channel">>}
             ,{<<"pvt_type">>, <<"debit">>}
            ], 
    j5_util:write_to_ledger(wh_util:to_binary(Timestamp), Props, Units, Limits, JObj).

-spec end_per_minute/3 :: (float(), #limits{}, wh_json:json_object()) -> wh_couch_return().
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
            ,max_postpay_amount=10000
            ,reserve_amount=5000
           }.

prepay_limits() ->
    #limits{allow_postpay=false
            ,allow_prepay=true
            ,max_postpay_amount=10000
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
