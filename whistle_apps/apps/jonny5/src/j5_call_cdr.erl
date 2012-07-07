%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_call_cdr).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_call:cdr_v(JObj),
    wh_util:put_callid(JObj),
    timer:sleep(crypto:rand_uniform(1000, 3000)),
    Cost = extract_cost(JObj),
    CCV = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_value(<<"Account-Billing">>, CCV) of
        <<"flat_rate">> -> ok;
        _ -> reconcile_cost(wh_json:get_value(<<"Account-ID">>, CCV), Cost, JObj)
    end,
    case wh_json:get_value(<<"Reseller-Billing">>, CCV) of
        <<"flat_rate">> -> ok;
        _ -> reconcile_cost(wh_json:get_value(<<"Reseller-ID">>, CCV), Cost, JObj)
    end.

-spec reconcile_cost/3 :: ('undefined' | ne_binary(), float(), wh_json:json_object()) -> 'ok'.
reconcile_cost(undefined, _, _) -> ok;
reconcile_cost(Ledger, Cost, JObj) ->
    lager:debug("reconciling bridge cost wiht account ~s", [Ledger]),
    SessionId = j5_util:get_session_id(JObj),
    Billed = j5_util:session_cost(SessionId, Ledger),
    case Billed - (-1 * wapi_money:dollars_to_units(Cost)) of
        Diff1 when Diff1 == 0 -> 
            lager:debug("no difference between bridge cost $~w and charges $~w", [abs(wapi_money:units_to_dollars(Diff1))
                                                                                  ,abs(wapi_money:units_to_dollars(Billed))
                                                                                 ]),
            ok;
        Diff2 when Diff2 < 0 ->
            case j5_util:write_credit_to_ledger(<<"end">>, Diff2, JObj, Ledger) of
                {ok, _} -> lager:debug("bridge cost $~w but we charged $~w, credited account $~w"
                                       ,[Cost
                                         ,abs(wapi_money:units_to_dollars(Billed))
                                         ,abs(wapi_money:units_to_dollars(Diff2))
                                        ]);
                {error, conflict} -> ok;
                {error, _R} ->
                    lager:debug("unable to update ledger ~s: ~p", [Ledger, _R]),
                    reconcile_cost(Ledger, Cost, JObj)
            end;
        Diff3 when Diff3 > 0 ->
            case j5_util:write_debit_to_ledger(<<"end">>, Diff3, JObj, Ledger) of
                {ok, _} ->
                    lager:debug("bridge cost $~w but we charged $~w, debited account $~w"
                                ,[Cost
                                  ,abs(wapi_money:units_to_dollars(Billed))
                                  ,abs(wapi_money:units_to_dollars(Diff3))
                                 ]);
                {error, conflict} -> ok;
                {error, _R} ->
                    lager:debug("unable to update ledger ~s: ~p", [Ledger, _R]),
                    reconcile_cost(Ledger, Cost, JObj)
            end
    end.

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
    lager:debug("final call rating at $~p/~ps with minumim ~ps and surcharge $~p for ~p secs: $~p"
                ,[Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    Cost.
