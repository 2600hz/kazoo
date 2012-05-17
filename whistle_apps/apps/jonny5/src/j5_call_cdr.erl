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
    timer:sleep(crypto:rand_uniform(0, 1000)),
    CCV = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_value(<<"Account-Billing">>, CCV) of
        <<"per_minute">> -> reconcile_cost(wh_json:get_value(<<"Account-ID">>, CCV), JObj);
        _ -> ok
    end,
    case wh_json:get_value(<<"Reseller-Billing">>, CCV) of
        <<"per_minute">> -> reconcile_cost(wh_json:get_value(<<"Reseller-ID">>, CCV), JObj);
        _ -> ok
    end.

-spec reconcile_cost/2 :: (ne_binary(), wh_json:json_object()) -> 'ok'.
reconcile_cost(Ledger, JObj) ->
    Cost = extract_cost(JObj),
    SessionId = j5_util:get_session_id(JObj),
    Billed = abs(wapi_money:units_to_dollars(j5_util:session_cost(SessionId, Ledger))),
    Diff = abs(Billed - Cost),
    case Billed > Cost of
        true ->
            case j5_util:write_credit_to_ledger(<<"end">>, Diff, JObj, Ledger) of
                {ok, _} -> lager:debug("bridge cost $~w but we charged $~w, credited account ~s $~w"
                                       ,[Cost, Billed, Ledger, Diff]);
                {error, conflict} -> ok
            end;
        false ->
            case j5_util:write_debit_to_ledger(<<"end">>, Diff, JObj, Ledger) of
                {ok, _} ->
                    lager:debug("bridge cost $~w but we charged $~w, debited account ~s $~w"
                                ,[Cost, Billed, Ledger, Diff]);
                {error, conflict} -> ok
            end
    end.

-spec extract_cost/1 :: (wh_json:json_object()) -> float().
extract_cost(JObj) ->    
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    DefaultRate = whapps_config:get_float(<<"jonny5">>, <<"default_rate">>, ?DEFAULT_RATE),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),
    Rate = wh_json:get_float_value(<<"Rate">>, CCVs, DefaultRate),
    RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, DefaultRateIncr),
    RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
    Surcharge = wh_json:get_float_value(<<"Surcharge">>, CCVs, 0.0),
    Cost = whapps_util:calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
    lager:debug("final call rating at $~p/~ps with minumim ~ps and surcharge $~p for ~p secs: $~p"
                ,[Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    Cost.

-spec bridge_cost/1 :: (wh_json:json_object()) -> float().
bridge_cost(JObj) ->
    BridgeId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Bridge-ID">>], JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    abs(wapi_money:units_to_dollars(j5_util:bridge_cost(BridgeId, AccountId))).
