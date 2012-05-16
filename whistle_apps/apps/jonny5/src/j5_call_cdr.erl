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

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_call:cdr_v(JObj),
    wh_util:put_callid(JObj),
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Per-Minute">>], JObj) of
        <<"true">> -> 
            Cost = extract_cost(JObj),
            io:format("Call should have cost: ~p~n", [Cost]);
        _Else -> ok
    end.

-spec extract_cost/1 :: (wh_json:json_object()) -> integer().
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
    lager:debug("rating call: ~p at incr: ~p with min: ~p and surcharge: ~p for ~p secs: $~p", [Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    Cost.
