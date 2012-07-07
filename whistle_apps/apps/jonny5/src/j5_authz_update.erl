%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_update).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_authz:update_v(JObj),
    wh_util:put_callid(JObj),
    timer:sleep(crypto:rand_uniform(0, 1000)),
    CCV = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_value(<<"Account-Billing">>, CCV) of
        <<"per_minute">> -> reconcile(wh_json:get_value(<<"Account-ID">>, CCV), CCV, JObj);
        _ -> ok
    end,
    case wh_json:get_value(<<"Reseller-Billing">>, CCV) of
        <<"per_minute">> -> reconcile(wh_json:get_value(<<"Reseller-ID">>, CCV), CCV, JObj);
        _ -> ok
    end.
 
-spec reconcile/3 :: (ne_binary(), wh_json:json_object(), wh_json:json_object()) -> 'ok'.
reconcile(Ledger, CCV, JObj) ->
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),    
    DefaultRate = whapps_config:get_float(<<"jonny5">>, <<"default_rate">>, ?DEFAULT_RATE),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),        
    Rate = wh_json:get_float_value(<<"Rate">>, CCV, DefaultRate),
    RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCV, DefaultRateIncr),
    case (Timestamp - Answered) of
        Time when Time < RateIncr ->
            lager:debug("call has not exceeded the rate increment yet", []);
        Time ->
            Debit = whapps_util:calculate_cost(Rate, RateIncr, 0, 0.0, Time + 60)
                - whapps_util:calculate_cost(Rate, RateIncr, 0, 0.0, Time),
            case j5_util:write_debit_to_ledger(Timestamp, wapi_money:dollars_to_units(Debit), JObj, Ledger) of
                {error, conflict} -> ok;
                {ok, _} ->
                    lager:debug("debited $~w from ~s for 60 seconds of talk time at $~w/~ws", [Debit, Ledger, Rate, RateIncr])
            end
    end.    
