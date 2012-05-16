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
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Per-Minute">>], JObj) of
        <<"true">> -> reconcile(JObj);
        _Else -> ok
    end.

-spec reconcile/1 :: (wh_json:json_object()) -> 'ok'.
reconcile(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    Answered = wh_json:get_integer_value(<<"Answered-Time">>, JObj),    
    DefaultRate = whapps_config:get_float(<<"jonny5">>, <<"default_rate">>, ?DEFAULT_RATE),
    DefaultRateIncr = whapps_config:get_integer(<<"jonny5">>, <<"default_rate_increment">>, 60),        
    Rate = wh_json:get_float_value(<<"Rate">>, CCVs, DefaultRate),
    RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, DefaultRateIncr),
    case (Timestamp - Answered) of
        Time when Time < RateIncr ->
            lager:debug("call has not exceeded the rate increment yet", []);
        Time ->
            Debit = whapps_util:calculate_cost(Rate, RateIncr, 0, 0.0, Time + 60)
                - whapps_util:calculate_cost(Rate, RateIncr, 0, 0.0, Time),
            j5_util:write_debit_to_ledger(JObj, Debit),
            io:format("debit ~p for ~p + ~p seconds as of talk time...~n", [Debit, Time, RateIncr])
    end.    
