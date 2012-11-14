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
    CCV = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_value(<<"Account-Billing">>, CCV) of
        <<"per_minute">> -> 
            AccountId = wh_json:get_value(<<"Account-ID">>, CCV),
            reconcil_per_minute(AccountId, JObj);
        _ -> ok 
    end,
    case wh_json:get_value(<<"Reseller-Billing">>, CCV) of
        <<"per_minute">> -> 
            ResellerId = wh_json:get_value(<<"Reseller-ID">>, CCV),
            reconcil_per_minute(ResellerId, JObj);
        _ -> ok
    end.

-spec reconcil_per_minute/2 :: (api_binary(), wh_json:json_object()) -> 'ok'.
reconcil_per_minute(undefined, _) -> ok;
reconcil_per_minute(Account, JObj) ->
    Cost = extract_cost(JObj),
    SessionId = j5_util:get_session_id(JObj),
    Billed = j5_util:session_cost(SessionId, Account),
    Units = Billed - (-1 * wapi_money:dollars_to_units(Cost)),
    case j5_util:end_per_minute(Units, j5_util:get_limits(Account), JObj) of
        {ok, _} -> 
            %% two side-effects 1) the view is rebuilt 2) the current balance is logged
            j5_util:current_balance(Account),
            ok;
        {error, conflict} -> ok;
        {error, _R} ->
            lager:debug("unable to update ledger ~s: ~p", [Account, _R]),
            reconcil_per_minute(Account, JObj)
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
    SessionId = j5_util:get_session_id(JObj),
    lager:debug("final session ~s rate at $~p/~ps with minumim ~ps and surcharge $~p for ~p secs: $~p"
                ,[SessionId, Rate, RateIncr, RateMin, Surcharge, BillingSecs, Cost]),
    Cost.
