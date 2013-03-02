%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wht_util).

-export([dollars_to_units/1]).
-export([units_to_dollars/1]).
-export([call_cost/1]).
-export([per_minute_cost/1]).
-export([calculate_cost/5]).

%% tracked in hundred-ths of a cent
-define(DOLLAR_TO_UNIT, 10000).

-include_lib("whistle/include/wh_types.hrl").

-spec dollars_to_units/1 :: (float() | integer()) -> integer().
dollars_to_units(Dollars) when is_number(Dollars) ->
    round(Dollars * ?DOLLAR_TO_UNIT).

-spec units_to_dollars/1 :: (number()) -> float().
units_to_dollars(Units) when is_number(Units) ->
    trunc(Units) / ?DOLLAR_TO_UNIT.

call_cost(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - wh_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    %% if we transition from allotment to per_minute the offset has a slight
    %% fudge factor to allow accounts with no credit to terminate the call
    %% on the next re-authorization cycle (to allow for the in-flight time)
    case BillingSecs =< 0 of
        true -> 0;
        false ->
            Rate = wh_json:get_integer_value(<<"Rate">>, CCVs, 0),
            RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            RateMin = wh_json:get_integer_value(<<"Rate-Minimum">>, CCVs, 0),
            Surcharge = wh_json:get_integer_value(<<"Surcharge">>, CCVs, 0),
            Cost = calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
            Discount = (wh_json:get_integer_value(<<"Discount-Percentage">>, CCVs, 0) * 0.01) * Cost,
            lager:warning("rate $~p/~ps, minumim ~ps, surcharge $~p, for ~ps, sub total $~p, discount $~p, total $~p"
                        ,[units_to_dollars(Rate)
                          ,RateIncr, RateMin
                          ,units_to_dollars(Surcharge)
                          ,BillingSecs
                          ,units_to_dollars(Cost)
                          ,units_to_dollars(Discount)
                          ,units_to_dollars(Cost - Discount)
                         ]),
            trunc(Cost - Discount)
    end.

per_minute_cost(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    BillingSecs = wh_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - wh_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    case BillingSecs =< 0 of
        true -> 0;
        false ->
            RateIncr = wh_json:get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            case wh_json:get_integer_value(<<"Rate">>, CCVs, 0) of
                0 -> 0;
                Rate ->
                    trunc((RateIncr / 60) * Rate)
            end
    end.

%% R :: rate, per minute, in dollars (0.01, 1 cent per minute)
%% RI :: rate increment, in seconds, bill in this increment AFTER rate minimum is taken from Secs
%% RM :: rate minimum, in seconds, minimum number of seconds to bill for
%% Sur :: surcharge, in dollars, (0.05, 5 cents to connect the call)
%% Secs :: billable seconds
-spec calculate_cost(integer() | integer(), integer(), integer(), integer(), integer()) -> float().
calculate_cost(_, _, _, _, 0) -> 0;
calculate_cost(R, 0, RM, Sur, Secs) ->
    calculate_cost(R, 60, RM, Sur, Secs);
calculate_cost(R, RI, RM, Sur, Secs) ->
    case Secs =< RM of
        true ->
            trunc(Sur + ((RM / 60) * R));
        false ->
            trunc(Sur + ((RM / 60) * R) + (wh_util:ceiling((Secs - RM) / RI) * ((RI / 60) * R)))
    end.
