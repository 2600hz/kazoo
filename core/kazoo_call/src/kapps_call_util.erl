%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_call_util).

-include("kapps_call_command.hrl").

-export([base_call_cost/3]).
-export([call_cost/1]).
-export([calculate_call/1, calculate_call/5]).
-export([per_minute_cost/1]).
-export([calculate_cost/5]).
-export([filter_ccvs/1]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec base_call_cost(kz_currency:units() | kz_currency:dollars()
                    ,kz_currency:units() | kz_currency:dollars()
                    ,kz_currency:units() | kz_currency:dollars()) ->
          kz_currency:units().
base_call_cost(RateCost, 0, RateSurcharge) ->
    base_call_cost(RateCost, 60, RateSurcharge);
base_call_cost(RateCost, RateMin, RateSurcharge)
  when is_integer(RateCost),
       is_integer(RateMin),
       is_integer(RateSurcharge) ->
    RateCost * (RateMin div 60) + RateSurcharge;
base_call_cost(RateCost, RateMin, RateSurcharge) ->
    Args = [maybe_convert_to_units(X) || X <- [RateCost, RateMin, RateSurcharge]],
    apply(fun base_call_cost/3, Args).

-spec maybe_convert_to_units(kz_currency:units() | kz_currency:dollars()) -> kz_currency:units().
maybe_convert_to_units(Units) when is_integer(Units) -> Units;
maybe_convert_to_units(Dollars) -> kz_currency:dollars_to_units(Dollars).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec call_cost(kz_json:object()) -> kz_currency:units().
call_cost(JObj) ->
    {_Seconds, Cost} = calculate_call(JObj),
    Cost.

-spec calculate_call(kz_json:object()) -> {integer(), kz_currency:units()}.
calculate_call(JObj) ->
    CCVs = kz_json:get_first_defined([<<"Custom-Channel-Vars">>
                                     ,<<"custom_channel_vars">>
                                     ]
                                    ,JObj
                                    ,JObj
                                    ),
    RateNoChargeTime = get_integer_value(<<"Rate-NoCharge-Time">>, CCVs),
    BillingSecs = get_integer_value(<<"Billing-Seconds">>, JObj)
        - get_integer_value(<<"Billing-Seconds-Offset">>, CCVs),
    %% if we transition from allotment to per_minute the offset has a slight
    %% fudge factor to allow accounts with no credit to terminate the call
    %% on the next re-authorization cycle (to allow for the in-flight time)
    case BillingSecs =< 0 of
        'true' -> {0, 0};
        'false' when BillingSecs =< RateNoChargeTime ->
            lager:info("billing seconds less then ~ps, no charge", [RateNoChargeTime]),
            {0, 0};
        'false' ->
            Rate = get_integer_value(<<"Rate">>, CCVs),
            RateIncr = get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            RateMin = get_integer_value(<<"Rate-Minimum">>, CCVs),
            Surcharge = get_integer_value(<<"Surcharge">>, CCVs),
            {ChargedSeconds, Cost} = calculate_call(Rate, RateIncr, RateMin, Surcharge, BillingSecs),
            Discount = trunc((get_integer_value(<<"Discount-Percentage">>, CCVs) * 0.01) * Cost),
            lager:info("rate $~p,"
                       " increment ~ps,"
                       " minimum ~ps,"
                       " surcharge $~p,"
                       " for ~ps (~ps),"
                       " no charge time ~ps,"
                       " sub total $~p,"
                       " discount $~p,"
                       " total $~p"
                      ,[kz_currency:units_to_dollars(Rate)
                       ,RateIncr, RateMin
                       ,kz_currency:units_to_dollars(Surcharge)
                       ,BillingSecs
                       ,ChargedSeconds
                       ,RateNoChargeTime
                       ,kz_currency:units_to_dollars(Cost)
                       ,kz_currency:units_to_dollars(Discount)
                       ,kz_currency:units_to_dollars(Cost - Discount)
                       ]),
            {ChargedSeconds, trunc(Cost - Discount)}
    end.

-spec get_integer_value(kz_term:ne_binary(), kz_json:object()) -> integer().
get_integer_value(Key, JObj) ->
    get_integer_value(Key, JObj, 0).

-spec get_integer_value(kz_term:ne_binary(), kz_json:object(), any()) -> integer().
get_integer_value(Key, JObj, Default) ->
    Keys = [Key, kz_json:normalize_key(Key)],
    kz_term:to_integer(
      kz_json:get_first_defined(Keys, JObj, Default)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec per_minute_cost(kz_json:object()) -> integer().
per_minute_cost(JObj) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    RateNoChargeTime = get_integer_value(<<"Rate-NoCharge-Time">>, CCVs),
    BillingSecs = kz_json:get_integer_value(<<"Billing-Seconds">>, JObj)
        - kz_json:get_integer_value(<<"Billing-Seconds-Offset">>, CCVs, 0),
    case BillingSecs =< 0 of
        'true' -> 0;
        'false' when BillingSecs =< RateNoChargeTime -> 0;
        'false' ->
            RateIncr = kz_json:get_integer_value(<<"Rate-Increment">>, CCVs, 60),
            case kz_json:get_integer_value(<<"Rate">>, CCVs, 0) of
                0 -> 0;
                Rate ->
                    trunc((RateIncr / 60) * Rate)
            end
    end.

%% R :: rate, per minute, in units (0.01, 1 cent per minute)
%% RI :: rate increment, in seconds, bill in this increment AFTER rate minimum is taken from Secs
%% RM :: rate minimum, in seconds, minimum number of seconds to bill for
%% Sur :: surcharge, in units, (0.05, 5 cents to connect the call)
%% Secs :: billable seconds

-spec calculate_cost(kz_currency:units(), integer(), integer(), kz_currency:units(), integer()) -> kz_currency:units().
calculate_cost(_, _, _, _, 0) -> 0;
calculate_cost(R, 0, RM, Sur, Secs) ->
    calculate_cost(R, 60, RM, Sur, Secs);
calculate_cost(R, RI, RM, Sur, Secs) ->
    {_Sec, Cost} = calculate_call(R, RI, RM, Sur, Secs),
    Cost.

-spec calculate_call(kz_currency:units(), integer(), integer(), kz_currency:units(), integer()) -> {integer(), kz_currency:units()}.
calculate_call(_, _, _, _, 0) -> 0;
calculate_call(R, 0, RM, Sur, Secs) ->
    calculate_call(R, 60, RM, Sur, Secs);
calculate_call(R, RI, RM, Sur, Secs) ->
    case Secs =< RM of
        'true' ->
            {RM, trunc(Sur + ((RM / 60) * R))};
        'false' ->
            {kz_term:ceiling( Secs / RI ) * RI
            ,trunc(Sur + ((RM / 60) * R) + (kz_term:ceiling((Secs - RM) / RI) * ((RI / 60) * R)))
            }
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_ccvs(kz_json:object()) -> kz_term:proplist().
filter_ccvs(CCVs) ->
    lager:debug("extracting CCVs from ~p", [CCVs]),
    {ReqCCVs, _} =
        kz_json:foldl(fun filter_ccvs/3
                     ,{[], reserved_ccv_keys()}
                     ,CCVs
                     ),
    ReqCCVs.

filter_ccvs(Key, Value, {Acc, Keys}) ->
    case is_private_ccv(Key, Keys) of
        'true' -> {Acc, Keys};
        'false' ->
            lager:debug("adding ccv ~s:~p", [Key, Value]),
            {[{Key, Value} | Acc], Keys}
    end.

-spec is_private_ccv(kz_term:ne_binary(), kz_term:ne_binaries()) -> boolean().
is_private_ccv(Key, Keys) ->
    lists:member(Key, Keys).

-spec reserved_ccv_keys() -> kz_term:ne_binaries().
reserved_ccv_keys() ->
    kapps_config:get_ne_binaries(<<"call_command">>, <<"reserved_ccv_keys">>, ?DEFAULT_CCV_KEYS).
