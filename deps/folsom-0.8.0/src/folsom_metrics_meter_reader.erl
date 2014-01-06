%%%
%%% Copyright 2011, Boundary
%%% Copyright 2011, Opscode
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_metrics_meter_reader.erl
%%% @author    Seth Falcon <seth@opscode.com>
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_meter_reader).

-export([new/1,
         tick/1,
         mark/1,
         mark/2,
         get_values/1,
         get_acceleration/1
        ]).


-record(meter_reader, {
          one,
          five,
          fifteen,
          count = 0,
          start_time,
          last_count = unset
         }).

-include("folsom.hrl").

new(Name) ->
    OneMin = folsom_ewma:one_minute_ewma(),
    FiveMin = folsom_ewma:five_minute_ewma(),
    FifteenMin = folsom_ewma:fifteen_minute_ewma(),

    ets:insert(?METER_READER_TABLE,
               {Name, #meter_reader{one = OneMin,
                                    five = FiveMin,
                                    fifteen = FifteenMin,
                                    start_time = folsom_utils:now_epoch_micro()}}).

tick(Name) ->
    #meter_reader{one = OneMin,
                  five = FiveMin,
                  fifteen = FifteenMin} = Meter = get_value(Name),

    OneMin1 = folsom_ewma:tick(OneMin),
    FiveMin1 = folsom_ewma:tick(FiveMin),
    FifteenMin1 = folsom_ewma:tick(FifteenMin),

    ets:insert(?METER_READER_TABLE,
               {Name, Meter#meter_reader{one = OneMin1,
                                         five = FiveMin1,
                                         fifteen = FifteenMin1}}).

mark(Name) ->
    mark(Name, 1).

mark(Name, Value) ->
    % skip first reading to bootstrap last value
    #meter_reader{count = Count,
                  last_count = LastCount,
                  one = OneMin,
                  five = FiveMin,
                  fifteen = FifteenMin} = Meter = get_value(Name),

    NewMeter = case LastCount of
                   unset ->
                       Meter#meter_reader{last_count = Value};
                   _ ->
                       Delta = Value - LastCount,
                       OneMin1 = folsom_ewma:update(OneMin, Delta),
                       FiveMin1 = folsom_ewma:update(FiveMin, Delta),
                       FifteenMin1 = folsom_ewma:update(FifteenMin, Delta),
                       Meter#meter_reader{count = Count + Delta,
                                          last_count = Value,
                                          one = OneMin1,
                                          five = FiveMin1,
                                          fifteen = FifteenMin1}
               end,

    ets:insert(?METER_READER_TABLE, {Name, NewMeter}).

get_values(Name) ->
    #meter_reader{one = OneMin,
                  five = FiveMin,
                  fifteen = FifteenMin} = Meter = get_value(Name),

    L = [
         {one, get_rate(OneMin)},
         {five, get_rate(FiveMin)},
         {fifteen, get_rate(FifteenMin)},
         {mean, get_mean_rate(Meter)},
         {acceleration, get_acceleration(Name)}
        ],

    [ {K,V} || {K,V} <- L, V /= undefined ].

get_acceleration(Name) ->
    #meter_reader{one = OneMin,
                  five = FiveMin,
                  fifteen = FifteenMin} = get_value(Name),

    [
     {one_to_five, calc_acceleration(get_rate(OneMin), get_rate(FiveMin), 300)},
     {five_to_fifteen, calc_acceleration(get_rate(FiveMin), get_rate(FifteenMin), 600)},
     {one_to_fifteen, calc_acceleration(get_rate(OneMin), get_rate(FifteenMin), 900)}
    ].

% internal functions

get_rate(EWMA) ->
    folsom_ewma:rate(EWMA).

get_mean_rate(#meter_reader{count = Count, start_time = Start}) ->
    calc_mean_rate(Start, Count).

get_value(Name) ->
    [{_, Value}] = ets:lookup(?METER_READER_TABLE, Name),
    Value.

calc_mean_rate(_, 0) ->
    0.0;
calc_mean_rate(Start, Count) ->
    Elapsed = folsom_utils:now_epoch_micro() - Start,
    Count / Elapsed.

calc_acceleration(Rate1, Rate2, Interval) ->
    % most current velocity minus previous velocity
    get_rate(Rate1, Rate2, Interval).

get_rate(Value1, Value2, Interval) ->
    Delta = Value1 - Value2,
    Delta / Interval.
