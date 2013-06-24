%%%
%%% Copyright 2011, Boundary
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
%%% File:      folsom_metrics.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics).

-export([
         new_counter/1,
         new_gauge/1,
         new_histogram/1,
         new_histogram/2,
         new_histogram/3,
         new_histogram/4,
         new_history/1,
         new_history/2,
         new_meter/1,
         new_meter_reader/1,
         delete_metric/1,
         notify/1,
         notify/2,
         notify/3,
         notify_existing_metric/3,
         get_metrics/0,
         metric_exists/1,
         get_metrics_info/0,
         get_metric_info/1,
         get_metric_value/1,
         get_histogram_statistics/1,
         get_histogram_statistics/2,
         get_history_values/2,
         histogram_timed_update/2,
         histogram_timed_update/3,
         histogram_timed_update/4
        ]).

-include("folsom.hrl").

%% Metrics API

new_counter(Name) ->
    folsom_ets:add_handler(counter, Name).

new_gauge(Name) ->
    folsom_ets:add_handler(gauge, Name).

new_histogram(Name) ->
    folsom_metrics:new_histogram(Name, ?DEFAULT_SAMPLE_TYPE, ?DEFAULT_SIZE, ?DEFAULT_ALPHA).

new_histogram(Name, SampleType) ->
    folsom_metrics:new_histogram(Name, SampleType, ?DEFAULT_SIZE, ?DEFAULT_ALPHA).

new_histogram(Name, SampleType, SampleSize) ->
    folsom_metrics:new_histogram(Name, SampleType, SampleSize, ?DEFAULT_ALPHA).

new_histogram(Name, SampleType, SampleSize, Alpha) ->
    folsom_ets:add_handler(histogram, Name, SampleType, SampleSize, Alpha).

new_history(Name) ->
    folsom_metrics:new_history(Name, ?DEFAULT_SIZE).

new_history(Name, SampleSize) ->
    folsom_ets:add_handler(history, Name, SampleSize).

new_meter(Name) ->
    folsom_ets:add_handler(meter, Name).

new_meter_reader(Name) ->
    folsom_ets:add_handler(meter_reader, Name).

delete_metric(Name) ->
    folsom_ets:delete_handler(Name).

notify(Event) ->
    folsom_ets:notify(Event).

notify(Name, Event) ->
    folsom_ets:notify(Name, Event).

notify(Name, Event, Type) ->
    folsom_ets:notify(Name, Event, Type).

notify_existing_metric(Name, Event, Type) ->
    folsom_ets:notify_existing_metric(Name, Event, Type).

get_metrics() ->
    folsom_ets:get_handlers().

metric_exists(Name) ->
    folsom_ets:handler_exists(Name).

get_metrics_info() ->
    folsom_ets:get_handlers_info().

get_metric_info(Name) ->
    [folsom_ets:get_info(Name)].

get_metric_value(Name) ->
    folsom_ets:get_values(Name).

get_histogram_statistics(Name) ->
    Values = folsom_ets:get_values(Name),
    bear:get_statistics(Values).

get_histogram_statistics(Name1, Name2) ->
    Values1 = get_metric_value(Name1),
    Values2 = get_metric_value(Name2),
    bear:get_statistics(Values1, Values2).

get_history_values(Name, Count) ->
    folsom_ets:get_history_values(Name, Count).

histogram_timed_update(Name, Fun) ->
    {Time, Value} = timer:tc(Fun),
    ok = notify({Name, Time}),
    Value.

histogram_timed_update(Name, Fun, Args) ->
    {Time, Value} = timer:tc(Fun, Args),
    ok = notify({Name, Time}),
    Value.

histogram_timed_update(Name, Mod, Fun, Args) ->
    {Time, Value} = timer:tc(Mod, Fun, Args),
    ok = notify({Name, Time}),
    Value.
