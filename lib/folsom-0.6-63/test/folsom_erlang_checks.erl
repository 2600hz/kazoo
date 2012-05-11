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
%%% File:      folsom_erlang_checks.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_erlang_checks).

-include_lib("eunit/include/eunit.hrl").

-export([
         create_metrics/0,
         populate_metrics/0,
         check_metrics/0,
         delete_metrics/0,
         vm_metrics/0,
         counter_metric/2,
         cpu_topology/0
        ]).

-define(DATA, [0, 1, 5, 10, 100, 200, 500, 750, 1000, 2000, 5000]).

-define(DATA1, [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]).

-include("folsom.hrl").

create_metrics() ->
    ok = folsom_metrics:new_counter(counter),
    ok = folsom_metrics:new_gauge(<<"gauge">>),

    ok = folsom_metrics:new_histogram(<<"uniform">>, uniform, 5000, 1),
    ok = folsom_metrics:new_histogram(exdec, exdec, 5000, 1),
    ok = folsom_metrics:new_histogram(none, none, 5000, 1),

    ok = folsom_metrics:new_histogram(nonea, none, 5000, 1),

    ok = folsom_metrics:new_histogram(timed, none, 5000, 1),

    ok = folsom_metrics:new_history(<<"history">>),
    ok = folsom_metrics:new_meter(meter),

    ok = folsom_metrics:new_meter_reader(meter_reader),

    ?debugFmt("ensuring meter tick is registered with gen_server~n", []),
    ok = ensure_meter_tick_exists(),

    ?debugFmt("ensuring multiple timer registrations dont cause issues", []),
    ok = folsom_meter_timer_server:register(meter, folsom_metrics_meter),
    ok = folsom_meter_timer_server:register(meter, folsom_metrics_meter),
    ok = folsom_meter_timer_server:register(meter, folsom_metrics_meter),

    ?debugFmt("~p", [folsom_meter_timer_server:dump()]),
    {state, List} = folsom_meter_timer_server:dump(),
    2 = length(List),

    10 = length(folsom_metrics:get_metrics()),

    ?debugFmt("~n~nmetrics: ~p~n", [folsom_metrics:get_metrics()]).

populate_metrics() ->
    ok = folsom_metrics:notify({counter, {inc, 1}}),
    ok = folsom_metrics:notify({counter, {dec, 1}}),

    ok = folsom_metrics:notify({<<"gauge">>, 2}),

    [ok = folsom_metrics:notify({<<"uniform">>, Value}) || Value <- ?DATA],

    [ok = folsom_metrics:notify({exdec, Value}) || Value <- lists:seq(1, 100000)],

    [ok = folsom_metrics:notify({none, Value}) || Value <- ?DATA],

    [ok = folsom_metrics:notify({nonea, Value}) || Value <- ?DATA1],

    3.141592653589793 = folsom_metrics:histogram_timed_update(timed, math, pi, []),

    ok = folsom_metrics:notify({<<"history">>, "string"}),

    {error, _, nonexistent_metric} = folsom_metrics:notify({historya, "5"}),
    ok = folsom_metrics:notify(historya, <<"binary">>, history),


    ?debugFmt("testing meter ...", []),

    % simulate an interval tick
    folsom_metrics_meter:tick(meter),

    [ok,ok,ok,ok,ok] =
        [ folsom_metrics:notify({meter, Item}) || Item <- [100, 100, 100, 100, 100]],

    % simulate an interval tick
    folsom_metrics_meter:tick(meter),

    ?debugFmt("testing meter reader ...", []),

    % simulate an interval tick
    folsom_metrics_meter_reader:tick(meter_reader),

    [ok,ok,ok,ok,ok] =
        [ folsom_metrics:notify({meter_reader, Item}) || Item <- [1, 10, 100, 1000, 10000]],

    % simulate an interval tick
    folsom_metrics_meter_reader:tick(meter_reader).

check_metrics() ->
    0 = folsom_metrics:get_metric_value(counter),

    2 = folsom_metrics:get_metric_value(<<"gauge">>),

    Histogram1 = folsom_metrics:get_histogram_statistics(<<"uniform">>),
    histogram_checks(Histogram1),

    % just check exdec for non-zero values
    Exdec = folsom_metrics:get_histogram_statistics(exdec),

    ?debugFmt("checking exdec sample~n~p~n", [Exdec]),

    ok = case proplists:get_value(median, Exdec) of
        Median when Median > 0 ->
                 ok;
             _ ->
                 error
         end,

    Histogram3 = folsom_metrics:get_histogram_statistics(none),
    histogram_checks(Histogram3),

    CoValues = folsom_metrics:get_histogram_statistics(none, nonea),
    histogram_co_checks(CoValues),

    List = folsom_metrics:get_metric_value(timed),
    ?debugFmt("timed update value: ~p", [List]),

    1 = length(folsom_metrics:get_metric_value(<<"history">>)),
    1 = length(folsom_metrics:get_metric_value(historya)),

    ?debugFmt("checking meter~n", []),
    Meter = folsom_metrics:get_metric_value(meter),
    ?debugFmt("~p", [Meter]),
    ok = case proplists:get_value(one, Meter) of
             Value when Value > 1 ->
                 ok;
             _ ->
                 error
         end,

    ?debugFmt("checking meter reader~n", []),
    MeterReader = folsom_metrics:get_metric_value(meter_reader),
    ?debugFmt("~p", [MeterReader]),
    ok = case proplists:get_value(one, MeterReader) of
             Value1 when Value1 > 1 ->
                 ok;
             _ ->
                 error
         end.


delete_metrics() ->
    12 = length(ets:tab2list(?FOLSOM_TABLE)),

    ok = folsom_metrics:delete_metric(counter),
    ok = folsom_metrics:delete_metric(<<"gauge">>),

    ok = folsom_metrics:delete_metric(<<"uniform">>),
    ok = folsom_metrics:delete_metric(exdec),
    ok = folsom_metrics:delete_metric(none),

    ok = folsom_metrics:delete_metric(<<"history">>),
    ok = folsom_metrics:delete_metric(historya),

    ok = folsom_metrics:delete_metric(nonea),
    ok = folsom_metrics:delete_metric(timed),
    ok = folsom_metrics:delete_metric(testcounter),

    1 = length(ets:tab2list(?METER_TABLE)),
    ok = folsom_metrics:delete_metric(meter),
    0 = length(ets:tab2list(?METER_TABLE)),

    1 = length(ets:tab2list(?METER_READER_TABLE)),
    ok = folsom_metrics:delete_metric(meter_reader),
    0 = length(ets:tab2list(?METER_READER_TABLE)),

    0 = length(ets:tab2list(?FOLSOM_TABLE)).

vm_metrics() ->
    List1 = folsom_vm_metrics:get_memory(),
    true = lists:keymember(total, 1, List1),

    List2 = folsom_vm_metrics:get_statistics(),
    true = lists:keymember(context_switches, 1, List2),

    List3 = folsom_vm_metrics:get_system_info(),
    true = lists:keymember(allocated_areas, 1, List3),

    [{_, [{backtrace, _}| _]} | _] = folsom_vm_metrics:get_process_info(),

    [{_, [{name, _}| _]} | _] = folsom_vm_metrics:get_port_info().


counter_metric(Count, Counter) ->
    ok = folsom_metrics:new_counter(Counter),

    ?debugFmt("running ~p counter inc/dec rounds~n", [Count]),
    for(Count, Counter),

    Result = folsom_metrics:get_metric_value(Counter),
    ?debugFmt("counter result: ~p~n", [Result]),

    0 = Result.

ensure_meter_tick_exists() ->
    {state, State} = folsom_meter_timer_server:dump(),
    2 = length(State),
    ok.

%% internal function

histogram_checks(List) ->
    ?debugFmt("checking histogram statistics", []),
    ?debugFmt("~p~n", [List]),
    0 = proplists:get_value(min, List),
    5000 = proplists:get_value(max, List),
    869.6363636363636 = proplists:get_value(arithmetic_mean, List),

    GeoMean = proplists:get_value(geometric_mean, List),
    ok = case GeoMean - 100.17443147308997 of
             GeoDiff when GeoDiff <  0.00000001 ->
                 ok;
             _ ->
                 error
         end,

    Value = proplists:get_value(harmonic_mean, List),
    %?debugFmt("~p~n", [Value]),
    ok = case Value - 8.333122900936845 of
             Diff when Diff < 0.00000001 ->
                 ok;
             _ ->
                 error
         end,

    200 = proplists:get_value(median, List),
    2254368.454545454 = proplists:get_value(variance, List),
    1501.4554454080394 = proplists:get_value(standard_deviation, List),
    1.8399452806806476 = proplists:get_value(skewness, List),
    2.2856772911293204 = proplists:get_value(kurtosis, List),
    List1 = proplists:get_value(percentile, List),
    percentile_check(List1),
    List2 = proplists:get_value(histogram, List),
    histogram_check(List2).

histogram_co_checks(List) ->
    ?debugFmt("checking histogram covariance and etc statistics", []),
    ?debugFmt("~p~n", [List]),
    [
     {covariance,17209.545454545456},
     {tau,1.0},
     {rho,0.760297020598996},
     {r,1.0}
    ] = List.

percentile_check(List) ->
    750 = proplists:get_value(75, List),
    2000 = proplists:get_value(95, List),
    5000 = proplists:get_value(99, List),
    5000 = proplists:get_value(999, List).

histogram_check(List) ->
    [{2400,10},{5000,1},{8000,0}] = List.

counter_inc_dec(Counter) ->
    ok = folsom_metrics:notify({Counter, {inc, 1}}),
    ok = folsom_metrics:notify({Counter, {dec, 1}}).

for(N, Counter) ->
    for(N, 0, Counter).
for(N, Count, _Counter) when N == Count ->
    ok;
for(N, LoopCount, Counter) ->
    counter_inc_dec(Counter),
    for(N, LoopCount + 1, Counter).

cpu_topology() ->
    Test = [{node,[{processor,[{core,[{thread,{logical,1}},{thread,{logical,9}}]},
                               {core,[{thread,{logical,3}},{thread,{logical,11}}]},
                               {core,[{thread,{logical,5}},{thread,{logical,13}}]},
                               {core,[{thread,{logical,7}},{thread,{logical,15}}]}]}]},
            {node,[{processor,[{core,[{thread,{logical,0}},{thread,{logical,8}}]},
                               {core,[{thread,{logical,2}},{thread,{logical,10}}]},
                               {core,[{thread,{logical,4}},{thread,{logical,12}}]},
                               {core,[{thread,{logical,6}},{thread,{logical,14}}]}]}]}],

    ExpectedResult = [{node,[{processor,[{core,[{thread,[logical,1]},{thread,[logical,9]}]},
                                         {core,[{thread,[logical,3]},{thread,[logical,11]}]},
                                         {core,[{thread,[logical,5]},{thread,[logical,13]}]},
                                         {core,[{thread,[logical,7]},{thread,[logical,15]}]}]}]},
                      {node,[{processor,[{core,[{thread,[logical,0]},{thread,[logical,8]}]},
                                         {core,[{thread,[logical,2]},{thread,[logical,10]}]},
                                         {core,[{thread,[logical,4]},{thread,[logical,12]}]},
                                         {core,[{thread,[logical,6]},{thread,[logical,14]}]}]}]}],

    ExpectedResult = folsom_vm_metrics:convert_cpu_topology(Test, []).
