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
%%% File:      folsom_metrics_histogram.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_histogram).

-export([new/1,
         new/2,
         new/3,
         new/4,
         update/2,
         get_value/1,
         get_values/1
         ]).

-include("folsom.hrl").

new(Name) ->
    new(Name, uniform).

new(Name, slide) ->
    new(Name, slide, ?DEFAULT_SLIDING_WINDOW);
new(Name, slide_uniform) ->
    new(Name, slide_uniform, {?DEFAULT_SLIDING_WINDOW, ?DEFAULT_SIZE});
new(Name, SampleType) ->
    new(Name, SampleType, ?DEFAULT_SIZE).

new(Name, SampleType, SampleSize) ->
    Sample = folsom_sample:new(SampleType, SampleSize),
    Hist = #histogram{type = SampleType, sample = Sample},
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist}).

new(Name, SampleType, SampleSize, Alpha) ->
    Sample = folsom_sample:new(SampleType, SampleSize, Alpha),
    Hist = #histogram{type = SampleType, sample = Sample},
    ets:insert(?HISTOGRAM_TABLE, {Name, Hist}).

update(Name, Value) ->
    Hist = get_value(Name),
    Sample = Hist#histogram.sample,
    case folsom_sample:update(Hist#histogram.type, Hist#histogram.sample, Value) of
        Sample ->
            %% sample didn't change, don't need to write it back
            true;
        NewSample ->
            ets:insert(?HISTOGRAM_TABLE, {Name, Hist#histogram{sample = NewSample}})
    end.

% gets the histogram record from ets
get_value(Name) ->
    [{_, Value}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    Value.

% pulls the sample out of the record gotten from ets
get_values(Name) ->
    Hist = get_value(Name),
    folsom_sample:get_values(Hist#histogram.type, Hist#histogram.sample).
