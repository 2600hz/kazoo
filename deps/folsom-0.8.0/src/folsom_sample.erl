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
%%% File:      folsom_sample.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_sample).

-export([
         new/1,
         new/2,
         new/3,
         update/3,
         get_values/2
        ]).

-include("folsom.hrl").

%% API

new(slide) ->
    new(slide, ?DEFAULT_SLIDING_WINDOW);
new(slide_uniform) ->
    new(slide_uniform, {?DEFAULT_SLIDING_WINDOW, ?DEFAULT_SIZE});
new(Type) ->
    new(Type, ?DEFAULT_SIZE, ?DEFAULT_ALPHA).

new(Type, Size) ->
    new(Type, Size, ?DEFAULT_ALPHA).

new(slide, Size, _) ->
    folsom_sample_slide:new(Size);
new(slide_uniform, Sizes, _) ->
    folsom_sample_slide_uniform:new(Sizes);
new(uniform, Size, _) ->
    folsom_sample_uniform:new(Size);
new(none, Size, _) ->
    folsom_sample_none:new(Size);
new(slide_sorted, Size, _) ->
    folsom_sample_slide_sorted:new(Size);
new(exdec, Size, Alpha) ->
    folsom_sample_exdec:new(Size, Alpha).

update(uniform, Sample, Value) ->
    folsom_sample_uniform:update(Sample, Value);
update(none, Sample, Value) ->
    folsom_sample_none:update(Sample, Value);
update(slide_sorted, Sample, Value) ->
    folsom_sample_slide_sorted:update(Sample, Value);
update(exdec, Sample, Value) ->
    folsom_sample_exdec:update(Sample, Value);
update(slide, Sample, Value) ->
    folsom_sample_slide:update(Sample, Value);
update(slide_uniform, Sample, Value) ->
    folsom_sample_slide_uniform:update(Sample, Value).


get_values(uniform, Sample) ->
    folsom_sample_uniform:get_values(Sample);
get_values(none, Sample) ->
    folsom_sample_none:get_values(Sample);
get_values(slide_sorted, Sample) ->
    folsom_sample_slide_sorted:get_values(Sample);
get_values(exdec, Sample) ->
    folsom_sample_exdec:get_values(Sample);
get_values(slide, Sample) ->
    folsom_sample_slide:get_values(Sample);
get_values(slide_uniform, Sample) ->
    folsom_sample_slide_uniform:get_values(Sample).
