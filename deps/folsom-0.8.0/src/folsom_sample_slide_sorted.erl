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
%%% File:      folsom_sample_slide_sorted.erl
%%% @author    Ramon Lastres <ramon.lastres@erlang-solutions.com>
%%% @doc
%%% simple sliding window histogram.
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_slide_sorted).

-export([
         new/1,
         update/2,
         get_values/1
        ]).

-include("folsom.hrl").

new(Size) ->
    #slide_sorted{size = Size}.

update(#slide_sorted{size = Size, reservoir = Reservoir, n = N} = Sample, Value)
  when N < Size ->
    ets:insert(Reservoir, {os:timestamp(), Value}),
    Sample#slide_sorted{n = N + 1};
update(#slide_sorted{reservoir = Reservoir, n = N, size = Size} = Sample, Value)
  when N == Size ->
    Oldest = ets:first(Reservoir),
    ets:delete(Reservoir, Oldest),
    ets:insert(Reservoir, {os:timestamp(), Value}),
    Sample.

get_values(#slide_sorted{reservoir = Reservoir}) ->
    {_, Values} = lists:unzip(ets:tab2list(Reservoir)),
    Values.
