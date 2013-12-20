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
%%% File:      folsom_sample_uniform.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% erlang implementation of a uniform random sample
%%% based on a java implementation by coda hale, which can be found at:
%%%
%%% https://github.com/codahale/metrics/blob/development/src/main/java/com/yammer/metrics/core/UniformSample.java
%%%
%%% that implementation is based on algorithm R in:
%%%
%%% http://www.cs.umd.edu/~samir/498/vitter.pdf
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_uniform).

-export([
         new/1,
         update/2,
         get_values/1
        ]).

-include("folsom.hrl").

new(Size) ->
    #uniform{size = Size}.

update(#uniform{size = Size, reservoir = Reservoir, n = N} = Sample, Value) when N =< Size ->
    ets:insert(Reservoir, {N, Value}),
    Sample#uniform{n = N + 1};

update(#uniform{reservoir = Reservoir, size = Size, n = N, seed = Seed} = Sample,
       Value) ->
    {Rnd, New_seed} = random:uniform_s(N, Seed),
    maybe_update(Rnd, Size, Value, Reservoir),
    Sample#uniform{n = N + 1, seed = New_seed}.

get_values(#uniform{reservoir = Reservoir}) ->
    {_, Values} = lists:unzip(ets:tab2list(Reservoir)),
    Values.

maybe_update(Rnd, Size, Value, Reservoir) when Rnd < Size ->
    ets:insert(Reservoir, {Rnd, Value});
maybe_update(_Rnd, _Size, _Value, _Reservoir) ->
    ok.
