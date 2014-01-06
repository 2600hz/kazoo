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
%%% File:      folsom_sample_none.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% no sampling, just a capped circular buffer
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_none).

-export([
         new/1,
         update/2,
         get_values/1
        ]).

-include("folsom.hrl").

new(Size) ->
    #none{size = Size}.

update(#none{size = Size, reservoir = Reservoir, n = N} = Sample, Value)
  when N =:= Size ->
    ets:insert(Reservoir, {N, Value}),
    Sample#none{n = 1};
update(#none{reservoir = Reservoir, n = N} = Sample, Value) ->
    ets:insert(Reservoir, {N, Value}),
    Sample#none{n = N  + 1}.

get_values(#none{reservoir = Reservoir}) ->
    {_, Values} = lists:unzip(ets:tab2list(Reservoir)),
    Values.
