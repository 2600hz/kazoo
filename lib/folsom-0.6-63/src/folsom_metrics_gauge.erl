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
%%% File:      folsom_metrics_gauge.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_metrics_gauge).

-export([new/1,
         update/2,
         clear/1,
         get_value/1]).

-include("folsom.hrl").

new(Name) ->
    Gauge = {Name, 0},
    ets:insert(?GAUGE_TABLE, Gauge).

update(Name, Value) ->
    Gauge = {Name, Value},
    ets:insert(?GAUGE_TABLE, Gauge).

clear(Name) ->
    new(Name).

get_value(Name) ->
    [{_, Values}] = ets:lookup(?GAUGE_TABLE, Name),
    Values.
