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
%%% File:      folsom.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom).
-export([start/0, stop/0]).
-export([start/2, stop/1]).
-behaviour(application).
-define(APP, ?MODULE).

start() ->
    application:start(?APP).

stop() ->
    application:stop(?APP).

start(_Type, _Args) ->
    {ok, Pid} = folsom_sup:start_link(),
    lists:foreach(fun configure/1,
      [{counter, new_counter},
       {gauge, new_gauge},
       {histogram, new_histogram},
       {history, new_history},
       {meter, new_meter},
       {meter_reader, new_meter_reader}]),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal
configure({K, New}) ->
    case application:get_env(?APP, K) of
        {ok, Specs} when is_list(Specs) ->
            [configure_metric(New, Spec) || Spec <- Specs];
        {ok, Spec} ->
            configure_metric(New, Spec);
        undefined -> ok
    end.

configure_metric(New, Spec) when is_list(Spec) ->
    apply(folsom_metrics, New, Spec);
configure_metric(New, Name) ->
    folsom_metrics:New(Name).
