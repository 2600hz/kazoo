%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(escalus_mongooseim).

-behaviour(escalus_server).

%% API
-export([pre_story/1,
         post_story/1,
         name/0]).

-spec pre_story(escalus:config()) -> escalus:config().
pre_story(Config) ->
    maybe_read_initial_metric_values(Config).

maybe_read_initial_metric_values(Config) ->
    Metrics = proplists:get_value(mongoose_metrics, Config, undefined),
    case Metrics of
        undefined ->
            Config;
        _ ->
            InitialMetrics = lists:foldl(fun read_metric_initial_value/2, [], Metrics),
            [{mongoose_metrics_initial, InitialMetrics} | Config]
    end.

-spec post_story(escalus:config()) -> escalus:config().
post_story(Config) ->
    maybe_check_metrics_post_story(Config).

maybe_check_metrics_post_story(Config) ->
    case proplists:get_value(mongoose_metrics_initial, Config) of
        undefined ->
            ok;
        InitialMetrics ->
            post_story_check_metrics(InitialMetrics)
    end,
    Config.

-spec name() -> mongooseim.
name() ->
    mongooseim.

read_metric_initial_value({Metric, _} = MetricSpec, Acc) ->
    Type = metric_type(Metric),
    maybe_reset_metric(Metric, Type),
    Value = get_value(Metric, Type),
    [{MetricSpec, Type, Value} | Acc];
read_metric_initial_value({Precond, Metric, Change}, Acc) ->
    case Precond() of
        true ->
            read_metric_initial_value({Metric, Change}, Acc);
        _ ->
            Acc
    end.

post_story_check_metrics(CountersToCheck) ->
    After = [{MetricSpec, OldValue, get_value(Metric, Type)} || {{Metric, _} = MetricSpec, Type, OldValue } <- CountersToCheck],
    [] = lists:foldl(fun check_metric_change/2, [], After),
    ok.

maybe_reset_metric(Metric, histogram) ->
    escalus_ejabberd:rpc(exometer, reset, [Metric]);
maybe_reset_metric(_, _) ->
    ok.

get_value(Metric, spiral) ->
    Values = get_values(Metric),
    lists:foldl(fun({_, [{count, X}, _]}, Sum) ->
        Sum + X
    end, 0, Values);
get_value(Metric, histogram) ->
    get_value(Metric);
get_value(Metric, _) ->
    get_value(Metric).

get_value(Metric) ->
    {ok, Value} = escalus_ejabberd:rpc(mongoose_metrics, get_metric_value, [Metric]),
    Value.

get_values(Metric) ->
    escalus_ejabberd:rpc(mongoose_metrics, get_metric_values, [Metric]).

metric_type(Metric) ->
    [{_, Type, _} | _] = escalus_ejabberd:rpc(exometer, find_entries, [Metric]),
    Type.

check_metric_change({{Metric, Change}, Before, After}, Acc) when is_integer(Change) ->
    case Before + Change =:= After of
        true ->
            Acc;
        _ ->
            [{Metric, {expected_diff, Change}, {before_story, Before}, {after_story, After}} | Acc]
    end;

check_metric_change({{Metric, changed}, Before, After}, Acc) ->
    case After of
        Before ->
            [{Metric, expected_change, {before_story, Before}, {after_story, After}} | Acc];
        _ ->
            Acc
    end;
check_metric_change({{Metric, Changes}, Before, After}, Acc) when is_list(Changes)->
    Check = fun({Property, Change}) ->
        {Property, BeforeProp} = lists:keyfind(Property, 1, Before),
        {Property, AfterProp} = lists:keyfind(Property, 1, After),
        check_change(BeforeProp, AfterProp, Change)
    end,
    case lists:all(Check, Changes) of
        false ->
            [{Metric, {expected_change, Changes}, {before_story, Before}, {after_story, After}} | Acc];
        _ ->
            Acc
    end.

check_change(Before, After, Change) when is_atom(Change) ->
    erlang:apply(erlang, Change, [After, Before]);
check_change(Before, After, Change) when is_integer(Change) ->
    After == Before + Change.