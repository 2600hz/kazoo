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
%%% File:      folsom_ets.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_ets).

%% API
-export([
         add_handler/2,
         add_handler/3,
         add_handler/4,
         add_handler/5,
         tag_handler/2,
         untag_handler/2,
         delete_handler/1,
         handler_exists/1,
         notify/1,
         notify/2,
         notify/3,
         tagged_notify/4,
         notify_existing_metric/3,
         get_handlers/0,
         get_handlers_info/0,
         get_info/1,
         get_values/1,
         get_history_values/2,
         get_group_values/1,
         get_group_values/2,
         get_tags/1
        ]).

-record(metric, {
          tags = sets:new(),
          type,
          history_size
         }).

-include_lib("folsom.hrl").

%%%===================================================================
%%% API
%%%===================================================================

add_handler(Type, Name) ->
    maybe_add_handler(Type, Name, handler_exists(Name)).

add_handler(Type, Name, SampleSize) ->
    maybe_add_handler(Type, Name, SampleSize, handler_exists(Name)).

add_handler(Type, Name, SampleType, SampleSize) ->
    maybe_add_handler(Type, Name, SampleType, SampleSize, handler_exists(Name)).

add_handler(Type, Name, SampleType, SampleSize, Alpha) ->
    maybe_add_handler(Type, Name, SampleType, SampleSize, Alpha, handler_exists(Name)).

tag_handler(Name, Tag) ->
    case handler_exists(Name) of
        true ->
            add_tag(Name, Tag);
        false ->
            {error, Name, nonexistent_metric}
    end.

untag_handler(Name, Tag) ->
    case handler_exists(Name) of
        true ->
            rm_tag(Name, Tag);
        false ->
            {error, Name, nonexistent_metric}
    end.

delete_handler(Name) ->
    {_, Info} = get_info(Name),
    ok = delete_metric(Name, proplists:get_value(type, Info)).

handler_exists(Name) ->
    ets:member(?FOLSOM_TABLE, Name).

%% old tuple style notifications
notify({Name, Event}) ->
    notify(Name, Event).

%% notify/2, checks metric type and makes sure metric exists
%% before notifying, returning error if not
notify(Name, Event) ->
    case handler_exists(Name) of
        true ->
            {_, Info} = get_info(Name),
            Type = proplists:get_value(type, Info),
            notify(Name, Event, Type, true);
        false ->
            {error, Name, nonexistent_metric}
    end.

%% notify/3, makes sure metric exist, if not creates metric
notify(Name, Event, Type) ->
    notify(Name, Event, Type, handler_exists(Name)).

tagged_notify(Name, Event, Type, Tags) ->
    R = notify(Name, Event, Type),
    case get_tags(Name) of
        {error, _, _} -> skip;
        CurrentTags ->
            [add_tag(Name, T) || T <- Tags, not sets:is_element(T, CurrentTags)]
    end,
    R.

%% assumes metric already exists, bypasses above checks
notify_existing_metric(Name, Event, Type) ->
    notify(Name, Event, Type, true).

get_handlers() ->
    proplists:get_keys(ets:tab2list(?FOLSOM_TABLE)).

get_handlers_info() ->
    [get_info(Id) || Id <- get_handlers()].

get_info(Name) ->
    case handler_exists(Name) of
        true ->
            [{_, #metric{type = Type, tags = Tags}}] = ets:lookup(?FOLSOM_TABLE, Name),
            {Name, [{type, Type}, {tags, Tags}]};
        false ->
            {error, Name, nonexistent_metric}
    end.

get_values(Name) ->
    case handler_exists(Name) of
        true ->
            {_, Info} = get_info(Name),
            get_values(Name, proplists:get_value(type, Info));
        false ->
            {error, Name, nonexistent_metric}
    end.

get_values(Name, counter) ->
    folsom_metrics_counter:get_value(Name);
get_values(Name, gauge) ->
    folsom_metrics_gauge:get_value(Name);
get_values(Name, histogram) ->
    folsom_metrics_histogram:get_values(Name);
get_values(Name, history) ->
    folsom_metrics_history:get_events(Name);
get_values(Name, meter) ->
    folsom_metrics_meter:get_values(Name);
get_values(Name, meter_reader) ->
    folsom_metrics_meter_reader:get_values(Name);
get_values(Name, duration) ->
    folsom_metrics_duration:get_values(Name);
get_values(Name, spiral) ->
    folsom_metrics_spiral:get_values(Name);
get_values(_, Type) ->
    {error, Type, unsupported_metric_type}.

get_history_values(Name, Count) ->
    folsom_metrics_history:get_events(Name, Count).

get_group_values(Tag) ->
    folsom_ets:get_group_values(Tag, '_').

get_group_values(Tag, Type) ->
    Metrics = ets:match(?FOLSOM_TABLE, {'$1', {metric, '$2', Type, '_'}}),
    [{Name, get_values(Name)} || [Name, Tags] <- Metrics, sets:is_element(Tag, Tags)].

get_tags(Name) ->
    case handler_exists(Name) of
        true ->
            {_, Info} = get_info(Name),
            proplists:get_value(tags, Info);
        false ->
            {error, Name, nonexistent_metric}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_add_handler(counter, Name, false) ->
    true = folsom_metrics_counter:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = counter}}),
    ok;
maybe_add_handler(gauge, Name, false) ->
    true = folsom_metrics_gauge:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = gauge}}),
    ok;
maybe_add_handler(histogram, Name, false) ->
    true = folsom_metrics_histogram:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = histogram}}),
    ok;
maybe_add_handler(duration, Name, false) ->
    true = folsom_metrics_histogram:new(Name),
    true = folsom_metrics_duration:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = duration}}),
    ok;
maybe_add_handler(history, Name, false) ->
    ok = folsom_metrics_history:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = history, history_size = ?DEFAULT_SIZE}}),
    ok;
maybe_add_handler(meter, Name, false) ->
    ok = folsom_meter_timer_server:register(Name, folsom_metrics_meter),
    true = folsom_metrics_meter:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = meter}}),
    ok;
maybe_add_handler(meter_reader, Name, false) ->
    ok = folsom_meter_timer_server:register(Name, folsom_metrics_meter_reader),
    true = folsom_metrics_meter_reader:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = meter_reader}}),
    ok;
maybe_add_handler(spiral, Name, false) ->
    true = folsom_metrics_spiral:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = spiral}}),
    ok;
maybe_add_handler(Type, _, false) ->
    {error, Type, unsupported_metric_type};
maybe_add_handler(_, Name, true) ->
    {error, Name, metric_already_exists}.

maybe_add_handler(histogram, Name, SampleType, false) ->
    true = folsom_metrics_histogram:new(Name, SampleType),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = histogram}}),
    ok;
maybe_add_handler(history, Name, SampleSize, false) ->
    ok = folsom_metrics_history:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = history, history_size = SampleSize}}),
    ok;
maybe_add_handler(Type, _, _, false) ->
    {error, Type, unsupported_metric_type};
maybe_add_handler(_, Name, _, true) ->
    {error, Name, metric_already_exists}.

maybe_add_handler(histogram, Name, SampleType, SampleSize, false) ->
    true = folsom_metrics_histogram:new(Name, SampleType, SampleSize),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = histogram}}),
    ok;
maybe_add_handler(Type, _, _, _, false) ->
    {error, Type, unsupported_metric_type};
maybe_add_handler(_, Name, _, _, true) ->
    {error, Name, metric_already_exists}.

maybe_add_handler(histogram, Name, SampleType, SampleSize, Alpha, false) ->
    true = folsom_metrics_histogram:new(Name, SampleType, SampleSize, Alpha),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = histogram}}),
    ok;
maybe_add_handler(duration, Name, SampleType, SampleSize, Alpha, false) ->
    true = folsom_metrics_histogram:new(Name, SampleType, SampleSize, Alpha),
    true = folsom_metrics_duration:new(Name),
    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = duration}}),
    ok;
maybe_add_handler(Type, _, _, _, _, false) ->
    {error, Type, unsupported_metric_type};
maybe_add_handler(_, Name, _, _, _, true) ->
    {error, Name, metric_already_exists}.

add_tag(Name, Tag) ->
    M = #metric{tags=Tags} = ets:lookup_element(?FOLSOM_TABLE, Name, 2),
    true = ets:update_element(?FOLSOM_TABLE, Name, {2, M#metric{tags=sets:add_element(Tag, Tags)}}),
    ok.

rm_tag(Name, Tag) ->
    M = #metric{tags=Tags} = ets:lookup_element(?FOLSOM_TABLE, Name, 2),
    true = ets:update_element(?FOLSOM_TABLE, Name, {2, M#metric{tags=sets:del_element(Tag, Tags)}}),
    ok.

delete_metric(Name, history) ->
    History = folsom_metrics_history:get_value(Name),
    ok = delete_history(Name, History),
    ok;
delete_metric(Name, histogram) ->
    Metric = folsom_metrics_histogram:get_value(Name),
    ok = delete_histogram(Name, Metric),
    ok;
delete_metric(Name, duration) ->
    Histo = folsom_metrics_histogram:get_value(Name),
    ok = delete_histogram(Name, Histo),
    true = ets:delete(?DURATION_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    ok;
delete_metric(Name, counter) ->
    ok = folsom_metrics_counter:delete(Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    ok;
delete_metric(Name, gauge) ->
    true = ets:delete(?GAUGE_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    ok;
delete_metric(Name, meter) ->
    ok = folsom_meter_timer_server:unregister(Name),
    true = ets:delete(?METER_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    ok;
delete_metric(Name, meter_reader) ->
    ok = folsom_meter_timer_server:unregister(Name),
    true = ets:delete(?METER_READER_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    ok;
delete_metric(Name, spiral) ->
    #spiral{tid=Tid, server=Pid} = folsom_metrics_spiral:get_value(Name),
    folsom_sample_slide_server:stop(Pid),
    ets:delete(?SPIRAL_TABLE, Name),
    ets:delete(?FOLSOM_TABLE, Name),
    ets:delete(Tid),
    ok.

delete_histogram(Name, #histogram{type = uniform, sample = #uniform{reservoir = Reservoir}}) ->
    true = ets:delete(?HISTOGRAM_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    true = ets:delete(Reservoir),
    ok;
delete_histogram(Name, #histogram{type = none, sample = #none{reservoir = Reservoir}}) ->
    true = ets:delete(?HISTOGRAM_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    true = ets:delete(Reservoir),
    ok;
delete_histogram(Name, #histogram{type = slide_sorted, sample = #slide_sorted{reservoir = Reservoir}}) ->
    true = ets:delete(?HISTOGRAM_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    true = ets:delete(Reservoir),
    ok;
delete_histogram(Name, #histogram{type = exdec}) ->
    true = ets:delete(?HISTOGRAM_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    ok;
delete_histogram(Name, #histogram{type = slide, sample = #slide{reservoir = Reservoir, server=Pid}}) ->
    folsom_sample_slide_server:stop(Pid),
    true = ets:delete(?HISTOGRAM_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    true = ets:delete(Reservoir),
    ok;
delete_histogram(Name, #histogram{type = slide_uniform, sample = #slide_uniform{reservoir = Reservoir, server=Pid}}) ->
    folsom_sample_slide_server:stop(Pid),
    true = ets:delete(?HISTOGRAM_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    true = ets:delete(Reservoir),
    ok.

delete_history(Name, #history{tid = Tid}) ->
    true = ets:delete(?HISTORY_TABLE, Name),
    true = ets:delete(?FOLSOM_TABLE, Name),
    true = ets:delete(Tid),
    ok.

notify(Name, {inc, Value}, counter, true) ->
    folsom_metrics_counter:inc(Name, Value),
    ok;
notify(Name, {inc, Value}, counter, false) ->
    add_handler(counter, Name),
    folsom_metrics_counter:inc(Name, Value),
    ok;
notify(Name, {dec, Value}, counter, true) ->
    folsom_metrics_counter:dec(Name, Value),
    ok;
notify(Name, {dec, Value}, counter, false) ->
    add_handler(counter, Name),
    folsom_metrics_counter:dec(Name, Value),
    ok;
notify(Name, clear, counter, true) ->
    folsom_metrics_counter:clear(Name),
    ok;
notify(Name, clear, counter, false) ->
    add_handler(counter, Name),
    folsom_metrics_counter:clear(Name),
    ok;
notify(Name, Value, gauge, true) ->
    folsom_metrics_gauge:update(Name, Value),
    ok;
notify(Name, Value, gauge, false) ->
    add_handler(gauge, Name),
    folsom_metrics_gauge:update(Name, Value),
    ok;
notify(Name, Value, histogram, true) ->
    folsom_metrics_histogram:update(Name, Value),
    ok;
notify(Name, Value, histogram, false) ->
    add_handler(histogram, Name),
    folsom_metrics_histogram:update(Name, Value),
    ok;
notify(Name, Value, history, true) ->
    [{_, #metric{history_size = HistorySize}}] = ets:lookup(?FOLSOM_TABLE, Name),
    folsom_metrics_history:update(Name, HistorySize, Value),
    ok;
notify(Name, Value, history, false) ->
    add_handler(history, Name),
    [{_, #metric{history_size = HistorySize}}] = ets:lookup(?FOLSOM_TABLE, Name),
    folsom_metrics_history:update(Name, HistorySize, Value),
    ok;
notify(Name, Value, meter, true) ->
    folsom_metrics_meter:mark(Name, Value),
    ok;
notify(Name, Value, meter, false) ->
    add_handler(meter, Name),
    folsom_metrics_meter:mark(Name, Value),
    ok;
notify(Name, Value, meter_reader, true) ->
    folsom_metrics_meter_reader:mark(Name, Value),
    ok;
notify(Name, Value, meter_reader, false) ->
    add_handler(meter, Name),
    folsom_metrics_meter_reader:mark(Name, Value),
    ok;
notify(Name, Value, duration, true) ->
    folsom_metrics_duration:update(Name, Value),
    ok;
notify(Name, Value, duration, false) ->
    add_handler(duration, Name),
    folsom_metrics_duration:update(Name, Value),
    ok;
notify(Name, Value, spiral, true) ->
    folsom_metrics_spiral:update(Name, Value),
    ok;
notify(Name, Value, spiral, false) ->
    add_handler(spiral, Name),
    folsom_metrics_spiral:update(Name, Value),
    ok;
notify(_, _, Type, _) ->
    {error, Type, unsupported_metric_type}.

