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
%%% File:      folsom_sample_exdec.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% erlang implementation of a exponentially-decaying random sample
%%% based on a java implementation by coda hale, which can be found at:
%%%
%%% https://github.com/codahale/metrics/blob/development/src/main/java/com/yammer/metrics/core/ExponentiallyDecayingSample.java
%%%
%%% that implementation is based on:
%%%
%%% http://dimacs.rutgers.edu/~graham/pubs/papers/fwddecay.pdf
%%% @end
%%%------------------------------------------------------------------

-module(folsom_sample_exdec).

-export([
         new/2,
         update/2,
         get_values/1
        ]).

-define(HOURSECS, 3600).

-include("folsom.hrl").

new(Size, Alpha) ->
    Now = folsom_utils:now_epoch(),
    #exdec{start = Now, next = Now + ?HOURSECS, alpha = Alpha, size = Size}.

update(#exdec{reservoir = Reservoir, alpha = Alpha, start = Start, next = Next} = Sample, Value) ->
    Timestamp = folsom_utils:now_epoch(),

    % immediately see if we need to rescale
    {NewRes, NewStart, NewNext} = rescale(Reservoir, Timestamp, Next, Start, Alpha),

    % now lets update the sample if the new value is worthy
    update(Sample#exdec{reservoir = NewRes, next = NewNext, start = NewStart}, Value, Timestamp).

get_values(#exdec{reservoir = Reservoir}) ->
    {_, Values} = lists:unzip(ets:tab2list(Reservoir)),
    Values.

% internal api

update(#exdec{reservoir = Reservoir, alpha = Alpha, start = Start, n = N, size = Size, seed = Seed} = Sample, Value, Timestamp) when N =< Size ->
    % since N is =< Size we can just add the new value to the sample

    {Rand, New_seed} = random:uniform_s(N, Seed),
    Priority = priority(Alpha, Timestamp, Start, Rand),
    true = ets:insert(Reservoir, {Priority, Value}),

    Sample#exdec{n = folsom_utils:get_ets_size(Reservoir), seed = New_seed};
update(#exdec{reservoir = Reservoir, alpha = Alpha, start = Start, n = N, seed = Seed} = Sample, Value, Timestamp) ->
    % when N is not =< Size we need to check to see if the priority of
    % the new value is greater than the first (smallest) existing priority

    {Rand, NewSeed} = random:uniform_s(N, Seed),
    Priority = priority(Alpha, Timestamp, Start, Rand),
    First = ets:first(Reservoir),

    update_on_priority(Sample, First, Priority, NewSeed, Value).

update_on_priority(#exdec{reservoir = Reservoir} = Sample, First, Priority, NewSeed, Value) when First < Priority ->
    true = case ets:insert_new(Reservoir, {Priority, Value}) of
        true ->
            % priority didnt already exist, so we created it and need to delete the first one
            ets:delete(Reservoir, First);
        false ->
            % priority existed, we dont need to do anything
            true
    end,
    Sample#exdec{n = folsom_utils:get_ets_size(Reservoir), seed = NewSeed};
update_on_priority(Sample, _, _, _, _) ->
    Sample.

% gaurd against a possible bug, T should always be =< ?HOURSECS
% also to prevent overflow issues make sure alpha is always =<
% math:log(1.79769313486231570815e+308) / 3599 = 0.19721664709457737
weight(Alpha, T) when T =< ?HOURSECS, Alpha =< 0.19721664709457737 ->
    math:exp(Alpha * T).

priority(Alpha, Time, Start, Rand) ->
    weight(Alpha, Time - Start) / Rand.

rescale(Reservoir, Now, Next, OldStart, Alpha) when Now >= Next ->
    NewStart = Now + ?HOURSECS,
    NewRes = delete_and_rescale(Reservoir, NewStart, OldStart, Alpha),
    {NewRes, Now, NewStart};
rescale(Reservoir, _, Next, Start, _) ->
    {Reservoir, Start, Next}.

delete_and_rescale(Reservoir, NewStart, OldStart, Alpha) ->
    % get the existing reservoir
    ResList = ets:tab2list(Reservoir),

    % create a new ets table to use
    NewRes = folsom_metrics_histogram_ets:new(folsom_exdec,[ordered_set, {write_concurrency, true}, public]),

    % populate it with new priorities and the existing values
    [true = ets:insert(NewRes, {recalc_priority(Priority, Alpha, NewStart, OldStart) ,Value}) || {Priority, Value} <- ResList],

    % delete the old ets table
    true = ets:delete(Reservoir),

    % return the new ets table
    NewRes.

recalc_priority(Priority, Alpha, Start, OldStart) ->
    Priority * math:exp(-Alpha * (Start - OldStart)).
