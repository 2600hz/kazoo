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
%%% File:      folsom_metrics_history.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_history).

-export([new/1,
         update/3,
         get_events/1,
         get_events/2,
         get_value/1
        ]).

-include("folsom.hrl").

-define(ETSOPTS, [
                  ordered_set,
                  public,
                  {write_concurrency, true}
                 ]).

new(Name) ->
    Tid = ets:new(history, ?ETSOPTS),
    ets:insert(?HISTORY_TABLE, {Name, #history{tid=Tid}}),
    ok.

update(Name, Size, Value) ->
    #history{tid=Tid} = get_value(Name),
    Key = folsom_utils:now_epoch_micro(),
    insert(Tid, Key, Size, Value, ets:info(Tid, size)).

get_value(Name) ->
    [{_, Value}] = ets:lookup(?HISTORY_TABLE, Name),
    Value.

get_events(Name) ->
    get_events(Name, ?DEFAULT_LIMIT).

get_events(Name, Count) ->
    #history{tid=Tid} = get_value(Name),
    get_last_events(Tid, Count).

% Internal API

insert(Name, Key, Size, Value, Count) when Count < Size ->
    true = ets:insert(Name, {Key, [{event, Value}]});
insert(Name, Key, _, Value, _) ->
    FirstKey = ets:first(Name),
    true = ets:delete(Name, FirstKey),
    true = ets:insert(Name, {Key, [{event, Value}]}).

get_last_events(Name, Count) ->
    LastKey = ets:last(Name),
    get_prev_event(Name, LastKey, Count, []).

% get_prev_event/4 used by get_last_events/2
get_prev_event(_, '$end_of_table', _, Acc) ->
    Acc;
get_prev_event(Name, Key, Count, Acc) when length(Acc) < Count ->
    Event = ets:lookup(Name, Key),
    get_prev_event(Name, ets:prev(Name, Key), Count, lists:append(Acc, Event));
get_prev_event(_, _, _, Acc) ->
    Acc.
