%%%
%%% Copyright 2012, Basho Technologies, Inc.  All Rights Reserved.
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
%%% File:      folsom_sample_slide.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc
%%% Sliding window sample. Last Window seconds readings are recorded.
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_sample_slide_uniform).

-export([
         new/1,
         update/2,
         get_values/1,
         moment/0,
         trim/2
        ]).

-include("folsom.hrl").

new({Window, SampleSize}) ->
    Sample = #slide_uniform{window = Window, size = SampleSize},
    Pid = folsom_sample_slide_sup:start_slide_server(?MODULE, Sample#slide_uniform.reservoir, Sample#slide_uniform.window),
    Sample#slide_uniform{server=Pid}.

update(#slide_uniform{reservoir = Reservoir, size = Size} = Sample0, Value) ->
    Now = folsom_utils:timestamp(),
    Moment = folsom_utils:now_epoch(Now),
    MCnt = folsom_utils:update_counter(Reservoir, Moment, 1),
    Sample = case MCnt > Size of
                 true ->
                     {Rnd, _NewSeed} = random:uniform_s(MCnt, Now),
                     maybe_update(Reservoir, {{Moment, Rnd}, Value}, Size),
                     Sample0;
                 false ->
                      ets:insert(Reservoir, {{Moment, MCnt}, Value}),
                      Sample0
    end,
    Sample.

maybe_update(Reservoir, {{_Moment, Rnd}, _Value}=Obj, Size) when Rnd =< Size ->
    ets:insert(Reservoir, Obj);
maybe_update(_Reservoir, _Obj, _Size) ->
    ok.

get_values(#slide_uniform{window = Window, reservoir = Reservoir}) ->
    Oldest = moment() - Window,
    ets:select(Reservoir, [{{{'$1', '_'},'$2'},[{'>=', '$1', Oldest}],['$2']}]).

moment() ->
    folsom_utils:now_epoch().

trim(Reservoir, Window) ->
    Oldest = moment() - Window,
    ets:select_delete(Reservoir, [{{{'$1', '_'},'_'},[{'<', '$1', Oldest}],['true']}]),
    %% and trim the counters
    ets:select_delete(Reservoir, [{{'$1','_'},[{is_integer, '$1'}, {'<', '$1', Oldest}],['true']}]).
