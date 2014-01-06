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
%%% File:      folsom_utils.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% various util functions
%%% @end
%%%------------------------------------------------------------------
-module(folsom_utils).

-export([
         to_atom/1,
         convert_tags/1,
         now_epoch/0,
         now_epoch/1,
         now_epoch_micro/0,
         timestamp/0,
         get_ets_size/1,
         update_counter/3
        ]).

to_atom(Binary) when is_binary(Binary) ->
    list_to_atom(binary_to_list(Binary));
to_atom(List) when is_list(List) ->
    list_to_atom(List).

convert_tags(Tags) ->
    [to_atom(Tag) || Tag <- Tags].

now_epoch() ->
    now_epoch(os:timestamp()).

now_epoch({Mega, Sec, _}) ->
    (Mega * 1000000 + Sec).

now_epoch_micro() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

%% useful because you can't meck os:timestamp for some reason
timestamp() ->
    os:timestamp().

get_ets_size(Tab) ->
    ets:info(Tab, size).

%% @doc
%% Same as {@link ets:update_counter/3} but inserts `{Key, Value}' if object
%% is missing in the table.
update_counter(Tid, Key, Value) when is_integer(Value) ->
    %% try to update the counter, will badarg if it doesn't exist
    try ets:update_counter(Tid, Key, Value) of
        Res ->
            Res
    catch
        error:badarg ->
            %% row didn't exist, create it
            %% use insert_new to avoid races
            case ets:insert_new(Tid, {Key, Value}) of
                true ->
                    Value;
                false ->
                    %% someone beat us to it
                    ets:update_counter(Tid, Key, Value)
            end
    end.
