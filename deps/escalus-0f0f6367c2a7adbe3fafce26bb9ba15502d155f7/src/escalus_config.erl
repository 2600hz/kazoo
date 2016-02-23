%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(escalus_config).

%% Public API
-export([get_config/2,
         get_config/3,
         get_config/4,
         get_config/5,
         get_ct/1]).

%% Public Types
-type key() :: atom().
-export_type([key/0]).

-type entry() :: {key(), any()}.
-export_type([entry/0]).

-type config() :: [entry()].
-export_type([config/0]).

-type key_path() :: key() | {key(), key()} | {key(), key(), key()}.
-export_type([key_path/0]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec get_config(key(), config()) -> any().
get_config(Option, Config) ->
    get_config(Option, Config, undefined).

-spec get_config(key(), config(), any()) -> any().
get_config(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} ->
            Value;
        false ->
            case escalus_ct:get_config(Option) of
                undefined ->
                    Default;
                Value ->
                    Value
            end
    end.

-spec get_config(key(), escalus_users:user_spec(), key(), config()) -> any().
get_config(USName, UserSpec, CName, Config) ->
    get_config(USName, UserSpec, CName, Config, undefined).

-spec get_config(key(), escalus_users:user_spec(), key(), config(), any())
    -> any().
get_config(USName, UserSpec, CName, Config, Default) ->
    case lists:keyfind(USName, 1, UserSpec) of
        {USName, Value} ->
            Value;
        false ->
            get_config(CName, Config, Default)
    end.

-spec get_ct(key_path()) -> any().
get_ct(Required) when is_atom(Required) ->
    escalus_ct:get_config(Required);
get_ct(Required) when is_tuple(Required) ->
    TopList = escalus_ct:get_config(erlang:element(1, Required)),
    get_ct_recurse(Required, 2, TopList).

-spec get_ct_recurse(key_path(), pos_integer(), config()) -> any().
get_ct_recurse(Required, Pos, LevelVal) when Pos < size(Required) ->
    Key = erlang:element(Pos, Required),
    {_, NewLevelVal} = lists:keyfind(Key, 1, LevelVal),
    get_ct_recurse(Required, Pos+1, NewLevelVal);

get_ct_recurse(Required, Pos, LevelVal) when Pos == size(Required) ->
    Key = erlang:element(Pos, Required),
    {_, NewLevelVal} = lists:keyfind(Key, 1, LevelVal),
    NewLevelVal.
