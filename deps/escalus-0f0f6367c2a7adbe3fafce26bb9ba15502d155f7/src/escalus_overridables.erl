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

-module(escalus_overridables).

-export([do/4, override/3]).

-spec do([{atom(), term()}], atom(), [term()], {atom(), atom()}) -> term().
do(Config, OverrideName, Args, Default) ->
    case get_mf(Config, OverrideName, Default) of
        {Mod, Fun} ->
            apply(Mod, Fun, Args);
        Fun ->
            apply(Fun, Args)
    end.


-spec override([{atom(), term()}], atom(), {atom(), atom()}) -> [{atom(), term()}].
override(Config, OverrideName, NewValue) ->
    OldOverrides = escalus_config:get_config(escalus_overrides, Config, []),
    NewOverrides = lists:keystore(OverrideName, 1, OldOverrides, {OverrideName, NewValue}),
    lists:keystore(escalus_overrides, 1, Config, {escalus_overrides, NewOverrides}).

%%==============================================================================
%% Helpers
%%==============================================================================

get_mf(Config, OverrideName, Default) ->
    case escalus_config:get_config(escalus_overrides, Config) of
        undefined ->
            Default;
        Hooks ->
            proplists:get_value(OverrideName, Hooks, Default)
    end.
