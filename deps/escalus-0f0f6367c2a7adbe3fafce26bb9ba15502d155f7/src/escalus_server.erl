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
-module(escalus_server).

%% API
-export([pre_story/1]).
-export([post_story/1]).

-export([name/1]).

-callback pre_story(escalus:config()) -> escalus:config().
-callback post_story(escalus:config()) -> escalus:config().
-callback name() -> atom().

-spec pre_story(escalus:config()) -> escalus:config().
pre_story(Config) ->
    call_server(get_server(Config), pre_story, [Config]).

-spec post_story(escalus:config()) -> escalus:config().
post_story(Config) ->
    call_server(get_server(Config), post_story, [Config]).

name(Config) ->
    call_server(get_server(Config), name, []).

get_server(Config) ->
    escalus_config:get_config(escalus_xmpp_server, Config, undefined).

call_server(undefined, Method, Args) ->
    call_undef_server(Method, Args);
call_server(ServerModule, Method, Args) ->
    erlang:apply(ServerModule, Method, Args).

%% behaviour implementation for udefined server

call_undef_server(pre_story, [Config]) ->
    Config;
call_undef_server(post_story, [Config]) ->
    Config;
call_undef_server(name, _) ->
    unknown.

