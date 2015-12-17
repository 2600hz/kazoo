%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkSIP Deep Debug plugin
-module(nksip_debug).

-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-compile({no_auto_import, [get/1, put/2]}).

-export([start/1, stop/1, print/1, print_all/0]).
-export([insert/2, insert/3, find/1, find/2, dump_msgs/0, reset_msgs/0]).
-export([version/0, deps/0, init/2, terminate/2]).

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").


% ===================================================================
%% Plugin specific
%% ===================================================================

%% @doc Version
-spec version() ->
    string().

version() ->
    "0.1".


%% @doc Dependant plugins
-spec deps() ->
    [{atom(), string()}].
    
deps() ->
    [].


% %% @doc Parses this plugin specific configuration
% -spec parse_config(nksip:optslist()) ->
%     {ok, nksip:optslist()} | {error, term()}.

% parse_config(Opts) ->
%     case nksip_lib:get_value(nksip_debug, Opts, false) of
%         Trace when is_boolean(Trace) ->
%             Cached1 = nksip_lib:get_value(cached_configs, Opts, []),
%             Cached2 = nksip_lib:store_value(config_nksip_debug, Trace, Cached1),
%             Opts1 = nksip_lib:store_value(cached_configs, Cached2, Opts),
%             {ok, Opts1};
%         _ ->
%             {error, {invalid_config, nksip_debug}}
%     end.


%% @doc Called when the plugin is started 
-spec init(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_siapp_srv:state()}.

init(_AppId, SipAppState) ->
    case whereis(nksip_debug_srv) of
        undefined ->
            Child = {
                nksip_debug_srv,
                {nksip_debug_srv, start_link, []},
                permanent,
                5000,
                worker,
                [nksip_debug_srv]
            },
            {ok, _Pid} = supervisor:start_child(nksip_sup, Child);
        _ ->
            ok
    end,
    {ok, SipAppState}.



%% @doc Called when the plugin is shutdown
-spec terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_sipapp_srv:state()}.

terminate(_AppId, SipAppState) ->  
    {ok, SipAppState}.



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Configures a SipApp to start debugging
-spec start(nksip:app_id()|nksip:app_name()) ->
    ok | {error, term()}.

start(App) ->
    case nksip:find_app_id(App) of
        {ok, AppId} ->
            Plugins1 = AppId:config_plugins(),
            Plugins2 = nksip_lib:store_value(nksip_debug, Plugins1),
            case nksip:update(AppId, [{plugins, Plugins2}, {debug, true}]) of
                {ok, _} -> ok;
                {error, Error} -> {error, Error}
            end;
        not_found ->
            {error, sipapp_not_found}
    end.


%% @doc Stop debugging in a specific SipApp
-spec stop(nksip:app_id()|nksip:app_name()) ->
    ok | {error, term()}.

stop(App) ->
    case nksip:find_app_id(App) of
        {ok, AppId} ->
            Plugins = AppId:config_plugins() -- [nksip_debug],
            case nksip:update(App, [{plugins, Plugins}, {debug, false}]) of
                {ok, _} -> ok;
                {error, Error} -> {error, Error}
            end;
        not_found ->
            {error, sipapp_not_found}
    end.    



%% ===================================================================
%% Internal
%% ===================================================================



%% @private
insert(#sipmsg{app_id=AppId, call_id=CallId}, Info) ->
    insert(AppId, CallId, Info).


%% @private
insert(AppId, CallId, Info) ->
    Time = nksip_lib:l_timestamp(),
    Info1 = case Info of
        {Type, Str, Fmt} when Type==debug; Type==info; Type==notice; 
                              Type==warning; Type==error ->
            {Type, nksip_lib:msg(Str, Fmt)};
        _ ->
            Info
    end,
    AppName = AppId:name(),
    catch ets:insert(nksip_debug_msgs, {CallId, Time, AppName, Info1}).


%% @private
find(CallId) ->
    Lines = lists:sort([{Time, AppId, Info} || {_, Time, AppId, Info} 
                         <- ets:lookup(nksip_debug_msgs, nksip_lib:to_binary(CallId))]),
    [{nksip_lib:l_timestamp_to_float(Time), AppId, Info} 
        || {Time, AppId, Info} <- Lines].


%% @private
find(AppId, CallId) ->
    [{Start, Info} || {Start, C, Info} <- find(CallId), C==AppId].


%% @private
print(CallId) ->
    [{Start, _, _}|_] = Lines = find(CallId),
    lists:foldl(
        fun({Time, App, Info}, Acc) ->
            io:format("~f (~f, ~f) ~p\n~p\n\n\n", 
                      [Time, (Time-Start)*1000, (Time-Acc)*1000, App, Info]),
            Time
        end,
        Start,
        Lines).


%% @private
print_all() ->
    [{_, Start, _, _}|_] = Lines = lists:keysort(2, dump_msgs()),
    lists:foldl(
        fun({CallId, Time, App, Info}, Acc) ->
            io:format("~p (~p, ~p) ~s ~p\n~p\n\n\n", 
                      [Time, (Time-Start), (Time-Acc), CallId, App, Info]),
            Time
        end,
        Start,
        Lines).


%% @private
dump_msgs() ->
    ets:tab2list(nksip_debug_msgs).


%% @private
reset_msgs() ->
    ets:delete_all_objects(nksip_debug_msgs).
