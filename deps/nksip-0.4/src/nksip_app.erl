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

%% @doc NkSIP OTP Application Module
-module(nksip_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(application).

-export([start/0, start/2, stop/1]).
-export([profile_output/0]).

-include("nksip.hrl").

-define(APP, nksip).

%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts NkSIP stand alone.
-spec start() -> 
    ok | {error, Reason::term()}.

start() ->
    case ensure_all_started(?APP, permanent) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.

%% @private OTP standard start callback
start(_Type, _Args) ->
    % application:set_env(nksip, profile, true),
    case application:get_env(nksip, profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            ok
    end,
    {ok, Pid} = nksip_sup:start_link(),
    MainIp = nksip_config:get(main_ip),
    MainIp6 = nksip_config:get(main_ip6),
    {ok, Vsn} = application:get_key(nksip, vsn),
    lager:notice("NkSIP v~s has started. Main IP is ~s (~s)", 
                    [Vsn, nksip_lib:to_host(MainIp), nksip_lib:to_host(MainIp6)]),
    {ok, Pid}.


%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @private
-spec profile_output() -> 
    ok.

profile_output() ->
    eprof:stop_profiling(),
    % eprof:log("nksip_procs.profile"),
    % eprof:analyze(procs),
    eprof:log("nksip.profile"),
    eprof:analyze(total).


%% @doc Ensure that an application and all of its transitive
%% dependencies are started.
ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
        {ok, Started} ->
            {ok, lists:reverse(Started)};
        {error, Reason, Started} ->
            [ application:stop(App) || App <- Started ],
            {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case application:start(Application, Type) of
        ok ->
            {ok, [Application | Started]};
        {error, {already_started, Application}} ->
            {ok, Started};
        {error, {not_started, Dependency}} ->
            case ensure_all_started(Dependency, Type, Started) of
                {ok, NewStarted} ->
                    ensure_all_started(Application, Type, NewStarted);
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, Reason, Started}
    end.
