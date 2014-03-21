%% @author Kirill Trofimov <sinnus@gmail.com>
%% @copyright 2012 Kirill Trofimov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(stress_test).
-author('Kirill Trofimov <sinnus@gmail.com>').
-export([start/0]).

start() ->
    ok = application:start(sasl),
    ok = application:start(stress_test),
    {ok, _} = ibrowse:start(),

    {ok, _} = stress_mgr:start_link(),

    {ok, Host} = application:get_env(stress_test, host),
    {ok, Port} = application:get_env(stress_test, port),
    {ok, N} = application:get_env(stress_test, clients),

    ibrowse:set_max_sessions(Host, Port, N),
    ibrowse:set_max_pipeline_size(Host, Port, N),

    SocketIoUrl = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/socket.io",

    error_logger:info_msg("***** Starting stress clients. Url ~p~n", [SocketIoUrl]),
    ok = start_stress_clients(SocketIoUrl, N),
    error_logger:info_msg("***** ~p stress clients started~n", [N]).

start_stress_clients(_, 0) ->
    ok;

start_stress_clients(SocketIoUrl, N) ->
    {ok, _} = stress_client:start_link(SocketIoUrl),
    start_stress_clients(SocketIoUrl, N - 1).
