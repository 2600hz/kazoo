%% -------------------------------------------------------------------
%%
%% Speed test suite
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

-module(nksip_loadtest).
-compile([export_all]).
-export([start/0, launch/1, full/0, full/1]).


%% @doc Starts a server to launch tests to.
start() ->
    nksip_loadtest_lib:start_server().


%% @doc Launches a new test. See {@link nksip_loadtest_lib:launch/1} for options.
launch(Opts) ->
    nksip_loadtest_lib:launch(Opts).


%% @doc Equivalent to `full[{messages, 10000}, {clients, 10}]'.
full() ->
    full([]).


%% @doc Performs a full load test suite, using 10.000 requests with 10 clients.
%%
%% Available options are:
%% - messages: Number of requests to send (default 10000)
%% - clients: Number of parallel clients to start (default 10)
%% - host: Use it in case the NkSIP server being tested is started at another node.
%%         Default is "127.0.0.1"
%% - port: NkSIP server listening port. 
%%         Default is 5060 for udp and tcp, and 5061 for tls
full(Opts) ->
    Messages = proplists:get_value(messages, Opts, 10000),
    Clients = proplists:get_value(clients, Opts, 10),
    nksip_loadtest_lib:start_server(),

    Opts1 = [{messages, Messages}, {clients, Clients}|Opts],
    only_uas_register(Opts1),
    only_uas_call(Opts1),
    nksip_loadtest_lib:start_clients(Clients),
    register([no_auto_start|Opts1]),
    call([no_auto_start|Opts1]),
    nksip_loadtest_lib:stop_clients(Clients),
    ok.
    

% Only UAS set of tests use the "raw" version of the load suite, using raw 
% packets as UAC, with low overhead. UDP can't be used this way.


%% @private
only_uas_register(Opts) ->
    % Raw versions don't retransmit, so UDP will have packet loss
    % {_, P1, R1} = launch([raw, register|Opts]),
    % io:format("UAS REGISTER UDP (~p% ok): ~p req/sec\n\n", [P1, R1]),
    {_, P2, R2} = launch([raw, register, tcp|Opts]),
    io:format("UAS REGISTER TCP (~p% ok): ~p req/sec\n\n", [P2, R2]),
    {_, P3, R3} = launch([raw, register, tls|Opts]),
    io:format("UAS REGISTER TLS (~p% ok): ~p req/sec\n\n\n\n", [P3, R3]).


%% @private
only_uas_call(Opts) ->
    % {_, P1, R1} = launch([raw, invite|Opts]),
    % io:format("UAS CALL UDP (~p% ok): ~p req/sec\n\n", [P1, R1]),
    {_, P2, R2} = launch([raw, invite, tcp|Opts]),
    io:format("UAS CALL TCP (~p% ok): ~p req/sec\n\n", [P2, R2]),
    {_, P3, R3} = launch([raw, invite, tls|Opts]),
    io:format("UAS CALL TLS (~p% ok): ~p req/sec\n\n\n\n", [P3, R3]).


% Full versions set of tests use UAC generation and UAS response

   
%% @private
register(Opts) ->
    {_, P4, R4} = launch([register|Opts]),
    io:format("UAC+UAS REGISTER UDP (~p% ok): ~p req/sec\n\n", [P4, R4]),
    {_, P5, R5} = launch([register, tcp|Opts]),
    io:format("UAC+UAS REGISTER TCP (~p% ok): ~p req/sec\n\n", [P5, R5]),
    {_, P6, R6} = launch([register, tls|Opts]),
    io:format("UAC+UAS REGISTER TLS (~p% ok): ~p req/sec\n\n\n\n", [P6, R6]).


%% @private
call(Opts) ->
    {_, P4, R4} = launch([invite|Opts]),
    io:format("UAC+UAS INVITE+ACK+BYE UDP (~p% ok): ~p req/sec\n\n", [P4, R4]),
    {_, P5, R5} = launch([invite, tcp|Opts]),
    io:format("UAC+UAS INVITE+ACK+BYE TCP (~p% ok): ~p req/sec\n\n", [P5, R5]),
    {_, P6, R6} = launch([invite, tls|Opts]),
    io:format("UAC+UAS INVITE+ACK+BYE TLS (~p% ok): ~p req/sec\n\n\n\n", [P6, R6]).
