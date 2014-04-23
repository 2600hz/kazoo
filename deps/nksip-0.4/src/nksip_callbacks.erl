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

%% @doc SipApp plugin callbacks default implementation

-module(nksip_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

% -include("nksip.hrl").
-compile([export_all]).

callback1() ->
	io:format("NKSIP: CALLBACK1\n"),
    ok1.

callback2(A) ->
	io:format("NKSIP: CALLBACK2(~p)\n", [A]),
    A.

callback3(A, B, C) ->
	io:format("NKSIP: CALLBACK3(~p, ~p, ~p)\n", [A, B, C]),
	{A,B,C}.
