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
-module(entop).

-author('mazen.harake@erlang-solutions.com').

-include("entop.hrl").
-include_lib("cecho/include/cecho.hrl").

%% Application API
-export([start/1]).

%% =============================================================================
%% Application API
%% =============================================================================
start(Node) ->
    State = #state{ node = Node },
    case net_kernel:connect(Node) of
	true ->
	    ViewPid = entop_view:start(State#state{ connected = true }),
	    control(ViewPid);
	false ->
	    halt(101)
    end.

control(ViewPid) ->
    P = cecho:getch(),
    case P of
	N when N >= 49 andalso N =< 57 -> ViewPid ! {sort, N - 48}, control(ViewPid);
	$> -> ViewPid ! {sort, next}, control(ViewPid);
	$< -> ViewPid ! {sort, prev}, control(ViewPid);
	$r -> ViewPid ! reverse_sort, control(ViewPid);
	$q -> do_exit(ViewPid);
	3 -> do_exit(ViewPid); %Ctrl-C
	_ -> ViewPid ! force_update, control(ViewPid)
    end.

do_exit(ViewPid) ->
    exit(ViewPid, normal), 
    application:stop(cecho),
    halt().

