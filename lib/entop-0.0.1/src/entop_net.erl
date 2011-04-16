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
-module(entop_net).

-author('mazen.harake@erlang-solutions.com').

-include("entop.hrl").

%% Module API
-export([fetch_data/2, reconnect/2]).

%% =============================================================================
%% Module API
%% =============================================================================
fetch_data(Node, Module) ->
    timer:tc(rpc, call, [Node, Module, get_data, []]).

reconnect(Parent, Node) ->
    case net_kernel:connect(Node) of
	true ->
	    Parent ! {nodeup, Node};
	false ->
	    timer:sleep(1000),
	    reconnect(Parent, Node)
    end.
	
