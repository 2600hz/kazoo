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

%% @private NkSIP webserver supervisor
-module(nksip_webserver_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(supervisor).

-export([start_child/1, terminate_child/1, get_all/0, terminate_all/0]).
-export([init/1, start_link/0]).

-include("nksip.hrl").


%% @private
start_child(Spec) ->
    case supervisor:start_child(?MODULE, Spec) of
        {ok, Pid} -> {ok, Pid};
        {error, {Error, _}} -> {error, Error};
        {error, Error} -> {error, Error}
    end.


%% @private
terminate_child(Ref) ->
    case supervisor:terminate_child(?MODULE, Ref) of
        ok -> supervisor:delete_child(?MODULE, Ref);
        {error, Reason} -> {error, Reason}
    end.

%% @private
terminate_all() ->
	lists:foreach(fun(Ref) -> terminate_child({ranch_listener_sup, Ref}) end, get_all()).


%% @private
get_all() ->
    [Ref || {{_, Ref}, _, _, _} <- supervisor:which_children(?MODULE)].


%% @private
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, []}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.




