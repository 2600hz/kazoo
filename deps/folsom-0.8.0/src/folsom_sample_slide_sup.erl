%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Basho Technologies, Inc.
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
%%%-------------------------------------------------------------------
%%% File:      folsom_sample_slide_sup.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc
%%% Starts simple_one_for_one children per slide sample
%%% @end
%%%-----------------------------------------------------------------
-module(folsom_sample_slide_sup).
-behaviour(supervisor).

%% beahvior functions
-export([start_link/0,
         init/1
        ]).

%% public functions
-export([start_slide_server/3]).

start_link () ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

start_slide_server(SampleMod, Reservoir, Window) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [SampleMod, Reservoir, Window]),
    Pid.

%% @private
init ([]) ->
    {ok,{{simple_one_for_one, 3, 180},
         [
          {undefined, {folsom_sample_slide_server, start_link, []},
           transient, brutal_kill, worker, [folsom_sample_slide_server]}
         ]}}.
