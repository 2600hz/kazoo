%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
%%

-module(rabbit_client_sup).

-behaviour(supervisor2).

-export([start_link/1, start_link/2]).

-export([init/1]).

-include("rabbit.hrl").

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-spec(start_link/1 :: (rabbit_types:mfargs()) ->
                           rabbit_types:ok_pid_or_error()).
-spec(start_link/2 :: ({'local', atom()}, rabbit_types:mfargs()) ->
                           rabbit_types:ok_pid_or_error()).

-endif.

%%----------------------------------------------------------------------------

start_link(Callback) ->
    supervisor2:start_link(?MODULE, Callback).

start_link(SupName, Callback) ->
    supervisor2:start_link(SupName, ?MODULE, Callback).

init({M,F,A}) ->
    {ok, {{simple_one_for_one_terminate, 0, 1},
          [{client, {M,F,A}, temporary, infinity, supervisor, [M]}]}}.
