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
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_connection_helper_sup).

-behaviour(supervisor2).

-export([start_link/0]).
-export([start_channel_sup_sup/1,
         start_queue_collector/1]).

-export([init/1]).

-include("rabbit.hrl").

%%----------------------------------------------------------------------------

-ifdef(use_specs).
-spec(start_link/0 :: () -> rabbit_types:ok_pid_or_error()).
-spec(start_channel_sup_sup/1 :: (pid()) -> rabbit_types:ok_pid_or_error()).
-spec(start_queue_collector/1 :: (pid()) -> rabbit_types:ok_pid_or_error()).
-endif.

%%----------------------------------------------------------------------------

start_link() ->
    supervisor2:start_link(?MODULE, []).

start_channel_sup_sup(SupPid) ->
    supervisor2:start_child(
          SupPid,
          {channel_sup_sup, {rabbit_channel_sup_sup, start_link, []},
           intrinsic, infinity, supervisor, [rabbit_channel_sup_sup]}).

start_queue_collector(SupPid) ->
    supervisor2:start_child(
      SupPid,
      {collector, {rabbit_queue_collector, start_link, []},
       intrinsic, ?MAX_WAIT, worker, [rabbit_queue_collector]}).

%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

