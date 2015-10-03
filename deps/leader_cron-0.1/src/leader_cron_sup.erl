%%==============================================================================
%% Copyright 2012 Jeremy Raymond
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

%%%-------------------------------------------------------------------
%%% @author Jeremy Raymond <jeraymond@gmail.com>
%%% @copyright (C) 2012, Jeremy Raymond
%%% @doc
%%% The {@link leader_cron} supervisor.
%%% @see leader_cron
%%%
%%% @end
%%% Created : 31 Jan 2012 by Jeremy Raymond <jeraymond@gmail.com>
%%%-------------------------------------------------------------------
-module(leader_cron_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the leader_cron supervisor with the given node list. See
%% {@link leader_cron:start_link/1}.
%%
%% @end
%%--------------------------------------------------------------------

-spec start_link([node()]) -> ignore | {error, term()} | {ok, pid()}.

start_link(Nodes) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Nodes]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->
    {error, no_node_list};
init([Nodes]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    LeaderCron = {leader_cron, {leader_cron, start_link, [Nodes]},
		  Restart, Shutdown, Type, [leader_cron]},

    {ok, {SupFlags, [LeaderCron]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
