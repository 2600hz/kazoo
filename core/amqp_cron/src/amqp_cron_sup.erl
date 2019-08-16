%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-, Jeremy Raymond
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Jeremy Raymond <jeraymond@gmail.com>
%%%
%%% @doc The {@link amqp_cron} supervisor.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_cron_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_ARGS('amqp_cron', [Nodes])]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the amqp_cron supervisor with the given node list. See
%% {@link amqp_cron:start_link/1}.
%%
%% @end
%%------------------------------------------------------------------------------

-spec start_link([node()]) -> kz_types:startlink_ret().

start_link(Nodes) ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, [Nodes]).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec init([node()]) -> kz_types:sup_init_ret().

init([]) ->
    {error, no_node_list};
init([Nodes]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
