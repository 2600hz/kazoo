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
%%% @doc Starts the amqp_cron application using the currently connected
%%% nodes as the node list (see {@link amqp_cron}). In general it
%%% is probably more useful to add {@link amqp_cron} or
%%% {@link amqp_cron_sup} to your own supervision tree where you
%%% can more reasonably control the node list.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_cron_app).

-behaviour(application).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    amqp_cron_sup:start_link([node()|nodes()]).

-spec stop(any()) -> 'ok'.
stop(_State) ->
    'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
