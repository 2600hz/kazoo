%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ Management Console.
%%
%%   The Initial Developer of the Original Code is GoPivotal, Inc.
%%   Copyright (c) 2010-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_agent_sup).

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

init([]) ->
    ExternalStats = {rabbit_mgmt_external_stats,
                     {rabbit_mgmt_external_stats, start_link, []},
                     permanent, 5000, worker, [rabbit_mgmt_external_stats]},
    {ok, {{one_for_one, 10, 10}, [ExternalStats]}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
