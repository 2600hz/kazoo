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
%%   Copyright (c) 2011-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_sup_sup).

%% We want there to be one management database in the cluster, with a
%% globally registered name. So we use mirrored_supervisor for
%% failover (in rabbit_mgmt_sup) and register a global name for the
%% database.
%%
%% Unfortunately it's more complicated than using these things
%% naively. The first problem is that on failover the mirrored
%% supervisor might move the DB to a new node before the global name
%% database notices and removes the old record. In that case starting
%% the new database will fail.
%%
%% The second problem is that after a network partition things get
%% worse. Since mirrored_supervisor uses Mnesia for global shared
%% state, we have effectively two (or more) mirrored_supervisors. But
%% the global name database does not do this, so at least one of them
%% cannot start the management database; so the mirrored supervisor
%% has to die. But what if the admin restarts the partition which
%% contains the management DB? In that case we need to start a new
%% management DB in the winning partition.
%%
%% Rather than try to get mirrored_supervisor to handle this
%% post-partition state we go for a simpler approach: allow the whole
%% mirrored_supervisor to die in the two edge cases above, and
%% whenever we want to call into the mgmt DB we will start it up if it
%% appears not to be there. See rabbit_mgmt_db:safe_call/3 for the
%% code which restarts the DB if necessary.

-behaviour(supervisor2).

-export([start_link/0, start_child/0]).
-export([init/1]).

-include_lib("rabbit_common/include/rabbit.hrl").

start_link() -> supervisor2:start_link({local, ?MODULE}, ?MODULE, []).

start_child() -> supervisor2:start_child( ?MODULE, sup()).

%%----------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_one, 0, 1}, [sup()]}}.

sup() ->
    {rabbit_mgmt_sup, {rabbit_mgmt_sup, start_link, []},
     temporary, ?MAX_WAIT, supervisor, [rabbit_mgmt_sup]}.
