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

-module(rabbit_jsonrpc_channel_app_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ChannelFactory = {rabbit_jsonrpc_channel_factory, 
                      {rabbit_jsonrpc_channel_factory, start_link, []},
                      permanent, 10000, worker, [rabbit_jsonrpc_channel_factory]},
    ChannelSup = {rabbit_jsonrpc_channel_sup, 
                  {rabbit_jsonrpc_channel_sup, start_link, []},
                  permanent, 10000, supervisor, [rabbit_jsonrpc_channel_sup]},

    {ok, {{one_for_one, 10, 10}, [ChannelSup, ChannelFactory]}}.
