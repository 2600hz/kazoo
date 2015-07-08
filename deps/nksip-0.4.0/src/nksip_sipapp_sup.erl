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

%% @private Core Supervisor Module.
%%
%% When a new SipApp's process is started, a new supervisor `{nksip_sipapp_sup, AppId}' is
%% added to the main NkSIP supervisor. 
%% Under this core supervisor, a new supervisor `transports_sup' is added 
%% to control the transport processes, and finally the core main process is also added.

-module(nksip_sipapp_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(supervisor).

-export([start_link/2, init/1]).

-include("nksip.hrl").


%% @private
start_link(AppId, Args) ->
    {ok, SupPid} = supervisor:start_link(?MODULE, [AppId, {{one_for_one, 10, 60}, []}]),
    Childs = [
        {nksip_transport_sup,
            {nksip_transport_sup, start_link, [AppId]},
            permanent,
            infinity,
            supervisor,
            [nksip_transport_sup]
        },
        {server,
            {nksip_sipapp_srv, start_link, [AppId, Args]},
            permanent,
            30000,
            worker,
            [nksip_sipapp_srv]
        }
    ],
    start_childs(SupPid, Childs).


%% @private
start_childs(SupPid, []) ->
    {ok, SupPid};
start_childs(SupPid, [ChildSpec|Rest]) ->
    case supervisor:start_child(SupPid, ChildSpec) of
        {ok, _}  -> 
            start_childs(SupPid, Rest);
        {error, {Error, _}} ->
            exit(SupPid, shutdown),
            {error, Error};
        {error, Error} ->
            exit(SupPid, shutdown),
            {error, Error}
    end.


%% @private
init([AppId, ChildsSpec]) ->
    % The SipApp ETS table is associated to its supervisor to avoid losing it
    % in case of process fail 
    ets:new(AppId, [named_table, public]),
    yes = nksip_proc:register_name({nksip_sipapp_sup, AppId}, self()),
    {ok, ChildsSpec}.


