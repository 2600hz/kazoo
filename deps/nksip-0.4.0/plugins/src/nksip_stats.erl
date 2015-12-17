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

%% @doc NkSIP Stats Plugin
%% This is a (yet) very simple stats collection plugin
-module(nksip_stats).

-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([info/0, get_uas_avg/0, response_time/1]).
-export([version/0, deps/0, parse_config/1, init/2, terminate/2]).

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").




% ===================================================================
%% Plugin specific
%% ===================================================================

%% @doc Version
-spec version() ->
    string().

version() ->
    "0.1".


%% @doc Dependant plugins
-spec deps() ->
    [{atom(), string()}].
    
deps() ->
    [].


%% @doc Parses this plugin specific configuration
-spec parse_config(nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.


parse_config(Opts) ->
    case nksip_config:get(nksip_stats_period) of
        undefined ->
            {ok, Opts};
        Period when is_integer(Period), Period>0 ->
            {ok, Opts};
        _ ->
            {error, {invalid_global_config, nksip_stats_period}}
    end.


%% @doc Called when the plugin is started 
-spec init(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_siapp_srv:state()}.

init(_AppId, SipAppState) ->
    case whereis(nksip_stats_srv) of
        undefined ->
            Child = {
                nksip_stats_srv,
                {nksip_stats_srv, start_link, []},
                permanent,
                5000,
                worker,
                [nksip_stats_srv]
            },
            {ok, _Pid} = supervisor:start_child(nksip_sup, Child);
        _ ->
            ok
    end,
    {ok, SipAppState}.



%% @doc Called when the plugin is shutdown
-spec terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_sipapp_srv:state()}.

terminate(_AppId, SipAppState) ->  
    % We don't remove nksip_stats_srv, in case other SipApp is using it
    {ok, SipAppState}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets some statistics about current number of calls, dialogs, queues, etc.
-spec info() ->
    nksip:optslist().

info() ->
    [
        {calls, nksip_counters:value(nksip_calls)},
        {dialogs, nksip_counters:value(nksip_dialogs)},
        {routers_queue, nksip_router:pending_msgs()},
        {routers_pending, nksip_router:pending_work()},
        {connections, nksip_counters:value(nksip_connections)},
        {counters_queue, nksip_counters:pending_msgs()},
        {core_queues, nksip_sipapp_srv:pending_msgs()},
        {uas_response, nksip_stats:get_uas_avg()}
    ].


%% @doc Gets the call statistics for the current period.
-spec get_uas_avg() ->
    {Min::integer(), Max::integer(), Avg::integer(), Std::integer()}.

get_uas_avg() ->
    gen_server:call(nksip_stats_srv, get_uas_avg).


%% @private Informs the module about the last response time
-spec response_time(nksip_lib:l_timestamp()) ->
    ok.

response_time(Time) when is_number(Time) ->
    gen_server:cast(nksip_stats_srv, {response_time, Time}).



