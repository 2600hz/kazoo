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

%% @doc Plugin implementing automatic registrations and pings support for SipApps.
-module(nksip_uac_auto_outbound).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start_register/4, stop_register/2, get_registers/1]).
-export([version/0, deps/0, parse_config/1, init/2, terminate/2]).

-include("nksip_uac_auto_outbound.hrl").


%% ===================================================================
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
    [{nksip_uac_auto_register, "^0\."}, {nksip_outbound, "^0\."}].


%% @doc Parses this plugin specific configuration
-spec parse_config(nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.

parse_config(Opts) ->
    Defaults = [
        {nksip_uac_auto_outbound_all_fail, 30},
        {nksip_uac_auto_outbound_any_ok, 90},  
        {nksip_uac_auto_outbound_max_time, 1800},
        {nksip_uac_auto_outbound_default_udp_ttl, 25},
        {nksip_uac_auto_outbound_default_tcp_ttl, 120}
    ],
    Opts1 = nksip_lib:defaults(Opts, Defaults),
    try
        case nksip_lib:get_value(nksip_uac_auto_outbound_all_fail, Opts1) of
            undefined -> ok;
            AllFail when is_integer(AllFail) andalso AllFail>=1 -> 
                ok;
            _ -> 
                throw(nksip_uac_auto_outbound_all_fail)
        end,
        case nksip_lib:get_value(nksip_uac_auto_outbound_any_ok, Opts1) of
            undefined -> ok;
            AnyOk when is_integer(AnyOk) andalso AnyOk>=1 -> 
                ok;
            _ -> 
                throw(nksip_uac_auto_outbound_any_ok)
        end,
        case nksip_lib:get_value(nksip_uac_auto_outbound_max_time, Opts1) of
            Max when is_integer(Max) andalso Max>=1 -> 
                ok;
            _ -> 
                throw(nksip_uac_auto_outbound_max_time)
        end,
        case nksip_lib:get_value(nksip_uac_auto_outbound_default_udp_ttl, Opts1) of
            Udp when is_integer(Udp) andalso Udp>=1 -> 
                ok;
            _ -> 
                throw(nksip_uac_auto_outbound_default_udp_ttl)
        end,
        case nksip_lib:get_value(nksip_uac_auto_outbound_default_tcp_ttl, Opts1) of
            Tcp when is_integer(Tcp) andalso Tcp>=1 -> 
                ok;
            _ -> 
                throw(nksip_uac_auto_outbound_default_tcp_ttl)
        end,
        {ok, Opts1}
    catch
        throw:Name -> {error, {invalid_config, Name}}
    end.



%% @doc Called when the plugin is started 
-spec init(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_siapp_srv:state()}.

init(AppId, SipAppState) ->
    Supported = AppId:config_supported(),
    StateOb = #state_ob{
        outbound = lists:member(<<"outbound">>, Supported),
        pos = 1,
        regs = []
    },
    SipAppState1 = nksip_sipapp_srv:set_meta(nksip_uac_auto_outbound, StateOb, SipAppState),
    {ok, SipAppState1}.


%% @doc Called when the plugin is shutdown
-spec terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_sipapp_srv:state()}.

terminate(_AppId, SipAppState) ->  
    #state_ob{regs=RegsOb} = 
        nksip_sipapp_srv:get_meta(nksip_uac_auto_outbound, SipAppState),
    lists:foreach(
        fun(#sipreg_ob{conn_monitor=Monitor, conn_pid=Pid}) -> 
            case is_reference(Monitor) of
                true -> erlang:demonitor(Monitor);
                false -> ok
            end, 
            case is_pid(Pid) of
                true -> nksip_connection:stop_refresh(Pid);
                false -> ok
            end
        end,
        RegsOb),
    SipAppState1 = nksip_sipapp_srv:set_meta(nksip_uac_auto_outbound, undefined, 
                                             SipAppState),
    {ok, SipAppState1}.




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Starts a new registration serie.
-spec start_register(nksip:app_name()|nksip:app_id(), term(), nksip:user_uri(),                 nksip:optslist()) -> 
    {ok, boolean()} | {error, term()}.

start_register(App, RegId, Uri, Opts) when is_list(Opts) ->
    Opts1 = [{user, ['$nksip_uac_auto_outbound']}|Opts],
    nksip_uac_auto_register:start_register(App, RegId, Uri, Opts1).


%% @doc Stops a previously started registration serie.
-spec stop_register(nksip:app_name()|nksip:app_id(), term()) -> 
    ok | not_found.

stop_register(App, RegId) ->
    nksip_uac_auto_register:stop_register(App, RegId).
    

%% @doc Get current registration status.
-spec get_registers(nksip:app_name()|nksip:app_id()) -> 
    [{RegId::term(), OK::boolean(), Time::non_neg_integer()}].
 
get_registers(App) ->
    nksip:call(App, '$nksip_uac_auto_outbound_get_registers').

    
