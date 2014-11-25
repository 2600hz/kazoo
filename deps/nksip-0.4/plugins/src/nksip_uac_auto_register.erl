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
-module(nksip_uac_auto_register).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start_ping/4, stop_ping/2, get_pings/1]).
-export([start_register/4, stop_register/2, get_registers/1]).
-export([version/0, deps/0, parse_config/1, init/2, terminate/2]).

-include("nksip_uac_auto_register.hrl").


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
    [].


%% @doc Parses this plugin specific configuration
-spec parse_config(nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.

parse_config(Opts) ->
    Defaults = [{nksip_uac_auto_register_timer, 5}],                   % (secs)
    Opts1 = nksip_lib:defaults(Opts, Defaults),
    case nksip_lib:get_value(nksip_uac_auto_register_timer, Opts1) of
        Timer when is_integer(Timer), Timer>0 -> 
            {ok, Opts1};
        _ -> 
            {error, {invalid_config, nksip_uac_auto_register_timer}}
    end.


%% @doc Called when the plugin is started 
-spec init(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_siapp_srv:state()}.

init(AppId, SipAppState) ->
    Timer = 1000 * nksip_sipapp_srv:config(AppId, nksip_uac_auto_register_timer),
    erlang:start_timer(Timer, self(), '$nksip_uac_auto_register_timer'),
    State = #state{pings=[], regs=[]},
    SipAppState1 = nksip_sipapp_srv:set_meta(nksip_uac_auto_register, State, SipAppState),
    {ok, SipAppState1}.


%% @doc Called when the plugin is shutdown
-spec terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
   {ok, nksip_sipapp_srv:state()}.

terminate(AppId, SipAppState) ->  
    #state{regs=Regs} = nksip_sipapp_srv:get_meta(nksip_uac_auto_register, SipAppState),
    lists:foreach(
        fun(#sipreg{ok=Ok}=Reg) -> 
            case Ok of
                true -> 
                    AppId:nkcb_uac_auto_register_launch_unregister(Reg, true, SipAppState);
                false ->
                    ok
            end
        end,
        Regs),
    SipAppState1 = nksip_sipapp_srv:set_meta(nksip_uac_auto_register, undefined, SipAppState),
    {ok, SipAppState1}.




%% ===================================================================
%% Public
%% ===================================================================


%% @doc Starts a new registration serie.
-spec start_register(nksip:app_name()|nksip:app_id(), term(), nksip:user_uri(), 
                     nksip:optslist()) -> 
    {ok, boolean()} | {error, term()}.

start_register(App, RegId, Uri, Opts) when is_list(Opts) ->
    try
        case nksip:find_app_id(App) of
            {ok, AppId} -> ok;
            _ -> AppId = throw(invalid_app)
        end,
        case lists:keymember(meta, 1, Opts) of
            true -> throw(meta_not_allowed);
            false -> ok
        end,
        case nksip_call_uac_make:make(AppId, 'REGISTER', Uri, Opts) of
            {ok, _, _} -> ok;
            {error, MakeError} -> throw(MakeError)
        end,
        Msg = {'$nksip_uac_auto_register_start_register', RegId, Uri, Opts},
        nksip:call(App, Msg)
    catch
        throw:Error -> {error, Error}
    end.


%% @doc Stops a previously started registration serie.
-spec stop_register(nksip:app_name()|nksip:app_id(), term()) -> 
    ok | not_found.

stop_register(App, RegId) ->
    nksip:call(App, {'$nksip_uac_auto_register_stop_register', RegId}).
    

%% @doc Get current registration status.
-spec get_registers(nksip:app_name()|nksip:app_id()) -> 
    [{RegId::term(), OK::boolean(), Time::non_neg_integer()}].
 
get_registers(App) ->
    nksip:call(App, '$nksip_uac_auto_register_get_registers').



%% @doc Starts a new automatic ping serie.
-spec start_ping(nksip:app_name()|nksip:app_id(), term(), nksip:user_uri(), 
                 nksip:optslist()) -> 
    {ok, boolean()} | {error, invalid_uri}.


start_ping(App, PingId, Uri, Opts) when is_list(Opts) ->
    try
        case nksip:find_app_id(App) of
            {ok, AppId} -> ok;
            _ -> AppId = throw(invalid_app)
        end,
        case lists:keymember(meta, 1, Opts) of
            true -> throw(meta_not_allowed);
            false -> ok
        end,
        case nksip_call_uac_make:make(AppId, 'OPTIONS', Uri, Opts) of
            {ok, _, _} -> ok;
            {error, MakeError} -> throw(MakeError)
        end,
        Msg = {'$nksip_uac_auto_register_start_ping', PingId, Uri, Opts},
        nksip:call(App, Msg)
    catch
        throw:Error -> {error, Error}
    end.


%% @doc Stops a previously started ping serie.
-spec stop_ping(nksip:app_name()|nksip:app_id(), term()) -> 
    ok | not_found.

stop_ping(App, PingId) ->
    nksip:call(App, {'$nksip_uac_auto_register_stop_ping', PingId}).
    

%% @doc Get current ping status.
-spec get_pings(nksip:app_name()|nksip:app_id()) -> 
    [{PingId::term(), OK::boolean(), Time::non_neg_integer()}].
 
get_pings(App) ->
    nksip:call(App, '$nksip_uac_auto_register_get_pings').

