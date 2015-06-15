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

%% @doc NkSIP Event State Compositor Plugin
%%
%% This module implements a Event State Compositor, according to RFC3903
%% By default, it uses the RAM-only built-in store, but any SipApp can implement 
%% sip_event_compositor_store/3 callback to use any external database.

-module(nksip_event_compositor).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("nksip_event_compositor.hrl").

-export([find/3, request/1, clear/1]).
-export([version/0, deps/0, parse_config/1, terminate/2]).
-export_type([reg_publish/0]).


%% ===================================================================
%% Types and records
%% ===================================================================

-type reg_publish() :: #reg_publish{}.


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
    Defaults = [{nksip_event_compositor_default_expires, 60}],
    Opts1 = nksip_lib:defaults(Opts, Defaults),
    Allow = nksip_lib:get_value(allow, Opts1),
    Opts2 = case lists:member(<<"PUBLISH">>, Allow) of
        true -> 
            Opts1;
        false -> 
            nksip_lib:store_value(allow, Allow++[<<"PUBLISH">>], Opts1)
    end,
    case nksip_lib:get_value(nksip_event_compositor_default_expires, Opts2) of
        Secs when is_integer(Secs), Secs>=1 ->
            {ok, Opts2};
        _ ->
            {error, {invalid_config, nksip_event_compositor_default_expires}}
    end.



%% @doc Called when the plugin is shutdown
-spec terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_sipapp_srv:state()}.

terminate(AppId, SipAppState) ->  
    clear(AppId),
    {ok, SipAppState}.




%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds a stored published information
-spec find(nksip:app_id()|term(), nksip:aor(), binary()) ->
    {ok, #reg_publish{}} | not_found | {error, term()}.

find(App, AOR, Tag) ->
    {ok, AppId} = nksip:find_app_id(App),
    nksip_event_compositor_lib:store_get(AppId, AOR, Tag).


%% @doc Processes a PUBLISH request according to RFC3903
-spec request(nksip:request()) ->
    nksip:sipreply().

request(#sipmsg{class={req, 'PUBLISH'}}=Req) ->
    #sipmsg{app_id=AppId, ruri=RUri, expires=Expires, body=Body} = Req,
    Expires1 = case is_integer(Expires) andalso Expires>0 of
        true -> 
            Expires;
        _ -> 
            nksip_sipapp_srv:config(AppId, nksip_event_compositor_default_expires)
    end,
    AOR = {RUri#uri.scheme, RUri#uri.user, RUri#uri.domain},
    case nksip_sipmsg:header(<<"sip-if-match">>, Req) of
        [] when Body == <<>> ->
            {invalid_request, <<"No Body">>};
        [] ->
            Tag = nksip_lib:uid(),
            nksip_event_compositor_lib:store_put(AppId, AOR, Tag, Expires1, Body);
        [Tag] ->
            case find(AppId, AOR, Tag) of
                {ok, _Reg} when Expires==0 -> 
                    nksip_event_compositor_lib:store_del(AppId, AOR, Tag);
                {ok, Reg} when Body == <<>> -> 
                    nksip_event_compositor_lib:store_put(AppId, AOR, Tag, Expires1, Reg);
                {ok, _} -> 
                    nksip_event_compositor_lib:store_put(AppId, AOR, Tag, Expires1, Body);
                not_found ->    
                    conditional_request_failed;
                {error, Error} ->
                    ?warning(AppId, <<>>, "Error calling callback: ~p", [Error]),
                    {internal_error, <<"Callback Invalid Response">>}
            end;
        _ ->
            invalid_request
    end.


%% @doc Clear all stored records by a SipApp's core.
-spec clear(nksip:app_name()|nksip:app_id()) -> 
    ok | callback_error | sipapp_not_found.

clear(App) -> 
    case nksip:find_app_id(App) of
        {ok, AppId} ->
            case nksip_event_compositor_lib:store_del_all(AppId) of
                ok -> ok;
                _ -> callback_error
            end;
        _ ->
            sipapp_not_found
    end.





