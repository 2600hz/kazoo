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

%% @doc NkSIP GRUU Plugin
-module(nksip_gruu).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").

-export([get_gruu_pub/1, get_gruu_temp/1, registrar_find/2]).
-export([version/0, deps/0, parse_config/1]).


%% ===================================================================
%% Plugin specific
%% ===================================================================

%% @doc Version
-spec version() ->
    string().

version() ->
    "0.1".


%% @doc Dependant plugins
%% If nksip_registrar is activated, it will update it
-spec deps() ->
    [{atom(), string()}].
    
deps() ->
    [].


%% @doc Parses this plugin specific configuration
-spec parse_config(nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.

parse_config(Opts) ->
    Supported = nksip_lib:get_value(supported, Opts),
    Opts1 = case lists:member(<<"gruu">>, Supported) of
        true -> Opts;
        false -> nksip_lib:store_value(supported, Supported++[<<"gruu">>], Opts)
    end,
    {ok, Opts1}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets the last detected public GRUU
-spec get_gruu_pub(nksip:app_name()|nksip:app_id()) ->
    {ok, nksip:uri()} | undefined | {error, term()}.

get_gruu_pub(App) ->
    case nksip:find_app_id(App) of
        {ok, AppId} -> 
            case nksip_config:get({nksip_gruu_pub, AppId}) of
                undefined -> undefined;
                Value -> {ok, Value}
            end;
        _ -> 
            {error, not_found}
    end.


%% @doc Gets the last detected temporary GRUU
-spec get_gruu_temp(nksip:app_name()|nksip:app_id()) ->
    {ok, nksip:uri()} | undefined | {error, term()}.

get_gruu_temp(App) ->
    case nksip:find_app_id(App) of
        {ok, AppId} -> 
            case nksip_config:get({nksip_gruu_temp, AppId}) of
                undefined -> undefined;
                Value -> {ok, Value}
            end;
        _ -> 
            {error, not_found}
    end.


%% @doc Use this function instead of nksip_registrar:find/2,4 to decode the generated GRUUs.
-spec registrar_find(nksip:app_name()|nksip:app_id(), nksip:uri()) ->
    [nksip:uri()].

registrar_find(App, Uri) ->
    case nksip:find_app_id(App) of
        {ok, AppId} -> 
            nksip_gruu_lib:find(AppId, Uri);
        _ ->
            []
    end.

    