%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_apps).

-include("kazoo_auth.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([get_auth_app/1, get_auth_app/2
        ]).


-spec get_auth_app(kz_term:ne_binary()) -> map() | {'error', kz_term:ne_binary()}.
get_auth_app(AppId) ->
    get_auth_app(AppId, 'none').

-spec get_auth_app(kz_term:ne_binary(), atom()) -> map() | {'error', kz_term:ne_binary()}.
get_auth_app(AppId, Option) ->
    case do_get_auth_app(AppId) of
        {'error', _} = Error -> Error;
        #{}=App when Option =:= 'app_with_provider' -> merge_provider(App);
        #{pvt_auth_provider := Provider}=App
          when Option =:= 'app_and_provider' ->
            case kz_auth_providers:get_auth_provider(Provider) of
                {'error', _}  = Error -> Error;
                #{} = MProvider -> #{auth_app => App
                                    ,auth_provider => MProvider
                                    }
            end;
        #{} when Option =:= 'app_and_provider' ->
            {'error', <<"no provider for app">>};
        #{}=App -> App
    end.

-spec do_get_auth_app(kz_term:ne_binary()) -> map() | {'error', kz_term:ne_binary()}.
do_get_auth_app(<<"kazoo">>) ->
    #{name => <<"kazoo">>
     ,pvt_server_key => ?SYSTEM_KEY_ID
     ,pvt_auth_provider => <<"kazoo">>
     };
do_get_auth_app(AppId) ->
    case kz_datamgr:open_cache_doc(?KZ_AUTH_DB, AppId) of
        {'ok', JObj} ->
            #{'_id' := Id} = Map = kz_maps:keys_to_atoms(kz_json:to_map(JObj)),
            Map#{name => Id};
        {'error', _} -> {'error', <<"AUTH - App ", AppId/binary, " not found">>}
    end.

merge_provider(#{pvt_auth_provider := ProviderId}=App) ->
    Provider = kz_auth_providers:get_auth_provider(ProviderId),
    kz_maps:merge(Provider, App);
merge_provider(App) -> App.
