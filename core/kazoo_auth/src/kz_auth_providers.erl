%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_auth_providers).

-include("kazoo_auth.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([provider_by_issuer/1
        ,get_auth_provider/1
        ,kazoo_auth_provider/0
        ]).


-define(PROVIDER_ISSUER, <<"auth/issuer_domain">>).

-spec kazoo_auth_provider() -> map().
kazoo_auth_provider() ->
    #{name => <<"kazoo">>
     ,jwt_identity_signature_secret => ?KAZOO_SIGNATURE_SECRET
     ,jwt_user_id_signature_hash => <<"sha256">>
     }.

-spec provider_by_issuer(kz_term:ne_binary()) -> map() | {'error', any()}.
provider_by_issuer(<<"kazoo">>) ->
    kazoo_auth_provider();
provider_by_issuer(Issuer) ->
    Options = [{'key', Issuer}
              ,'include_docs'
              ],
    case kz_datamgr:get_single_result(?KZ_AUTH_DB, ?PROVIDER_ISSUER, Options) of
        {'ok', JObj} -> provider_from_json(kz_json:get_value(<<"doc">>, JObj));
        _ -> {'error', 'not_found'}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_auth_provider(kz_term:ne_binary()) -> map() | {'error', any()}.
get_auth_provider(<<"kazoo">>) ->
    kazoo_auth_provider();
get_auth_provider(ProviderId) ->
    case kz_datamgr:open_doc(?KZ_AUTH_DB, ProviderId) of
        {'ok', JObj} -> provider_from_json(JObj);
        {'error', _} -> {'error', <<"OAUTH - Provider ", ProviderId/binary, " not found">>}
    end.

-spec provider_from_json(kz_json:object()) -> map().
provider_from_json(JObj) ->
    Provider = kz_maps:keys_to_atoms(kz_json:to_map(JObj)),
    Routines = [fun maybe_set_provider_name/1
               ],
    lists:foldl(fun(F, A) -> F(A) end, Provider, Routines).

-spec maybe_set_provider_name(map()) -> map().
maybe_set_provider_name(#{'_id' := Id}=Provider) -> Provider#{name => Id};
maybe_set_provider_name(#{}=Provider) -> Provider.
