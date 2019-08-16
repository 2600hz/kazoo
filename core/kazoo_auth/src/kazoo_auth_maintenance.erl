%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_auth_maintenance).

-include("kazoo_auth.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([register_auth_app/4]).
-export([register_common_providers/0]).
-export([register_auth_app_key/2]).

-export([refresh/0
        ,register_views/0
        ]).

-export([flush/0
        ,flush_private_keys/0
        ,flush_profiles/0
        ,flush_tokens/0
        ]).

-export([ensure_secret/0]).


%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_auth_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
register_auth_app(AccountId, OAuthId, Secret, Provider) ->
    Doc = kz_json:from_list([{<<"_id">>, OAuthId}
                            ,{<<"pvt_account_id">>, AccountId}
                            ,{<<"pvt_secret">>, Secret}
                            ,{<<"pvt_user_prefix">>, kz_binary:rand_hex(16)}
                            ,{<<"pvt_auth_provider">>, Provider}
                            ,{<<"pvt_type">>, <<"app">>}
                            ]),
    case kz_datamgr:open_doc(?KZ_AUTH_DB, OAuthId) of
        {'ok', _JObj} -> {'error', <<"already registered">>};
        {'error', _} -> kz_datamgr:save_doc(?KZ_AUTH_DB, Doc)
    end.

-spec register_auth_app_key(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_json:object()} |
                                   kz_datamgr:data_error().
register_auth_app_key(AppId, PemFile) ->
    Pem = kz_auth_keys:get_private_key_from_file(PemFile),
    KeyId = kz_binary:rand_hex(16),
    {'ok', _Key} = kz_auth_keys:new_private_key(KeyId, Pem),
    Updates = [{[<<"pvt_server_key">>], KeyId}],
    UpdateOptions = [{'update', Updates}],
    kz_datamgr:update_doc(?KZ_AUTH_DB, AppId, UpdateOptions).

-spec refresh() -> 'ok'.
refresh() ->
    case kz_datamgr:db_exists(?KZ_AUTH_DB) of
        'false' ->
            init_db(kz_datamgr:db_create(?KZ_AUTH_DB));
        'true' -> init_db('true')
    end.

-spec init_db(boolean()) -> 'ok'.
init_db('false') ->
    lager:error("error trying to create auth database");
init_db('true') ->
    _ = kapps_maintenance:refresh(?KZ_AUTH_DB),
    'ok'.

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('kazoo_auth').

-spec register_common_providers() -> 'ok'.
register_common_providers() ->
    kz_datamgr:revise_docs_from_folder(?KZ_AUTH_DB, 'kazoo_auth', "providers").

-spec flush() -> 'ok'.
flush() ->
    flush_private_keys(),
    flush_profiles(),
    flush_tokens().

-spec flush_private_keys() -> 'ok'.
flush_private_keys() ->
    kz_cache:flush_local(?PK_CACHE).

-spec flush_profiles() -> 'ok'.
flush_profiles() ->
    kz_cache:flush_local(?PROFILE_CACHE).

-spec flush_tokens() -> 'ok'.
flush_tokens() ->
    kz_cache:flush_local(?TOKENS_CACHE).

-spec ensure_secret() -> 'ok'.
ensure_secret() ->
    _ = case kapps_config:get_ne_binary(?CONFIG_CAT, ?KAZOO_SIGNATURE_ID) of
            'undefined' -> kapps_config:set_string(?CONFIG_CAT, ?KAZOO_SIGNATURE_ID, ?KAZOO_GEN_SIGNATURE_SECRET);
            _ -> 'ok'
        end,
    'ok'.
