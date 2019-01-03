%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
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

-export([refresh/0, flush/0]).

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

-spec register_auth_app_key(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
register_auth_app_key(AppId, PemFile) ->
    Pem = kz_auth_keys:get_private_key_from_file(PemFile),
    KeyId = kz_binary:rand_hex(16),
    {ok, _Key} = kz_auth_keys:new_private_key(KeyId, Pem),
    Props = [{<<"pvt_server_key">>, KeyId}],
    kz_datamgr:update_doc(?KZ_AUTH_DB, AppId, Props).

-spec refresh() -> 'ok'.
refresh() ->
    kz_datamgr:revise_views_from_folder(?KZ_AUTH_DB, 'kazoo_auth').

-spec register_common_providers() -> 'ok'.
register_common_providers() ->
    kz_datamgr:revise_docs_from_folder(?KZ_AUTH_DB, 'kazoo_auth', "providers").

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?PROFILE_CACHE),
    kz_cache:flush_local(?PK_CACHE),
    kz_cache:flush_local(?TOKENS_CACHE).

-spec ensure_secret() -> 'ok'.
ensure_secret() ->
    _ = case kapps_config:get_ne_binary(?CONFIG_CAT, ?KAZOO_SIGNATURE_ID) of
            'undefined' -> kapps_config:set_string(?CONFIG_CAT, ?KAZOO_SIGNATURE_ID, ?KAZOO_GEN_SIGNATURE_SECRET);
            _ -> 'ok'
        end,
    'ok'.
