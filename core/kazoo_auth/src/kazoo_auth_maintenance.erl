
-module(kazoo_auth_maintenance).

-include("kazoo_auth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_auth_app/5]).
-export([register_common_providers/0]).

-export([refresh/0, flush/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec register_auth_app(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> any().
register_auth_app(AccountId, OAuthId, EMail, Secret, Provider) ->
    Doc = kz_json:from_list([{<<"_id">>, OAuthId}
                            ,{<<"pvt_account_id">>, AccountId}
                            ,{<<"pvt_secret">>,Secret}
                            ,{<<"pvt_email">>, EMail}
                            ,{<<"pvt_user_prefix">>, kz_util:rand_hex_binary(16)}
                            ,{<<"pvt_auth_provider">>, Provider}
                            ,{<<"pvt_type">>, <<"app">>}
                            ]),
    case kz_datamgr:open_doc(?KZ_AUTH_DB, OAuthId) of
        {'ok', _JObj} -> {'error', <<"already registered">>};
        {'error', _} -> kz_datamgr:save_doc(?KZ_AUTH_DB, Doc)
    end.

-spec refresh() -> 'ok'.
refresh() ->
    kz_datamgr:revise_views_from_folder(?KZ_AUTH_DB, 'kazoo_auth').


-spec register_common_providers() -> 'ok'.
register_common_providers() ->
    kz_datamgr:revise_docs_from_folder(?KZ_AUTH_DB, 'kazoo_auth', "providers").

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?PK_CACHE),
    kz_cache:flush_local(?TOKENS_CACHE).
