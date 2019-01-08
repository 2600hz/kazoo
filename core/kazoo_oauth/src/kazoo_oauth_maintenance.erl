%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_oauth_maintenance).

-include("kazoo_oauth.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([register_oauth_app/5]).
-export([register_common_providers/0]).


%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_oauth_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
register_oauth_app(AccountId, OAuthId, EMail, Secret, Provider) ->
    Doc = kz_json:from_list([
                             {<<"_id">>, OAuthId}
                            ,{<<"pvt_account_id">>, AccountId}
                            ,{<<"pvt_secret">>,Secret}
                            ,{<<"pvt_email">>, EMail}
                            ,{<<"pvt_user_prefix">>, kz_binary:rand_hex(16)}
                            ,{<<"pvt_oauth_provider">>, Provider}
                            ,{<<"pvt_type">>, <<"app">>}
                            ]),
    case kz_datamgr:open_doc(?KZ_OAUTH_DB, OAuthId) of
        {'ok', _JObj} -> {'error', <<"already registered">>};
        {'error', _} -> kz_datamgr:save_doc(?KZ_OAUTH_DB, Doc)
    end.

-spec register_common_providers() -> 'ok'.
register_common_providers() ->
    {'ok', _} = kz_datamgr:revise_doc_from_file(?KZ_OAUTH_DB, 'kazoo_oauth', <<"google.json">>),
    'ok'.
