
-module(kazoo_oauth_maintenance).

-include("kazoo_oauth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_oauth_app/5]).
-export([register_common_providers/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec register_oauth_app(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> _.
register_oauth_app(AccountId, OAuthId, EMail, Secret, Provider) ->
    Doc = wh_json:from_list([
           {<<"_id">>, OAuthId}
          ,{<<"pvt_account_id">>, AccountId}
          ,{<<"pvt_secret">>,Secret}
          ,{<<"pvt_email">>, EMail}
          ,{<<"pvt_user_prefix">>, wh_util:rand_hex_binary(16)}
          ,{<<"pvt_oauth_provider">>, Provider}
          ,{<<"pvt_type">>, <<"app">>}
          ]),
    case couch_mgr:open_doc(?KZ_OAUTH_DB, OAuthId) of
        {'ok', _JObj} -> {'error', <<"already registered">>};
        {'error', _} -> couch_mgr:save_doc(?KZ_OAUTH_DB, Doc)
    end.

-spec register_common_providers() -> 'ok'.
register_common_providers() ->
    couch_mgr:load_doc_from_file(?KZ_OAUTH_DB, 'kazoo_oauth', <<"google.json">>).
