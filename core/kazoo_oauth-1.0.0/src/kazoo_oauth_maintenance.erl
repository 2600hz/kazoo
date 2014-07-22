
-module(kazoo_oauth_maintenance).

-include("kazoo_oauth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_oauth_app/5]).



%% ====================================================================
%% Internal functions
%% ====================================================================

-spec register_oauth_app(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> any().
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
    case couch_mgr:open_doc(?OAUTH_DB, OAuthId) of
        {'ok', _JObj} ->
            {'error', <<"already registered">>};
        {'error', _} ->
            couch_mgr:save_doc(?OAUTH_DB, Doc)
    end.
