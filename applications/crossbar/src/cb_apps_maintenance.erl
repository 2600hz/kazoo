%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_apps_maintenance).

-export([migrate/1]).

-include("crossbar.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
migrate(Account) when is_binary(Account) ->
    case kzd_accounts:fetch(Account) of
        {'error', _R}=Error -> Error;
        {'ok', JObj} ->
            CurrentApps = kzd_apps_store:apps(JObj),
            case kz_term:is_empty(CurrentApps) of
                'true' -> {'error', 'migrated'};
                'false' ->
                    Doc = kzd_apps_store:new(Account),
                    save(Account, kzd_apps_store:set_apps(Doc, CurrentApps), JObj)
            end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> {'ok', kz_json:object()} | {'error', any()}.
save(Account, Doc, AccountDoc) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case kz_datamgr:save_doc(AccountDb, Doc) of
        {'error', _R}=Error -> Error;
        {'ok', _}=Ok ->
            _ = save_account(Account, AccountDoc),
            Ok

    end.


-spec save_account(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
save_account(Account, AccountDoc) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case kz_datamgr:ensure_saved(AccountDb, kz_json:delete_key(<<"apps">>, AccountDoc)) of
        {'error', _R} ->
            lager:error("failed to save ~s : ~p", [AccountDb, _R]);
        {'ok', JObj} ->
            case kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, JObj) of
                {'error', _R} ->
                    lager:error("failed to save ~s in accounts db: ~p", [Account, _R]);
                {'ok', _} -> 'ok'
            end
    end.
