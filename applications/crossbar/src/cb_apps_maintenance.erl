%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
        {'ok', AccountJObj} ->
            CurrentApps = kzd_apps_store:apps(AccountJObj),
            case kz_term:is_empty(CurrentApps) of
                'true' -> {'error', 'migrated'};
                'false' ->
                    AppsStoreDoc = kzd_apps_store:new(Account),
                    save(Account, kzd_apps_store:set_apps(AppsStoreDoc, CurrentApps))
            end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(kz_term:ne_binary(), kzd_apps_store:doc()) ->
          {'ok', kzd_accounts:doc()} |
          kz_datamgr:data_error().
save(Account, AppsStoreDoc) ->
    AccountDb = kzs_util:format_account_db(Account),
    case kz_datamgr:save_doc(AccountDb, AppsStoreDoc) of
        {'error', _R}=Error -> Error;
        {'ok', _SavedAppsStoreDoc}=Ok ->
            _ = save_account(Account),
            Ok

    end.

-spec save_account(kz_term:ne_binary()) -> 'ok'.
save_account(Account) ->
    case kzd_accounts:update(Account, [{[<<"apps">>], 'null'}]) of
        {'error', _R} ->
            lager:error("failed to save ~s : ~p", [Account, _R]);
        {'ok', _AccountDoc} -> 'ok'
    end.
