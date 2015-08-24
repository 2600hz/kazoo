%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_apps_maintenance).

-export([migrate/1]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec migrate(ne_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
migrate(Account) when is_binary(Account) ->
    case kz_account:fetch(Account) of
        {'error', _R}=Error -> Error;
        {'ok', JObj} ->
            CurrentApps = kz_apps_store:apps(JObj),
            Doc = kz_apps_store:new(Account),
            save(Account, kz_apps_store:set_apps(Doc, CurrentApps), JObj)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(ne_binary(), wh_json:object(), wh_json:object()) -> {'ok', wh_json:object()} | {'error', any()}.
save(Account, Doc, AccountDoc) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:save_doc(AccountDb, Doc) of
        {'error', _R}=Error -> Error;
        {'ok', _}=Ok ->
            case couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"apps">>, AccountDoc)) of
                {'error', _R} ->
                    lager:error("failed to save ~s : ~p", [AccountDb, _R]),
                    Ok;
                {'ok', JObj} ->
                    _ = crossbar_util:replicate_account_definition(JObj),
                    Ok
            end
    end.