%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_apps_util).

-export([allowed_apps/1]).
-export([allowed_app/2]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get allowed apps fron service plans
%% @end
%%--------------------------------------------------------------------
-spec allowed_apps(ne_binary()) -> wh_json:objects().

allowed_apps(AccountId) ->
    ServicePlan = wh_services:service_plan_json(AccountId),
    case wh_json:get_value(<<"ui_apps">>, ServicePlan) of
        'undefined' ->
            load_default_apps();
        UIApps ->
            maybe_load_all_apps(UIApps)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get allowed apps fron service plans
%% @end
%%--------------------------------------------------------------------
-spec allowed_app(ne_binary(), ne_binary()) -> 'undefined' | wh_json:object().
allowed_app(AccountId, AppId) ->
    Apps = allowed_apps(AccountId),
    filter_apps(Apps, AppId, 'undefined').

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_apps(wh_json:objects(), ne_binary(), 'undefined' | wh_json:object()) -> 'undefined' | wh_json:object().
filter_apps([], _, Acc) -> Acc;
filter_apps([App|Apps], AppId, Acc) ->
    case wh_json:get_value(<<"_id">>, App) of
        AppId -> App;
        _ -> filter_apps(Apps, AppId, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_default_apps() -> wh_json:objects().
load_default_apps() ->
    MasterAccountDb = get_master_account_db(),
    lager:debug("loading default apps from master account : ~s", [MasterAccountDb]),
    case couch_mgr:get_results(MasterAccountDb, <<"apps_store/crossbar_listing">>, ['include_docs']) of
        {'error', _E} ->
            lager:error("failed to lookup apps in ~s", [MasterAccountDb]),
            [];
        {'ok', JObjs} ->
            [maybe_set_account(MasterAccountDb, wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_load_all_apps(wh_json:objects()) -> wh_json:objects().
maybe_load_all_apps(AppsPlan) ->
    lager:debug("maybe loading all apps"),
    case wh_json:is_true([<<"_all">>, <<"enabled">>], AppsPlan) of
        'false' -> load_apps(wh_json:delete_key(<<"_all">>, AppsPlan));
        'true' -> load_all_apps(wh_json:get_value(<<"_all">>, AppsPlan))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_all_apps(wh_json:object()) -> wh_json:objects().
load_all_apps(AppsPlan) ->
    lager:debug("loading all apps"),
    Account = wh_json:get_first_defined([<<"account">>, <<"account_id">>, <<"account_db">>], AppsPlan),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:get_results(AccountDb, <<"apps_store/crossbar_listing">>, ['include_docs']) of
        {'error', _E} ->
            lager:error("failed to lookup apps in ~s", [AccountDb]),
            [];
        {'ok', JObjs} ->
            [maybe_set_account(Account, wh_json:get_value(<<"doc">>, JObj)) || JObj <- JObjs]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_apps(wh_json:object()) -> wh_json:objects().
load_apps(AppsPlan) ->
    lager:debug("loading specific apps"),
    wh_json:foldl(
        fun load_apps_foldl/3
        ,[]
        ,AppsPlan
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_apps_foldl(ne_binary(), wh_json:object(), wh_json:objects()) -> wh_json:objects().
load_apps_foldl(_, JObj, Acc) ->
    AppId = wh_json:get_value(<<"app_id">>, JObj),
    case wh_json:is_true(<<"enabled">>, JObj) of
        'false' -> Acc;
        'true' ->
            Account = wh_json:get_first_defined([<<"account">>, <<"account_id">>, <<"account_db">>], JObj),
            AccountDb = wh_util:format_account_id(Account, 'encoded'),
            case couch_mgr:open_doc(AccountDb, AppId) of
                {'error', _E} ->
                    lager:error("failed to lookup apps ~s in ~s", [AppId, AccountDb]),
                    Acc;
                {'ok', AppDoc} ->
                    [maybe_set_account(Account, AppDoc)|Acc]
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_account(ne_binary(), wh_json:object()) -> wh_json:object().
maybe_set_account(Account, JObj) ->
    PvtAccountDb = wh_json:get_value(<<"pvt_account_db">>, JObj),
    PvtAccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    case {PvtAccountDb, PvtAccountId} of
        {'undefined', _} -> set_account(Account, JObj);
        {_, 'undefined'} -> set_account(Account, JObj);
        {_, _} -> JObj
    end.

-spec set_account(ne_binary(), wh_json:object()) -> wh_json:object().
set_account(Account, JObj) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Corrected =
        wh_json:set_values([
            {<<"pvt_account_id">>, AccountId}
            ,{<<"pvt_account_db">>, AccountDb}
        ], JObj),
    case couch_mgr:save_doc(AccountDb, Corrected) of
        {'error', _R} ->
            lager:error("failed to correct app"),
            Corrected;
        {'ok', Doc} -> Doc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_master_account_db() -> ne_binary().
get_master_account_db() ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    wh_util:format_account_id(MasterAccountId, 'encoded').