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
-export([is_authorized/3]).
-export([load_default_apps/0]).
-export([create_apps_store_doc/1]).

-include("crossbar.hrl").

-define(PLAN_CATEGORY, <<"ui_apps">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get allowed apps from service plans
%% @end
%%--------------------------------------------------------------------
-spec allowed_apps(ne_binary()) -> wh_json:objects().
allowed_apps(AccountId) ->
    ServicePlan = wh_services:service_plan_json(AccountId),
    case has_all_apps_in_service_plan(ServicePlan) of
        'true' ->
            DefaultApps = load_default_apps(),
            filter_apps(AccountId, DefaultApps);
        'false' ->
            Apps = find_enabled_apps(get_plan_apps(ServicePlan)),
            filter_apps(AccountId, Apps)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get app object if allowed
%% @end
%%--------------------------------------------------------------------
-spec allowed_app(ne_binary(), ne_binary()) -> api_object().
allowed_app(AccountId, AppId) ->
    FindApp = fun(App) -> AppId == wh_doc:id(App) end,
    case lists:filter(FindApp, allowed_apps(AccountId)) of
        [App] -> App;
        [] -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_authorized(AccountId, UserId, AppId) ->
    case get_apps_store_doc(AccountId) of
        {'error', _R} ->
            lager:error("failed to fetch apps store doc in ~s : ~p", [AccountId, _R]),
            'false';
        {'ok', Doc} ->
            AppJObj = wh_json:get_value(AppId, kzd_apps_store:apps(Doc)),
            Allowed = wh_json:get_value(<<"allowed_users">>, AppJObj, <<"specific">>),
            Users = wh_json:get_value(<<"users">>, AppJObj, []),
            case {Allowed, Users} of
                {<<"all">>, _} -> 'true';
                {<<"specific">>, []} -> 'false';
                {<<"specific">>, Users} when is_list(Users) ->
                    lists:member(UserId, Users);
                {<<"admins">>, _} ->
                    <<"admin">> =:= get_user_priv_level(AccountId, UserId);
                {_A, _U} ->
                    lager:error("unknown data ~p : ~p", [_A, _U]),
                    'false'
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_default_apps() -> wh_json:objects().
load_default_apps() ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    case couch_mgr:get_results(MasterAccountDb, ?CB_APPS_STORE_LIST, ['include_docs']) of
        {'error', _E} ->
            lager:error("failed to lookup apps in ~s", [MasterAccountDb]),
            [];
        {'ok', JObjs} ->
            [maybe_set_account(MasterAccountDb, JObj) || JObj <- JObjs]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_apps_store_doc(ne_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
create_apps_store_doc(Account) ->
    Doc = kzd_apps_store:new(Account),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:save_doc(AccountDb, Doc).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_apps_store_doc(ne_binary()) -> {'ok', wh_json:object()} | {'error', any()}.
get_apps_store_doc(Account) ->
    case kzd_apps_store:fetch(Account) of
        {'error', 'not_found'} ->
            cb_apps_maintenance:migrate(Account);
        Result -> Result
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_user_priv_level(ne_binary(), ne_binary()) -> binary().
get_user_priv_level(AccountId, UserId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'error', _R} ->
            lager:error("failed to open user ~s in ~s", [UserId, AccountDb]),
            'undefined';
        {'ok', JObj} ->
            wh_json:get_value(<<"priv_level">>, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_all_apps_in_service_plan(kzd_service_plan:doc()) -> boolean().
has_all_apps_in_service_plan(ServicePlan) ->
    %% If the "ui_apps" key is empty, return true
    %% else "ui_apps._all.enabled" == true
    wh_util:is_empty(kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY))
        orelse kzd_item_plan:is_enabled(kzd_service_plan:category_plan(ServicePlan, ?PLAN_CATEGORY)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_apps(ne_binary(), wh_json:objects()) -> wh_json:objects().
-spec filter_apps(ne_binary(), wh_json:objects(), wh_json:object()) -> wh_json:objects().
filter_apps(AccountId, Apps) ->
    case get_apps_store_doc(AccountId) of
        {'error', _R} ->
            lager:error("failed to fetch apps store doc in ~s : ~p", [AccountId, _R]),
            filter_apps(AccountId, Apps, wh_json:new());
        {'ok', Doc} ->
            filter_apps(AccountId, Apps, Doc)
    end.

filter_apps(AccountId, Apps, JObj) ->
    lists:foldl(
        fun(App, Acc) ->
            case is_filtered(AccountId, App)
                orelse is_blacklisted(App, JObj)
            of
                'true' -> Acc;
                'false' ->
                    [add_permissions(App, JObj)|Acc]
            end
        end
        ,[]
        ,Apps
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_permissions(wh_json:object(), wh_json:object()) -> wh_json:object().
add_permissions(App, JObj) ->
    AppsPerm = kzd_apps_store:apps(JObj),
    AppPerm = wh_json:get_value(wh_doc:id(App), AppsPerm, wh_json:new()),
    wh_json:merge_recursive([App, AppPerm]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_blacklisted(wh_json:object(), wh_json:object()) -> boolean().
is_blacklisted(App, JObj) ->
    Blacklist = kzd_apps_store:blacklist(JObj),
    lists:member(wh_doc:id(App), Blacklist).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_filtered(ne_binary(), wh_json:object()) -> boolean().
-spec is_filtered(ne_binary(), wh_json:object(), ne_binary()) -> boolean().
is_filtered(AccountId, App) ->
    is_filtered(AccountId, App, wh_json:get_value(<<"name">>, App)).

is_filtered(AccountId, _App, <<"port">>) ->
    lager:debug("filtering port application"),
    ResellerId = wh_services:find_reseller_id(AccountId),
    MaybeHide =
        case kz_whitelabel:fetch(ResellerId) of
            {'error', 'not_found'} -> 'false';
            {'error', _R} ->
                lager:error("failed to load whitelabel doc for ~s: ~p", [ResellerId, _R]),
                'true';
            {'ok', JObj} ->
                kz_whitelabel:port_hide(JObj)
        end,
    lager:debug("hiding port application: ~p", [MaybeHide]),
    MaybeHide;
is_filtered(_AccountId, _App, _Name) ->
    lager:debug("not filtering ~s", [_Name]),
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_account(ne_binary(), wh_json:object()) -> wh_json:object().
maybe_set_account(Account, Doc) ->
    JObj = wh_json:get_value(<<"doc">>, Doc),
    case {wh_doc:account_db(JObj), wh_doc:account_id(JObj)} of
        {'undefined', _} -> set_account(Account, JObj);
        {_, 'undefined'} -> set_account(Account, JObj);
        {_, _} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_account(ne_binary(), wh_json:object()) -> wh_json:object().
set_account(Account, JObj) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Corrected =
        wh_json:set_values(
          [{<<"pvt_account_id">>, wh_util:format_account_id(Account, 'raw')}
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
-spec get_plan_apps(kzd_service_plan:doc()) -> wh_json:object().
get_plan_apps(ServicePlan) ->
    Apps = kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY, wh_json:new()),
    wh_json:delete_key(<<"_all">>, Apps).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_enabled_apps(wh_json:object()) -> wh_json:objects().
find_enabled_apps(PlanAppsJObj) ->
    wh_json:foldl(
        fun find_enabled_apps_fold/3
        ,[]
        ,PlanAppsJObj
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_enabled_apps_fold(ne_binary(), kzd_item_plan:doc(), wh_json:objects()) -> wh_json:objects().
find_enabled_apps_fold(AppName, PlanApp, Acc) ->
    AppId = wh_json:get_value(<<"app_id">>, PlanApp),
    case (not kzd_item_plan:is_enabled(PlanApp))
        orelse find_app(AppId, PlanApp)
    of
        'true' ->
            lager:debug("excluding app ~s(~s)", [AppName, AppId]),
            Acc;
        'undefined' ->
            lager:debug("excluding app ~s(~s)", [AppName, AppId]),
            Acc;
        AppJObj ->
            lager:debug("including app ~s(~s) from plan", [AppName, AppId]),
            [AppJObj|Acc]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_app(ne_binary(), wh_json:object()) -> api_object().
find_app(AppId, PlanApp) ->
    Account = wh_json:get_first_defined([<<"account_db">>, <<"account_id">>], PlanApp),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AppId) of
        {'ok', JObj} -> JObj;
        {'error', _R} ->
            lager:error("failed to get ~s in ~s: ~p", [AppId, AccountDb, _R]),
            'undefined'
    end.