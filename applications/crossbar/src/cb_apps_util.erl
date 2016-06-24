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

-include_lib("crossbar/src/crossbar.hrl").


-define(PLAN_CATEGORY, <<"ui_apps">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get allowed apps from service plans
%% @end
%%--------------------------------------------------------------------
-spec allowed_apps(ne_binary()) -> kz_json:objects().
allowed_apps(AccountId) ->
    ServicePlan = kz_services:service_plan_json(AccountId),
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
    FindApp = fun(App) -> AppId == kz_doc:id(App) end,
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
            AppJObj = kz_json:get_value(AppId, kzd_apps_store:apps(Doc)),
            AllowedType = kz_json:get_value(<<"allowed_users">>, AppJObj, <<"specific">>),
            SpecificIds = get_specific_ids(
                            kz_json:get_value(<<"users">>, AppJObj, [])
                           ),
            case {AllowedType, SpecificIds} of
                {<<"all">>, _} -> 'true';
                {<<"specific">>, []} -> 'false';
                {<<"specific">>, UserIds} ->
                    lists:member(UserId, UserIds);
                {<<"admins">>, _} ->
                    <<"admin">> =:= get_user_priv_level(AccountId, UserId);
                {_A, _U} ->
                    lager:error("unknown data ~p : ~p", [_A, _U]),
                    'false'
            end
    end.

-spec get_specific_ids(kz_json:objects()) -> ne_binaries().
get_specific_ids(Users) ->
    get_specific_ids(Users, []).

-spec get_specific_ids(kz_json:objects(), ne_binaries()) -> ne_binaries().
get_specific_ids([], UserIds) -> UserIds;
get_specific_ids([User|Users], UserIds) ->
    case kz_json:get_ne_value(<<"id">>, User) of
        'undefined' -> get_specific_ids(Users, UserIds);
        UserId -> get_specific_ids(Users, [UserId|UserIds])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_default_apps() -> kz_json:objects().
load_default_apps() ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    case kz_datamgr:get_results(MasterAccountDb, ?CB_APPS_STORE_LIST, ['include_docs']) of
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
-spec create_apps_store_doc(ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
create_apps_store_doc(Account) ->
    Doc = kzd_apps_store:new(Account),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    kz_datamgr:save_doc(AccountDb, Doc).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_apps_store_doc(ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
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
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, UserId) of
        {'error', _R} ->
            lager:error("failed to open user ~s in ~s", [UserId, AccountDb]),
            'undefined';
        {'ok', JObj} ->
            kz_json:get_value(<<"priv_level">>, JObj)
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
    kz_util:is_empty(kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY))
        orelse kzd_item_plan:is_enabled(kzd_service_plan:category_plan(ServicePlan, ?PLAN_CATEGORY)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_apps(ne_binary(), kz_json:objects()) -> kz_json:objects().
-spec filter_apps(ne_binary(), kz_json:objects(), kz_json:object()) -> kz_json:objects().
filter_apps(AccountId, Apps) ->
    case get_apps_store_doc(AccountId) of
        {'error', _R} ->
            lager:error("failed to fetch apps store doc in ~s : ~p", [AccountId, _R]),
            filter_apps(AccountId, Apps, kz_json:new());
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
-spec add_permissions(kz_json:object(), kz_json:object()) -> kz_json:object().
add_permissions(App, JObj) ->
    AppsPerm = kzd_apps_store:apps(JObj),
    case kz_json:get_ne_value(kz_doc:id(App), AppsPerm) of
        'undefined' -> App;
        AppPerm ->
            kz_json:merge_recursive([kzd_app:publish(App), AppPerm])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_blacklisted(kz_json:object(), kz_json:object()) -> boolean().
is_blacklisted(App, JObj) ->
    Blacklist = kzd_apps_store:blacklist(JObj),
    lists:member(kz_doc:id(App), Blacklist).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_filtered(ne_binary(), kz_json:object()) -> boolean().
-spec is_filtered(ne_binary(), kz_json:object(), ne_binary()) -> boolean().
is_filtered(AccountId, App) ->
    is_filtered(AccountId, App, kz_json:get_value(<<"name">>, App)).

is_filtered(AccountId, _App, <<"port">>) ->
    lager:debug("filtering port application"),
    ResellerId = kz_services:find_reseller_id(AccountId),
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
-spec maybe_set_account(ne_binary(), kz_json:object()) -> kz_json:object().
maybe_set_account(Account, Doc) ->
    JObj = kz_json:get_value(<<"doc">>, Doc),
    case {kz_doc:account_db(JObj), kz_doc:account_id(JObj)} of
        {'undefined', _} -> set_account(Account, JObj);
        {_, 'undefined'} -> set_account(Account, JObj);
        {_, _} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_account(ne_binary(), kz_json:object()) -> kz_json:object().
set_account(Account, JObj) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    Corrected =
        kz_json:set_values(
          [{<<"pvt_account_id">>, kz_util:format_account_id(Account, 'raw')}
           ,{<<"pvt_account_db">>, AccountDb}
          ], JObj),
    case kz_datamgr:save_doc(AccountDb, Corrected) of
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
-spec get_plan_apps(kzd_service_plan:doc()) -> kz_json:object().
get_plan_apps(ServicePlan) ->
    Apps = kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY, kz_json:new()),
    kz_json:delete_key(<<"_all">>, Apps).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_enabled_apps(kz_json:object()) -> kz_json:objects().
find_enabled_apps(PlanAppsJObj) ->
    kz_json:foldl(
        fun find_enabled_apps_fold/3
        ,[]
        ,PlanAppsJObj
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_enabled_apps_fold(ne_binary(), kzd_item_plan:doc(), kz_json:objects()) -> kz_json:objects().
find_enabled_apps_fold(AppName, PlanApp, Acc) ->
    AppId = kz_json:get_value(<<"app_id">>, PlanApp),
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
            [kz_json:delete_key(<<"published">>, AppJObj)|Acc]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_app(ne_binary(), kz_json:object()) -> api_object().
find_app(AppId, PlanApp) ->
    Account = kz_json:get_first_defined([<<"account_db">>, <<"account_id">>], PlanApp),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case kz_datamgr:open_cache_doc(AccountDb, AppId) of
        {'ok', JObj} -> JObj;
        {'error', _R} ->
            lager:error("failed to get ~s in ~s: ~p", [AppId, AccountDb, _R]),
            'undefined'
    end.
