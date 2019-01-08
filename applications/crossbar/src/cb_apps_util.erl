%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_apps_util).

-export([allowed_apps/1, allowed_apps/2]).
-export([allowed_app/2]).
-export([authorized_apps/2]).
-export([load_default_apps/0]).
-export([create_apps_store_doc/1]).

-include("crossbar.hrl").

-define(PLAN_CATEGORY, <<"ui_apps">>).

%%------------------------------------------------------------------------------
%% @doc Get allowed applications from service plans.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_apps(kz_term:ne_binary()) -> kz_json:objects().
allowed_apps(AccountId) ->
    allowed_apps(AccountId, 'undefined').

-spec allowed_apps(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_json:objects().
allowed_apps(AccountId, UserId) ->
    case find_service_plan_with_apps(AccountId) of
        'undefined' ->
            DefaultApps = load_default_apps(),
            filter_apps(AccountId, UserId, DefaultApps);
        ServicePlan ->
            Apps = find_enabled_apps(get_plan_apps(ServicePlan)),
            filter_apps(AccountId, UserId, Apps)
    end.

-spec authorized_apps(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:objects().
authorized_apps(AccountId, UserId) ->
    allowed_apps(AccountId, UserId).

%%------------------------------------------------------------------------------
%% @doc Get a application object if allowed.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_app(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
allowed_app(AccountId, AppId) ->
    case [App || App <- allowed_apps(AccountId),
                 AppId =:= kz_doc:id(App)
         ]
    of
        [App|_] ->
            %% More than one service plan can have the same app, hence taking the head
            App;
        [] -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_default_apps() -> kz_json:objects().
load_default_apps() ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    case kz_datamgr:get_results(MasterAccountDb, ?CB_APPS_STORE_LIST, ['include_docs']) of
        {'ok', JObjs} -> [maybe_set_account(MasterAccountDb, JObj) || JObj <- JObjs];
        {'error', _E} ->
            lager:error("failed to lookup apps in ~s", [MasterAccountDb]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_apps_store_doc(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
create_apps_store_doc(Account) ->
    Doc = kzd_apps_store:new(Account),
    kz_datamgr:save_doc(kz_util:format_account_db(Account), Doc).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_apps_store_doc(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_apps_store_doc(Account) ->
    case kzd_apps_store:fetch(Account) of
        {'error', 'not_found'} ->
            cb_apps_maintenance:migrate(Account);
        Result -> Result
    end.

%%------------------------------------------------------------------------------
%% @doc Find the first Service plan in Account or Account's reseller
%% hierarchy which has `ui_apps' or has `ui_apps._all'
%% @end
%%------------------------------------------------------------------------------

-spec find_service_plan_with_apps(kz_term:ne_binary()) -> kz_term:api_object().
find_service_plan_with_apps(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    find_service_plan_with_apps(AccountId, ResellerId).

-spec find_service_plan_with_apps(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:api_object().
find_service_plan_with_apps(AccountId, 'undefined') ->
    lager:debug("reseller account is undefined, checking account ~s service plan", [AccountId]),
    check_service_plan(AccountId);
find_service_plan_with_apps(ResellerId, ResellerId) ->
    lager:debug("reached to top level reseller ~s", [ResellerId]),
    check_service_plan(ResellerId);
find_service_plan_with_apps(AccountId, ResellerId) ->
    ReResellerId = kz_services:find_reseller_id(ResellerId),

    ServicePlan = kz_services:service_plan_json(AccountId),
    case {is_apps_in_service_plan_empty(ServicePlan)
         ,is_show_all_in_service_plan_enabled(ServicePlan)
         }
    of
        {'true', _} ->
            lager:debug("account ~s doesn't have apps in service plan, checking reseller ~s", [AccountId, ResellerId]),
            find_service_plan_with_apps(ResellerId, ReResellerId);
        {'false', 'true'} ->
            lager:debug("service plan for ~s was set to show all apps", [AccountId]),
            'undefined';
        {'false', 'false'} ->
            lager:debug("account ~s has apps in service plan", [AccountId]),
            ServicePlan
    end.

-spec check_service_plan(kz_term:ne_binary()) -> kz_term:api_object().
check_service_plan(AccountId) ->
    ServicePlan = kz_services:service_plan_json(AccountId),
    case has_all_or_apps_in_service_plan(ServicePlan) of
        'true' -> 'undefined';
        'false' -> ServicePlan
    end.

-spec has_all_or_apps_in_service_plan(kzd_service_plan:doc()) -> boolean().
has_all_or_apps_in_service_plan(ServicePlan) ->
    %% If the "ui_apps" key is empty, return true
    %% else "ui_apps._all.enabled" == true
    is_apps_in_service_plan_empty(ServicePlan)
        orelse is_show_all_in_service_plan_enabled(ServicePlan).

-spec is_apps_in_service_plan_empty(kzd_service_plan:doc()) -> boolean().
is_apps_in_service_plan_empty(ServicePlan) ->
    kz_term:is_empty(kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY)).

-spec is_show_all_in_service_plan_enabled(kzd_service_plan:doc()) -> boolean().
is_show_all_in_service_plan_enabled(ServicePlan) ->
    kzd_item_plan:is_enabled(kzd_service_plan:category_plan(ServicePlan, ?PLAN_CATEGORY)).


-spec filter_apps(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:objects()) -> kz_json:objects().
filter_apps(AccountId, UserId, Apps) ->
    case get_apps_store_doc(AccountId) of
        {'ok', Doc} -> filter_apps(AccountId, UserId, Apps, Doc);
        {'error', _R} ->
            lager:error("failed to fetch apps store doc in ~s : ~p", [AccountId, _R]),
            filter_apps(AccountId, UserId, Apps, kz_json:new())
    end.

-spec filter_apps(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:objects(), kz_json:object()) -> kz_json:objects().
filter_apps(AccountId, UserId, Apps, AppStoreJObj) ->
    [add_permissions(App, AppStoreJObj)
     || App <- Apps,
        is_authorized(AccountId, UserId, kz_doc:id(App), AppStoreJObj)
            andalso not is_filtered(AccountId, App)
            andalso not is_blacklisted(App, AppStoreJObj)
    ].


-spec is_authorized(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_json:object()) -> boolean().
is_authorized(_, 'undefined', _, _) ->
    'true';
is_authorized(AccountId, UserId, AppId, AppStoreJObj) ->
    AppJObj = kz_json:get_value(AppId, kzd_apps_store:apps(AppStoreJObj)),
    AllowedType = kzd_app:allowed_users(AppJObj, <<"specific">>),
    SpecificIds = get_specific_ids(kzd_app:users(AppJObj)),
    case {AllowedType, SpecificIds} of
        {<<"all">>, _} -> 'true';
        {<<"specific">>, []} -> 'false';
        {<<"specific">>, UserIds} ->
            lists:member(UserId, UserIds);
        {<<"admins">>, _} ->
            kzd_user:is_account_admin(AccountId, UserId);
        {_A, _U} ->
            lager:error("unknown data ~p : ~p", [_A, _U]),
            'false'
    end.

-spec get_specific_ids(kz_json:objects()) -> kz_term:ne_binaries().
get_specific_ids(Users) ->
    [Id || User <- Users,
           (Id = kz_doc:id(User)) =/= 'undefined'
    ].


-spec add_permissions(kz_json:object(), kz_json:object()) -> kz_json:object().
add_permissions(App, JObj) ->
    AppsPerm = kzd_apps_store:apps(JObj),
    case kz_json:get_ne_value(kz_doc:id(App), AppsPerm) of
        'undefined' -> App;
        AppPerm ->
            kz_json:merge([kzd_app:publish(App), AppPerm])
    end.


-spec is_blacklisted(kz_json:object(), kz_json:object()) -> boolean().
is_blacklisted(App, JObj) ->
    Blacklist = kzd_apps_store:blacklist(JObj),
    lists:member(kz_doc:id(App), Blacklist).


-spec is_filtered(kz_term:ne_binary(), kz_json:object()) -> boolean().
is_filtered(AccountId, App) ->
    is_filtered(AccountId, App, kzd_app:name(App)).

-spec is_filtered(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> boolean().
is_filtered(AccountId, _App, <<"port">>=_AppName) ->
    lager:debug("filtering '~s' application", [_AppName]),
    ResellerId = kz_services:find_reseller_id(AccountId),
    MaybeHide =
        case kzd_whitelabel:fetch(ResellerId) of
            {'ok', JObj} -> kzd_whitelabel:hide_port(JObj);
            {'error', 'not_found'} -> 'false';
            {'error', _R} ->
                lager:error("failed to load whitelabel doc for ~s: ~p", [ResellerId, _R]),
                'true'
        end,
    lager:debug("hiding '~s' application: ~p", [_AppName, MaybeHide]),
    MaybeHide;
is_filtered(_AccountId, _App, _AppName) ->
    lager:debug("not filtering '~s'", [_AppName]),
    'false'.


-spec maybe_set_account(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
maybe_set_account(Account, Doc) ->
    JObj = kz_json:get_value(<<"doc">>, Doc),
    case 'undefined' =:= kz_doc:account_db(JObj)
        orelse 'undefined' =:= kz_doc:account_id(JObj)
    of
        'true' -> set_account(Account, JObj);
        'false' -> JObj
    end.


-spec set_account(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_account(Account, JObj) ->
    AccountDb = kz_util:format_account_db(Account),
    Corrected =
        kz_json:set_values(
          [{<<"pvt_account_id">>, kz_util:format_account_id(Account)}
          ,{<<"pvt_account_db">>, AccountDb}
          ], JObj),
    case kz_datamgr:save_doc(AccountDb, Corrected) of
        {'ok', Doc} -> Doc;
        {'error', _R} ->
            lager:error("failed to correct app"),
            Corrected
    end.


-spec get_plan_apps(kzd_service_plan:doc()) -> kz_json:object().
get_plan_apps(ServicePlan) ->
    Apps = kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY, kz_json:new()),
    kz_json:delete_key(<<"_all">>, Apps).


-spec find_enabled_apps(kz_json:object()) -> kz_json:objects().
find_enabled_apps(PlanAppsJObj) ->
    kz_json:foldl(fun find_enabled_apps_fold/3, [], PlanAppsJObj).

-spec find_enabled_apps_fold(kz_term:ne_binary(), kzd_item_plan:doc(), kz_json:objects()) -> kz_json:objects().
find_enabled_apps_fold(AppName, PlanApp, Acc) ->
    AppId = kz_json:get_value(<<"app_id">>, PlanApp),
    case kzd_item_plan:is_enabled(PlanApp)
        andalso find_app(AppId, PlanApp)
    of
        DisabledOrUndefined
          when DisabledOrUndefined =:= 'false'
               orelse DisabledOrUndefined =:= 'undefined' ->
            lager:debug("excluding app ~s(~s)", [AppName, AppId]),
            Acc;
        AppJObj ->
            lager:debug("including app ~s(~s) from plan", [AppName, AppId]),
            [kz_json:delete_key(<<"published">>, AppJObj) | Acc]
    end.

-spec find_app(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_object().
find_app(AppId, PlanApp) ->
    Account = kz_json:get_first_defined([<<"account_db">>
                                        ,<<"account_id">>
                                        ]
                                       ,PlanApp
                                       ),
    case kzd_app:fetch(Account, AppId) of
        {'ok', JObj} -> JObj;
        {'error', _R} ->
            lager:error("failed to get ~s in ~s: ~p", [AppId, Account, _R]),
            'undefined'
    end.
