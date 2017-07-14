%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
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
-spec allowed_apps(ne_binary()) -> kz_json:objects().
allowed_apps(AccountId) ->
    case find_service_plan_with_apps(AccountId) of
        'undefined' ->
            DefaultApps = load_default_apps(),
            filter_apps(AccountId, DefaultApps);
        ServicePlan ->
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
    case [App || App <- allowed_apps(AccountId),
                 AppId =:= kzd_app:id(App)
         ]
    of
        [App|_] ->
            %% More than one service plan can have the same app, hence taking the head
            App;
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
            end
    end.

-spec get_specific_ids(kz_json:objects()) -> ne_binaries().
get_specific_ids(Users) ->
    [Id || User <- Users,
           (Id = kz_doc:id(User)) =/= 'undefined'
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_default_apps() -> kz_json:objects().
load_default_apps() ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    case kz_datamgr:get_results(MasterAccountDb, ?CB_APPS_STORE_LIST, ['include_docs']) of
        {'ok', JObjs} -> [maybe_set_account(MasterAccountDb, JObj) || JObj <- JObjs];
        {'error', _E} ->
            lager:error("failed to lookup apps in ~s", [MasterAccountDb]),
            []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_apps_store_doc(ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
create_apps_store_doc(Account) ->
    Doc = kzd_apps_store:new(Account),
    kz_datamgr:save_doc(kz_util:format_account_db(Account), Doc).

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

%% @private
%% @doc Find the first Service plan in Account or Account's reseller
%% hierarchy which has ui_apps or has ui_apps._all
-spec find_service_plan_with_apps(ne_binary()) -> api_object().
find_service_plan_with_apps(AccountId) ->
    ResellerId = kz_services:find_reseller_id(AccountId),
    find_service_plan_with_apps(AccountId, ResellerId).

-spec find_service_plan_with_apps(ne_binary(), api_binary()) -> api_object().
find_service_plan_with_apps(AccountId, 'undefined') ->
    lager:debug("reseller account is undefined, checking account ~s service plan", [AccountId]),
    check_service_plan(AccountId);
find_service_plan_with_apps(ResellerId, ResellerId) ->
    lager:debug("reached to top level reseller ~s", [ResellerId]),
    check_service_plan(ResellerId);
find_service_plan_with_apps(AccountId, ResellerId) ->
    ReResellerId = kz_services:find_reseller_id(ResellerId),

    ServicePlan = kz_services:service_plan_json(AccountId),
    case {has_apps_in_service_plan(ServicePlan)
         ,is_all_in_apps_service_plan(ServicePlan)
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

-spec check_service_plan(ne_binary()) -> api_object().
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
    has_apps_in_service_plan(ServicePlan)
        orelse is_all_in_apps_service_plan(ServicePlan).

-spec has_apps_in_service_plan(kzd_service_plan:doc()) -> boolean().
has_apps_in_service_plan(ServicePlan) ->
    kz_term:is_empty(kzd_service_plan:category(ServicePlan, ?PLAN_CATEGORY)).

-spec is_all_in_apps_service_plan(kzd_service_plan:doc()) -> boolean().
is_all_in_apps_service_plan(ServicePlan) ->
    kzd_item_plan:is_enabled(kzd_service_plan:category_plan(ServicePlan, ?PLAN_CATEGORY)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_apps(ne_binary(), kz_json:objects()) -> kz_json:objects().
-spec filter_apps(ne_binary(), kz_json:objects(), kz_json:object()) -> kz_json:objects().
filter_apps(AccountId, Apps) ->
    case get_apps_store_doc(AccountId) of
        {'ok', Doc} -> filter_apps(AccountId, Apps, Doc);
        {'error', _R} ->
            lager:error("failed to fetch apps store doc in ~s : ~p", [AccountId, _R]),
            filter_apps(AccountId, Apps, kz_json:new())
    end.

filter_apps(AccountId, Apps, JObj) ->
    [add_permissions(App, JObj)
     || App <- Apps,
        not is_filtered(AccountId, App)
            andalso not is_blacklisted(App, JObj)
    ].

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
            kz_json:merge([kzd_app:publish(App), AppPerm])
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
    is_filtered(AccountId, App, kzd_app:name(App)).

is_filtered(AccountId, _App, <<"port">>=_AppName) ->
    lager:debug("filtering '~s' application", [_AppName]),
    ResellerId = kz_services:find_reseller_id(AccountId),
    MaybeHide =
        case kz_whitelabel:fetch(ResellerId) of
            {'ok', JObj} -> kz_whitelabel:port_hide(JObj);
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_account(ne_binary(), kz_json:object()) -> kz_json:object().
maybe_set_account(Account, Doc) ->
    JObj = kz_json:get_value(<<"doc">>, Doc),
    case 'undefined' =:= kz_doc:account_db(JObj)
        orelse 'undefined' =:= kz_doc:account_id(JObj)
    of
        'true' -> set_account(Account, JObj);
        'false' -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_account(ne_binary(), kz_json:object()) -> kz_json:object().
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
    kz_json:foldl(fun find_enabled_apps_fold/3, [], PlanAppsJObj).

-spec find_enabled_apps_fold(ne_binary(), kzd_item_plan:doc(), kz_json:objects()) -> kz_json:objects().
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

-spec find_app(ne_binary(), kz_json:object()) -> api_object().
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
