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
    case maybe_app_docs_from_services(AccountId) of
        'undefined' ->
            AppJObjs = load_default_apps(),
            filter_apps(AccountId, UserId, AppJObjs);
        AppJObjs ->
            filter_apps(AccountId, UserId, AppJObjs)
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


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_apps_store_doc(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
create_apps_store_doc(Account) ->
    Doc = kzd_apps_store:new(Account),
    kz_datamgr:save_doc(kz_util:format_account_db(Account), Doc).

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
-spec maybe_app_docs_from_services(kz_term:ne_binary()) -> kz_term:api_objects().
maybe_app_docs_from_services(AccountId) ->
    ResellerId = kz_services_reseller:get_id(AccountId),
    case AccountId =:= ResellerId
        orelse kz_term:is_empty(ResellerId)
    of
        'true' ->
            lager:debug("reached to top level reseller ~s", [ResellerId]),
            app_docs_from_services(ResellerId);
        'false' ->
            maybe_app_docs_from_services(AccountId, ResellerId)
    end.

-spec maybe_app_docs_from_services(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_objects().
maybe_app_docs_from_services(AccountId, ResellerId) ->
    case app_docs_from_services(AccountId) of
        'undefined' ->
            lager:debug("account ~s doesn't have apps in service plan, checking reseller ~s"
                       ,[AccountId, ResellerId]
                       ),
            maybe_app_docs_from_services(ResellerId);
        Apps ->
            lager:debug("account ~s has apps in service plan", [AccountId]),
            Apps
    end.

-spec app_docs_from_services(kz_term:ne_binary()) -> kz_term:api_objects().
app_docs_from_services(AccountId) ->
    ServicesApps = kz_services_applications:fetch(AccountId),
    case kz_term:is_empty(ServicesApps) of
        'true' -> 'undefined';
        'false' ->
            kz_json:foldl(fun app_docs_from_services_fold/3, [], ServicesApps)
    end.

-spec app_docs_from_services_fold(kz_term:ne_binary(), kz_json:object(), kz_json:objects()) -> kz_json:objects().
app_docs_from_services_fold(Id, ServicesApp, AppJObjs) ->
    case kz_json:is_true(<<"enabled">>, ServicesApp, 'true') of
        'false' ->
            AppName = kz_json:get_ne_binary_value(<<"name">>, ServicesApp),
            lager:debug("excluding app ~s(~s)", [AppName, Id]),
            AppJObjs;
        'true' ->
            Vendor = kz_json:get_ne_binary_value(<<"vendor_id">>, ServicesApp),
            maybe_append_app_doc(Vendor, Id, AppJObjs)
    end.

-spec maybe_append_app_doc(kz_term:api_binary(), kz_term:ne_binary(), kz_json:objects()) -> kz_term:api_objects().
maybe_append_app_doc(Vendor, Id, AppJObjs) ->
    case fetch_app_doc(Vendor, Id) of
        'undefined' -> AppJObjs;
        AppJObj -> [AppJObj|AppJObjs]
    end.

-spec fetch_app_doc(kz_term:api_binary(), kz_term:ne_binary()) -> kz_term:api_object().
fetch_app_doc('undefined', Id) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    fetch_app_doc(MasterAccountId, Id);
fetch_app_doc(Vendor, Id) ->
    case kzd_app:fetch(Vendor, Id) of
        {'ok', JObj} ->
            AppName = kzd_app:name(JObj),
            lager:debug("including ~s from services assignments", [AppName]),
            kz_json:delete_key(<<"published">>, JObj);
        {'error', _R} ->
            lager:error("failed to get app doc ~s/~s: ~p", [Vendor, Id, _R]),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

-spec get_specific_ids(kz_json:ne_binaries()) -> kz_term:ne_binaries().
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
    ResellerId = kz_services_reseller:get_id(AccountId),
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
