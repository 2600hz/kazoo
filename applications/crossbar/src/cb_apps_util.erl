%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_apps_util).

-export([allowed_apps/1
        ,allowed_apps/2
        ]).
-export([allowed_app/2]).
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
    Routines = [fun ensure_allowed_service_apps/3
               ,fun allowed_apps_store_doc/3
               ,fun allowed_whitelabel_doc/3
               ,fun allowed_service_plan/3
               ,fun allowed_master_account/3
               ],
    lists:foldl(fun(F, AppJObjs) ->
                        F(AccountId, UserId, AppJObjs)
                end
               ,load_default_apps()
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc Load the application list from the accounts service plan, or the
%% reseller if that is empty. Ensure that any enabled applications
%% from an account other than master are loaded into the list.
%% @end
%%------------------------------------------------------------------------------
-spec ensure_allowed_service_apps(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:objects()) -> kz_json:objects().
ensure_allowed_service_apps(AccountId, _UserId, AppJObjs) ->
    ServicesApps = get_services_apps(AccountId),
    ensure_allowed_service_apps(ServicesApps, AppJObjs).

-spec ensure_allowed_service_apps(kz_json:object(), kz_json:objects()) -> kz_json:objects().
ensure_allowed_service_apps(ServicesApps, AppJObjs) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    kz_json:foldl(ensure_allowed_service_apps_fold(MasterAccountId), [], ServicesApps) ++ AppJObjs.

-spec ensure_allowed_service_apps_fold(kz_term:ne_binary()) ->
                                              fun((kz_term:ne_binary(), kz_json:object(), kz_json:objects()) -> kz_json:objects()).
ensure_allowed_service_apps_fold(MasterAccountId) ->
    fun(AppId, ServicesAppJObj, AppJObjs) ->
            VendorId = kz_json:get_ne_binary_value(<<"vendor_id">>, ServicesAppJObj, MasterAccountId),
            Enabled = kz_json:is_true(<<"enabled">>, ServicesAppJObj, 'true'),
            case VendorId =/= MasterAccountId
                andalso Enabled
            of
                'true' -> maybe_append_app_doc(VendorId, AppId, AppJObjs);
                'false' -> AppJObjs
            end
    end.

-spec maybe_append_app_doc(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_append_app_doc(VendorId, AppId, AppJObjs) ->
    case kzd_app:fetch(VendorId, AppId) of
        {'ok', AppJObj} ->
            lager:debug("including non-master app ~s due to service plan"
                       ,[kzd_app:name(AppJObj)]
                       ),
            [AppJObj|AppJObjs];
        {'error', _R} ->
            lager:error("failed to get app doc ~s/~s: ~p"
                       ,[VendorId, AppId, _R]
                       ),
            AppJObjs
    end.

%%------------------------------------------------------------------------------
%% @doc Load the application list from the accounts service plan, or the
%% reseller if that is empty. Set the published parameter on each app doc
%% if its set in the service plan.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_service_plan(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:objects()) -> kz_json:objects().
allowed_service_plan(AccountId, _UserId, AppJObjs) ->
    ServicesApps = get_services_apps(AccountId),
    lists:map(fun(AppJObj) ->
                      AppId = kz_doc:id(AppJObj),
                      case kz_json:is_true([AppId, <<"enabled">>], ServicesApps, 'undefined') of
                          'undefined' -> AppJObj;
                          'true' ->
                              lager:debug("service plan explicitly enables access to ~s"
                                         ,[kzd_app:name(AppJObj)]
                                         ),
                              kzd_app:publish(AppJObj);
                          'false' ->
                              lager:debug("service plan explicitly disables access to ~s"
                                         ,[kzd_app:name(AppJObj)]
                                         ),
                              kzd_app:unpublish(AppJObj)
                      end
              end
             ,AppJObjs
             ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_apps_store_doc(kz_term:ne_binary(), kz_term:api_binary(), kz_json:objects()) -> kz_json:objects().
allowed_apps_store_doc(AccountId, UserId, AppJObjs) ->
    case get_apps_store_doc(AccountId) of
        {'error', _R} ->
            lager:error("failed to fetch apps store doc in ~s : ~p"
                       ,[AccountId, _R]
                       ),
            AppJObjs;
        {'ok', AppStoreJObj} ->
            lists:map(allowed_apps_store_doc_map(AccountId, UserId, AppStoreJObj)
                     ,AppJObjs
                     )
    end.

-spec allowed_apps_store_doc_map(kz_term:ne_binary(), kz_term:api_binary(), kz_json:object()) ->
                                        fun((kz_json:object()) -> kz_json:object()).
allowed_apps_store_doc_map(AccountId, UserId, AppStoreJObj) ->
    fun(AppJObj) ->
            AppId = kz_doc:id(AppJObj),
            case kz_json:get_ne_json_value(AppId, kzd_apps_store:apps(AppStoreJObj)) of
                'undefined' -> AppJObj;
                AppPermissions ->
                    case not is_blacklisted(AppJObj, AppStoreJObj)
                        andalso is_authorized(AccountId, UserId, AppPermissions)
                    of
                        'true' ->
                            kz_json:merge([kzd_app:publish(AppJObj), AppPermissions]);
                        'false' ->
                            lager:debug("app store doc explicitly disables access to ~s"
                                       ,[kzd_app:name(AppJObj)]
                                       ),
                            kz_json:merge([kzd_app:unpublish(AppJObj), AppPermissions])
                    end
            end
    end.

-spec get_apps_store_doc(kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_apps_store_doc(AccountId) ->
    case kzd_apps_store:fetch(AccountId) of
        {'error', 'not_found'} ->
            cb_apps_maintenance:migrate(AccountId);
        Result -> Result
    end.

-spec is_authorized(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:object()) -> boolean().
is_authorized(_, 'undefined', _) ->
    'true';
is_authorized(AccountId, UserId, AppPermissions) ->
    AllowedType = kzd_app:allowed_users(AppPermissions, <<"specific">>),
    SpecificIds = get_specific_ids(kzd_app:users(AppPermissions)),
    case {AllowedType, SpecificIds} of
        {<<"all">>, _} -> 'true';
        {<<"specific">>, []} -> 'false';
        {<<"specific">>, UserIds} ->
            lists:member(UserId, UserIds);
        {<<"admins">>, _} ->
            kzd_users:is_account_admin(AccountId, UserId);
        {_A, _U} ->
            lager:error("unknown data ~p : ~p", [_A, _U]),
            'false'
    end.

-spec get_specific_ids(kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_specific_ids(UserIds) ->
    [UserId || UserId <- UserIds, is_binary(UserId)].

-spec is_blacklisted(kz_json:object(), kz_json:object()) -> boolean().
is_blacklisted(App, JObj) ->
    Blacklist = kzd_apps_store:blacklist(JObj),
    lists:member(kz_doc:id(App), Blacklist).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_whitelabel_doc(kz_term:ne_binary(), kz_term:api_binary(), kz_json:objects()) -> kz_json:objects().
allowed_whitelabel_doc(AccountId, _UserId, AppJObjs) ->
    WhitelabelJObj = get_whitelabel_doc(AccountId),
    lists:map(fun(AppJObj) ->
                      case kzd_app:name(AppJObj) =:= <<"port">>
                          andalso kzd_whitelabel:hide_port(WhitelabelJObj)
                      of
                          'false' -> AppJObj;
                          'true' ->
                              lager:debug("whitelabel doc explicitly disables access to ~s"
                                         ,[kzd_app:name(AppJObj)]
                                         ),
                              kzd_app:unpublish(AppJObj)
                      end
              end
             ,AppJObjs
             ).

-spec get_whitelabel_doc(kz_term:ne_binary()) -> kz_json:object().
get_whitelabel_doc(AccountId) ->
    case kzd_whitelabel:fetch(AccountId) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            kzd_whitelabel:new();
        {'error', _R} ->
            lager:error("failed to load whitelabel doc for ~s: ~p"
                       ,[AccountId, _R]
                       ),
            kzd_whitelabel:new()
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_master_account(kz_term:ne_binary(), kz_term:api_binary(), kz_json:objects()) -> kz_json:objects().
allowed_master_account(AccountId, _UserId, AppJObjs) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    case AccountId =:= MasterAccountId of
        'false' -> AppJObjs;
        'true' ->
            lager:debug("granting master account access to all apps"),
            [kzd_app:publish(AppJObj)
             || AppJObj <- AppJObjs
            ]
    end.

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
    AccountId = kz_util:format_account_id(Account),
    AccountDb = kz_util:format_account_db(Account),
    JObj = kz_json:get_value(<<"doc">>, Doc),
    case kz_doc:account_db(JObj) =/= AccountDb
        orelse kz_doc:account_id(JObj) =/= AccountId
    of
        'false' -> JObj;
        'true' ->
            set_account(Account, JObj)
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
-spec get_services_apps(kz_term:ne_binary()) -> kz_json:object().
get_services_apps(AccountId) ->
    ServicesApps = kz_services_applications:fetch(AccountId),
    ResellerId = kz_services_reseller:get_id(AccountId),
    case kz_term:is_empty(ServicesApps) of
        'true' when ResellerId =:= AccountId ->
            ServicesApps;
        'true' ->
            lager:debug("account ~s doesn't have apps in service plan, checking reseller ~s"
                       ,[AccountId, ResellerId]
                       ),
            kz_services_applications:fetch(AccountId);
        'false' ->
            lager:debug("account ~s has apps in service plan", [AccountId]),
            ServicesApps
    end.
