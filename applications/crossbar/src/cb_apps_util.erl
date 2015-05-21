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

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get allowed apps from service plans
%% @end
%%--------------------------------------------------------------------
-spec allowed_apps(ne_binary()) -> wh_json:objects().
allowed_apps(AccountId)
  when is_binary(AccountId) ->
    ServicePlan = wh_services:service_plan_json(AccountId),
    DefaultApps = load_default_apps(),
    Apps = filter_apps(AccountId, DefaultApps),
    case 'undefined' == wh_json:get_value(<<"ui_apps">>, ServicePlan)
        orelse wh_json:is_true([<<"ui_apps">>, <<"_all">>, <<"enabled">>], ServicePlan)
    of
        'true' -> Apps;
        'false' -> find_enabled_apps(get_plan_apps(ServicePlan), Apps, [])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Get app object if allowed
%% @end
%%--------------------------------------------------------------------
-spec allowed_app(ne_binary(), ne_binary()) -> api_object().
allowed_app(AccountId, AppId)
  when is_binary(AccountId), is_binary(AppId) ->
    FindApp = fun(App) -> AppId == wh_doc:id(App) end,
    case lists:filter(FindApp, allowed_apps(AccountId)) of
        [App] -> App;
        [] -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check user permissions of specified app
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(wh_json:object(), ne_binary(), wh_json:object()) -> boolean().
is_authorized(AccountDoc, UserId, App) ->
    JObj = wh_json:get_value([<<"apps">>, wh_doc:id(App)], AccountDoc),
    Allowed = wh_json:get_value(<<"allowed_users">>, JObj, <<"specific">>),
    Users = wh_json:get_value(<<"users">>, JObj, []),
    case {Allowed, Users} of
        {<<"all">>, _} -> 'true';
        {<<"specific">>, []} -> 'false';
        {<<"specific">>, Users} when is_list(Users) ->
            lists:member(UserId, Users);
        {<<"admins">>, _} ->
            AccountId = wh_doc:id(AccountDoc),
            <<"admin">> =:= get_user_priv_level(AccountId, UserId);
        {_A, _U} ->
            lager:error("unknown data ~p : ~p", [_A, _U]),
            'false'
    end.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec filter_apps(ne_binary(), wh_json:objects()) -> wh_json:objects().
filter_apps(AccountId, Apps) ->
    lists:foldl(
        fun(App, Acc) ->
            Name = wh_json:get_value(<<"name">>, App),
            Filtered = filter_app(AccountId, App, Name),
            case wh_json:is_empty(Filtered) of
                'true' -> Acc;
                'false' -> [Filtered|Acc]
            end
        end
        ,[]
        ,Apps
    ).

-spec filter_app(ne_binary(), wh_json:object(), ne_binary()) -> wh_json:object().
filter_app(AccountId, App, <<"port">>) ->
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
    case MaybeHide of
        'true' -> wh_json:new();
        'false' -> App
    end;
filter_app(_AccountId, App, _Name) ->
    lager:debug("not filtering ~s", [_Name]),
    App.

-spec load_default_apps() -> wh_json:objects().
load_default_apps() ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    lager:debug("loading default apps from master account : ~s", [MasterAccountDb]),
    case couch_mgr:get_results(MasterAccountDb, ?CB_APPS_STORE_LIST, ['include_docs']) of
        {'error', _E} ->
            lager:error("failed to lookup apps in ~s", [MasterAccountDb]),
            [];
        {'ok', JObjs} ->
            [maybe_set_account(MasterAccountDb, JObj) || JObj <- JObjs]
    end.

-spec maybe_set_account(ne_binary(), wh_json:object()) -> wh_json:object().
maybe_set_account(Account, Doc) ->
    JObj = wh_json:get_value(<<"doc">>, Doc),
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
        wh_json:set_values(
          [{<<"pvt_account_id">>, AccountId}
           ,{<<"pvt_account_db">>, AccountDb}
          ], JObj),
    case couch_mgr:save_doc(AccountDb, Corrected) of
        {'error', _R} ->
            lager:error("failed to correct app"),
            Corrected;
        {'ok', Doc} -> Doc
    end.

-spec get_plan_apps(wh_json:object()) -> wh_json:objects().
get_plan_apps(ServicePlan) ->
    JObjs = wh_json:get_value(<<"ui_apps">>, ServicePlan),
    wh_json:delete_key(<<"_all">>, JObjs).

-spec find_enabled_apps(wh_json:objects(), wh_json:objects(), wh_json:objects()) -> wh_json:objects().
find_enabled_apps([], _DefaultApps, Acc) -> Acc;
find_enabled_apps([PlanApp|PlanApps], DefaultApps, Acc) ->
    [AppName] = wh_json:get_keys(PlanApp),
    AppId     = wh_json:get_value(<<"app_id">>, PlanApp),
    case wh_json:is_false(<<"enabled">>, PlanApp)
        orelse find_app(AppName, AppId, DefaultApps)
    of
        'true' -> find_enabled_apps(PlanApps, DefaultApps, Acc);
        AppJObj -> find_enabled_apps(PlanApps, DefaultApps, [AppJObj|Acc])
    end.

-spec find_app(ne_binary(), ne_binary(), wh_json:objects()) ->
                      wh_json:object().
-spec find_app(ne_binary(), ne_binary(), wh_json:objects()
               ,ne_binary(), ne_binary(), wh_json:object()
              ) ->
                      wh_json:object().
find_app(AppName, AppId, [AppJObj|DefaultApps]) ->
    find_app(AppName, AppId, DefaultApps
             ,wh_doc:id(AppJObj)
             ,wh_json:get_value(<<"name">>, AppJObj)
             ,AppJObj
            ).

find_app(AppName, AppId, _DefaultApps, AppId, AppName, AppJObj) ->
    AppJObj;
find_app(AppName, AppId, DefaultApps, _Id, _Name, _JObj) ->
    find_app(AppName, AppId, DefaultApps).
