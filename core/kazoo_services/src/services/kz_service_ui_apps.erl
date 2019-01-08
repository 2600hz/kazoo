%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_ui_apps).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).
-export([is_in_use/1]).

-define(ACCOUNTS_DB, <<"accounts">>).
-define(CATEGORY, <<"ui_apps">>).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    case kz_datamgr:open_doc(?ACCOUNTS_DB, AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current ui apps in service: ~p for account: ~s", [_R, AccountId]),
            Services;
        {'ok', AccountDoc} ->
            reconcile_account(Services, AccountDoc)
    end.

-spec reconcile_account(kz_services:services(), kz_json:object()) -> kz_services:services().
reconcile_account(Services, AccountDoc) ->
    kz_json:foldl(fun(_, AppJObj, S) ->
                          case is_in_use(AppJObj) of
                              'false' -> S;
                              'true' ->
                                  AppName = kzd_app:name(AppJObj),
                                  kz_services:update(?CATEGORY, AppName, 1, S)
                          end
                  end
                 ,kz_services:reset_category(?CATEGORY, Services)
                 ,kz_json:get_value(<<"ui_apps">>, AccountDoc, kz_json:new())
                 ).

-spec reconcile(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
reconcile(Services, AppName) ->
    %% Because you can only be charged once for an app
    NewServices = kz_services:reset_category(?CATEGORY, Services),
    kz_services:update(?CATEGORY, AppName, 1, NewServices).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_in_use(kz_json:object()) -> boolean().
is_in_use(AppJObj) ->
    Allowed = kzd_app:allowed_users(AppJObj, <<"specific">>),
    Users = kzd_app:users(AppJObj),
    case {Allowed, Users} of
        {<<"all">>, _} -> 'true';
        {<<"specific">>, []} -> 'false';
        {<<"specific">>, _} -> 'true';
        {<<"admins">>, _} -> 'true';
        {_A, _U} ->
            lager:error("unknown data ~p : ~p", [_A, _U]),
            'false'
    end.
