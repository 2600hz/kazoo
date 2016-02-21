%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_service_ui_apps).

-export([reconcile/1, reconcile/2]).
-export([is_in_use/1]).

-define(ACCOUNTS_DB, <<"accounts">>).
-define(CATEGORY, <<"ui_apps">>).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), ne_binary()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    case kz_datamgr:open_doc(?ACCOUNTS_DB, AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current ui apps in service: ~p for account: ~s", [_R, AccountId]),
            Services;
        {'ok', AccountDoc} ->
            reconcile_account(Services, AccountDoc)
    end.

-spec reconcile_account(wh_services:services(), wh_json:object()) -> wh_services:services().
reconcile_account(Services, AccountDoc) ->
    wh_json:foldl(
      fun(_, AppJObj, S) ->
              case is_in_use(AppJObj) of
                  'false' -> S;
                  'true' ->
                      AppName = wh_json:get_value(<<"name">>, AppJObj),
                      wh_services:update(?CATEGORY, AppName, 1, S)
              end
      end
      ,wh_services:reset_category(<<"ui_apps">>, Services)
      ,wh_json:get_value(<<"ui_apps">>, AccountDoc, wh_json:new())
     ).

reconcile(Services, AppName) ->
    %% Because you can only be charged once for an app
    wh_services:update(
      ?CATEGORY
      ,AppName
      ,1
      ,wh_services:reset_category(<<"ui_apps">>, Services)
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_in_use(wh_json:object()) -> boolean().
is_in_use(AppJObj) ->
    Allowed = wh_json:get_value(<<"allowed_users">>, AppJObj, <<"specific">>),
    Users = wh_json:get_value(<<"users">>, AppJObj, []),
    case {Allowed, Users} of
        {<<"all">>, _} -> 'true';
        {<<"specific">>, []} -> 'false';
        {<<"specific">>, _} -> 'true';
        {<<"admins">>, _} -> 'true';
        {_A, _U} ->
            lager:error("unknown data ~p : ~p", [_A, _U]),
            'false'
    end.
