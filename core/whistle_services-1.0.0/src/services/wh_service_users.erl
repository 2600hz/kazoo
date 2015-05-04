%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_users).

-export([reconcile/1]).
-export([reconcile/2]).

-include("../whistle_services.hrl").

-define(SERVICE_CATEGORY, <<"users">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), api_binary()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                  ],
    case couch_mgr:get_results(AccountDb, <<"services/users">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current users in service: ~p", [_R]),
            Services;
        {'ok', []} -> wh_services:reset_category(?SERVICE_CATEGORY, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = wh_json:get_value(<<"key">>, JObj),
                                Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
                                wh_services:update(?SERVICE_CATEGORY, Item, Quantity, S)
                        end, wh_services:reset_category(?SERVICE_CATEGORY, Services), JObjs)
    end.

reconcile(Services, 'undefined') -> Services;
reconcile(Services0, UserType) ->
    Services1 = reconcile(Services0),
    Quantity = wh_services:updated_quantity(?SERVICE_CATEGORY, UserType, Services1),
    wh_services:update(?SERVICE_CATEGORY, UserType, Quantity+1, Services1).
