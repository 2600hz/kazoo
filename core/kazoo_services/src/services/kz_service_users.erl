%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_users).

-export([reconcile/1]).
-export([reconcile/2]).

-include("kazoo_services.hrl").

-define(SERVICE_CATEGORY, <<"users">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), api_binary()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"services/users">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current users in service: ~p", [_R]),
            Services;
        {'ok', []} -> kz_services:reset_category(?SERVICE_CATEGORY, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = kz_json:get_value(<<"key">>, JObj),
                                Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),
                                kz_services:update(?SERVICE_CATEGORY, Item, Quantity, S)
                        end, kz_services:reset_category(?SERVICE_CATEGORY, Services), JObjs)
    end.

reconcile(Services, 'undefined') -> Services;
reconcile(Services0, UserType) ->
    Services1 = reconcile(Services0),
    Quantity = kz_services:updated_quantity(?SERVICE_CATEGORY, UserType, Services1),
    kz_services:update(?SERVICE_CATEGORY, UserType, Quantity+1, Services1).
