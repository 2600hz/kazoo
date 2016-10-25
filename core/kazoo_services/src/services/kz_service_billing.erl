%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% VCCS Telecom
%%%-------------------------------------------------------------------
-module(kz_service_billing).

-export([reconcile/1, reconcile/2]).

-include("kazoo_services.hrl").

-define(SERVICE_CATEGORY, <<"billing">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                  ,'group'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"services/opaque_billing">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current opaque billing quantites: ~p", [_R]),
            Services;
        {'ok', []} -> kz_services:reset_category(?SERVICE_CATEGORY, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = kz_json:get_value(<<"key">>, JObj),
                                Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),
                                kz_services:update(?SERVICE_CATEGORY, Item, Quantity, S)
                        end, kz_services:reset_category(?SERVICE_CATEGORY, Services), JObjs)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services(), kz_json:object()) -> kz_services:services().
reconcile(Services, 'undefined') -> Services;
reconcile(Services0, Item) ->
    Services1 = reconcile(Services0),
    Quantity = kz_services:updated_quantity(?SERVICE_CATEGORY, Item, Services1),
    kz_services:update(?SERVICE_CATEGORY, Item, Quantity+1, Services1).
