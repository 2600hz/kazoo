%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author VCCS Telecom
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_billing).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).

-include("services.hrl").

-define(SERVICE_CATEGORY, <<"billing">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
            lists:foldl(fun reconcile_fold/2
                       ,kz_services:reset_category(?SERVICE_CATEGORY, Services)
                       ,JObjs
                       )
    end.

-spec reconcile_fold(kz_json:object(), kz_servers:services()) -> kz_services:services().
reconcile_fold(JObj, S) ->
    Item = kz_json:get_value(<<"key">>, JObj),
    Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),
    kz_services:update(?SERVICE_CATEGORY, Item, Quantity, S).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile(kz_services:services(), kz_term:api_binary()) -> kz_services:services().
reconcile(Services, 'undefined') -> Services;
reconcile(Services0, Item) ->
    Services1 = reconcile(Services0),
    Quantity = kz_services:updated_quantity(?SERVICE_CATEGORY, Item, Services1),
    kz_services:update(?SERVICE_CATEGORY, Item, Quantity+1, Services1).
