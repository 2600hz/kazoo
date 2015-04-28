%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_devices).

-export([reconcile/1]).
-export([reconcile/2]).

-include("../whistle_services.hrl").

-define(CATEGORY, <<"devices">>).

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
    case couch_mgr:get_results(AccountDb, <<"services/devices">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current devices in service: ~p", [_R]),
            Services;
        {'ok', []} ->
            wh_services:reset_category(?CATEGORY, Services);
        {'ok', JObjs} ->
            lists:foldl(fun reconcile_device/2
                        ,wh_services:reset_category(?CATEGORY, Services)
                        ,JObjs
                       )
    end.

reconcile(Services, 'undefined') ->
    Services;
reconcile(Services0, DeviceType) ->
    Services1 = reconcile(Services0),
    Quantity = wh_services:update_quantity(?CATEGORY, DeviceType, Services1),

    wh_services:update(?CATEGORY, DeviceType, Quantity+1, Services1).

-spec reconcile_device(wh_json:object(), wh_services:services()) -> wh_services:services().
reconcile_device(JObj, Services) ->
    Item = wh_json:get_value(<<"key">>, JObj),
    Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),

    CurrentQuantity = wh_services:quantity(?CATEGORY, Item, Services),

    wh_services:update(?CATEGORY, Item, Quantity+CurrentQuantity, Services).
