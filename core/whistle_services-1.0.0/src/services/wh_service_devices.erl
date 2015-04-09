%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_devices).

-export([reconcile/1]).
-export([reconcile/2]).

-include("../whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), api_binary()) -> wh_services:services().
reconcile(Services) ->
    reconcile(Services, 'undefined').

reconcile(Services, DeviceType) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                  ],
    case couch_mgr:get_results(AccountDb, <<"services/devices">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current devices in service: ~p", [_R]),
            Services;
        {'ok', []} when DeviceType =:= 'undefined' -> wh_services:reset_category(<<"devices">>, Services);
        {'ok', []} ->
            wh_services:update(<<"devices">>, DeviceType, 1, Services);
        {'ok', JObjs} ->
            reconcile_devices(Services, DeviceType, JObjs)
    end.

-spec reconcile_devices(wh_services:services(), api_binary(), wh_json:objects()) ->
                               wh_services:services().
reconcile_devices(Services, DeviceType, JObjs) ->
    S = lists:foldl(fun(JObj, S) -> reconcile_devices_fold(JObj, S, DeviceType) end
                    ,wh_services:reset_category(<<"devices">>, Services)
                    ,JObjs
                   ),
    Keys = couch_mgr:get_result_keys(JObjs),
    case lists:member(DeviceType, Keys) of
        'false' -> wh_services:update(<<"devices">>, DeviceType, 1, S);
        'true' -> S
    end.

-spec reconcile_devices_fold(wh_json:object(), wh_services:services(), ne_binary()) ->
                                    wh_services:services().
reconcile_devices_fold(JObj, S, DeviceType) ->
    Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
    case wh_json:get_value(<<"key">>, JObj) of
        DeviceType -> wh_services:update(<<"devices">>, DeviceType, Quantity+1, S);
        Item -> wh_services:update(<<"devices">>, Item, Quantity, S)
    end.
