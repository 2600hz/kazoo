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
-spec reconcile(wh_services:services(), api_binary() | wh_json:object()) -> wh_services:services().
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
            lager:debug("empty results when reconciling ~s", [AccountId]),
            wh_services:reset_category(?CATEGORY, Services);
        {'ok', JObjs} ->
            lager:debug("reconciling ~p devices in ~s: ~p", [length(JObjs), AccountId, JObjs]),
            lists:foldl(fun reconcile_device/2
                        ,wh_services:reset_category(?CATEGORY, Services)
                        ,JObjs
                       )
    end.

reconcile(Services, 'undefined') ->
    Services;
reconcile(Services, <<_/binary>> = DeviceType) ->
    case wh_services:is_dirty(Services) of
        'true' ->
            lager:debug("doing full reconcile for ~s", [DeviceType]),
            do_reconcile(reconcile(Services), DeviceType);
        'false' ->
            lager:debug("doing partial reconcile for ~s", [DeviceType]),
            do_reconcile(Services, DeviceType)
    end.

-spec do_reconcile(wh_services:services(), ne_binary()) -> wh_services:services().
do_reconcile(Services, DeviceType) ->
    Quantity = wh_services:updated_quantity(?CATEGORY, DeviceType, Services),
    lager:debug("increment ~s.~s to ~p+1", [?CATEGORY, DeviceType, Quantity]),
    wh_services:update(?CATEGORY, DeviceType, Quantity+1, Services).

-spec reconcile_device(wh_json:object(), wh_services:services()) -> wh_services:services().
reconcile_device(JObj, Services) ->
    Item = wh_json:get_value(<<"key">>, JObj),
    Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),

    lager:debug("reconciling device ~s to ~p", [Item, Quantity]),

    wh_services:update(?CATEGORY, Item, Quantity, Services).
