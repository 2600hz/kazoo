%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_devices).
-behaviour(kz_gen_service).

-export([reconcile/1]).
-export([reconcile/2]).

-include("services.hrl").

-define(CATEGORY, <<"devices">>).

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
    case kz_datamgr:get_results(AccountDb, <<"services/devices">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current devices in service: ~p", [_R]),
            Services;
        {'ok', []} ->
            lager:debug("empty results when reconciling ~s", [AccountId]),
            kz_services:reset_category(?CATEGORY, Services);
        {'ok', JObjs} ->
            lager:debug("reconciling ~p devices in ~s: ~p", [length(JObjs), AccountId, JObjs]),
            lists:foldl(fun reconcile_device/2
                       ,kz_services:reset_category(?CATEGORY, Services)
                       ,JObjs
                       )
    end.

-spec reconcile(kz_services:services(), kz_term:api_binary() | kz_json:object()) -> kz_services:services().
reconcile(Services, 'undefined') ->
    Services;
reconcile(Services, <<_/binary>> = DeviceType) ->
    case kz_services:is_dirty(Services) of
        'true' ->
            lager:debug("doing full reconcile for ~s", [DeviceType]),
            do_reconcile(reconcile(Services), DeviceType);
        'false' ->
            lager:debug("doing partial reconcile for ~s", [DeviceType]),
            do_reconcile(Services, DeviceType)
    end.

-spec do_reconcile(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
do_reconcile(Services, DeviceType) ->
    Quantity = kz_services:quantity(?CATEGORY, DeviceType, Services),
    lager:debug("increment ~s.~s to ~p+1", [?CATEGORY, DeviceType, Quantity]),
    kz_services:update(?CATEGORY, DeviceType, Quantity+1, Services).

-spec reconcile_device(kz_json:object(), kz_services:services()) -> kz_services:services().
reconcile_device(JObj, Services) ->
    Item = kz_json:get_value(<<"key">>, JObj),
    Quantity = kz_json:get_integer_value(<<"value">>, JObj, 0),

    lager:debug("reconciling device ~s to ~p", [Item, Quantity]),

    kz_services:update(?CATEGORY, Item, Quantity, Services).
