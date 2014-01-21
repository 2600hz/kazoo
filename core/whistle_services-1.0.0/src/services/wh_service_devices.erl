%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz, INC
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
        {'ok', []} -> wh_services:reset_category(<<"devices">>, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = wh_json:get_value(<<"key">>, JObj),
                                Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
                                wh_services:update(<<"devices">>, Item, Quantity, S)
                        end, wh_services:reset_category(<<"devices">>, Services), JObjs)
    end.

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
        {'ok', []} -> wh_services:reset_category(<<"devices">>, Services);
        {'ok', JObjs} ->
            S = lists:foldl(
                    fun(JObj, S) ->
                        Item = wh_json:get_value(<<"key">>, JObj),
                        Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
                        case Item =:= DeviceType of
                            'false' -> wh_services:update(<<"devices">>, Item, Quantity, S);
                            'true' -> wh_services:update(<<"devices">>, Item, Quantity+1, S)
                        end
                    end
                    ,wh_services:reset_category(<<"devices">>, Services)
                    ,JObjs
                ),
            Keys = couch_mgr:get_result_keys(JObjs),
            case lists:member(DeviceType, Keys) of
                'false' -> wh_services:update(<<"devices">>, DeviceType, 1, S);
                'true' -> S
            end
    end.




