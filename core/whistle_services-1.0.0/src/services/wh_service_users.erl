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
    case couch_mgr:get_results(AccountDb, <<"services/users">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current users in service: ~p", [_R]),
            Services;
        {'ok', []} -> wh_services:reset_category(<<"users">>, Services);
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, S) ->
                                Item = wh_json:get_value(<<"key">>, JObj),
                                Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
                                wh_services:update(<<"users">>, Item, Quantity, S)
                        end, wh_services:reset_category(<<"users">>, Services), JObjs)
    end.

reconcile(Services, DeviceType) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['reduce'
                   ,'group'
                  ],
    case couch_mgr:get_results(AccountDb, <<"services/users">>, ViewOptions) of
        {'error', _R} ->
            lager:debug("unable to get current users in service: ~p", [_R]),
            Services;
        {'ok', []} -> wh_services:reset_category(<<"users">>, Services);
        {'ok', JObjs} ->
            lists:foldl(
                fun(JObj, S) ->
                    Item = wh_json:get_value(<<"key">>, JObj),
                    Quantity = wh_json:get_integer_value(<<"value">>, JObj, 0),
                    case Item =:= DeviceType of
                        'false' -> wh_services:update(<<"users">>, Item, Quantity, S);
                        'true' -> wh_services:update(<<"users">>, Item, Quantity+1, S)
                    end
                end
                ,wh_services:reset_category(<<"users">>, Services)
                ,JObjs
            )
    end.

