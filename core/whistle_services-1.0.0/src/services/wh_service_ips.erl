%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_ips).

-export([reconcile/1, reconcile/2]).

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
    AccountId = wh_services:account_id(Services),
    case kz_ips:assigned(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current dedicated ips in service: ~p", [_R]),
            Services;
        {'ok', JObjs} ->
            S = wh_services:reset_category(<<"ips">>, Services),
            wh_services:update(<<"ips">>, <<"dedicated">>, length(JObjs), S)
    end.

reconcile(Services, 'undefined') -> Services;
reconcile(Services0, IpType) when is_binary(IpType) ->
    Services1 = reconcile(Services0),
    Quantity = wh_services:update_quantity(<<"ips">>, IpType, Services1),
    wh_services:update(<<"ips">>, IpType, Quantity+1, Services1);
reconcile(Services, Props) ->
    lists:foldl(
        fun reconcile_foldl/2
        ,reconcile(Services)
        ,Props
    ).

reconcile_foldl({Type, Quantity}, Services) when is_integer(Quantity) ->
    OldQuantity = wh_services:update_quantity(<<"ips">>, Type, Services),
    wh_services:update(<<"ips">>, Type, OldQuantity+Quantity, Services);
reconcile_foldl({Type, Quantity}, Services) ->
    reconcile_foldl({Type, wh_util:to_integer(Quantity)}, Services).
