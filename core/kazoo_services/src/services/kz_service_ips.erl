%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_ips).

-export([reconcile/1, reconcile/2]).

-include("services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), api_binary() | kz_proplist()) ->
                       kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    case kz_ips:assigned(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current dedicated ips in service: ~p", [_R]),
            Services;
        {'ok', JObjs} ->
            S = kz_services:reset_category(<<"ips">>, Services),
            kz_services:update(<<"ips">>, <<"dedicated">>, length(JObjs), S)
    end.

reconcile(Services, 'undefined') -> Services;
reconcile(Services0, IpType) when is_binary(IpType) ->
    Services1 = reconcile(Services0),
    Quantity = kz_services:updated_quantity(<<"ips">>, IpType, Services1),
    kz_services:update(<<"ips">>, IpType, Quantity+1, Services1);
reconcile(Services, Props) ->
    lists:foldl(
      fun reconcile_foldl/2
               ,reconcile(Services)
               ,Props
     ).

-spec reconcile_foldl({ne_binary(), integer() | ne_binary()}, kz_services:services()) ->
                             kz_services:services().
reconcile_foldl({Type, Quantity}, Services) ->
    OldQuantity = kz_services:updated_quantity(<<"ips">>, Type, Services),
    kz_services:update(<<"ips">>
                      ,Type
                      ,OldQuantity + kz_term:to_integer(Quantity)
                      ,Services
                      ).
