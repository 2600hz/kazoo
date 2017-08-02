%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kz_service_ledgers).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).

-include("services.hrl").


-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    case kz_ledgers:get(AccountId) of
        {'ok', JObjs} -> reconcile_account(Services, JObjs);
        {'error', _R} ->
            lager:warning("did not get ledgers in service for ~s: ~p", [AccountId, _R]),
            Services
    end.

-spec reconcile(kz_services:services(), ne_binary()) -> kz_services:services().
reconcile(Services, Type=?NE_BINARY) ->
    NewServices = reconcile(Services),
    Quantity = kz_services:updated_quantity(category(), Type, NewServices),
    kz_services:update(category(), Type, Quantity + 1, NewServices);
reconcile(Services, Props) ->
    NewServices = reconcile(Services),
    lists:foldl(fun reconcile_foldl/2, NewServices, Props).


%% Internals

category() -> <<"ledgers">>.

-spec reconcile_foldl({ne_binary(), integer() | ne_binary()}, kz_services:services()) ->
                             kz_services:services().
reconcile_foldl({Type, Quantity}, Services) ->
    OldQuantity = kz_services:updated_quantity(category(), Type, Services),
    NewQuantity = OldQuantity + kz_term:to_integer(Quantity),
    kz_services:update(category(), Type, NewQuantity, Services).

-spec reconcile_account(kz_services:services(), kz_json:objects()) ->
                               kz_services:services().
reconcile_account(Services, JObjs) ->
    ResetServices = kz_services:reset_category(category(), Services),
    kz_json:foldl(fun reconcile_account_foldl/3, ResetServices, JObjs).

reconcile_account_foldl(Type, Quantity, Services)
  when Quantity < 0 ->
    kz_services:update(category(), Type, -1 * Quantity, Services);
reconcile_account_foldl(_Type, _Quantity, Services)->
    lager:debug("quantity ~p for ~p is not negative, ignoring", [_Quantity, _Type]),
    Services.
