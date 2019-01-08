%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_ledgers).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).

-define(CATEGORY, <<"ledgers">>).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    case kz_ledgers:get(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current ledgers in service: ~p for account: ~s", [_R, AccountId]),
            Services;
        {'ok', JObjs} ->
            reconcile_account(Services, JObjs)
    end.

-spec reconcile(kz_services:services(), kz_term:ne_binary() | kz_term:proplist()) -> kz_services:services().
reconcile(Services, Type) when is_binary(Type) ->
    Services1 = reconcile(Services),
    Quantity = kz_services:updated_quantity(?CATEGORY, Type, Services1),
    kz_services:update(?CATEGORY, Type, Quantity+1, Services1);
reconcile(Services, Props) ->
    lists:foldl(fun reconcile_foldl/2
               ,reconcile(Services)
               ,Props
               ).

%%------------------------------------------------------------------------------
%% Internal Function Definitions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_foldl({kz_term:ne_binary(), integer() | kz_term:ne_binary()}, kz_services:services()) -> kz_services:services().
reconcile_foldl({Type, Quantity}, Services) ->
    OldQuantity = kz_services:updated_quantity(?CATEGORY, Type, Services),
    kz_services:update(?CATEGORY
                      ,Type
                      ,OldQuantity + kz_term:to_integer(Quantity)
                      ,Services
                      ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_account(kz_services:services(), kz_json:object()) -> kz_services:services().
reconcile_account(Services, JObj) ->
    kz_json:foldl(fun reconcile_account_foldl/3
                 ,kz_services:reset_category(?CATEGORY, Services)
                 ,JObj
                 ).

reconcile_account_foldl(Type, Quantity, Services) when Quantity < 0 ->
    kz_services:update(?CATEGORY, Type, -1*Quantity, Services);
reconcile_account_foldl(_Type, _Quantity, Services)->
    lager:debug("quantity for ~p is not negative ignoring (~p)", [_Type, _Quantity]),
    Services.
