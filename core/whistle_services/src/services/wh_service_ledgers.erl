%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_service_ledgers).

-export([reconcile/1, reconcile/2]).

-define(CATEGORY, <<"ledgers">>).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
-spec reconcile(wh_services:services(), ne_binary()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    case kz_ledgers:get(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get current ledgers in service: ~p for account: ~s", [_R, AccountId]),
            Services;
        {'ok', JObjs} ->
            reconcile_account(Services, JObjs)
    end.

reconcile(Services, Type) when is_binary(Type) ->
    Services1 = reconcile(Services),
    Quantity = wh_services:updated_quantity(?CATEGORY, Type, Services1),
    wh_services:update(?CATEGORY, Type, Quantity+1, Services1);
reconcile(Services, Props) ->
    lists:foldl(
        fun reconcile_foldl/2
        ,reconcile(Services)
        ,Props
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reconcile_foldl({ne_binary(), integer() | ne_binary()}, wh_services:services()) -> wh_services:services().
reconcile_foldl({Type, Quantity}, Services) ->
    OldQuantity = wh_services:updated_quantity(?CATEGORY, Type, Services),
    wh_services:update(
        ?CATEGORY
        ,Type
        ,OldQuantity + wh_util:to_integer(Quantity)
        ,Services
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reconcile_account(wh_services:services(), wh_json:objects()) -> wh_services:services().
reconcile_account(Services, JObjs) ->
    wh_json:foldl(
        fun reconcile_account_foldl/3
        ,wh_services:reset_category(?CATEGORY, Services)
        ,JObjs
    ).

reconcile_account_foldl(Type, Quantity, Services) when Quantity < 0 ->
    wh_services:update(?CATEGORY, Type, -1*Quantity, Services);
reconcile_account_foldl(_Type, _Quantity, Services)->
    lager:debug("quantity for ~p is not negative ignoring (~p)", [_Type, _Quantity]),
    Services.
