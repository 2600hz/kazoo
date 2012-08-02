%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_plan).

-include_lib("whistle_services/src/whistle_services.hrl").

-export([fetch/3]).
-export([create_items/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> 'undefined' | wh_json:json_object().
fetch(PlanId, VendorDb, _Overrides) ->
    case couch_mgr:open_doc(VendorDb, PlanId) of
        {ok, JObj} -> JObj;
        {error, _R} ->
            lager:debug("unable to open service plan ~s in vendor db ~s: ~p", [PlanId, VendorDb, _R]),
            undefined
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec create_items/2 :: (wh_json:json_object(), wh_services:services()) -> item().
-spec create_items/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), wh_services:services()) -> item().

create_items(ServicePlan, Services) ->
    Plan = wh_json:get_value(<<"plan">>, ServicePlan, wh_json:new()),
    [create_items(Category, Item, wh_json:get_value([Category, Item], Plan), Services)
     || Category <- wh_json:get_keys(Plan)
            ,Item <- wh_json:get_keys(Category, Plan)
    ].

create_items(Category, Item, Plan, Services) ->
    io:format("PROCESS ~s/~s: ~p~n", [Category, Item, Plan]),
    Quantity = get_item_quantity(Category, Item, Plan, Services),
    CorrectedQuantity = case wh_json:get_integer_value(<<"minimum">>, Plan, 0) of
                            Min when Min > Quantity -> Min;
                            _Else -> Quantity
                        end,
    #wh_service_item{category = Category
                     ,item = wh_json:get_ne_value(<<"as">>, Plan, Item) 
                     ,quantity = CorrectedQuantity
                     ,rate = get_item_rate(CorrectedQuantity, Plan)
                     ,single_discount = false
                     ,single_discount_price = 0.00
                     ,cumulative_discount = false
                     ,cumulative_discount_price = 0.00
                    }.                     

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_item_rate/2 :: (wh_json:json_object(), non_neg_integer()) -> ne_binary().
get_item_rate(Quantity, Plan) ->
    Rates = wh_json:get_value(<<"rates">>, Plan, wh_json:new()),
    L1 = [wh_util:to_integer(K) || K <- wh_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] -> wh_json:get_float_value(<<"rate">>, Plan);
        Range -> wh_json:get_float_value(wh_util:to_binary(hd(Range)), Rates)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_item_quantity/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), wh_services:services()) -> integer().
get_item_quantity(Category, <<"_all">>, Plan, Services) ->
    Exceptions = wh_json:get_value(<<"exceptions">>, Plan, []),
    case wh_json:is_true(<<"cascade">>, Plan) of
        true -> wh_services:cascade_category_quantity(Category, Exceptions, Services);
        false -> wh_services:category_quantity(Category, Exceptions, Services)
    end;    
get_item_quantity(Category, Item, Plan, Services) ->
    case wh_json:is_true(<<"cascade">>, Plan) of
        true -> wh_services:cascade_quantity(Category, Item, Services);
        false -> wh_services:quantity(Category, Item, Services)
    end.
