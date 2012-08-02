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
-export([create_items/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> 'undefined' | wh_json:json_object().
fetch(PlanId, VendorDb, _Overrides) ->
    case couch_mgr:open_doc(VendorDb, PlanId) of
        {ok, JObj} -> 
            lager:debug("using service plan ~s in vendor db ~s", [PlanId, VendorDb]),
            JObj;
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
-spec create_items/3 :: (wh_json:json_object(), wh_service_items:items(), wh_services:services()) -> wh_service_items:items().
-spec create_items/5 :: (ne_binary(), ne_binary(), wh_json:json_object(), wh_service_items:items(), wh_services:services()) -> wh_service_items:items().

create_items(ServicePlan, Items, Services) ->
    Plan = wh_json:get_value(<<"plan">>, ServicePlan, wh_json:new()),
    Plans = [{Category, Item}
             || Category <- wh_json:get_keys(Plan)
                    ,Item <- wh_json:get_keys(Category, Plan)
            ],
    lists:foldl(fun({Category, Item}, I) ->
                        ItemPlan = wh_json:get_value([Category, Item], Plan),
                        create_items(Category, Item, ItemPlan, I, Services)
                end, Items, Plans).

create_items(Category, Item, ItemPlan, Items, Services) ->
    Quantity = get_item_quantity(Category, Item, ItemPlan, Services),
    CorrectedQuantity = case wh_json:get_integer_value(<<"minimum">>, ItemPlan, 0) of
                            Min when Min > Quantity -> 
                                lager:debug("minimum '~s/~s' not met with ~p, enforcing quantity ~p", [Category, Item, Quantity, Min]),
                                Min;
                            _ -> Quantity
                        end,
    case CorrectedQuantity > 0 of
        false -> Items;
        true ->
            Rate = get_rate(CorrectedQuantity, ItemPlan),
            As = wh_json:get_ne_value(<<"as">>, ItemPlan, Item),
            Routines = [fun(I) -> wh_service_items:update(Category, As, CorrectedQuantity, Rate, I) end
                        ,fun(I) ->
                                 case wh_json:get_value([<<"discounts">>, <<"single">>], ItemPlan) of
                                     undefined -> I;
                                     SingleDiscount ->
                                         SingleRate = wh_json:get_float_value(<<"rate">>, SingleDiscount, Rate),
                                         wh_service_items:set_single_discount(Category, Item, SingleRate, I)
                                 end
                         end
                        ,fun(I) ->
                                 case wh_json:get_value([<<"discounts">>, <<"cumulative">>], ItemPlan) of
                                     undefined -> I;
                                     CumulativeDiscount ->
                                         CumulativeQuantity = case wh_json:get_integer_value(<<"maximum">>, CumulativeDiscount, 0) of
                                                                  Max when Max < CorrectedQuantity -> 
                                                                      lager:debug("item '~s/~s' quantity ~p exceeds cumulative discount max, using ~p"
                                                                                  ,[Category, Item, CorrectedQuantity, Max]),
                                                                      Max;
                                                                  _ -> CorrectedQuantity
                                                              end,
                                         CumulativeRate = case get_rate(CorrectedQuantity, CumulativeDiscount) of
                                                              undefined -> Rate;
                                                              Else -> Else
                                                          end,
                                         wh_service_items:set_cumulative_discount(Category, Item, CumulativeQuantity, CumulativeRate, I)
                                 end
                         end
                       ],
            lists:foldl(fun(F, I) -> F(I) end, Items, Routines)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_rate/2 :: (non_neg_integer(), wh_json:json_object()) -> ne_binary().
get_rate(Quantity, JObj) ->
    Rates = wh_json:get_value(<<"rates">>, JObj, wh_json:new()),
    L1 = [wh_util:to_integer(K) || K <- wh_json:get_keys(Rates)],
    case lists:dropwhile(fun(K) -> Quantity > K end, lists:sort(L1)) of
        [] -> wh_json:get_float_value(<<"rate">>, JObj);
        Range -> wh_json:get_float_value(wh_util:to_binary(hd(Range)), Rates)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_item_quantity/4 :: (ne_binary(), ne_binary(), wh_json:json_object(), wh_services:services()) -> integer().
get_item_quantity(Category, <<"_all">>, ItemPlan, Services) ->
    Exceptions = wh_json:get_value(<<"exceptions">>, ItemPlan, []),
    case wh_json:is_true(<<"cascade">>, ItemPlan) of
        false -> wh_services:category_quantity(Category, Exceptions, Services);
        true -> 
            lager:debug("collecting '~s' as a cascaded sum", [Category]),
            wh_services:cascade_category_quantity(Category, Exceptions, Services)
    end;    
get_item_quantity(Category, Item, ItemPlan, Services) ->
    case wh_json:is_true(<<"cascade">>, ItemPlan) of
        false -> wh_services:quantity(Category, Item, Services);
        true -> 
            lager:debug("collecting '~s/~s' as a cascaded quantity", [Category, Item]),
            wh_services:cascade_quantity(Category, Item, Services)
    end.
