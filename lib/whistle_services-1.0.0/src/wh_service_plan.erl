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
%% Given a vendor database and service plan id, fetch the document.
%% Merge any plan overrides into the plan property.
%% @end
%%--------------------------------------------------------------------
-spec fetch/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> 'undefined' | wh_json:json_object().
fetch(PlanId, VendorDb, Overrides) ->
    case couch_mgr:open_doc(VendorDb, PlanId) of
        {ok, JObj} -> 
            lager:debug("using service plan ~s in vendor db ~s", [PlanId, VendorDb]),
            wh_json:merge_recursive(JObj, wh_json:from_list([{<<"plan">>, Overrides}]));
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

create_items(ServicePlan, ServiceItems, Services) ->
    Plan = wh_json:get_value(<<"plan">>, ServicePlan, wh_json:new()),
    Plans = [{Category, Item}
             || Category <- wh_json:get_keys(Plan)
                    ,Item <- wh_json:get_keys(Category, Plan)
            ],
    lists:foldl(fun({Category, Item}, I) ->
                        create_items(Category, Item, I, ServicePlan, Services)
                end, ServiceItems, Plans).

create_items(Category, Item, ServiceItems, ServicePlan, Services) ->
    ItemPlan = wh_json:get_value([<<"plan">>, Category, Item], ServicePlan),
    ItemQuantity = get_item_quantity(Category, Item, ItemPlan, Services),
    Quantity = case wh_json:get_integer_value(<<"minimum">>, ItemPlan, 0) of
                   Min when Min > ItemQuantity -> 
                       lager:debug("minimum '~s/~s' not met with ~p, enforcing quantity ~p", [Category, Item, ItemQuantity, Min]),
                       Min;
                   _ -> ItemQuantity
               end,
    case Quantity > 0 of
        false -> ServiceItems;
        true ->
            Rate = get_rate(Quantity, ItemPlan),
            %% allow service plans to re-map item names (IE: softphone items "as" sip_device)
            As = wh_json:get_ne_value(<<"as">>, ItemPlan, Item),
            Routines = [fun(I) -> wh_service_item:set_category(Category, I) end
                        ,fun(I) -> wh_service_item:set_item(As, I) end
                        ,fun(I) -> wh_service_item:set_quantity(Quantity, I) end
                        ,fun(I) -> wh_service_item:set_rate(Rate, I) end
                        ,fun(I) ->
                                 case wh_json:get_value([<<"discounts">>, <<"single">>], ItemPlan) of
                                     undefined -> I;
                                     SingleDiscount ->
                                         SingleRate = wh_json:get_float_value(<<"rate">>, SingleDiscount, Rate),
                                         wh_service_item:set_single_discount_rate(SingleRate, I)
                                 end
                         end
                        ,fun(I) ->
                                 case wh_json:get_value([<<"discounts">>, <<"cumulative">>], ItemPlan) of
                                     undefined -> I;
                                     CumulativeDiscount ->
                                         CumulativeQuantity = case wh_json:get_integer_value(<<"maximum">>, CumulativeDiscount, 0) of
                                                                  Max when Max < Quantity -> 
                                                                      lager:debug("item '~s/~s' quantity ~p exceeds cumulative discount max, using ~p"
                                                                                  ,[Category, Item, Quantity, Max]),
                                                                      Max;
                                                                  _ -> Quantity
                                                              end,
                                         CumulativeRate = case get_rate(Quantity, CumulativeDiscount) of
                                                              undefined -> Rate;
                                                              Else -> Else
                                                          end,
                                         I2 = wh_service_item:set_cumulative_discount(CumulativeQuantity, I),
                                         wh_service_item:set_cumulative_discount_rate(CumulativeRate, I2)
                                 end
                         end
                        ,fun(I) -> wh_service_item:set_bookkeepers(bookkeeper_jobj(Category, Item, ServicePlan), I) end
                       ],
            ServiceItem = lists:foldl(fun(F, I) -> F(I) end, wh_service_items:find(Category, Item, ServiceItems), Routines),
            wh_service_items:update(ServiceItem, ServiceItems)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec bookkeeper_jobj/3 :: (ne_binary(), ne_binary(), wh_json:json_object()) -> wh_json:json_object().
bookkeeper_jobj(Category, Item, ServicePlan) ->
    lists:foldl(fun(Bookkeeper, J) ->
                        Mapping = wh_json:get_value([<<"bookkeepers">>, Bookkeeper, Category, Item]
                                                    ,ServicePlan, wh_json:new()),
                        wh_json:set_value(Bookkeeper, Mapping, J)
                end, wh_json:new(), wh_json:get_keys(<<"bookkeepers">>, ServicePlan)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If tiered rates are provided, find the value to use given the current
%% quantity.  If no rates are viable attempt to use the "rate" property.
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
%% Get the item quantity, drawing solely from the provided account or
%% (when the service plan dictates) the summed (cascaded) decendants.
%% Also handle the special case were we should sum all items in a 
%% given category, except a list of item names to ignore during
%% summation.
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
