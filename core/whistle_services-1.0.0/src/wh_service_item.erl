%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_item).

-export([public_json/1]).
-export([empty/0]).
-export([set_category/2
         ,category/1
        ]).
-export([set_item/2
         ,item/1
        ]).
-export([set_quantity/2
         ,quantity/1
        ]).
-export([set_rate/2
         ,rate/1
        ]).
-export([set_single_discount/2
         ,single_discount/1
        ]).
-export([set_single_discount_rate/2
         ,single_discount_rate/1
        ]).
-export([set_cumulative_discount/2
         ,cumulative_discount/1
        ]).
-export([set_cumulative_discount_rate/2
         ,cumulative_discount_rate/1
        ]).
-export([set_bookkeepers/2]).
-export([bookkeeper/2]).


-include("whistle_services.hrl").

-record(wh_service_item, {category :: api_binary()
                          ,item :: api_binary()
                          ,quantity = 0 :: api_integer()
                          ,rate = 0.0 :: api_float()
                          ,single_discount = 'false' :: boolean()
                          ,single_discount_rate = 0.00 :: api_float()
                          ,cumulative_discount = 0 :: api_integer()
                          ,cumulative_discount_rate = 0.00 :: api_float()
                          ,bookkeepers = wh_json:new() :: wh_json:object()
                         }).

-type item() :: #wh_service_item{}.
-export_type([item/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(item()) -> wh_json:object().
public_json(Item) ->
    Props = [{<<"category">>, Item#wh_service_item.category}
             ,{<<"item">>, Item#wh_service_item.item}
             ,{<<"quantity">>, Item#wh_service_item.quantity}
             ,{<<"rate">>, Item#wh_service_item.rate}
             ,{<<"single_discount">>, Item#wh_service_item.single_discount}
             ,{<<"single_discount_rate">>, Item#wh_service_item.single_discount_rate}
             ,{<<"cumulative_discount">>, Item#wh_service_item.cumulative_discount}
             ,{<<"cumulative_discount_rate">>, Item#wh_service_item.cumulative_discount_rate}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec empty() -> item().
empty() ->
    #wh_service_item{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec category(item()) -> api_binary().
category(#wh_service_item{category=Category}) ->
    Category.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_category(ne_binary(), item()) -> item().
set_category(Category, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{category=Category}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec item(item()) -> api_binary().
item(#wh_service_item{item=Item}) ->
    Item.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_item(ne_binary(), item()) -> item().
set_item(Item, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{item=Item}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity(item()) -> integer().
quantity(#wh_service_item{quantity=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_quantity(api_binary() | integer(), item()) -> item().
set_quantity('undefined', #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{quantity='undefined'};
set_quantity(Q, #wh_service_item{}=ServiceItem) ->
    Quantity = wh_util:to_integer(Q),
    case Quantity > 0 of
        'false' -> ServiceItem#wh_service_item{quantity=Quantity};
        'true'->
            ServiceItem#wh_service_item{single_discount='true'
                                        ,quantity=Quantity
                                       }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec rate(item()) -> api_float().
rate(#wh_service_item{rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_rate(api_binary() | float(), item()) -> item().
set_rate('undefined', #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{rate='undefined'};
set_rate(Rate, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{rate=wh_util:to_float(Rate)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec single_discount(item()) -> boolean().
single_discount(#wh_service_item{single_discount=SingleDiscount}) ->
    SingleDiscount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount(ne_binary() | boolean(), item()) -> item().
set_single_discount(SingleDiscount, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{single_discount=wh_util:is_true(SingleDiscount)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec single_discount_rate(item()) -> api_float().
single_discount_rate(#wh_service_item{single_discount_rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount_rate(api_binary() | float(), item()) -> item().
set_single_discount_rate('undefined', #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{single_discount_rate='undefined'};
set_single_discount_rate(Rate, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{single_discount_rate=wh_util:to_float(Rate)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cumulative_discount(item()) -> api_integer().
cumulative_discount(#wh_service_item{cumulative_discount=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount(api_binary() | integer(), item()) -> item().
set_cumulative_discount('undefined', #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{cumulative_discount='undefined'};
set_cumulative_discount(Quantity, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{cumulative_discount=wh_util:to_integer(Quantity)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cumulative_discount_rate(item()) -> api_float().
cumulative_discount_rate(#wh_service_item{cumulative_discount_rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount_rate(api_binary() | float(), item()) -> item().
set_cumulative_discount_rate('undefined', #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{cumulative_discount_rate='undefined'};
set_cumulative_discount_rate(Rate, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{cumulative_discount_rate=wh_util:to_float(Rate)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bookkeeper(wh_json:key(), item()) -> api_object().
bookkeeper(Bookkeeper, #wh_service_item{bookkeepers=Bookkeepers}) ->
    wh_json:get_ne_value(Bookkeeper, Bookkeepers).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_bookkeepers(wh_json:object(), item()) -> item().
set_bookkeepers(Bookkeepers, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{bookkeepers=Bookkeepers}.
