%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_item).

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

-record(wh_service_item, {category = undefined
                          ,item = undefined
                          ,quantity = 0
                          ,rate = undefined
                          ,single_discount = false
                          ,single_discount_rate = 0.00
                          ,cumulative_discount = 0
                          ,cumulative_discount_rate = 0.00
                          ,bookkeepers = wh_json:new()
                         }).

-type(item() :: #wh_service_item{}).
-export_type([item/0]).

-include_lib("whistle_services/src/whistle_services.hrl").

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
-spec category/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
category(#wh_service_item{category=Category}) ->
    Category.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_category/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_category(Category, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{category=Category}.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec item/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
item(#wh_service_item{item=Item}) ->
    Item.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_item/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_item(Item, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{item=Item}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec quantity/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
quantity(#wh_service_item{quantity=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_quantity/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_quantity(Quantity, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{quantity=Quantity}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec rate/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
rate(#wh_service_item{rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_rate/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_rate(Rate, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{rate=Rate}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec single_discount/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
single_discount(#wh_service_item{single_discount=SingleDiscount}) ->
    SingleDiscount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_single_discount(SingleDiscount, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{single_discount=SingleDiscount}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec single_discount_rate/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
single_discount_rate(#wh_service_item{single_discount_rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount_rate/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_single_discount_rate(Rate, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{single_discount_rate=Rate}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec cumulative_discount/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
cumulative_discount(#wh_service_item{cumulative_discount=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_cumulative_discount(Quantity, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{cumulative_discount=Quantity}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec cumulative_discount_rate/1 :: (#wh_service_item{}) -> 'undefined' | ne_binary().
cumulative_discount_rate(#wh_service_item{cumulative_discount_rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount_rate/2 :: (ne_binary(), #wh_service_item{}) -> #wh_service_item{}.
set_cumulative_discount_rate(Rate, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{cumulative_discount_rate=Rate}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec bookkeeper/2 :: (ne_binary(), #wh_service_item{}) -> 'undefined' | term().
bookkeeper(Bookkeeper, #wh_service_item{bookkeepers=Bookkeepers}) ->
    wh_json:get_ne_value(Bookkeeper, Bookkeepers).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_bookkeepers/2 :: (wh_json:json_object(), #wh_service_item{}) -> #wh_service_item{}.
set_bookkeepers(Bookkeepers, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{bookkeepers=Bookkeepers}.
