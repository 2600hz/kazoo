%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_items).

-export([empty/0]).
-export([update_quantity/4]).
-export([update_rate/4]).

-include_lib("whistle_services/src/whistle_services.hrl").

-record(wh_service_item, {category = undefined
                          ,item = undefined
                          ,quantity = 0
                          ,rate = undefined
                          ,single_discount = false
                          ,single_discount_price = 0.00
                          ,cumulative_discount = false
                          ,cumulative_discount_price = 0.00
                          ,minimum = 0
                         }).
-type(item() :: #wh_service_item{}).
-type(items() :: [item(),...] | []).

-export_type([item/0]).
-export_type([items/0]).

empty() -> 
    dict:new().

update_quantity(Category, Item, Quantity, Items) ->
    Key = {Category, Item},
    dict:update(Key, fun(#wh_service_item{quantity=ExistingQuantity}=ExistingItem) -> 
                             ExistingItem#wh_service_item{quantity=ExistingQuantity + Quantity}
                     end, #wh_service_item{quantity=Quantity, category=Category, item=Item}, Items).

update_rate(Category, Item, Rate, Items) ->
    Key = {Category, Item},
    dict:update(Key, fun(ExistingItem) -> 
                             ExistingItem#wh_service_item{rate=Rate}
                     end, #wh_service_item{rate=Rate, category=Category, item=Item}, Items).
