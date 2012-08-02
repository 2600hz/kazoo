%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_items).

-export([empty/0]).
-export([update/5]).
-export([set_single_discount/4]).
-export([set_cumulative_discount/5]).

-include_lib("whistle_services/src/whistle_services.hrl").

-record(wh_service_item, {category = undefined
                          ,item = undefined
                          ,quantity = 0
                          ,rate = undefined
                          ,single_discount = false
                          ,single_discount_rate = 0.00
                          ,cumulative_discount = 0
                          ,cumulative_discount_rate = 0.00
                         }).
-type(item() :: #wh_service_item{}).
-type(items() :: [item(),...] | []).

-export_type([item/0]).
-export_type([items/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec empty/0 :: () -> items().
empty() -> 
    dict:new().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update/5 :: (ne_binary(), ne_binary(), integer(), 'undefined' | float(), items()) -> items().
update(Category, Item, Quantity, Rate, Items) ->
    case wh_util:is_empty(Rate) of
        true -> lager:debug("set item '~s/~s' quantity ~p @ default rate", [Category, Item, Quantity]);
        false -> lager:debug("set item '~s/~s' quantity ~p @ $~p", [Category, Item, Quantity, Rate])
    end,
    Key = {Category, Item},
    dict:update(Key, fun(#wh_service_item{}=ExistingItem) ->
                             ExistingItem#wh_service_item{quantity=Quantity, rate=Rate}
                     end, #wh_service_item{quantity=Quantity, rate=Rate, category=Category, item=Item}, Items).    

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount/4 :: (ne_binary(), ne_binary(), 'undefined' | float(), items()) -> items().
set_single_discount(Category, Item, Rate, Items) ->
    case wh_util:is_empty(Rate) of
        true -> lager:debug("set item '~s/~s' single discount @ default rate", [Category, Item]);
        false -> lager:debug("set item '~s/~s' single discount @ $~p", [Category, Item, Rate])
    end,
    Key = {Category, Item},
    dict:update(Key, fun(#wh_service_item{}=ExistingItem) ->
                             ExistingItem#wh_service_item{single_discount=true, single_discount_rate=Rate}
                     end, #wh_service_item{single_discount=true, single_discount_rate=Rate
                                           ,category=Category, item=Item}, Items).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount/5 :: (ne_binary(), ne_binary(), integer(), 'undefined' | float(), items()) -> items().
set_cumulative_discount(Category, Item, Quantity, Rate, Items) ->
    case wh_util:is_empty(Rate) of
        true -> lager:debug("set item '~s/~s' cumulative discount quantity ~p @ default rate", [Category, Item, Quantity]);
        false -> lager:debug("set item '~s/~s' cumulative discount quantity ~p @ $~p", [Category, Item, Quantity, Rate])
    end,
    Key = {Category, Item},
    dict:update(Key, fun(#wh_service_item{}=ExistingItem) ->
                             ExistingItem#wh_service_item{cumulative_discount=Quantity, cumulative_discount_rate=Rate}
                     end, #wh_service_item{cumulative_discount=Quantity, cumulative_discount_rate=Rate
                                           ,category=Category, item=Item}, Items).
