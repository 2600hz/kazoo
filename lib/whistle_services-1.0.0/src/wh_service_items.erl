%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_items).

-export([empty/0]).
-export([to_list/1]).
-export([find/3]).
-export([update/2]).

-type(items() :: dict:new()).
-export_type([items/0]).

-include_lib("whistle_services/src/whistle_services.hrl").

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
-spec to_list/1 :: (items()) -> [wh_service_item:item(),...] | [].
to_list(ServiceItems) ->
    [ServiceItem || {_, ServiceItem} <- dict:to_list(ServiceItems)].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find/3 :: (ne_binary(), ne_binary(), items()) -> wh_service_item:item().
find(Category, Item, ServiceItems) ->
    Key = {Category, Item},
    case dict:find(Key, ServiceItems) of
        {ok, I} -> I;
        error -> wh_service_item:empty()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (wh_service_item:item(), items()) -> wh_service_item:items().
update(ServiceItem, ServiceItems) ->
    Category = wh_service_item:category(ServiceItem),
    Item = wh_service_item:item(ServiceItem),
    _ = case wh_service_item:rate(ServiceItem) of
            undefined ->
                lager:debug("set '~s/~s' with quantity ~p @ default rate"
                            ,[Category, Item, wh_service_item:quantity(ServiceItem)]);
            Rate ->
                lager:debug("set '~s/~s' with quantity ~p @ $~p"
                            ,[Category, Item, wh_service_item:quantity(ServiceItem), Rate])
        end,
    CumulativeDiscount = wh_service_item:cumulative_discount(ServiceItem),
    _ = case wh_util:is_empty(CumulativeDiscount) 
            orelse wh_service_item:cumulative_discount_rate(ServiceItem) 
        of
            true -> ok;
            undefined ->
                lager:debug("set '~s/~s' cumulative discount with quantity ~p @ default rate"
                            ,[Category, Item, CumulativeDiscount]);
            CumulativeRate ->
                lager:debug("set '~s/~s' cumulative discount with quantity ~p @ $~p"
                            ,[Category, Item, CumulativeDiscount, CumulativeRate])

        end,
    _ = case wh_service_item:single_discount(ServiceItem) 
            andalso wh_service_item:single_discount_rate(ServiceItem)
        of
            false -> ok;
            undefined -> 
                lager:debug("set '~s/~s' single discount at default rate"
                            ,[Category, Item]);
            SingleRate ->
                lager:debug("set '~s/~s' single discount for $~p"
                            ,[Category, Item, SingleRate])
        end,
    Key = {wh_service_item:category(ServiceItem), wh_service_item:item(ServiceItem)},
    dict:store(Key, ServiceItem, ServiceItems).
