%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_items).

-export([empty/0]).
-export([get_udapted_items/2]).
-export([to_list/1]).
-export([public_json/1]).
-export([find/3]).
-export([update/2]).

-type items() :: dict().
-export_type([items/0]).

-include("whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec empty() -> items().
empty() ->
    dict:new().

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_udapted_items(items(), items()) -> items().
-spec get_udapted_items(any(), wh_service_item:item(), items(), items()) -> items().
get_udapted_items(Items1, Items2) ->
    dict:fold(
        fun(Key, Item1, Acc) ->
            get_udapted_items(Key, Item1, Items2, Acc)
        end
        ,dict:new()
        ,Items1
    ).

get_udapted_items(Key, Item1, Items2, NewItem) ->
    case get_item(Key, Items2) of
        'error' -> NewItem;
        Item2 ->
            case compare_quantity(Item1, Item2) of
                'true' -> NewItem;
                'false' -> dict:store(Key, Item1, NewItem)
            end
    end.

-spec get_item(any(), items()) -> wh_service_item:item().
get_item(Key, Items) ->
    case dict:find(Key, Items) of
        'error' -> 'error';
        {'ok', Item} -> Item
    end.

-spec compare_quantity(wh_service_item:item(), wh_service_item:item()) -> boolean().
compare_quantity(Item1, Item2) ->
    wh_service_item:quantity(Item1) =:= wh_service_item:quantity(Item2).


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_list(items()) -> [wh_service_item:item(),...] | [].
to_list(ServiceItems) ->
    [ServiceItem || {_, ServiceItem} <- dict:to_list(ServiceItems)].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(items()) -> wh_json:objects().
public_json(ServiceItems) ->
    lists:foldl(fun({_, ServiceItem}, JObj) ->
                        ItemJObj = wh_service_item:public_json(ServiceItem),
                        Category = wh_service_item:category(ServiceItem),
                        Item = wh_service_item:item(ServiceItem),
                        case wh_json:get_value(Category, JObj) of
                            'undefined' ->
                                TJObj = wh_json:set_value(Item, ItemJObj, wh_json:new()),
                                wh_json:set_value(Category, TJObj, JObj);
                            VJObj ->
                                TJObj = wh_json:set_value(Item, ItemJObj, VJObj),
                                wh_json:set_value(Category, TJObj, JObj)
                        end
                end, wh_json:new(), dict:to_list(ServiceItems)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find(ne_binary(), ne_binary(), items()) -> wh_service_item:item().
find(Category, Item, ServiceItems) ->
    Key = {Category, Item},
    case dict:find(Key, ServiceItems) of
        {'ok', I} -> I;
        'error' -> wh_service_item:empty()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update(wh_service_item:item(), items()) -> wh_service_item:items().
update(ServiceItem, ServiceItems) ->
    _ = log_update(ServiceItem),
    Key = {wh_service_item:category(ServiceItem), wh_service_item:item(ServiceItem)},
    dict:store(Key, ServiceItem, ServiceItems).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Nasty conditional logging (but functional).... bleh...
%% @end
%%--------------------------------------------------------------------
-spec log_update(wh_service_item:item()) -> 'ok'.
log_update(ServiceItem) ->
    Category = wh_service_item:category(ServiceItem),
    Item = wh_service_item:item(ServiceItem),
    _ = log_update_rate(Category, Item, ServiceItem),
    _ = log_update_cumulative_discount(Category, Item, ServiceItem),
    log_update_single_discount(Category, Item, ServiceItem).

-spec log_update_rate(ne_binary(), ne_binary(), wh_service_item:item()) -> 'ok'.
log_update_rate(Category, Item, ServiceItem) ->
    Rate =  wh_service_item:rate(ServiceItem),
    lager:debug("set '~s/~s' with quantity ~p @ $~p"
                ,[Category, Item, wh_service_item:quantity(ServiceItem), Rate]
               ).

-spec log_update_cumulative_discount(ne_binary(), ne_binary(), wh_service_item:item()) -> 'ok'.
log_update_cumulative_discount(Category, Item, ServiceItem) ->
    CumulativeDiscount = wh_service_item:cumulative_discount(ServiceItem),
    case wh_util:is_empty(CumulativeDiscount)
        orelse wh_service_item:cumulative_discount_rate(ServiceItem)
    of
        'true' -> 'ok';
        CumulativeRate ->
            lager:debug("set '~s/~s' cumulative discount with quantity ~p @ $~p"
                        ,[Category, Item, CumulativeDiscount, CumulativeRate])
    end.

-spec log_update_single_discount(ne_binary(), ne_binary(), wh_service_item:item()) -> 'ok'.
log_update_single_discount(Category, Item, ServiceItem) ->
    case wh_service_item:single_discount(ServiceItem)
        andalso wh_service_item:single_discount_rate(ServiceItem)
    of
        'false' -> 'ok';
        SingleRate ->
            lager:debug("set '~s/~s' single discount for $~p"
                        ,[Category, Item, SingleRate])
    end.
