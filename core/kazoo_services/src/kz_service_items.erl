%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_items).

-export([empty/0]).
-export([get_updated_items/2]).
-export([to_list/1]).
-export([public_json/1]).
-export([find/3]).
-export([update/2]).

-opaque items() :: dict:dict().
-export_type([items/0]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> items().
empty() ->
    dict:new().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec get_updated_items(items(), items()) -> items().
get_updated_items(UpdatedItems, ExistingItems) ->
    dict:fold(fun(Key, UpdatedItem, DifferingItems) ->
                      get_updated_items(Key, UpdatedItem, ExistingItems, DifferingItems)
              end
             ,dict:new()
             ,UpdatedItems
             ).

-spec get_updated_items(any(), kz_service_item:item(), items(), items()) -> items().
get_updated_items(Key, UpdatedItem, ExistingItems, DifferingItems) ->
    case get_item(Key, ExistingItems) of
        'error' -> DifferingItems;
        ExistingItem ->
            case compare(UpdatedItem, ExistingItem) of
                'true' -> DifferingItems;
                'false' -> dict:store(Key, UpdatedItem, DifferingItems)
            end
    end.

-spec get_item(any(), items()) -> kz_service_item:item() | 'error'.
get_item(Key, Items) ->
    case dict:find(Key, Items) of
        'error' -> 'error';
        {'ok', Item} -> Item
    end.

-spec compare(kz_service_item:item(), kz_service_item:item()) -> boolean().
compare(Item1, Item2) ->
    compare_quantity(Item1, Item2)
        orelse compare_rate(Item1, Item2).

-spec compare_quantity(kz_service_item:item(), kz_service_item:item()) -> boolean().
compare_quantity(Item1, Item2) ->
    kz_service_item:quantity(Item1) =:= 0
        orelse kz_service_item:quantity(Item1) =:= kz_service_item:quantity(Item2).

-spec compare_rate(kz_service_item:item(), kz_service_item:item()) -> boolean().
compare_rate(Item1, _) ->
    case kz_service_item:rate(Item1) of
        'undefined' -> 'true';
        Rate -> not(Rate > 0)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_list(items()) -> kz_service_item:items().
to_list(ServiceItems) ->
    [ServiceItem || {_, ServiceItem} <- dict:to_list(ServiceItems)].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(items()) -> kz_json:object().
public_json(ServiceItems) ->
    lists:foldl(fun public_json_fold/2, kz_json:new(), dict:to_list(ServiceItems)).

-spec public_json_fold({_, kz_service_item:item()}, kz_json:object()) -> kz_json:object().
public_json_fold({_, ServiceItem}, JObj) ->
    ItemJObj = kz_service_item:public_json(ServiceItem),
    Category = kz_service_item:category(ServiceItem),
    Item = kz_service_item:item(ServiceItem),
    case kz_json:get_value(Category, JObj) of
        'undefined' ->
            TJObj = kz_json:set_value(Item, ItemJObj, kz_json:new()),
            kz_json:set_value(Category, TJObj, JObj);
        VJObj ->
            TJObj = kz_json:set_value(Item, ItemJObj, VJObj),
            kz_json:set_value(Category, TJObj, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find(kz_term:ne_binary(), kz_term:ne_binary(), items()) -> kz_service_item:item().
find(Category, Item, ServiceItems) ->
    Key = {Category, Item},
    case dict:find(Key, ServiceItems) of
        {'ok', I} -> I;
        'error' -> kz_service_item:empty()
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_service_item:item(), items()) -> items().
update(ServiceItem, ServiceItems) ->
    _ = log_update(ServiceItem),
    Key = {kz_service_item:category(ServiceItem), kz_service_item:item(ServiceItem)},
    dict:store(Key, ServiceItem, ServiceItems).

%%------------------------------------------------------------------------------
%% @doc Nasty conditional logging (but functional).... bleh...
%% @end
%%------------------------------------------------------------------------------
-spec log_update(kz_service_item:item()) -> 'ok'.
log_update(ServiceItem) ->
    Category = kz_service_item:category(ServiceItem),
    Item = kz_service_item:item(ServiceItem),
    _ = log_update_rate(Category, Item, ServiceItem),
    _ = log_update_cumulative_discount(Category, Item, ServiceItem),
    log_update_single_discount(Category, Item, ServiceItem).

-spec log_update_rate(kz_term:ne_binary(), kz_term:ne_binary(), kz_service_item:item()) -> 'ok'.
log_update_rate(Category, Item, ServiceItem) ->
    case kz_service_item:rate(ServiceItem) of
        'undefined' -> 'ok';
        Rate ->
            lager:debug("set '~s/~s' with quantity ~p @ $~p"
                       ,[Category, Item, kz_service_item:quantity(ServiceItem), Rate]
                       )
    end.

-spec log_update_cumulative_discount(kz_term:ne_binary(), kz_term:ne_binary(), kz_service_item:item()) -> 'ok'.
log_update_cumulative_discount(Category, Item, ServiceItem) ->
    CumulativeDiscount = kz_service_item:cumulative_discount(ServiceItem),
    case kz_term:is_empty(CumulativeDiscount)
        orelse kz_service_item:cumulative_discount_rate(ServiceItem)
    of
        'true' -> 'ok';
        CumulativeRate ->
            lager:debug("set '~s/~s' cumulative discount with quantity ~p @ $~p"
                       ,[Category, Item, CumulativeDiscount, CumulativeRate]
                       )
    end.

-spec log_update_single_discount(kz_term:ne_binary(), kz_term:ne_binary(), kz_service_item:item()) -> 'ok'.
log_update_single_discount(Category, Item, ServiceItem) ->
    case kz_service_item:single_discount(ServiceItem)
        andalso kz_service_item:single_discount_rate(ServiceItem)
    of
        'false' -> 'ok';
        SingleRate ->
            lager:debug("set '~s/~s' single discount for $~p"
                       ,[Category, Item, SingleRate]
                       )
    end.
