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
-export([set_name/2
         ,name/1
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
-export([set_bookkeepers/2
         ,bookkeeper/2
        ]).
-export([set_activation_charge/2
         ,activation_charge/1
        ]).
-export([set_minimum/2
         ,minimum/1
        ]).
-export([set_exceptions/2
         ,exceptions/1
        ]).


-include("whistle_services.hrl").

-record(wh_service_item, {category :: api_binary()
                          ,item :: api_binary()
                          ,name :: api_binary()
                          ,quantity = 0 :: api_integer()
                          ,rate = 0.0 :: api_float()
                          ,single_discount = 'false' :: boolean()
                          ,single_discount_rate = 0.00 :: api_float()
                          ,cumulative_discount = 0 :: api_integer()
                          ,cumulative_discount_rate = 0.00 :: api_float()
                          ,bookkeepers = wh_json:new() :: wh_json:object()
                          ,activation_charge = 0.00 :: api_float()
                          ,minimum = 0 :: api_integer()
                          ,exceptions = [] :: ne_binaries()
                         }).

-type item() :: #wh_service_item{}.
-type items() :: [item()].
-export_type([item/0
              ,items/0
             ]).

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
             ,{<<"name">>, Item#wh_service_item.name}
             ,{<<"quantity">>, Item#wh_service_item.quantity}
             ,{<<"rate">>, Item#wh_service_item.rate}
             ,{<<"single_discount">>, Item#wh_service_item.single_discount}
             ,{<<"single_discount_rate">>, Item#wh_service_item.single_discount_rate}
             ,{<<"cumulative_discount">>, Item#wh_service_item.cumulative_discount}
             ,{<<"cumulative_discount_rate">>, Item#wh_service_item.cumulative_discount_rate}
             ,{<<"activation_charge">>, Item#wh_service_item.activation_charge}
             ,{<<"minimum">>, Item#wh_service_item.minimum}
             ,{<<"exceptions">>, Item#wh_service_item.exceptions}
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
-spec name(item()) -> api_binary().
name(#wh_service_item{name=Name}) ->
    Name.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_name(ne_binary(), item()) -> item().
set_name(Name, #wh_service_item{}=ServiceItem) ->
    ServiceItem#wh_service_item{name=Name}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity(item()) -> api_integer().
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
-spec bookkeeper(wh_json:key(), item()) -> api_object() | ne_binary().
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charge(item()) -> float().
activation_charge(#wh_service_item{activation_charge=Charge}) -> Charge.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_activation_charge(float(), item()) -> item().
set_activation_charge(Charge, ServiceItem) ->
    ServiceItem#wh_service_item{activation_charge=Charge}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec minimum(item()) -> integer().
minimum(#wh_service_item{minimum=Min}) -> Min.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_minimum(integer(), item()) -> item().
set_minimum(Min, ServiceItem) ->
    ServiceItem#wh_service_item{minimum=Min}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec exceptions(item()) -> ne_binaries().
exceptions(#wh_service_item{exceptions=Exc}) -> Exc.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_exceptions(ne_binaries(), item()) -> item().
set_exceptions(Exc, ServiceItem) ->
    ServiceItem#wh_service_item{exceptions=Exc}.
