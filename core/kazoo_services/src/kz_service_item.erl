%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_item).

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


-include("services.hrl").

-record(kz_service_item, {category :: kz_term:api_binary()
                         ,item :: kz_term:api_binary()
                         ,name :: kz_term:api_binary()
                         ,quantity = 0 :: kz_term:api_integer()
                         ,rate = 0.0 :: kz_term:api_float()
                         ,single_discount = 'false' :: boolean()
                         ,single_discount_rate = 0.00 :: kz_term:api_float()
                         ,cumulative_discount = 0 :: kz_term:api_integer()
                         ,cumulative_discount_rate = 0.00 :: kz_term:api_float()
                         ,bookkeepers = kz_json:new() :: kz_json:object()
                         ,activation_charge = 0.00 :: kz_term:api_float()
                         ,minimum = 0 :: kz_term:api_integer()
                         ,exceptions = [] :: kz_term:ne_binaries()
                         }).

-type item() :: #kz_service_item{}.
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
-spec public_json(item()) -> kz_json:object().
public_json(Item) ->
    kz_json:from_list(
      [{<<"category">>, Item#kz_service_item.category}
      ,{<<"item">>, Item#kz_service_item.item}
      ,{<<"name">>, Item#kz_service_item.name}
      ,{<<"quantity">>, Item#kz_service_item.quantity}
      ,{<<"rate">>, Item#kz_service_item.rate}
      ,{<<"single_discount">>, Item#kz_service_item.single_discount}
      ,{<<"single_discount_rate">>, Item#kz_service_item.single_discount_rate}
      ,{<<"cumulative_discount">>, Item#kz_service_item.cumulative_discount}
      ,{<<"cumulative_discount_rate">>, Item#kz_service_item.cumulative_discount_rate}
      ,{<<"activation_charge">>, Item#kz_service_item.activation_charge}
      ,{<<"minimum">>, Item#kz_service_item.minimum}
      ,{<<"exceptions">>, Item#kz_service_item.exceptions}
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec empty() -> item().
empty() ->
    #kz_service_item{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec category(item()) -> kz_term:api_binary().
category(#kz_service_item{category=Category}) ->
    Category.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_category(kz_term:ne_binary(), item()) -> item().
set_category(Category, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{category=Category}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec item(item()) -> kz_term:api_binary().
item(#kz_service_item{item=Item}) ->
    Item.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_item(kz_term:ne_binary(), item()) -> item().
set_item(Item, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{item=Item}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec name(item()) -> kz_term:api_binary().
name(#kz_service_item{name=Name}) ->
    Name.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_name(kz_term:ne_binary(), item()) -> item().
set_name(Name, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{name=Name}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity(item()) -> kz_term:api_integer().
quantity(#kz_service_item{quantity=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_quantity(kz_term:api_binary() | integer(), item()) -> item().
set_quantity('undefined', #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{quantity='undefined'};
set_quantity(Q, #kz_service_item{}=ServiceItem) ->
    Quantity = kz_term:to_integer(Q),
    case Quantity > 0 of
        'false' -> ServiceItem#kz_service_item{quantity=Quantity};
        'true'->
            ServiceItem#kz_service_item{single_discount='true'
                                       ,quantity=Quantity
                                       }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec rate(item()) -> kz_term:api_float().
rate(#kz_service_item{rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_rate(kz_term:api_binary() | float(), item()) -> item().
set_rate('undefined', #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{rate='undefined'};
set_rate(Rate, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{rate=kz_term:to_float(Rate)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec single_discount(item()) -> boolean().
single_discount(#kz_service_item{single_discount=SingleDiscount}) ->
    SingleDiscount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount(kz_term:ne_binary() | boolean(), item()) -> item().
set_single_discount(SingleDiscount, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{single_discount=kz_term:is_true(SingleDiscount)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec single_discount_rate(item()) -> kz_term:api_float().
single_discount_rate(#kz_service_item{single_discount_rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_single_discount_rate(kz_term:api_binary() | float(), item()) -> item().
set_single_discount_rate('undefined', #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{single_discount_rate='undefined'};
set_single_discount_rate(Rate, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{single_discount_rate=kz_term:to_float(Rate)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cumulative_discount(item()) -> kz_term:api_integer().
cumulative_discount(#kz_service_item{cumulative_discount=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount(kz_term:api_binary() | integer(), item()) -> item().
set_cumulative_discount('undefined', #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{cumulative_discount='undefined'};
set_cumulative_discount(Quantity, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{cumulative_discount=kz_term:to_integer(Quantity)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cumulative_discount_rate(item()) -> kz_term:api_float().
cumulative_discount_rate(#kz_service_item{cumulative_discount_rate=Rate}) ->
    Rate.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_cumulative_discount_rate(kz_term:api_binary() | float(), item()) -> item().
set_cumulative_discount_rate('undefined', #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{cumulative_discount_rate='undefined'};
set_cumulative_discount_rate(Rate, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{cumulative_discount_rate=kz_term:to_float(Rate)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bookkeeper(kz_json:path(), item()) -> kz_term:api_object() | kz_term:ne_binary().
bookkeeper(Bookkeeper, #kz_service_item{bookkeepers=Bookkeepers}) ->
    kz_json:get_ne_value(Bookkeeper, Bookkeepers).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_bookkeepers(kz_json:object(), item()) -> item().
set_bookkeepers(Bookkeepers, #kz_service_item{}=ServiceItem) ->
    ServiceItem#kz_service_item{bookkeepers=Bookkeepers}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charge(item()) -> float().
activation_charge(#kz_service_item{activation_charge=Charge}) -> Charge.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_activation_charge(float(), item()) -> item().
set_activation_charge(Charge, ServiceItem) ->
    ServiceItem#kz_service_item{activation_charge=Charge}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec minimum(item()) -> integer().
minimum(#kz_service_item{minimum=Min}) -> Min.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_minimum(integer(), item()) -> item().
set_minimum(Min, ServiceItem) ->
    ServiceItem#kz_service_item{minimum=Min}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec exceptions(item()) -> kz_term:ne_binaries().
exceptions(#kz_service_item{exceptions=Exc}) -> Exc.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_exceptions(kz_term:ne_binaries(), item()) -> item().
set_exceptions(Exc, ServiceItem) ->
    ServiceItem#kz_service_item{exceptions=Exc}.
