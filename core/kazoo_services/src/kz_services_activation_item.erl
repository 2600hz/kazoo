%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_activation_item).

-export([create/1]).
-export([public_json/1]).

-export([category_name/1
        ,set_category_name/2
        ]).
-export([display_name/1
        ,set_display_name/2
        ]).
-export([billable_quantity/1
        ,set_billable_quantity/2
        ]).
-export([billing_rate/1
        ,set_billing_rate/2
        ]).
-export([item_name/1
        ,set_item_name/2
        ]).
-export([total/1
        ,set_total/2
        ]).

-export([empty/0
        ,setters/1
        ,setters/2
        ]).

-include("services.hrl").

-record(activation_item, {category_name :: kz_term:ne_binary()
                         ,display_name :: kz_term:ne_binary()
                         ,billable_quantity = 0 :: non_neg_integer()
                         ,billing_rate = 0.0 :: float()
                         ,item_name :: kz_term:ne_binary()
                         ,total = 0.0 :: float()
                         }).

-opaque item() :: #activation_item{}.
-type setter_fun() :: {fun((item(), Value) -> item()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([item/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> item().
empty() ->
    #activation_item{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category_name(item()) -> kz_term:ne_binary().
category_name(#activation_item{category_name=CategoryName}) ->
    CategoryName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_category_name(item(), kz_term:ne_binary()) -> item().
set_category_name(ActivationItem, CategoryName) ->
    ActivationItem#activation_item{category_name = CategoryName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec display_name(item()) -> kz_term:ne_binary().
display_name(#activation_item{display_name=DisplayName}) ->
    DisplayName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_display_name(item(), kz_term:ne_binary()) -> item().
set_display_name(ActivationItem, DisplayName) ->
    ActivationItem#activation_item{display_name = DisplayName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billable_quantity(item()) -> non_neg_integer().
billable_quantity(#activation_item{billable_quantity=Quantity}) ->
    Quantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_billable_quantity(item(), non_neg_integer()) -> item().
set_billable_quantity(ActivationItem, Quantity) ->
    ActivationItem#activation_item{billable_quantity = Quantity}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billing_rate(item()) -> float().
billing_rate(#activation_item{billing_rate=Rate}) ->
    Rate.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_billing_rate(item(), float()) -> item().
set_billing_rate(ActivationItem, Rate) ->
    ActivationItem#activation_item{billing_rate = Rate}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec item_name(item()) -> kz_term:ne_binary().
item_name(#activation_item{item_name=ItemName}) ->
    ItemName.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_item_name(item(), kz_term:ne_binary()) -> item().
set_item_name(ActivationItem, ItemName) ->
    ActivationItem#activation_item{item_name = ItemName}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec total(item()) -> float().
total(#activation_item{total=Total}) ->
    Total.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_total(item(), float()) -> item().
set_total(ActivationItem, Total) ->
    ActivationItem#activation_item{total = Total}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> item().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(item(), setter_funs()) -> item().
setters(ActivationItem, Routines) ->
    lists:foldl(fun({Setter, Value}, I) ->
                        Setter(I, Value)
                end
               ,ActivationItem
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services_items:item()) -> item() | 'undefined'.
create(Item) ->
    Plan = kz_services_item:item_plan(Item),
    Key = [<<"difference">>, <<"billable">>],
    Changes = kz_json:get_integer_value(Key, kz_services_item:changes(Item, kz_json:new()), 0),
    ActivationCharge = kzd_item_plan:activation_charge(Plan, 'undefined'),
    create(Item, Changes, ActivationCharge).

-spec create(kz_services_items:item(), integer(), kz_term:api_float()) -> item() | 'undefined'.
create(_, Changes, _A)
  when Changes =< 0 ->
    'undefined';
create(_, _, 'undefined') ->
    'undefined';
create(_, _, ActivationCharge)
  when ActivationCharge =< 0.0 ->
    'undefined';
create(Item, Changes, ActivationCharge) ->
    Setters = [{fun set_category_name/2, kz_services_item:category_name(Item)}
              ,{fun set_display_name/2, kz_services_item:display_name(Item)}
              ,{fun set_billable_quantity/2, Changes}
              ,{fun set_billing_rate/2, ActivationCharge}
              ,{fun set_item_name/2, kz_services_item:item_name(Item)}
              ,{fun set_total/2, Changes * ActivationCharge}
              ],
    setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(item()) -> kz_json:object().
public_json(ActivationItem) ->
    Setters =
        [{fun kzd_activation_item:set_billable/2, billable_quantity(ActivationItem)}
        ,{fun kzd_activation_item:set_category/2, category_name(ActivationItem)}
        ,{fun kzd_activation_item:set_item/2, item_name(ActivationItem)}
        ,{fun kzd_activation_item:set_name/2, display_name(ActivationItem)}
        ,{fun kzd_activation_item:set_rate/2, billing_rate(ActivationItem)}
        ,{fun kzd_activation_item:set_total/2, total(ActivationItem)}
        ],
    kz_doc:setters(kzd_activation_item:new(), Setters).
