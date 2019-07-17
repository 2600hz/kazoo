%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_invoice).

-export([bookkeeper_hash/1
        ,set_bookkeeper_hash/2
        ]).
-export([bookkeeper_id/1]).
-export([bookkeeper_type/1]).
-export([bookkeeper_vendor_id/1]).
-export([items/1
        ,set_items/2
        ]).
-export([activation_charges/1
        ,set_activation_charges/2
        ]).
-export([plan/1]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).

-export([create/3]).
-export([has_changes/1]).
-export([has_additions/1]).
-export([has_billable_additions/1]).

-include("services.hrl").

-record(invoice, {bookkeeper_hash :: kz_term:api_binary()
                 ,items = kz_services_items:empty() :: kz_services_items:items()
                 ,activation_charges = kz_services_activation_items:empty() :: kz_services_activation_items:items()
                 ,plan = kz_services_plan:empty() :: kz_services_plan:plan()
                 }).

-opaque invoice() :: #invoice{}.
-type setter_fun() :: {fun((invoice(), Value) -> invoice()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([invoice/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_hash(invoice()) -> kz_term:api_binary().
bookkeeper_hash(#invoice{bookkeeper_hash=BookkeeperHash}) ->
    BookkeeperHash.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_hash(invoice(), kz_term:api_binary()) -> invoice().
set_bookkeeper_hash(Invoice, BookkeeperHash) ->
    Invoice#invoice{bookkeeper_hash=BookkeeperHash}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_id(invoice()) -> kz_term:ne_binary().
bookkeeper_id(Invoice) ->
    kz_services_plan:bookkeeper_id(plan(Invoice)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(invoice()) -> kz_term:ne_binary().
bookkeeper_type(Invoice) ->
    kz_services_plan:bookkeeper_type(plan(Invoice)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(invoice()) -> kz_term:ne_binary().
bookkeeper_vendor_id(Invoice) ->
    kz_services_plan:bookkeeper_vendor_id(plan(Invoice)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec items(invoice()) -> kz_services_items:items().
items(#invoice{items=Items}) ->
    Items.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_items(invoice(), kz_services_items:items()) -> invoice().
set_items(Invoice, Items) ->
    Invoice#invoice{items=Items}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec activation_charges(invoice()) -> kz_services_activation_items:items().
activation_charges(#invoice{activation_charges=ActivationCharges}) ->
    ActivationCharges.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_activation_charges(invoice(), kz_services_activation_items:items()) -> invoice().
set_activation_charges(Invoice, ActivationCharges) ->
    Invoice#invoice{activation_charges=ActivationCharges}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan(invoice()) -> kz_services_plan:plan().
plan(#invoice{plan=Plan}) ->
    Plan.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan(invoice(), kz_services_plan:plan()) -> invoice().
set_plan(Invoice, Plan) ->
    Invoice#invoice{plan=Plan}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> invoice().
empty() ->
    #invoice{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> invoice().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(invoice(), setter_funs()) -> invoice().
setters(Invoice, Routines) ->
    lists:foldl(fun({Setter, Value}, I) ->
                        Setter(I, Value)
                end
               ,Invoice
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(invoice()) -> kz_json:object().
public_json(Invoice) ->
    %% TODO: calculate totals (taxes)...
    ItemsJObjs = kz_services_items:public_json(items(Invoice)),
    ActivationCharges = kz_services_activation_items:public_json(
                          activation_charges(Invoice)
                         ),
    PlanJObj = kz_services_plan:jobj(plan(Invoice)),
    Props = [{<<"activation_charges">>, ActivationCharges}
            ,{<<"items">>, ItemsJObjs}
            ,{<<"taxes">>, []}
            ,{<<"summary">>, summary(Invoice, ItemsJObjs, ActivationCharges)}
            ,{<<"plan">>, kzd_service_plan:plan(PlanJObj)}
            ,{<<"bookkeeper">>, kzd_service_plan:bookkeeper(PlanJObj)}
            ],
    kz_json:from_list(Props).

-spec summary(invoice(), kz_json:objects(), kz_json:objects()) -> kz_json:object().
summary(_Invoice, ItemsJObjs, ActivationItems) ->
    kz_json:from_list(
      [{<<"today">>, sum_item_totals(ActivationItems)}
      ,{<<"recurring">>, sum_item_totals(ItemsJObjs)}
      ]
     ).

-spec sum_item_totals(kz_json:objects()) -> float().
sum_item_totals(ItemsJObjs) ->
    sum_item_totals(ItemsJObjs, 0.0).

-spec sum_item_totals(kz_json:objects(), float()) -> float().
sum_item_totals([], Total) -> Total;
sum_item_totals([ItemJObj|ItemsJObjs], Total) ->
    ItemTotal = kz_json:get_float_value(<<"total">>, ItemJObj, 0.0),
    sum_item_totals(ItemsJObjs, ItemTotal + Total).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services:services(), kz_term:ne_binary(), kz_services_plan:plan()) ->
                    invoice().
create(Services, BookkeeperHash, Plan) ->
    lager:debug("generating invoice for bookkeeper ~s", [BookkeeperHash]),
    Items = kz_services_items:create(Services, Plan),
    Setters = [{fun set_bookkeeper_hash/2, BookkeeperHash}
              ,{fun set_items/2, Items}
              ,{fun set_plan/2, Plan}
              ],
    setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_changes(invoice()) -> boolean().
has_changes(#invoice{items=Items}) ->
    kz_services_items:has_changes(Items).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_additions(invoice()) -> boolean().
has_additions(#invoice{items=Items}) ->
    kz_services_items:has_additions(Items).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_billable_additions(invoice()) -> boolean().
has_billable_additions(#invoice{items=Items}) ->
    kz_services_items:has_billable_additions(Items).
