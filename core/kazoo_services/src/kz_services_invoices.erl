%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_invoices).

-export([empty/0]).
-export([create/1]).
-export([foldl/3]).

-export([public_json/1]).
-export([has_changes/1]).
-export([changed/1]).
-export([has_additions/1]).
-export([additions/1]).
-export([has_billable_additions/1]).
-export([billable_additions/1]).

-include("services.hrl").

-type invoices() :: [kz_services_invoice:invoice()].
-type fold_fun() :: fun((kz_services_invoice:invoice(), Acc) -> Acc).
-export_type([invoices/0
             ,fold_fun/0
             ]
            ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> invoices().
empty() -> [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services:services()) -> invoices().
create(Services) ->
    HydratedServices = kz_services:hydrate_plans(Services),
    CurrentInvoices = create_current_invoices(HydratedServices),
    ProposedInvoices = create_proposed_invoices(HydratedServices),
    annotate(Services, CurrentInvoices, ProposedInvoices).

-spec annotate(kz_services:services(), invoices(), invoices()) -> invoices().
annotate(Services, CurrentInvoices, ProposedInvoices) ->
    Dict =
        foldl(fun(Invoice, D) ->
                      Key = kz_services_invoice:bookkeeper_hash(Invoice),
                      dict:store(Key, {Invoice, 'undefined'}, D)
              end
             ,dict:new()
             ,CurrentInvoices
             ),
    TentativeInvoices =
        dict:to_list(
          foldl(fun(Invoice, D) ->
                        Key = kz_services_invoice:bookkeeper_hash(Invoice),
                        dict:update(Key
                                   ,fun({Current, _}) ->
                                            {Current, Invoice}
                                    end
                                   ,{'undefined', Invoice}
                                   ,D
                                   )
                end
               ,Dict
               ,ProposedInvoices
               )
         ),
    do_annotate(Services, [Invoices || {_BookkeeperHash, Invoices} <- TentativeInvoices], []).

-type tentative_invoices() :: [{kz_services_invoice:invoice() | 'undefined', kz_services_invoice:invoice() | 'undefined'}].
-spec do_annotate(kz_services:services(), tentative_invoices(), invoices()) -> invoices().
do_annotate(_Services, [], Invoices) -> Invoices;
do_annotate(Services, [{CurrentInvoice, 'undefined'}|TentativeInvoices], Invoices) ->
    %% All plans associated with a bookkeeper was removed
    CurrentItems = kz_services_invoice:items(CurrentInvoice),
    ProposedItems = kz_services_items:reset(CurrentItems),
    Items = kz_services_items:annotate(CurrentItems, ProposedItems),
    Setters = [{fun kz_services_invoice:set_items/2, Items}
              ,{fun kz_services_invoice:set_activation_charges/2
               ,kz_services_activation_items:empty()
               }
              ],
    do_annotate(Services
               ,TentativeInvoices
               ,[kz_services_invoice:setters(CurrentInvoice, Setters)|Invoices]
               );
do_annotate(Services, [{'undefined', ProposedInvoice}|TentativeInvoices], Invoices) ->
    %% A plan(s) associated with a new bookkeeper was added
    ProposedItems = kz_services_invoice:items(ProposedInvoice),
    CurrentItems = kz_services_items:reset(ProposedItems),
    Items = kz_services_items:annotate(CurrentItems, ProposedItems),
    ActivationItems = kz_services_activation_items:create(Items),
    Setters = [{fun kz_services_invoice:set_items/2, Items}
              ,{fun kz_services_invoice:set_activation_charges/2
               ,kz_services_activation_items:create(ActivationItems)
               }
              ],
    do_annotate(Services
               ,TentativeInvoices
               ,[kz_services_invoice:setters(ProposedInvoice, Setters)|Invoices]
               );
do_annotate(Services, [{CurrentInvoice, ProposedInvoice}|TentativeInvoices], Invoices) ->
    CurrentItems = kz_services_invoice:items(CurrentInvoice),
    ProposedItems = kz_services_invoice:items(ProposedInvoice),
    Items = kz_services_items:annotate(CurrentItems, ProposedItems),
    Setters = [{fun kz_services_invoice:set_items/2, Items}
              ,{fun kz_services_invoice:set_activation_charges/2
               ,kz_services_activation_items:create(Items)
               }
              ],
    do_annotate(Services
               ,TentativeInvoices
               ,[kz_services_invoice:setters(ProposedInvoice, Setters)|Invoices]
               ).

-spec create_current_invoices(kz_services:services()) -> invoices().
create_current_invoices(Services) ->
    lager:debug("creating current invoices", []),
    create_invoices(reset(Services), kz_services:current_plans(Services)).

-spec create_proposed_invoices(kz_services:services()) -> invoices().
create_proposed_invoices(Services) ->
    lager:debug("creating proposed invoices", []),
    create_invoices(Services, kz_services:proposed_plans(Services)).

-spec create_invoices(kz_services:services(), kz_services_plans:plans()) -> invoices().
create_invoices(Services, Plans) ->
    kz_services_plans:foldl(create_invoices_foldl(Services), empty(), Plans).

-spec create_invoices_foldl(kz_services:services()) -> kz_services_plans:foldl_fun().
create_invoices_foldl(Services) ->
    fun(BookkeeperHash, PlansList, Invoices) ->
            Plan = kz_services_plans:merge(PlansList),
            [kz_services_invoice:create(Services, BookkeeperHash, Plan)
             |Invoices
            ]
    end.

-spec reset(kz_services:services()) -> kz_services:services().
reset(Services) ->
    Routines = [fun kz_services:reset_invoices/1
               ,fun kz_services:reset_updates/1
               ,fun kz_services:reset_quantities/1
               ],
    lists:foldl(fun(F, S) -> F(S) end, Services, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec foldl(fold_fun(), Acc, invoices()) -> Acc.
foldl(FoldFun, Acc, Invoices) ->
    lists:foldl(fun(Invoice, A) -> FoldFun(Invoice, A) end
               ,Acc
               ,Invoices
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(kz_services:services() | invoices()) -> kz_json:objects().
public_json(Thing) ->
    [kz_services_invoice:public_json(Invoice)
     || Invoice <- maybe_services(Thing)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_changes(kz_services:services() | invoices()) -> boolean().
has_changes(Thing) ->
    changed(Thing) =/= [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec changed(kz_services:services() | invoices()) -> invoices().
changed(Thing) ->
    Invoices = maybe_services(Thing),
    [Invoice
     || Invoice <- Invoices
            ,kz_services_invoice:has_changes(Invoice)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_additions(kz_services:services() | invoices()) -> boolean().
has_additions(Thing) ->
    additions(Thing) =/= [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec additions(kz_services:services() | invoices()) -> invoices().
additions(Thing) ->
    Invoices = maybe_services(Thing),
    [Invoice
     || Invoice <- Invoices
            ,kz_services_invoice:has_additions(Invoice)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_billable_additions(kz_services:services() | invoices()) -> boolean().
has_billable_additions(Thing) ->
    billable_additions(Thing) =/= [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billable_additions(kz_services:services() | invoices()) -> invoices().
billable_additions(Thing) ->
    Invoices = maybe_services(Thing),
    [Invoice
     || Invoice <- Invoices,
        kz_services_invoice:has_billable_additions(Invoice)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_services(kz_services:services() | invoices()) -> invoices().
maybe_services(Thing) ->
    case kz_services:is_services(Thing) of
        'true' -> kz_services:invoices(Thing);
        'false' -> Thing
    end.
