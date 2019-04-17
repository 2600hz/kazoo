%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
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

-opaque invoices() :: [kz_services_invoice:invoice()].
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
    lager:debug("creating current invoices", []),
    CurrentInvoices = create_current_invoices(Services),
    lager:debug("creating proposed invoices", []),
    ProposedInvoices = create_proposed_invoices(Services),
    annotate(Services, CurrentInvoices, ProposedInvoices).

-spec annotate(kz_services:services(), invoices(), invoices()) -> invoices().
annotate(Services, CurrentInvoices, ProposedInvoices) ->
    annotate(Services, CurrentInvoices, ProposedInvoices, []).

-spec annotate(kz_services:services(), invoices(), invoices(), invoices()) -> invoices().
annotate(_Services, [], [], Invoices) -> Invoices;
annotate(Services, [CurrentInvoice|CurrentInvoices], [], Invoices) ->
    %% A bookkeeper was removed from the plan?
    CurrentItems = kz_services_items:reset(
                     kz_services_invoice:items(CurrentInvoice)
                    ),
    ProposedItems = kz_services_items:empty(),
    Items = kz_services_items:annotate(CurrentItems, ProposedItems),
    Setters = [{fun kz_services_invoice:set_items/2, Items}
              ,{fun kz_services_invoice:set_activation_charges/2, kz_services_activation_items:empty()}
              ],
    annotate(Services
            ,CurrentInvoices
            ,[]
            ,[kz_services_invoice:setters(CurrentInvoice, Setters)|Invoices]
            );
annotate(Services, CurrentInvoices, [ProposedInvoice|ProposedInvoices], Invoices) ->
    BookkeeperHash = kz_services_invoice:bookkeeper_hash(ProposedInvoice),
    ProposedItems = kz_services_invoice:items(ProposedInvoice),
    case split_invoices(CurrentInvoices, BookkeeperHash) of
        {'undefined', RemainingCurrentInvoices} ->
            %% this is the first time this bookkeeper is appeared in the plan
            CurrentItems = kz_services_items:empty(),
            Items = kz_services_items:annotate(CurrentItems, ProposedItems),
            ActivationItems = kz_services_activation_items:create(Items),
            Setters = [{fun kz_services_invoice:set_items/2, Items}
                      ,{fun kz_services_invoice:set_activation_charges/2, ActivationItems}
                      ],
            annotate(Services
                    ,RemainingCurrentInvoices
                    ,ProposedInvoices
                    ,[kz_services_invoice:setters(ProposedInvoice, Setters)|Invoices]
                    );
        {CurrentInvoice, RemainingCurrentInvoices} ->
            CurrentItems = kz_services_invoice:items(CurrentInvoice),
            Items = kz_services_items:annotate(CurrentItems, ProposedItems),
            ActivationItems = kz_services_activation_items:create(Items),
            Setters = [{fun kz_services_invoice:set_items/2, Items}
                      ,{fun kz_services_invoice:set_activation_charges/2, ActivationItems}
                      ],
            annotate(Services
                    ,RemainingCurrentInvoices
                    ,ProposedInvoices
                    ,[kz_services_invoice:setters(ProposedInvoice, Setters)|Invoices]
                    )
    end.

-spec create_current_invoices(kz_services:services()) -> invoices().
create_current_invoices(Services) ->
    ServicesJObj = kz_services:current_services_jobj(Services),
    Plans = kz_services_plans:fetch(
              kz_services:set_services_jobj(Services, ServicesJObj)
             ),
    create_invoices(reset(Services), Plans).

-spec create_proposed_invoices(kz_services:services()) -> invoices().
create_proposed_invoices(Services) ->
    create_invoices(Services, kz_services:plans(Services)).

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

-type api_invoice() :: kz_serivces_invoice:invoice() | 'undefined'.
-spec split_invoices(invoices(), kz_term:ne_binary()) -> {api_invoice(), invoices()}.
split_invoices(Invoices, BookkeeperHash) ->
    case lists:splitwith(fun(Invoice) ->
                                 kz_services_invoice:bookkeeper_hash(Invoice) =:= BookkeeperHash
                         end, Invoices)
    of
        {[], RemainingInvoices} ->
            {'undefined', RemainingInvoices};
        {[Invoice], RemainingInvoices} ->
            {Invoice, RemainingInvoices}
    end.

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
