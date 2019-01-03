%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_service_phone_numbers).
-behaviour(kz_gen_service).

-export([reconcile/1, reconcile/2]).
-export([feature_activation_charge/2]).
-export([phone_number_activation_charge/2]).

-include("services.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(NUMBER_SERVICES, <<"number_services">>).
-define(PHONE_NUMBERS, <<"phone_numbers">>).
-define(NUMBER_CARRIERS, <<"number_carriers">>).

-define(KEY_VALUE, <<"value">>).
-define(CLASSIFICATIONS, [?KEY_VALUE, <<"classifications">>]).
-define(FEATURES, [?KEY_VALUE, <<"features">>]).

-define(MAP_CATEGORIES, #{?CLASSIFICATIONS => ?PHONE_NUMBERS
                         ,?FEATURES => ?NUMBER_SERVICES
                         }).

%% ?NUMBER_CARRIERS is nested under ?CLASSIFICATIONS, so we add it "manually".
-define(DEFAULT_RESET_CATEGORIES, [?NUMBER_CARRIERS | maps:values(?MAP_CATEGORIES)]).

-type pn() :: knm_phone_number:knm_phone_number().
-type pns() :: [pn()].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec reconcile(kz_services:services()) -> kz_services:services().
reconcile(Services) ->
    AccountDb = kz_util:format_account_db(kz_services:account_id(Services)),
    case kz_datamgr:get_results(AccountDb, <<"numbers/reconcile_services">>) of
        {error, _R} ->
            lager:debug("unable to get reconcile_services for phone numbers: ~p", [_R]),
            Services;
        {ok, []} -> reset(Services);
        {ok, [JObj]} ->
            F = fun (Path, Cat, S) -> update_categories_fold(Path, Cat, S, JObj) end,
            maps:fold(F, reset(Services), ?MAP_CATEGORIES)
    end.

-spec reconcile(kz_services:services(), pns()) -> kz_services:services().
reconcile(Services, PNs) ->
    update_numbers(Services, PNs).

-spec reset(kz_services:services()) -> kz_services:services().
reset(Services) ->
    reset(Services, ?DEFAULT_RESET_CATEGORIES).

-spec reset(kz_services:services(), kz_term:ne_binaries()) -> kz_services:services().
reset(Services, []) -> Services;
reset(Services, [Category | Categories]) ->
    reset(kz_services:reset_category(Category, Services), Categories).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature_activation_charge(kz_term:ne_binary(), kz_services:services()) -> integer().
feature_activation_charge(Feature, Services) ->
    Name = knm_providers:service_name(Feature, kz_services:account_id(Services)),
    Charge = kz_services:activation_charges(?NUMBER_SERVICES, Name, Services),
    wht_util:dollars_to_units(Charge).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec phone_number_activation_charge(kz_services:services(), kz_term:ne_binary()) -> integer().
phone_number_activation_charge(Services, Number) ->
    case knm_converters:classify(Number) of
        'undefined' -> 0;
        Classification ->
            Charge = kz_services:activation_charges(?PHONE_NUMBERS, Classification, Services),
            wht_util:dollars_to_units(Charge)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_categories_fold(kz_term:ne_binary(), kz_term:ne_binary(), kz_services:services(), kz_json:object()) -> kz_json:object().
update_categories_fold(Path, Category, Services, JObj) ->
    kz_json:foldl(fun (SubCat, Count, S) -> update_quantities_fold(SubCat, Count, S, Category) end
                 ,kz_services:reset_category(Category, Services)
                 ,kz_json:get_value(Path, JObj)
                 ).

-spec update_quantities_fold(kz_term:ne_binary(), non_neg_integer(), kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
update_quantities_fold(Feature, Count, Services, ?NUMBER_SERVICES=Category) ->
    Name = knm_providers:service_name(Feature, kz_services:account_id(Services)),
    Quantity = kz_services:updated_quantity(Category, Name, Services),
    kz_services:update(Category, Name, Quantity + Count, Services);

update_quantities_fold(Classification, Carriers, Services0, ?PHONE_NUMBERS=Category) ->
    Services1 = kz_json:foldl(fun manually_update_nested_quantities_fold/3, Services0, Carriers),
    case sum_carriers_per_class(Carriers) of
        0 -> Services1;
        CountPerClass ->
            Quantity = kz_services:updated_quantity(Category, Classification, Services1),
            kz_services:update(Category, Classification, Quantity + CountPerClass, Services1)
    end.

-spec sum_carriers_per_class(kz_json:object()) -> non_neg_integer().
sum_carriers_per_class(Carriers) ->
    kz_json:foldl(fun (<<"knm_local">>, _, Sum) -> Sum;
                      (_Carrier, Count, Sum) ->
                          Count + Sum
                  end, 0, Carriers).

-spec manually_update_nested_quantities_fold(kz_term:ne_binary(), non_neg_integer(), kz_services:services()) -> kz_services:services().
manually_update_nested_quantities_fold(Carrier, Count, S) ->
    Cat = ?NUMBER_CARRIERS,
    Q = kz_services:updated_quantity(Cat, Carrier, S),
    kz_services:update(Cat, Carrier, Q + Count, S).

-spec update_numbers(kz_services:services(), pns()) -> kz_services:services().
update_numbers(Services, []) -> Services;
update_numbers(Services, [PN|PNs]) ->
    case knm_converters:is_reconcilable(knm_phone_number:number(PN)) of
        'false' -> Services;
        'true' ->
            Routines = [fun(S) -> update_number_quantities(S, PN) end
                       ,fun(S) ->
                                Features = knm_phone_number:features_list(PN),
                                update_feature_quantities(Features, S)
                        end
                       ],
            UpdatedServices = lists:foldl(fun(F, S) -> F(S) end, Services, Routines),
            update_numbers(UpdatedServices, PNs)
    end.

-spec update_number_quantities(kz_services:services(), pn()) -> kz_services:services().
update_number_quantities(Services, PN) ->
    DID = knm_phone_number:number(PN),
    case is_number_billable(PN)
        andalso knm_converters:classify(DID)
    of
        'false' -> Services;
        'undefined' -> Services;
        Classification ->
            Quantity = kz_services:quantity(?PHONE_NUMBERS, Classification, Services),
            kz_services:update(?PHONE_NUMBERS, Classification, Quantity + 1, Services)
    end.

-spec is_number_billable(pn()) -> boolean().
is_number_billable(PN) ->
    IsBillable = knm_carriers:is_number_billable(PN),
    lager:debug("is ~s's ~s billable: ~p", [knm_phone_number:module_name(PN)
                                           ,knm_phone_number:number(PN)
                                           ,IsBillable
                                           ]),
    IsBillable.

-spec update_feature_quantities(kz_term:ne_binaries(), kz_services:services()) -> kz_services:services().
update_feature_quantities([], Services) -> Services;
update_feature_quantities([Feature|Features], Services) ->
    Name = knm_providers:service_name(Feature, kz_services:account_id(Services)),
    Quantity = kz_services:quantity(?NUMBER_SERVICES, Name, Services),
    UpdatedServices = kz_services:update(?NUMBER_SERVICES, Name, Quantity + 1, Services),
    update_feature_quantities(Features, UpdatedServices).
