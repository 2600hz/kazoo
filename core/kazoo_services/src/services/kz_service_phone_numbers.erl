%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_service_phone_numbers).

-export([reconcile/1, reconcile/2]).
-export([feature_activation_charge/2]).
-export([phone_number_activation_charge/2]).

-include("kazoo_services.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(PHONE_NUMBERS, <<"phone_numbers">>).
-define(PHONE_NUMBERS_NON_BILLABLE, <<"phone_numbers_non_billable">>).
-define(NUMBER_SERVICES, <<"number_services">>).
-define(NUMBER_CARRIERS, <<"number_carriers">>).

-define(KEY_VALUE, <<"value">>).
-define(BILLABLE, [?KEY_VALUE, <<"classifications">>, <<"billable">>]).
-define(NON_BILLABLE, [?KEY_VALUE, <<"classifications">>, <<"non_billable">>]).
-define(FEATURES, [?KEY_VALUE, <<"features">>]).
-define(MODULES, [?KEY_VALUE, <<"modules">>]).

-define(DEFAULT_RESET_CATEGORIES, [?PHONE_NUMBERS
                                  ,?NUMBER_SERVICES
                                  ,?PHONE_NUMBERS_NON_BILLABLE
                                  ,?NUMBER_CARRIERS
                                  ]).

-type pn() :: knm_phone_number:knm_phone_number().
-type pns() :: [pn()].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), pns()) -> kz_services:services().
reconcile(Services) ->
    AccountDb = kz_util:format_account_db(kz_services:account_id(Services)),
    case kz_datamgr:get_results(AccountDb, <<"numbers/reconcile_services">>) of
        {error, _R} ->
            lager:debug("unable to get reconcile_services for phone numbers: ~p", [_R]),
            Services;
        {ok, []} -> reset(Services);
        {ok, [JObj]} ->
            Categories = #{?BILLABLE => ?PHONE_NUMBERS
                          ,?NON_BILLABLE => ?PHONE_NUMBERS_NON_BILLABLE
                          ,?FEATURES => ?NUMBER_SERVICES
                          ,?MODULES => ?NUMBER_CARRIERS
                          },
            F = fun (P, C, S) -> update_categories_fold(P, C, S, JObj) end,
            maps:fold(F, Services, Categories)
    end.

reconcile(Services, PNs) ->
    update_numbers(reset(Services), PNs).

-spec reset(kz_services:services()) -> kz_services:services().
reset(Services) ->
    reset(Services, ?DEFAULT_RESET_CATEGORIES).

-spec reset(kz_services:services(), ne_binaries()) -> kz_services:services().
reset(Services, []) -> Services;
reset(Services, [Category | Categories]) ->
    reset(kz_services:reset_category(Category, Services), Categories).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec feature_activation_charge(ne_binary(), kz_services:services()) -> integer().
feature_activation_charge(Feature, Services) ->
    Name = knm_providers:service_name(Feature, kz_services:account_id(Services)),
    Charge = kz_services:activation_charges(?NUMBER_SERVICES, Name, Services),
    wht_util:dollars_to_units(Charge).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec phone_number_activation_charge(kz_services:services(), ne_binary()) -> integer().
phone_number_activation_charge(Services, Number) ->
    case knm_converters:classify(Number) of
        'undefined' -> 0;
        Classification ->
            Charge = kz_services:activation_charges(?PHONE_NUMBERS, Classification, Services),
            wht_util:dollars_to_units(Charge)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_categories_fold(Path, Category, Services, JObj) ->
    kz_json:foldl(fun (SubCat, Count, S) -> update_quantities_fold(SubCat, Count, S, Category) end
                 ,kz_services:reset_category(Category, Services)
                 ,kz_json:get_value(Path, JObj)
                 ).

update_quantities_fold(Feature, Count, Services, ?NUMBER_SERVICES=Category) ->
    Name = knm_providers:service_name(Feature, kz_services:account_id(Services)),
    Quantity = kz_services:updated_quantity(Category, Name, Services),
    kz_services:update(Category, Name, Quantity + Count, Services);
update_quantities_fold(SubCategory, Count, Services, Category) ->
    Quantity = kz_services:updated_quantity(Category, SubCategory, Services),
    kz_services:update(Category, SubCategory, Quantity + Count, Services).

%% @private
-spec update_numbers(kz_services:services(), pns()) -> kz_services:services().
update_numbers(Services, []) ->
    Services;
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

%% @private
-spec update_number_quantities(kz_services:services(), pn()) -> kz_services:services().
update_number_quantities(Services, PN) ->
    DID = knm_phone_number:number(PN),
    case is_number_billable(PN)
        andalso knm_converters:classify(DID)
    of
        'false' -> Services;
        'undefined' -> Services;
        Classification ->
            Quantity = kz_services:updated_quantity(?PHONE_NUMBERS, Classification, Services),
            kz_services:update(?PHONE_NUMBERS, Classification, Quantity + 1, Services)
    end.

%% @private
-spec is_number_billable(pn()) -> boolean().
is_number_billable(PN) ->
    IsBillable = knm_carriers:is_number_billable(PN),
    lager:debug("is ~s's ~s billable: ~p", [knm_phone_number:module_name(PN)
                                           ,knm_phone_number:number(PN)
                                           ,IsBillable
                                           ]),
    IsBillable.

%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_feature_quantities(ne_binaries(), kz_services:services()) -> kz_services:services().
update_feature_quantities([], Services) -> Services;
update_feature_quantities([Feature|Features], Services) ->
    Name = knm_providers:service_name(Feature, kz_services:account_id(Services)),
    Quantity = kz_services:updated_quantity(?NUMBER_SERVICES, Name, Services),
    UpdatedServices = kz_services:update(?NUMBER_SERVICES, Name, Quantity + 1, Services),
    update_feature_quantities(Features, UpdatedServices).
