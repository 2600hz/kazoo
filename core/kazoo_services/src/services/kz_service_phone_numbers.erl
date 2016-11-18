%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
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
-define(NUMBER_SERVICES, <<"number_services">>).
-define(LISTING_BY_NUMBER, <<"numbers/list_by_number">>).

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
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, ?LISTING_BY_NUMBER, ['include_docs']) of
        {'error', _R} ->
            lager:debug("unable to get current phone numbers in service: ~p", [_R]),
            Services;
        {'ok', JObjs} ->
            reconcile(Services
                     ,[knm_phone_number:from_json(kz_json:get_value(<<"doc">>, JObj))
                       || JObj <- JObjs
                      ])
    end.

reconcile(Services, PNs) ->
    S1 = kz_services:reset_category(?PHONE_NUMBERS, Services),
    S2 = kz_services:reset_category(?NUMBER_SERVICES, S1),
    update_numbers(S2, PNs).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(pn()) -> boolean().
is_number_billable(PN) ->
    IsBillable = (catch knm_phone_number:is_billable(PN)),
    lager:debug("is ~s's ~s billable: ~p", [knm_phone_number:module_name(PN)
                                           ,knm_phone_number:number(PN)
                                           ,IsBillable
                                           ]),
    IsBillable.

%%--------------------------------------------------------------------
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
