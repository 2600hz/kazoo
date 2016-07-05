%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(kz_services:services()) -> kz_services:services().
-spec reconcile(kz_services:services(), kz_json:objects()) -> kz_services:services().

reconcile(Services) ->
    AccountId = kz_services:account_id(Services),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, ?LISTING_BY_NUMBER, ['include_docs']) of
        {'error', _R} ->
            lager:debug("unable to get current phone numbers in service: ~p", [_R]),
            Services;
        {'ok', JObjs} ->
            Docs = [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            reconcile(Services, Docs)
    end.

reconcile(Services, JObjs) ->
    S1 = kz_services:reset_category(?PHONE_NUMBERS, Services),
    S2 = kz_services:reset_category(?NUMBER_SERVICES, S1),
    update_numbers(S2, JObjs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec feature_activation_charge(ne_binary(), kz_services:services()) -> integer().
feature_activation_charge(?DASH_KEY, Services) ->
    feature_activation_charge(?EMERGENCY_SERVICES_KEY, Services);
feature_activation_charge(?VITELITY_KEY, Services) ->
    feature_activation_charge(?EMERGENCY_SERVICES_KEY, Services);
feature_activation_charge(Feature, Services) ->
    Charge = kz_services:activation_charges(?NUMBER_SERVICES, Feature, Services),
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
-spec update_numbers(kz_services:services(), kz_json:objects()) -> kz_services:services().
update_numbers(Services, []) ->
    Services;
update_numbers(Services, [JObj|JObjs]) ->
    Number = kz_doc:id(JObj),
    case knm_converters:is_reconcilable(Number) of
        'false' -> Services;
        'true' ->
            Routines = [fun(S) -> update_number_quantities(S, JObj) end
		       ,fun(S) ->
				Features = kz_json:get_value(?PVT_FEATURES, JObj, kz_json:new()),
				update_feature_quantities(Features, S)
			end
                       ],
            UpdatedServices = lists:foldl(fun(F, S) -> F(S) end, Services, Routines),
            update_numbers(UpdatedServices, JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_number_quantities(kz_services:services(), kz_json:object()) ->
                                      kz_services:services().
update_number_quantities(Services, JObj) ->
    Number = kz_doc:id(JObj),
    ModuleName = kz_json:get_atom_value(?PVT_MODULE_NAME, JObj),
    case is_number_billable(Number, ModuleName)
        andalso knm_converters:classify(Number)
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
-spec is_number_billable(ne_binary(), api_binary() | atom()) -> boolean().
is_number_billable(DID, 'undefined') ->
    case knm_number:get(DID) of
        {'error', _R} ->
            lager:debug("failed to get ~s: ~p", [DID, _R]),
            'false';
        {'ok', Number} ->
            case knm_phone_number:module_name(knm_number:phone_number(Number)) of
                'undefined' ->
                    lager:debug("number ~s had no number manager module, not billable", [DID]),
                    'false';
                Module ->
                    is_number_billable(DID, Module)
            end
    end;
is_number_billable(DID, M) ->
    Module = kz_util:to_atom(M, 'true'),
    case catch Module:is_number_billable(DID) of
        'true' ->
            lager:debug("number ~s is billable: ~s", [DID, Module]),
            'true';
        'false' ->
            lager:debug("number ~s is not billable: ~s", [DID, Module]),
            'false';
        _Else ->
            lager:debug("number ~s is not billable due to provider ~s error: ~p", [DID, Module, _Else]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_feature_quantities(ne_binaries() | kz_json:object(), kz_services:services()) -> kz_services:services().
update_feature_quantities([], Services) ->
    Services;
update_feature_quantities([?DASH_KEY|Features], Services) ->
    update_feature_quantities([?EMERGENCY_SERVICES_KEY|Features], Services);
update_feature_quantities([?VITELITY_KEY|Features], Services) ->
    update_feature_quantities([?EMERGENCY_SERVICES_KEY|Features], Services);
update_feature_quantities([Feature|Features], Services) ->
    Quantity = kz_services:updated_quantity(?NUMBER_SERVICES, Feature, Services),
    UpdatedServices = kz_services:update(?NUMBER_SERVICES, Feature, Quantity + 1, Services),
    update_feature_quantities(Features, UpdatedServices);
update_feature_quantities(JObj, Services) ->
    update_feature_quantities(kz_json:get_keys(JObj), Services).
