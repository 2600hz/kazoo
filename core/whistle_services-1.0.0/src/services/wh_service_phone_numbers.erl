%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_phone_numbers).

-export([reconcile/1, reconcile/2]).
-export([feature_activation_charge/2]).
-export([phone_number_activation_charge/2]).

-include("../whistle_services.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec feature_activation_charge(ne_binary(), wh_services:services()) -> integer().
feature_activation_charge(<<"dash_e911">>, Services) ->
    feature_activation_charge(<<"e911">>, Services);
feature_activation_charge(Feature, Services) ->
    Charge = wh_services:activation_charges(<<"number_services">>, Feature, Services),
    wht_util:dollars_to_units(Charge).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec phone_number_activation_charge(ne_binary(), wh_services:services()) -> integer().
phone_number_activation_charge(Number, Services) ->
    case wnm_util:classify_number(Number) of
        'undefined' -> 0;
        Classification ->
            Charge = wh_services:activation_charges(<<"phone_numbers">>, Classification, Services),
            wht_util:dollars_to_units(Charge)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, <<"phone_numbers">>) of
        {'error', _R} ->
            lager:debug("unable to get current phone_numbers in service: ~p", [_R]),
            Services;
        {'ok', JObj} -> reconcile(JObj, Services)
    end.

reconcile(PhoneNumbers, Services) ->
    S1 = wh_services:reset_category(<<"phone_numbers">>, Services),
    S2 = wh_services:reset_category(<<"number_services">>, S1),
    update_numbers(wh_json:get_keys(wh_json:public_fields(PhoneNumbers)), PhoneNumbers, S2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_numbers(ne_binaries(), wh_json:object(), wh_services:services()) -> wh_services:services().
update_numbers([], _, Services) ->
    Services;
update_numbers([Number|Numbers], JObj, Services) ->
    case wnm_util:is_reconcilable(Number) of
        'false' -> Services;
        'true' ->
            Routines = [fun(S) -> update_number_quantities(Number, S, JObj) end
                        ,fun(S) ->
                                 Features = wh_json:get_value([Number, <<"features">>], JObj, []),
                                 update_feature_quantities(Features, S)
                         end
                       ],
            UpdatedServices = lists:foldl(fun(F, S) -> F(S) end, Services, Routines),
            update_numbers(Numbers, JObj, UpdatedServices)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_number_quantities(ne_binary(), wh_services:services(), wh_json:object()) -> wh_services:services().
update_number_quantities(Number, Services, JObj) ->
    ModuleName = wh_json:get_value([Number, <<"module_name">>], JObj, 'undefined'),
    case is_number_billable(Number, ModuleName) andalso wnm_util:classify_number(Number) of
        'false' -> Services;
        'undefined' -> Services;
        Classification ->
            Quantity = wh_services:update_quantity(<<"phone_numbers">>, Classification, Services),
            wh_services:update(<<"phone_numbers">>, Classification, Quantity + 1, Services)
    end.

is_number_billable(DID, 'undefined') ->
    case catch wnm_number:get(DID) of
        #number{module_name=Module}=Number ->
            case catch Module:is_number_billable(Number) of
                'true' ->
                    lager:debug("number ~s is billable: ~s", [DID,Module]),
                    'true';
                'false' ->
                    lager:debug("number ~s is not billable: ~s", [DID,Module]),
                    'false';
                Err ->
                    lager:debug("number ~s is not billable due to provider ~s error: ~p", [DID,Module,Err]),
                    'false'
            end;
        Other ->
            lager:debug("number ~s is not billable due to number error: ~p", [DID,Other]),
            'false'
    end;
is_number_billable(DID, Module) ->
    case catch Module:is_number_billable(DID) of
        'true' ->
            lager:debug("number ~s is billable: ~s", [DID, Module]),
            'true';
        'false' ->
            lager:debug("number ~s is not billable: ~s", [DID, Module]),
            'false';
        Err ->
            lager:debug("number ~s is not billable due to provider ~s error: ~p", [DID, Module, Err]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_feature_quantities(ne_binaries(), wh_services:services()) -> wh_services:services().
update_feature_quantities([], Services) ->
    Services;
update_feature_quantities([<<"dash_e911">>|Features], Services) ->
    update_feature_quantities([<<"e911">>|Features], Services);
update_feature_quantities([Feature|Features], Services) ->
    Quantity = wh_services:update_quantity(<<"number_services">>, Feature, Services),
    UpdatedServices = wh_services:update(<<"number_services">>, Feature, Quantity + 1, Services),
    update_feature_quantities(Features, UpdatedServices).
