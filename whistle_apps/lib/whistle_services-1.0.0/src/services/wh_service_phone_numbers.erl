%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_phone_numbers).

-export([reconcile/1]).
-export([feature_activation_charge/2]).
-export([phone_number_activation_charge/2]).

-include_lib("whistle_services/src/whistle_services.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec feature_activation_charge/2 :: (ne_binary(), wh_services:services()) -> integer().
feature_activation_charge(<<"dash_e911">>, Services) ->
    feature_activation_charge(<<"e911">>, Services);
feature_activation_charge(Feature, Services) ->
    Charge = wh_services:activation_charges(<<"number_services">>, Feature, Services),
    wapi_money:dollars_to_units(Charge).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec phone_number_activation_charge/2 :: (ne_binary(), wh_services:services()) -> integer().
phone_number_activation_charge(Number, Services) ->
    case wnm_util:classify_number(Number) of
        undefined -> 0;
        Classification ->
            Charge = wh_services:activation_charges(<<"phone_numbers">>, Classification, Services),
            wapi_money:dollars_to_units(Charge)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile/1 :: (wh_services:services()) -> wh_services:services().
reconcile(Services) ->
    AccountId = wh_services:account_id(Services),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(AccountDb, <<"phone_numbers">>) of
        {error, _R} ->
            lager:debug("unable to get current phone_numbers in service: ~p", [_R]),
            Services;
        {ok, JObj} ->
            S1 = wh_services:reset_category(<<"phone_numbers">>, Services),
            S2 = wh_services:reset_category(<<"number_services">>, S1),
            update_numbers(wh_json:get_keys(wh_json:public_fields(JObj)), JObj, S2)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_numbers/3 :: ([ne_binary(),...] | [], wh_json:json_object(), wh_services:services()) -> wh_services:services().
update_numbers([], _, Services) ->
    Services;
update_numbers([Number|Numbers], JObj, Services) ->
    case wnm_util:is_reconcilable(Number) of
        false -> Services;
        true ->
            Routines = [fun(S) -> update_number_quantities(Number, S) end
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
-spec update_number_quantities/2 :: (ne_binary(), wh_services:services()) -> wh_services:services().
update_number_quantities(Number, Services) ->
    case wnm_util:classify_number(Number) of
        undefined -> Services;
        Classification ->
            Quantity = wh_services:update_quantity(<<"phone_numbers">>, Classification, Services),
            wh_services:update(<<"phone_numbers">>, Classification, Quantity + 1, Services)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_feature_quantities/2 :: ([ne_binary(),...] | [], wh_services:services()) -> wh_services:services().
update_feature_quantities([], Services) ->
    Services;
update_feature_quantities([<<"dash_e911">>|Features], Services) ->
    update_feature_quantities([<<"e911">>|Features], Services);
update_feature_quantities([Feature|Features], Services) ->
    Quantity = wh_services:update_quantity(<<"number_services">>, Feature, Services),
    UpdatedServices = wh_services:update(<<"number_services">>, Feature, Quantity + 1, Services),
    update_feature_quantities(Features, UpdatedServices).
