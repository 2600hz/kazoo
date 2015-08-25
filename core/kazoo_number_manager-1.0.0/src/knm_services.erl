%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_services).

-export([activate_feature/2
         ,deactivate_feature/2
         ,deactivate_features/2
         ,maybe_update_services/1
         ,activate_phone_number/1
        ]).

-include("knm.hrl").

-define(KEY_NUMBER_ACTIVATION_CHARGES, <<"number_activation_charges">>).
-define(KEY_TRANSACTIONS, <<"transactions">>).
-define(KEY_ACTIVATION_CHARGES, <<"activation">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_feature(knm_number:knm_number(), ne_binary()) ->
                              knm_number_return().
-spec activate_feature(knm_number:knm_number(), ne_binary(), ne_binary()) ->
                              knm_number_return().
-spec activate_feature(knm_number:knm_number(), ne_binary(), ne_binary(), wh_services:services()) ->
                              knm_number_return().
activate_feature(Number, Feature) ->
    BillingId = fetch_billing_id(Number),
    activate_feature(Number, Feature, BillingId).

activate_feature(Number, Feature, BillingId) ->
    Services = fetch_services(Number),
    activate_feature(Number, Feature, BillingId, Services).

activate_feature(Number, Feature, BillingId, Services) ->
    Units = wh_service_phone_numbers:feature_activation_charge(Feature, Services),
    Charges = knm_number:charges(Number, Feature),
    TotalCharges = Charges + Units,

    case wh_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            lager:error(
              "not enough credit to activate feature '~s' for $~p"
              ,[Feature, wht_util:units_to_dollars(Units)]
             ),
            {'error', 'not_enough_credit'};
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),

            lager:debug("adding feature ~s to ~s", [Feature, knm_phone_number:number(PhoneNumber)]),

            Transaction = create_transaction(Number, Feature, Units),

            {'ok'
             ,knm_number:set_phone_number(
                knm_number:add_transaction(
                  knm_number:set_charges(Number, Feature, TotalCharges)
                  ,Transaction
                 ),
                knm_phone_number:set_feature(PhoneNumber, Feature, wh_json:new())
               )
            }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_feature(knm_number:knm_number(), ne_binary()) ->
                                {'ok', knm_number:knm_number()}.
deactivate_feature(Number, Feature) ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    {'ok'
     ,knm_number:set_phone_number(
        Number
        ,knm_phone_number:set_features(PhoneNumber, wh_json:delete_key(Feature, Features))
       )
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_features(knm_number:knm_number(), ne_binaries()) ->
                                 {'ok', knm_number:knm_number()}.
deactivate_features(Number, Features) ->
    PhoneNumber = knm_number:phone_number(Number),
    ExistingFeatures = knm_phone_number:features(PhoneNumber),
    {'ok'
     ,knm_number:set_phone_number(
        Number
        ,knm_phone_number:set_features(PhoneNumber, wh_json:delete_keys(Features, ExistingFeatures))
       )
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_services(knm_number:knm_number()) ->
                                   knm_number:knm_number().
-spec maybe_update_services(knm_number:knm_number(), boolean()) ->
                                   knm_number:knm_number().
maybe_update_services(Number) ->
    maybe_update_services(
      Number
      ,knm_phone_number:dry_run(knm_number:phone_number(Number))
     ).

maybe_update_services(Number, 'true') ->
    JObj = knm_phone_number:to_json(knm_number:phone_number(Number)),
    Services = wh_service_phone_numbers:reconcile(fetch_services(Number), [JObj]),
    knm_number:set_services(Number, Services);
maybe_update_services(Number, 'false') ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    _ = wh_services:reconcile(AssignedTo, <<"phone_numbers">>),

    PrevAssignedTo = knm_phone_number:prev_assigned_to(PhoneNumber),
    _ = wh_services:reconcile(PrevAssignedTo, <<"phone_numbers">>),

    Services = fetch_services(Number),
    Transactions = knm_number:transactions(Number),

    _ = wh_services:commit_transactions(Services, Transactions),
    Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_phone_number(knm_number:knm_number()) ->
                                   knm_number:knm_number().
-spec activate_phone_number(knm_number:knm_number(), ne_binary()) ->
                                   knm_number:knm_number().
-spec activate_phone_number(knm_number:knm_number(), ne_binary(), integer()) ->
                                   knm_number:knm_number().
activate_phone_number(Number) ->
    BillingId = fetch_billing_id(Number),
    activate_phone_number(Number, BillingId).

activate_phone_number(Number, BillingId) ->
    Services = fetch_services(Number),
    Units = wh_service_phone_numbers:phone_number_activation_charge(
              Services
              ,knm_phone_number:number(knm_number:phone_number(Number))
             ),
    activate_phone_number(Number, BillingId, Units).

activate_phone_number(Number, _BillingId, 0) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    lager:debug("no activation charge for ~s", [Num]),
    Number;
activate_phone_number(Number, BillingId, Units) ->
    Charges = knm_number:charges(Number, ?KEY_ACTIVATION_CHARGES),
    TotalCharges = Charges + Units,

    case wh_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            Message = io_lib:format("not enough credit to activate number for $~p"
                                    ,[wht_util:units_to_dollars(Units)]
                                   ),
            lager:error(Message),
            knm_errors:service_restriction(Message);
        'true' ->
            Transaction = create_transaction(Number, Units),

            knm_number:set_charges(
              knm_number:add_transaction(Number, Transaction)
              ,?KEY_ACTIVATION_CHARGES
              ,TotalCharges
             )
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_services(knm_number:knm_number()) ->
                            wh_services:services().
fetch_services(Number) ->
    case knm_number:services(Number) of
        'undefined' ->
            AssignedTo = knm_phone_number:assigned_to(knm_number:phone_number(Number)),
            wh_services:fetch(AssignedTo);
        Services ->
            Services
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_billing_id(knm_number:knm_number()) ->
                              ne_binary().
fetch_billing_id(Number) ->
    wh_services:get_billing_id(fetch_services(Number)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_transaction(knm_number:knm_number(), integer()) ->
                                wh_transaction:transaction().
create_transaction(Number, Units) ->
    BillingId = fetch_billing_id(Number),
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    LedgerId = wh_util:format_account_id(BillingId, 'raw'),
    Num = knm_phone_number:number(PhoneNumber),

    Routines = [fun(T) -> set_activation_reason(T, LedgerId, AccountId, <<"number">>) end
                ,fun(T) -> wh_transaction:set_number(Num, T) end
                ,fun(T) -> set_feature_description(T, wh_util:to_binary(Num)) end
               ],
    lager:debug("staging number activation charge $~p for ~s via billing account ~s"
                ,[wht_util:units_to_dollars(Units), AccountId, LedgerId]
               ),

    lists:foldl(
      fun(F, T) -> F(T) end
      ,wh_transaction:debit(LedgerId, Units)
      ,Routines
     ).

-spec create_transaction(knm_number:knm_number(), ne_binary(), integer()) ->
                                wh_transaction:transaction().
create_transaction(Number, Feature, Units) ->
    BillingId = fetch_billing_id(Number),
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    LedgerId = wh_util:format_account_id(BillingId, 'raw'),
    Num = knm_phone_number:number(PhoneNumber),

    Routines = [fun(T) -> set_activation_reason(T, LedgerId, AccountId, <<"feature">>) end
                ,fun(T) -> wh_transaction:set_feature(Feature, T) end
                ,fun(T) -> wh_transaction:set_number(Num, T) end
                ,fun(T) -> set_feature_description(T, wh_util:to_binary(Feature)) end
               ],
    lager:debug("staging feature '~s' activation charge $~p for ~s via billing account ~s"
                ,[Feature, wht_util:units_to_dollars(Units), AccountId, LedgerId]
               ),

    lists:foldl(
      fun(F, T) -> F(T) end
      ,wh_transaction:debit(LedgerId, Units)
      ,Routines
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_feature_description(wh_transaction:transaction(), ne_binary()) ->
                                     wh_transaction:transaction().
set_feature_description(T, Feature) ->
    Description = <<"number feature activation for ", Feature/binary>>,
    wh_transaction:set_description(Description, T).

-spec set_activation_reason(wh_transaction:transaction(), ne_binary(), ne_binary(), ne_binary()) ->
                                    wh_transaction:transaction().
set_activation_reason(Transaction, LedgerId, LedgerId, Key) ->
    wh_transaction:set_reason(<<Key/binary, "_activation">>, Transaction);
set_activation_reason(Transaction, _LedgetId, AccountId, Key) ->
    wh_transaction:set_reason(<<"sub_account_", Key/binary, "_activation">>
                              ,wh_transaction:set_sub_account_info(AccountId, Transaction)
                             ).
