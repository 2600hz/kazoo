%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
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
        ,update_services/1
        ,activate_phone_number/1
        ,activation_charges/1
        ,phone_number_activation_charges/1
        ]).

-include("knm.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(KEY_NUMBER_ACTIVATION_CHARGES, <<"number_activation_charges">>).
-define(KEY_TRANSACTIONS, <<"transactions">>).
-define(KEY_ACTIVATION_CHARGES, <<"activation">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_feature(knm_number:knm_number(), ne_binary()) ->
                              knm_number:knm_number().
-spec activate_feature(knm_number:knm_number(), ne_binary(), ne_binary()) ->
                              knm_number:knm_number().
-spec activate_feature(knm_number:knm_number(), ne_binary(), ne_binary(), kz_services:services()) ->
                              knm_number:knm_number().
activate_feature(Number, Feature) ->
    BillingId = fetch_billing_id(Number),
    activate_feature(Number, Feature, BillingId).

activate_feature(Number, Feature, BillingId) ->
    Services = fetch_services(Number),
    activate_feature(Number, Feature, BillingId, Services).

activate_feature(Number, Feature, BillingId, Services) ->
    Units = kz_service_phone_numbers:feature_activation_charge(Feature, Services),
    Charges = knm_number:charges(Number, Feature),
    TotalCharges = Charges + Units,

    case kz_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            lager:error("not enough credit to activate feature '~s' for $~p"
                       ,[Feature, wht_util:units_to_dollars(Units)]),
            knm_errors:not_enough_credit(Number, Units);
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),

            lager:debug("adding feature ~s to ~s", [Feature, knm_phone_number:number(PhoneNumber)]),

            Transaction = create_transaction(Number, Feature, Units),

            knm_number:set_phone_number(
              knm_number:add_transaction(
                knm_number:set_charges(Number, Feature, TotalCharges)
                                        ,Transaction
               ),
              knm_phone_number:set_feature(PhoneNumber, Feature, kz_json:new())
             )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_feature(knm_number:knm_number(), ne_binary()) ->
                                knm_number:knm_number().
deactivate_feature(Number, Feature) ->
    PhoneNumber = knm_number:phone_number(Number),
    Features = knm_phone_number:features(PhoneNumber),
    PN = knm_phone_number:set_features(PhoneNumber, kz_json:delete_key(Feature, Features)),
    knm_number:set_phone_number(Number, PN).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_features(knm_number:knm_number(), ne_binaries()) ->
                                 knm_number:knm_number().
deactivate_features(Number, Features) ->
    PhoneNumber = knm_number:phone_number(Number),
    ExistingFeatures = knm_phone_number:features(PhoneNumber),
    PN = knm_phone_number:set_features(PhoneNumber, kz_json:delete_keys(Features, ExistingFeatures)),
    knm_number:set_phone_number(Number, PN).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_services(knm_number:knm_number()) -> knm_number:knm_number().
-ifdef(TEST).
update_services(Number) -> Number.
-else.
update_services(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    update_services(Number
                   ,knm_phone_number:dry_run(PhoneNumber)
                   ,knm_phone_number:batch_run(PhoneNumber)
                   ).

-spec update_services(knm_number:knm_number(), boolean(), boolean()) ->
                             knm_number:knm_number().
update_services(Number, _, 'true') ->
    lager:debug("batch_run-ing btw"),
    Number;
update_services(Number, 'true', _) ->
    lager:debug("somewhat dry_run-ing btw"),
    JObj = knm_phone_number:to_json(knm_number:phone_number(Number)),
    Services = kz_service_phone_numbers:reconcile(fetch_services(Number), [JObj]),
    knm_number:set_services(Number, Services);
update_services(Number, 'false', _) ->
    PhoneNumber = knm_number:phone_number(Number),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    _ = kz_services:reconcile(AssignedTo, <<"phone_numbers">>),
    PrevAssignedTo = knm_phone_number:prev_assigned_to(PhoneNumber),
    _ = kz_services:reconcile(PrevAssignedTo, <<"phone_numbers">>),
    Services = fetch_services(Number),
    Transactions = knm_number:transactions(Number),
    _ = kz_services:commit_transactions(Services, Transactions),
    Number.
-endif.

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
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    Units = kz_service_phone_numbers:
        phone_number_activation_charge(Services, Num),
    activate_phone_number(Number, BillingId, Units).

activate_phone_number(Number, _BillingId, 0) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    lager:debug("no activation charge for ~s", [Num]),
    Number;
activate_phone_number(Number, BillingId, Units) ->
    Charges = knm_number:charges(Number, ?KEY_ACTIVATION_CHARGES),
    TotalCharges = Charges + Units,

    case kz_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            Message = io_lib:format("not enough credit to activate number for $~p"
                                   ,[wht_util:units_to_dollars(Units)]),
            lager:error(Message),
            knm_errors:service_restriction(Message);
        'true' ->
            Transaction = create_transaction(Number, Units),

            knm_number:set_charges(
              knm_number:add_transaction(Number, Transaction)
                                  ,?KEY_NUMBER_ACTIVATION_CHARGES
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
-spec fetch_services(knm_number:knm_number()) -> kz_services:services().
-ifdef(TEST).
fetch_services(_Number) -> kz_services:new().
-else.
fetch_services(Number) ->
    case knm_number:services(Number) of
        'undefined' ->
            AssignedTo = knm_phone_number:assigned_to(knm_number:phone_number(Number)),
            kz_services:fetch(AssignedTo);
        Services ->
            Services
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_billing_id(knm_number:knm_number()) ->
                              ne_binary().
-ifdef(TEST).
fetch_billing_id(_Number) -> ?RESELLER_ACCOUNT_ID.
-else.
fetch_billing_id(Number) ->
    kz_services:get_billing_id(fetch_services(Number)).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_transaction(knm_number:knm_number(), integer()) ->
                                kz_transaction:transaction().
create_transaction(Number, Units) ->
    BillingId = fetch_billing_id(Number),
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    LedgerId = kz_util:format_account_id(BillingId, 'raw'),
    Num = knm_phone_number:number(PhoneNumber),

    Routines = [fun(T) -> set_activation_reason(T, LedgerId, AccountId, <<"number">>) end
               ,fun(T) -> kz_transaction:set_number(Num, T) end
               ,fun(T) -> set_feature_description(T, kz_util:to_binary(Num)) end
               ],
    lager:debug("staging number activation charge $~p for ~s via billing account ~s"
               ,[wht_util:units_to_dollars(Units), AccountId, LedgerId]),

    lists:foldl(fun(F, T) -> F(T) end
               ,kz_transaction:debit(LedgerId, Units)
               ,Routines
               ).

-spec create_transaction(knm_number:knm_number(), ne_binary(), integer()) ->
                                kz_transaction:transaction().
create_transaction(Number, Feature, Units) ->
    BillingId = fetch_billing_id(Number),
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    LedgerId = kz_util:format_account_id(BillingId),
    Num = knm_phone_number:number(PhoneNumber),

    Routines = [fun(T) -> set_activation_reason(T, LedgerId, AccountId, <<"feature">>) end
               ,fun(T) -> kz_transaction:set_feature(Feature, T) end
               ,fun(T) -> kz_transaction:set_number(Num, T) end
               ,fun(T) -> set_feature_description(T, kz_util:to_binary(Feature)) end
               ],
    lager:debug("staging feature '~s' activation charge $~p for ~s via billing account ~s"
               ,[Feature, wht_util:units_to_dollars(Units), AccountId, LedgerId]),

    lists:foldl(fun(F, T) -> F(T) end
               ,kz_transaction:debit(LedgerId, Units)
               ,Routines
               ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_feature_description(kz_transaction:transaction(), ne_binary()) ->
                                     kz_transaction:transaction().
set_feature_description(T, Feature) ->
    Description = <<"number feature activation for ", Feature/binary>>,
    kz_transaction:set_description(Description, T).

-spec set_activation_reason(kz_transaction:transaction(), ne_binary(), ne_binary(), ne_binary()) ->
                                   kz_transaction:transaction().
set_activation_reason(Transaction, LedgerId, LedgerId, Key) ->
    kz_transaction:set_reason(<<Key/binary, "_activation">>, Transaction);
set_activation_reason(Transaction, _LedgetId, AccountId, Key) ->
    kz_transaction:set_reason(<<"sub_account_", Key/binary, "_activation">>
                             ,kz_transaction:set_sub_account_info(AccountId, Transaction)
                             ).

phone_number_activation_charges(Number) ->
    knm_number:charges(Number, ?KEY_NUMBER_ACTIVATION_CHARGES).

activation_charges(Number) ->
    knm_number:charges(Number, ?KEY_ACTIVATION_CHARGES).
