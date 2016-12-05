%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Pierre Fenoll
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

-define(KEY_NUMBER_ACTIVATION_CHARGES, <<"number_activation_charges">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-type set_feature() :: {ne_binary(), kz_json:object()}.
-spec activate_feature(knm_number:knm_number(), set_feature() | ne_binary()) ->
                              knm_number:knm_number().
activate_feature(Number, Feature=?NE_BINARY) ->
    activate_feature(Number, {Feature, kz_json:new()});
activate_feature(Number, FeatureToSet) ->
    BillingId = fetch_billing_id(Number),
    activate_feature(Number, FeatureToSet, BillingId).

-ifdef(TEST).
activate_feature(Number, {Feature,FeatureData}, _) ->
    %% Adding feature regardless of service plan
    PN = knm_phone_number:set_feature(knm_number:phone_number(Number), Feature, FeatureData),
    knm_number:set_phone_number(Number, PN).
-else.
activate_feature(Number, FeatureToSet, BillingId) ->
    Services = fetch_services(Number),
    activate_feature(Number, FeatureToSet, BillingId, Services).

-spec activate_feature(knm_number:knm_number(), set_feature(), ne_binary()) ->
                              knm_number:knm_number().
-spec activate_feature(knm_number:knm_number(), set_feature(), ne_binary(), kz_services:services()) ->
                              knm_number:knm_number().
activate_feature(Number, {Feature,FeatureData}, BillingId, Services) ->
    Units = kz_service_phone_numbers:feature_activation_charge(Feature, Services),
    Charges = knm_number:charges(Number, Feature),
    TotalCharges = Charges + Units,

    case TotalCharges =:= 0
        orelse kz_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            lager:error("not enough credit to activate feature '~s' for $~p ($~p)"
                       ,[Feature, wht_util:units_to_dollars(Units), wht_util:units_to_dollars(TotalCharges)]),
            knm_errors:not_enough_credit(Number, Units);
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),
            lager:debug("adding feature ~s to ~s"
                       ,[Feature, knm_phone_number:number(PhoneNumber)]),
            N = maybe_create_activation_transaction(Number, Feature, Units, TotalCharges),
            PN = knm_phone_number:set_feature(PhoneNumber, Feature, FeatureData),
            knm_number:set_phone_number(N, PN)
    end.

-spec maybe_create_activation_transaction(knm_number:knm_number(), ne_binary(), integer(), number()) -> knm_number:knm_number().
maybe_create_activation_transaction(Number, _Feature, _Units, 0) ->
    lager:debug("no charges for feature ~s activation", [_Feature]),
    Number;
maybe_create_activation_transaction(Number, Feature, Units, TotalCharges) ->
    lager:debug("creating transaction for feature ~s activation : ~p", [Feature, TotalCharges]),
    Transaction = create_transaction(Number, Feature, Units),
    N = knm_number:set_charges(Number, Feature, TotalCharges),
    knm_number:add_transaction(N, Transaction).
-endif.

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
-spec update_services(knm_numbers:collection()) -> knm_numbers:collection().
-ifdef(TEST).
update_services(T=#{todo := Ns}) -> knm_numbers:ok(Ns, T).
-else.
update_services(T=#{todo := Ns, options := Options}) ->
    case {knm_number_options:batch_run(Options)
         ,knm_number_options:dry_run(Options)
         }
    of
        {true, _} ->
            lager:debug("batch_run-ing btw"),
            knm_numbers:ok(Ns, T);
        {_, true} ->
            lager:debug("somewhat dry_run-ing btw"),
            PNs = [knm_number:phone_number(N) || N <- Ns],
            AssignedTo = knm_numbers:assigned_to(T),
            Services = kz_service_phone_numbers:reconcile(do_fetch_services(AssignedTo), PNs),
            knm_numbers:ok(Ns, T#{services => Services});
        {_, false} ->
            AssignedTo = knm_numbers:assigned_to(T),
            _ = kz_services:reconcile(AssignedTo, <<"phone_numbers">>),
            PrevAssignedTo = knm_numbers:prev_assigned_to(T),
            _ = kz_services:reconcile(PrevAssignedTo, <<"phone_numbers">>),
            Services = do_fetch_services(AssignedTo),
            _ = 'undefined' =/= AssignedTo
                andalso kz_services:commit_transactions(Services, knm_numbers:transactions(T)),
            knm_numbers:ok(Ns, T#{services => Services})
    end.
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_phone_number(knm_numbers:collection()) -> knm_numbers:collection().
activate_phone_number(T=#{services := undefined}) ->
    AssignedTo = knm_numbers:assigned_to(T),
    activate_phone_number(T#{services => do_fetch_services(AssignedTo)});
activate_phone_number(T0=#{todo := Ns, services := Services}) ->
    AssignedTo = knm_numbers:assigned_to(T0),
    BillingId = fetch_billing_id(T0),
    F = fun (N, T) ->
                Num = knm_phone_number:number(knm_number:phone_number(N)),
                case kz_service_phone_numbers:phone_number_activation_charge(Services, Num) of
                    0 ->
                        lager:debug("no activation charge for ~s", [Num]),
                        knm_numbers:ok(N, T);
                    Units ->
                        T1 = do_activate(T, Num, Units, BillingId, AssignedTo),
                        knm_numbers:ok(N, T1)
                end
        end,
    lists:foldl(F, T0, Ns).

do_activate(T, Num, Units, BillingId, AssignedTo) ->
    TotalCharges = activation_charges(T) + Units,
    case kz_services:check_bookkeeper(BillingId, TotalCharges) of
        false ->
            Message =
                iolist_to_binary(
                  io_lib:format("not enough credit to activate number for $~p"
                               ,[wht_util:units_to_dollars(Units)])),
            lager:error(Message),
            Error = knm_errors:to_json(service_restriction, undefined, Message),
            knm_numbers:ko(Num, Error, T);
        true ->
            Transaction = create_transaction(Num, Units, BillingId, AssignedTo),
            knm_numbers:charge(?KEY_NUMBER_ACTIVATION_CHARGES
                              ,TotalCharges
                              ,knm_numbers:transaction(Transaction, T)
                              )
    end.

%% @public
-spec phone_number_activation_charges(knm_numbers:collection()) -> non_neg_integer().
phone_number_activation_charges(T) ->
    knm_numbers:charge(?KEY_NUMBER_ACTIVATION_CHARGES, T).

%% @public
-spec activation_charges(knm_numbers:collection()) -> non_neg_integer().
activation_charges(T) ->
    knm_numbers:charge(<<"activation">>, T).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).
do_fetch_services(undefined) -> kz_services:new();
do_fetch_services(?MATCH_ACCOUNT_RAW(_)) -> kz_services:new().
-else.
-spec fetch_services(knm_number:knm_number()) -> kz_services:services().
fetch_services(Number) ->
    case knm_number:services(Number) of
        'undefined' ->
            do_fetch_services(
              knm_phone_number:assigned_to(
                knm_number:phone_number(Number)));
        Services ->
            Services
    end.

do_fetch_services(undefined) -> kz_services:new();
do_fetch_services(?MATCH_ACCOUNT_RAW(AssignedTo)) -> kz_services:fetch(AssignedTo).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_billing_id(knm_number:knm_number()) -> api_ne_binary();
                      (knm_numbers:collection()) -> api_ne_binary().
-ifdef(TEST).
fetch_billing_id(_Number) -> ?RESELLER_ACCOUNT_ID.
-else.
fetch_billing_id(#{services := Services}) ->
    kz_services:get_billing_id(Services);
fetch_billing_id(Number) ->
    kz_services:get_billing_id(fetch_services(Number)).
-endif.

%% @private
create_transaction(Num, Units, BillingId, AccountId) ->
    LedgerId = kz_util:format_account_id(BillingId),
    Fs = [fun(T) -> set_activation_reason(T, LedgerId, AccountId, <<"number">>) end
         ,fun(T) -> kz_transaction:set_number(Num, T) end
         ,fun(T) -> set_feature_description(T, kz_util:to_binary(Num)) end
         ],
    lager:debug("staging number activation charge $~p for ~s via billing account ~s"
               ,[wht_util:units_to_dollars(Units), AccountId, LedgerId]),
    T0 = kz_transaction:debit(LedgerId, Units),
    lists:foldl(fun(F, T) -> F(T) end, T0, Fs).

-ifndef(TEST).
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
-endif.

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
