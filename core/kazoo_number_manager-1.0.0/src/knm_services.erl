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

-export([fetch/1]).

-include("knm.hrl").

-define(KEY_NUMBER_ACTIVATION_CHARGES, <<"number_activation_charges">>).
-define(KEY_TRANSACTIONS, <<"transactions">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_feature(knm_phone_number:knm_number(), ne_binary()) ->
                              number_return().
-spec activate_feature(knm_phone_number:knm_number(), ne_binary(), ne_binary()) ->
                              number_return().
-spec activate_feature(knm_phone_number:knm_number(), ne_binary(), ne_binary(), wh_services:services()) ->
                              number_return().
activate_feature(Number, Feature) ->
    {Number1, BillingId} = fetch_billing_id(Number),
    activate_feature(Number1, Feature, BillingId).

activate_feature(Number, Feature, BillingId) ->
    {Number1, Services} = fetch_services(Number),
    activate_feature(Number1, Feature, BillingId, Services).

activate_feature(Number, Feature, BillingId, Services) ->
    Units = wh_service_phone_numbers:feature_activation_charge(Feature, Services),
    Charges = knm_phone_number:fetch_storage(Number, <<"feature_charges">>),
    TotalCharges = Charges + Units,

    case wh_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            lager:error(
              "not enough credit to activate feature '~s' for $~p"
              ,[Feature, wht_util:units_to_dollars(Units)]
             ),
            {'error', 'not_enough_credit'};
        'true' ->
            lager:debug("adding feature ~s to ~s", [Feature, knm_phone_number:number(Number)]),
            Transactions = knm_phone_number:fetch_storage(Number, ?KEY_TRANSACTIONS, []),
            Transaction = create_transaction(Number, Feature, Units),
            Updates = [{fun knm_phone_number:set_feature/3, [Feature, wh_json:new()]}
                       ,{fun knm_phone_number:store/3, [<<"feature_charges">>, TotalCharges]}
                       ,{fun knm_phone_number:store/3, [?KEY_TRANSACTIONS, [Transaction|Transactions]]}
                      ],
            {'ok', knm_phone_number:setters(Number, Updates)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_feature(knm_phone_number:knm_number(), ne_binary()) ->
                                {'ok', knm_phone_number:knm_number()}.
deactivate_feature(Number, Feature) ->
    Features = knm_phone_number:features(Number),
    {'ok', knm_phone_number:set_features(Number, wh_json:delete_key(Feature, Features))}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_features(knm_phone_number:knm_number(), ne_binaries()) ->
                                 {'ok', knm_phone_number:knm_number()}.
deactivate_features(Number, Features) ->
    ExistingFeatures = knm_phone_number:features(Number),
    {'ok', knm_phone_number:set_features(Number, wh_json:delete_keys(Features, ExistingFeatures))}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_services(knm_phone_number:knm_number()) ->
                                   knm_phone_number:knm_number().
-spec maybe_update_services(knm_phone_number:knm_number(), boolean()) ->
                                   knm_phone_number:knm_number().
maybe_update_services(Number) ->
    maybe_update_services(Number, knm_phone_number:dry_run(Number)).

maybe_update_services(Number, 'true') ->
    JObj = knm_phone_number:to_json(Number),
    Services  = wh_service_phone_numbers:reconcile([JObj], fetch(Number)),
    knm_phone_number:store(Number, <<"services">>, Services);
maybe_update_services(Number, 'false') ->
    AssignedTo = knm_phone_number:assigned_to(Number),
    _ = wh_services:reconcile(AssignedTo, <<"phone_numbers">>),

    PrevAssignedTo = knm_phone_number:prev_assigned_to(Number),
    _ = wh_services:reconcile(PrevAssignedTo, <<"phone_numbers">>),

    Services = fetch(Number),
    Transactions = knm_phone_number:fetch_storage(Number, ?KEY_TRANSACTIONS, []),
    _ = wh_services:commit_transactions(Services, Transactions),

    Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_phone_number(knm_phone_number:knm_number()) ->
                                   number_return().
-spec activate_phone_number(knm_phone_number:knm_number(), ne_binary()) ->
                                   number_return().
-spec activate_phone_number(knm_phone_number:knm_number(), ne_binary(), integer()) ->
                                   number_return().
activate_phone_number(Number) ->
    {Number1, BillingId} = fetch_billing_id(Number),
    activate_phone_number(Number1, BillingId).

activate_phone_number(Number, BillingId) ->
    {Number1, Services} = fetch_services(Number),
    Units = wh_service_phone_numbers:phone_number_activation_charge(
              knm_phone_number:number(Number1)
              ,Services
             ),
    activate_phone_number(Number1, BillingId, Units).

activate_phone_number(Number, _BillingId, 0) ->
    Num  = knm_phone_number:number(Number),
    lager:debug("no activation charge for ~s", [Num]),
    Number;
activate_phone_number(Number, BillingId, Units) ->
    Charges = knm_phone_number:fetch_storage(Number, ?KEY_NUMBER_ACTIVATION_CHARGES),
    TotalCharges = Charges + Units,

    case wh_services:check_bookkeeper(BillingId, TotalCharges) of
        'false' ->
            lager:error(
              "not enough credit to activate number for $~p"
              ,[wht_util:units_to_dollars(Units)]
             ),
            {'error', 'not_enough_credit'};
        'true' ->
            Transactions = knm_phone_number:fetch_storage(Number, ?KEY_TRANSACTIONS, []),
            Transaction = create_transaction(Number, Units),
            Updates = [{fun knm_phone_number:store/3, [?KEY_NUMBER_ACTIVATION_CHARGES, TotalCharges]}
                       ,{fun knm_phone_number:store/3, [?KEY_TRANSACTIONS, [Transaction|Transactions]]}
                      ],
            {'ok', knm_phone_number:setters(Number, Updates)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(knm_phone_number:knm_number()) -> wh_services:services().
fetch(Number) ->
    {_, Services} = fetch_services(Number),
    Services.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_services(knm_phone_number:knm_number()) ->
                            {knm_phone_number:knm_number(), wh_services:services()}.
fetch_services(Number) ->
    case knm_phone_number:fetch_storage(Number, <<"services">>) of
        'undefined' ->
            AssignedTo = knm_phone_number:assigned_to(Number),
            Services = wh_services:fetch(AssignedTo),
            {knm_phone_number:store(Number, <<"services">>, Services), Services};
        Services ->
            {Number, Services}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_billing_id(knm_phone_number:knm_number()) ->
                              {knm_phone_number:knm_number(), ne_binary()}.
fetch_billing_id(Number) ->
    case knm_phone_number:fetch_storage(Number, <<"billing_id">>) of
        'undefined' ->
            AssignedTo = knm_phone_number:assigned_to(Number),
            BillingId = wh_services:get_billing_id(AssignedTo),
            {knm_phone_number:store(Number, <<"billing_id">>, BillingId), BillingId};
        BillingId ->
            {Number, BillingId}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_transaction(knm_phone_number:knm_number(), integer()) ->
                                wh_transaction:transaction().
create_transaction(Number, Units) ->
    {_, BillingId} = fetch_billing_id(Number),
    AccountId = knm_phone_number:assigned_to(Number),
    LedgerId = wh_util:format_account_id(BillingId, 'raw'),
    Num = knm_phone_number:number(Number),

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

-spec set_activation_reason(wh_transaction:transaction(), ne_binary(), ne_binary(), ne_binary()) ->
                                    wh_transaction:transaction().
set_activation_reason(Transaction, LedgerId, LedgerId, Key) ->
    wh_transaction:set_reason(<<Key/binary, "_activation">>, Transaction);
set_activation_reason(Transaction, _LedgetId, AccountId, Key) ->
    wh_transaction:set_reason(<<"sub_account_", Key/binary, "_activation">>
                              ,wh_transaction:set_sub_account_info(AccountId, Transaction)
                             ).

-spec create_transaction(knm_phone_number:knm_number(), ne_binary(), integer()) ->
                                wh_transaction:transaction().
create_transaction(Number, Feature, Units) ->
    {_, BillingId} = fetch_billing_id(Number),
    AccountId = knm_phone_number:assigned_to(Number),
    LedgerId = wh_util:format_account_id(BillingId, 'raw'),
    Num = knm_phone_number:number(Number),

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
