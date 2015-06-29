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

-export([
    activate_feature/2
    ,deactivate_feature/2
    ,deactivate_features/2
    ,maybe_update_services/1
]).

-export([fetch/1]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec activate_feature(number(), ne_binary()) -> number().
-spec activate_feature(number(), ne_binary(), ne_binary()) -> number().
-spec activate_feature(number(), ne_binary(), ne_binary(), wh_services:services()) -> number().
activate_feature(Number, Feature) ->
    {Number1, BillingId} = fetch_billing_id(Number),
    activate_feature(Number1, Feature, BillingId).

activate_feature(Number, Feature, BillingId) ->
    {Number1, Services} = fetch_services(Number),
    activate_feature(Number1, Feature, BillingId, Services).

activate_feature(Number, Feature, BillingId, Services) ->
    Units = wh_service_phone_numbers:feature_activation_charge(Feature, Services),
    Charges = knm_phone_number:fetch_storage(Number, <<"charges">>),
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
            Transactions = knm_phone_number:fetch_storage(Number, <<"transactions">>, []),
            Transaction = create_transaction(Number, Feature, Units),
            Updates =[
                {fun knm_phone_number:set_feature/3, [Feature, wh_json:new()]}
                ,{fun knm_phone_number:store/3, [<<"charges">>, TotalCharges]}
                ,{fun knm_phone_number:store/3, [<<"transactions">>, [Transaction|Transactions]]}
            ],
            {'ok', knm_phone_number:setters(Number, Updates)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_feature(number(), ne_binary()) -> number().
deactivate_feature(Number, Feature) ->
    Features = knm_phone_number:features(Number),
    knm_phone_number:set_features(Number, wh_json:delete_key(Feature, Features)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec deactivate_features(number(), ne_binaries()) -> number().
deactivate_features(Number, Features) ->
    Feats = knm_phone_number:features(Number),
    knm_phone_number:set_features(Number, wh_json:delete_keys(Features, Feats)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_services(number()) -> number().
-spec maybe_update_services(number(), boolean()) -> number().
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
    Transactions = knm_phone_number:fetch_storage(Number, <<"transactions">>, []),
    _ = wh_services:commit_transactions(Services, Transactions),

    Number.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(number()) -> wh_services:services().
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
-spec fetch_services(number()) -> {number(), wh_services:services()}.
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
-spec fetch_billing_id(number()) -> {number(), ne_binary()}.
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
-spec create_transaction(number(), ne_binary(), number()) -> wh_transaction:transaction().
create_transaction(Number, Feature, Units) ->
    {_, BillingId} = fetch_billing_id(Number),
    AccountId = knm_phone_number:assigned_to(Number),
    LedgerId = wh_util:format_account_id(BillingId, 'raw'),
    Num = knm_phone_number:number(Number),

    Routines = [fun(T) ->
                    case LedgerId =/= AccountId of
                        'false' ->
                            wh_transaction:set_reason(<<"feature_activation">>, T);
                        'true' ->
                            T1 = wh_transaction:set_sub_account_info(AccountId, T),
                            wh_transaction:set_reason(<<"sub_account_feature_activation">>, T1)
                    end
                end
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