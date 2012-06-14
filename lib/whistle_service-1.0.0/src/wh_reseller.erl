%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% @end
%%%
%%% @contributors
%%% Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wh_reseller).

-export([fetch/1]).
-export([get_plans/1, get_plans/2]).
-export([get_reseller_id/1]).
-export([is_master_reseller/1]).
-export([process_activation_charges/3]).
-export([update_quantity/4]).
-export([increment_quantity/3]).
-export([reset_category_addons/2]).
-export([commit_changes/1]).
-export([find_reseller/1, find_reseller/2]).
-export([get_default_service_plan/1]).
-export([set_service_plans/2, set_service_plans/3]).
-export([assign/1]).
-export([unassign/1]).
-export([assign_representative/1]).
-export([unassign_representative/1]).
-export([get_represenative/1]).
-export([admins/1]).
-export([settings/2]).

-include("wh_service.hrl").

-record(wh_reseller, {reseller = 'undefined' :: 'undefined' | ne_binary()
                      ,plans = [] :: [] | [wh_service_plan:plan(),...]
                      ,billing_id = 'undefined' :: 'undefined' | ne_binary()
                      ,account_id = 'undefined' :: 'undefined' | ne_binary()
                      ,bt_customer = 'undefined' :: 'undefined' | #bt_customer{}
                      ,bt_subscriptions = dict:new() :: dict()
                      ,bt_transactions = dict:new() :: dict()
                      ,bt_merchant_id = 'undefined' :: 'undefined' | ne_binary()
                      ,bt_public_key = 'undefined' :: 'undefined' | ne_binary()
                      ,bt_private_key = 'undefined' :: 'undefined' | ne_binary()
                      ,reset_addons = sets:new() :: set()
                     }).

-type(reseller() :: #wh_reseller{}).
-export_type([reseller/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account id this will create a list of service plans that
%% the account and possibly a reseller are subscribed to.
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (ne_binary()) -> {'ok', reseller()} | {'error', 'no_service_plan'}.
fetch(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),    
    AccountDb = wh_util:format_account_id(Account, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {error, _R}=E ->
            lager:debug("unabled to open account definition for ~s: ~p", [Account, _R]),
            E;
        {ok, JObj} ->
            Reseller = wh_reseller:get_reseller_id(JObj),
            BillingId = wh_json:get_value(<<"pvt_billing_id">>, JObj, AccountId),
            case get_plans(Reseller, JObj) of
                [] -> {error, no_service_plan};
                Plans ->
                    lager:debug("found reseller ~s for account ~s with billing id ~s", [Reseller, AccountId, BillingId]),
                    {ok, #wh_reseller{reseller = Reseller
                                      ,plans = Plans
                                      ,bt_customer = bt_customer(BillingId)
                                      ,account_id = AccountId
                                      ,billing_id = BillingId
                                     }}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the category and name of a service element process any 
%% activation charges
%% @end
%%--------------------------------------------------------------------
-spec process_activation_charges/3 :: (ne_binary(), ne_binary(), reseller()) -> reseller().
process_activation_charges(Category, Name, #wh_reseller{plans=Plans}=Reseller) ->
    process_activation_charges(Category, Name, Reseller, Plans).     

process_activation_charges(_, _, Reseller, []) ->
    Reseller;
process_activation_charges(Category, Name, #wh_reseller{billing_id=BillingId}=Reseller, [Plan|Plans]) ->
    case wh_service_plan:get_activation_charge(Category, Name, Plan) of
        undefined -> process_activation_charges(Category, Name, Reseller, Plans);
        Amount ->
            _ = braintree_transaction:quick_sale(BillingId, Amount),
            process_activation_charges(Category, Name, Reseller, Plans)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Update the quantity by finding all associated subscription/addon 
%% mappings for the given category.name service element.  If the
%% subscription is updated it is moved to #wh_reseller.bt_subscriptions
%% @end
%%--------------------------------------------------------------------
-spec update_quantity/4 :: (ne_binary(), ne_binary(), ne_binary() | integer(), reseller()) -> reseller().
update_quantity(Category, Name, Quantity, #wh_reseller{plans=Plans}=Reseller) ->
    update_quantity(Category, Name, Quantity, Reseller, Plans).

update_quantity(_, _, _, Reseller, []) ->
    Reseller;
update_quantity(Category, Name, Quantity, #wh_reseller{account_id=AccountId}=Reseller, [Plan|Plans]) ->
    case wh_service_plan:get_recurring_plan(Category, Name, Plan) of
        undefined -> update_quantity(Category, Name, Quantity, Reseller, Plans);
        {PlanId, AddOnId} ->
            SubscriptionId = <<AccountId/binary, "_", PlanId/binary>>,
            Subscription = get_subscription(SubscriptionId, PlanId, Reseller),
            R = update_subscription_quanity(SubscriptionId, Subscription, AddOnId, Quantity, Reseller),
            update_quantity(Category, Name, Quantity, R, Plans)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec increment_quantity/3 :: (ne_binary(), ne_binary(), reseller()) -> reseller().
increment_quantity(Category, Name, #wh_reseller{plans=Plans}=Reseller) ->
    increment_quantity(Category, Name, Reseller, Plans).

increment_quantity(_, _, Reseller, []) ->
    Reseller;
increment_quantity(Category, Name, #wh_reseller{account_id=AccountId
                                                ,reset_addons=ResetAddOns}=Reseller
                   ,[Plan|Plans]) ->
    case wh_service_plan:get_recurring_plan(Category, Name, Plan) of
        undefined -> increment_quantity(Category, Name, Reseller, Plans);
        {PlanId, AddOnId} ->
            SubscriptionId = <<AccountId/binary, "_", PlanId/binary>>,
            Subscription = get_subscription(SubscriptionId, PlanId, Reseller),
            AddOn = wh_util:to_list(AddOnId),
            R = case sets:is_element(AddOn, ResetAddOns) of
                    false -> 
                        update_subscription_quanity(SubscriptionId, Subscription, AddOnId, 1, Reseller);
                    true ->  
                        increment_subscription_quanity(SubscriptionId, Subscription, AddOnId, Reseller)
                end,
            increment_quantity(Category, Name, R#wh_reseller{reset_addons=sets:add_element(AddOn, ResetAddOns)}, Plans)            
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the quantity of any subscribed addons on a given category to 0
%% @end
%%--------------------------------------------------------------------
-spec reset_category_addons/2 :: (ne_binary(), reseller()) -> reseller().
reset_category_addons(Category, #wh_reseller{plans=Plans}=Reseller) ->
    reset_category_addons(Category, Plans, Reseller).

reset_category_addons(_, [], Reseller) ->
    Reseller;
reset_category_addons(Category, [Plan|Plans], #wh_reseller{account_id=AccountId}=Reseller) ->
    Updated = lists:foldr(fun({PlanId, AddOnId}, #wh_reseller{reset_addons=ResetAddOns}=R) ->
                                  SubscriptionId = <<AccountId/binary, "_", PlanId/binary>>,
                                  case subscribed_to_addon(SubscriptionId, AddOnId, R) of
                                      false -> R;
                                      true ->
                                          Subscription = get_subscription(SubscriptionId, PlanId, R),
                                          (update_subscription_quanity(SubscriptionId, Subscription, AddOnId, 0, R))
                                              #wh_reseller{reset_addons=sets:add_element(AddOnId, ResetAddOns)}
                                  end
                          end, Reseller, wh_service_plan:get_category_addons(Category, Plan)),
    reset_category_addons(Category, Plans, Updated).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec commit_changes/1 :: (reseller()) -> ok.
commit_changes(#wh_reseller{bt_subscriptions=Subscriptions}) ->
    _ = [braintree_subscription:update(Subscription)
         || {_, Subscription} <- dict:to_list(Subscriptions)
        ],
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_reseller/1 :: ([ne_binary(),...] | []) -> ne_binary().
-spec find_reseller/2 :: ([ne_binary(),...] | [], ne_binary()) -> ne_binary().

find_reseller(Tree) ->
    {ok, MasterAccountId} = whapps_util:get_master_account_id(),
    find_reseller(Tree, MasterAccountId).                                                   

find_reseller([], MasterAccountId) ->             
    MasterAccountId;
find_reseller([ParentId|Tree], MasterAccountId) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, ParentId) of
        {ok, JObj} ->
            case get_reseller_id(JObj) =:= MasterAccountId of
                true -> ParentId;
                false -> find_reseller(Tree, MasterAccountId)
            end;
        {error, _R} ->
            lager:debug("ignoring the ancestor ~s during reseller hunt, unable to open the account definition: ~p", [ParentId, _R]),
            find_reseller(Tree, MasterAccountId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account get all the service plans subscribed to
%% from the resellers account.
%% @end
%%--------------------------------------------------------------------
-spec get_plans/1 :: (ne_binary()) -> [wh_service_plan:plan(),...] | [].
-spec get_plans/2 :: (ne_binary(), wh_json:json_object()) -> [wh_service_plan:plan(),...] | [].

get_plans(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),    
    AccountDb = wh_util:format_account_id(Account, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {error, _R}=E ->
            lager:debug("unabled to open account definition for ~s: ~p", [Account, _R]),
            E;
        {ok, JObj} ->
            Reseller = get_reseller_id(JObj),
            get_plans(Reseller, JObj)
    end.
        
get_plans(Reseller, JObj) ->
    [Plan
     || PlanId <- wh_service_plan:get_plan_ids(JObj)
            ,begin
                 {ok, Plan} = wh_service_plan:fetch(Reseller, PlanId),
                 true
             end
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_service_plans/2 :: (wh_json:json_object(), 'undefined' | ne_binary() | [ne_binary(),...]) -> wh_json:json_object().
-spec set_service_plans/3 :: (wh_json:json_object(), 'undefined' | ne_binary() | [ne_binary(),...], 'undefined' | ne_binary()) 
                            -> wh_json:json_object().

set_service_plans(JObj, ServicePlans) ->
    set_service_plans(JObj, ServicePlans, undefined).

set_service_plans(JObj, ServicePlans, undefined) ->
    case find_reseller(wh_json:get_value(<<"pvt_tree">>, JObj, [])) of
        undefined -> JObj;
        Reseller -> set_service_plans(JObj, ServicePlans, Reseller)
    end;
set_service_plans(JObj, ServicePlans, Reseller) ->
    wh_service_plan:set_service_plans(wh_json:set_value(<<"pvt_reseller_id">>, Reseller, JObj)
                                      ,ServicePlans
                                      ,Reseller).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_default_service_plan/1 :: (ne_binary()) -> 'undefined' | ne_binary().
get_default_service_plan(Reseller) ->
    AccountId = wh_util:format_account_id(Reseller, raw),    
    AccountDb = wh_util:format_account_id(Reseller, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {error, _} -> undefined;
        {ok, JObj} -> wh_json:get_ne_value(<<"default_service_plan">>, JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account definition or reseller record find the reseller id.
%% If not present then assume it is being resold by the master account.
%% @end
%%--------------------------------------------------------------------
-spec get_reseller_id/1 :: (reseller() | wh_json:json_object()) -> ne_binary().
get_reseller_id(#wh_reseller{reseller=Reseller}) ->
    Reseller;
get_reseller_id(JObj) ->
    case wh_json:get_value(<<"pvt_reseller_id">>, JObj) of
        undefined ->
            {ok, MasterAccount} = whapps_util:get_master_account_id(),
            MasterAccount;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account definition or reseller record determine if it
%% is being resold by the master account
%% @end
%%--------------------------------------------------------------------
-spec is_master_reseller/1 :: (reseller() | wh_json:json_object()) -> boolean().
is_master_reseller(Reseller) ->
    {ok, MasterAccount} = whapps_util:get_master_account_id(),
    get_reseller_id(Reseller) =:= MasterAccount.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign the first account in the pvt_tree who is a reseller of the 
%% master account
%% @end
%%--------------------------------------------------------------------
-spec assign/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
-spec assign/2 :: (ne_binary(), wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.

assign(JObj) ->
    case whapps_util:get_master_account_id() of
        {error, _R}=E ->
            lager:debug("unable to assign reseller, master account is not known: ~p", [_R]),
            E;
        {ok, MasterAccountId} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, JObj, []),
            ResellerId = find_reseller(Tree, MasterAccountId),
            assign(ResellerId, JObj)
    end.            
        
assign(ResellerId, JObj) ->
    ResellerDb = wh_util:format_account_id(ResellerId, encoded),
    case couch_mgr:db_exists(ResellerDb) of
        false -> {error, bad_reseller_id};
        true ->
            AccountId = wh_json:get_value(<<"_id">>, JObj),
            AccountDb = wh_util:format_account_id(AccountId, encoded),
            _ =  assign_representative(JObj),
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_reseller_id">>, ResellerId, JObj)) of
                {error, _}=E -> E;
                {ok, JObj} -> 
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Assign the master account as the reseller for this account
%% @end
%%--------------------------------------------------------------------
-spec unassign/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
unassign(JObj) ->
    case whapps_util:get_master_account_id() of
        {error, _R}=E ->
            lager:debug("unable to unassign reseller, master account is not known: ~p", [_R]),
            E;
        {ok, MasterAccountId} ->
            AccountId = wh_json:get_value(<<"_id">>, JObj),
            AccountDb = wh_util:format_account_id(AccountId, encoded),
            _ = unassign_representative(JObj),
            case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_reseller_id">>, MasterAccountId, JObj)) of
                {error, _}=E -> E;
                {ok, JObj} -> 
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to assign to an account rep in the resellers account
%% @end
%%--------------------------------------------------------------------
-spec assign_representative/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
assign_representative(JObj) ->
    case wh_json:get_value(<<"pvt_reseller_id">>, JObj) of
        undefined -> {error, no_reseller};
        ResellerId ->
            ResellerDb = wh_util:format_account_id(ResellerId, encoded),
            ViewOptions = [{<<"limit">>, 1}
                           ,{<<"include_docs">>, true}
                          ],
            case couch_mgr:get_results(ResellerDb, <<"reseller/count_assignments">>, ViewOptions) of
                {error, _R}=E ->
                    lager:debug("failed to assign reseller representative: ~p", [_R]),
                    E;
                {ok, [Results]} ->
                    AccountId = wh_json:get_value(<<"_id">>, JObj),
                    Rep = wh_json:get_value(<<"doc">>, Results),
                    Assignments = wh_json:get_value(<<"pvt_account_assignments">>, Rep, []), 
                    couch_mgr:save_doc(ResellerDb, wh_json:set_value(<<"pvt_account_assignments">>, [AccountId|Assignments], Rep))
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempt to remove any assignments to a resellers rep
%% @end
%%--------------------------------------------------------------------
-spec unassign_representative/1 :: (json:json_object()) -> ok.
unassign_representative(JObj) ->
    case wh_json:get_value(<<"pvt_reseller_id">>, JObj) of
        undefined -> ok;
        ResellerId ->
            AccountId = wh_json:get_value(<<"_id">>, JObj),
            ResellerDb = wh_util:format_account_id(ResellerId, encoded),
            ViewOptions = [{<<"include_docs">>, true}
                           ,{<<"key">>, AccountId}
                          ],
            case couch_mgr:get_results(ResellerDb, <<"reseller/find_assignments">>, ViewOptions) of
                {error, _R} ->
                    lager:debug("failed to find reseller representatives: ~p", [_R]),
                    ok;
                {ok, Results} ->
                    _ = [begin
                             Rep = wh_json:get_value(<<"doc">>, Result),
                             Assignments = wh_json:get_value(<<"pvt_account_assignments">>, Rep, []),                          
                             couch_mgr:save_doc(ResellerDb
                                                ,wh_json:set_value(<<"pvt_account_assignments">>
                                                                       ,lists:delete(AccountId, Assignments)
                                                                   ,Rep)
                                               )
                         end || Result <- Results],
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_represenative/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
get_represenative(JObj) ->
    AccountId = wh_json:get_value(<<"_id">>, JObj),
    ResellerId = get_reseller_id(JObj),
    ResellerDb = wh_util:format_account_id(ResellerId, encoded),    
    ViewOptions = [{<<"include_docs">>, true}
                   ,{<<"key">>, AccountId}
                  ],
    case couch_mgr:get_results(ResellerDb, <<"reseller/find_assignments">>, ViewOptions) of
        {error, _R}=E -> 
            lager:debug("unable to find reseller account represenatives: ~p", [_R]),
            E;
        {ok, []} -> assign_representative(JObj);
        {ok, [Rep|_]} -> {ok, wh_json:get_value(<<"doc">>, Rep, wh_json:new())}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a list of users who are admins in the resellers account
%% @end
%%--------------------------------------------------------------------
-spec admins/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_objects()} | {'error', _}.
admins(JObj) ->
    ResellerId = get_reseller_id(JObj),
    ResellerDb = wh_util:format_account_id(ResellerId, encoded),
    ViewOptions = [{<<"key">>, <<"user">>}
                   ,{<<"include_docs">>, true}
                  ],
    case couch_mgr:get_results(ResellerDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {ok, Users} -> 
            Admins = [wh_json:get_value(<<"doc">>, User) 
                      || User <- Users
                             ,wh_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>
                     ],
            {ok, Admins};
        {error, _R}=E -> 
            lager:debug("failed to find reseller ~s account admins: ~p", [ResellerId, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec settings/2 :: (ne_binary(), wh_json:json_object()) -> wh_json:json_objects().
settings(Key, JObj) ->
    ResellerId = get_reseller_id(JObj),
    whapps_account_config:get(ResellerId, Key).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec bt_customer/1 :: (ne_binary()) -> #bt_customer{}.
bt_customer(BillingId) ->
    lager:debug("requesting braintree customer ~s", [BillingId]),
    try braintree_customer:find(BillingId) of
        Customer -> Customer
    catch
        throw:{not_found, _} ->
            lager:debug("braintree customer ~s not found, creating new account", [BillingId]),
            braintree_customer:create(BillingId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_subscription/3 :: (ne_binary(), ne_binary(), reseller()) -> #bt_subscription{}.
get_subscription(SubscriptionId, PlanId, #wh_reseller{bt_subscriptions=Subscriptions, bt_customer=Customer}) ->
    case dict:find(SubscriptionId, Subscriptions) of
        {ok, Subscription} -> Subscription; 
        error ->
            try braintree_customer:get_subscription(SubscriptionId, Customer) of
                Subscription -> Subscription
            catch
                throw:{not_found, _} ->                        
                    braintree_customer:new_subscription(SubscriptionId, PlanId, Customer)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribed_to_addon/3 :: (ne_binary(), ne_binary(), reseller()) -> boolean().
subscribed_to_addon(SubscriptionId, AddOnId, #wh_reseller{bt_customer=Customer}) ->
    try
        Subscription = braintree_customer:get_subscription(SubscriptionId, Customer),
        _ = braintree_subscription:get_addon(Subscription, AddOnId),
        true
    catch
        throw:{not_found, _} -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_subscription_quanity/5 :: (ne_binary(), #bt_subscription{}, ne_binary(), ne_binary() | integer(), reseller()) -> reseller().
update_subscription_quanity(SubscriptionId, Subscription, AddOnId, Quantity, #wh_reseller{bt_subscriptions=Subscriptions
                                                                                         ,bt_customer=Customer}=Reseller) ->
    CustomerId = braintree_customer:get_id(Customer),
    lager:info("updating customer ~s subscription ~s addon ~s quantity to ~p", [CustomerId, SubscriptionId, AddOnId, Quantity]),
    UpdatedSubscription = braintree_subscription:update_addon_quantity(Subscription, AddOnId, Quantity),
    Reseller#wh_reseller{bt_subscriptions=dict:store(SubscriptionId, UpdatedSubscription, Subscriptions)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec increment_subscription_quanity/4 :: (ne_binary(), #bt_subscription{}, ne_binary(), reseller()) -> reseller().
increment_subscription_quanity(SubscriptionId, Subscription, AddOnId, #wh_reseller{bt_subscriptions=Subscriptions
                                                                                   ,bt_customer=Customer}=Reseller) ->
    CustomerId = braintree_customer:get_id(Customer),
    lager:info("increment customer ~s subscription ~s addon ~s", [CustomerId, SubscriptionId, AddOnId]),
    UpdatedSubscription = braintree_subscription:increment_addon_quantity(Subscription, AddOnId),
    Reseller#wh_reseller{bt_subscriptions=dict:store(SubscriptionId, UpdatedSubscription, Subscriptions)}.
