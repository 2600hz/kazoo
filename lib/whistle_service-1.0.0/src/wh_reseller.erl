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
-export([update_quantity/4]).
-export([commit_changes/1]).
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
                     }).

-type(reseller() :: [#wh_reseller{},...] | []).
-export_type([reseller/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account id this will create a list of service plans that
%% the account and possibly a reseller are subscribed to.
%% @end
%%--------------------------------------------------------------------
-spec fetch/1 :: (ne_binary()) -> {'ok', reseller()}.

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
            lager:debug("found reseller ~s for account ~s with billing id ~s", [Reseller, AccountId, BillingId]),
            {ok, #wh_reseller{reseller = Reseller
                              ,plans = get_plans(Reseller, JObj)
                              ,bt_customer = bt_customer(BillingId)
                              ,account_id = AccountId
                              ,billing_id = BillingId
                             }}
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
update_quantity(Category, Name, Quantity, Reseller, [Plan|Plans]) ->
    Routines = [fun(#wh_reseller{account_id=AccountId}=R) -> 
                        PlanId = wh_service_plan:get_plan_id(Category, Name, Plan),
                        AddOnId = wh_service_plan:get_addon_id(Category, Name, Plan),
                        case wh_util:is_empty(PlanId) orelse wh_util:is_empty(AddOnId) of
                            true -> R;
                            false ->
                                SubscriptionId = <<AccountId/binary, "_", PlanId/binary>>,
                                {ok, Subscription} = get_subscription(SubscriptionId, PlanId, Reseller),
                                update_subscription_quanity(SubscriptionId, Subscription, AddOnId, Quantity, R)
                        end
                end
               ],
    R = lists:foldl(fun(F, R) -> F(R) end, Reseller, Routines),
    update_quantity(Category, Name, Quantity, R, Plans).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec commit_changes/1 :: (reseller()) -> ok.
commit_changes(#wh_reseller{bt_subscriptions=Subscriptions}) ->
    [begin
         case braintree_subscription:update(Subscription) of
             {ok, _} -> 
                 SubscriptionId = braintree_subscription:get_id(Subscription),
                 lager:debug("commited changes to subscription ~s", [SubscriptionId]),
                 ok;
             {error, _}=E ->
                 throw(E)
         end
     end
     || {_, Subscription} <- dict:to_list(Subscriptions)
    ].

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
            Reseller = wh_reseller:get_reseller_id(JObj),
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
    case couch_mgr:db_exist(ResellerDb) of
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
    case get_reseller_id(JObj) of
        undefined -> {error, no_reseller};
        ResellerId ->
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
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a list of users who are admins in the resellers account
%% @end
%%--------------------------------------------------------------------
-spec admins/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_objects()} | {'error', _}.
admins(JObj) ->
    case get_reseller_id(JObj) of
        undefined -> {error, no_reseller};
        ResellerId ->
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
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec settings/2 :: (ne_binary(), wh_json:json_object()) -> wh_json:json_objects().
settings(Key, JObj) ->
    case get_reseller_id(JObj) of
        undefined -> wh_json:new();
        ResellerId -> whapps_account_config:get(ResellerId, Key)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec bt_customer/1 :: (ne_binary()) -> #bt_customer{}.
bt_customer(BillingId) ->
    lager:debug("requesting braintree customer ~s", [BillingId]),
    case braintree_customer:find(BillingId) of
        {ok, Customer} -> Customer;
        {error, not_found} ->
            lager:debug("braintree customer ~s not found, creating new account", [BillingId]),
            {ok, Customer} =  braintree_customer:create(BillingId),
            Customer
    end.

-spec find_reseller/2 :: ([ne_binary(),...] | [], ne_binary()) -> ne_binary().
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

-spec get_subscription/3 :: (ne_binary(), ne_binary(), reseller()) -> {'ok', #bt_subscription{}} | {'error', _}.
get_subscription(SubscriptionId, PlanId, #wh_reseller{bt_subscriptions=Subscriptions, bt_customer=Customer}) ->
    case dict:find(SubscriptionId, Subscriptions) of
        {ok, _}=Ok -> Ok;
        error ->
            case braintree_customer:get_subscription(SubscriptionId, Customer) of
                {error, not_found} -> braintree_customer:new_subscription(SubscriptionId, PlanId, Customer);
                {ok, _}=Ok -> Ok
            end
    end.

-spec update_subscription_quanity/5 :: (ne_binary(), #bt_subscription{}, ne_binary(), ne_binary() | integer(), reseller()) -> reseller().
update_subscription_quanity(SubscriptionId, Subscription, AddOnId, Quantity, #wh_reseller{bt_subscriptions=Subscriptions
                                                                                         ,bt_customer=Customer}=Reseller) ->
    CustomerId = braintree_customer:get_id(Customer),
    lager:info("updating customer ~s subscription ~s addon ~s quantity to ~p", [CustomerId, SubscriptionId, AddOnId, Quantity]),
    {ok, UpdatedSubscription} = braintree_subscription:update_addon_quantity(Subscription, AddOnId, Quantity),
    Reseller#wh_reseller{bt_subscriptions=dict:store(SubscriptionId, UpdatedSubscription, Subscriptions)}.
