%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services).

-export([add_service_plan/2]).
-export([delete_service_plan/2]).
-export([service_plan_json/1]).
-export([public_json/1]).

-export([empty/0]).
-export([allow_updates/1]).
-export([from_service_json/1]).
-export([reconcile/1, reconcile/2]).
-export([fetch/1]).
-export([update/4]).
-export([save/1]).
-export([delete/1]).

-export([activation_charges/3]).
-export([commit_transactions/2]).
-export([select_bookkeeper/1]).
-export([set_billing_id/2]).
-export([get_billing_id/1]).
-export([find_reseller_id/1]).

-export([account_id/1]).
-export([quantity/3]).
-export([update_quantity/3]).
-export([category_quantity/3]).
-export([cascade_quantity/3]).
-export([cascade_category_quantity/3]).
-export([reset_category/2]).
-export([get_service_module/1]).

-export([is_reseller/1]).

-export([calulate_charges/2]).

-include("whistle_services.hrl").

-record(wh_services, {account_id = 'undefined'
                      ,billing_id = 'undefined'
                      ,current_billing_id = 'undefined'
                      ,new_billing_id = 'false'
                      ,dirty = 'false'
                      ,deleted = 'false'
                      ,status = <<"good_standing">>
                      ,jobj = wh_json:new()
                      ,updates = wh_json:new()
                      ,cascade_quantities = wh_json:new()
                     }).

-define(QUANTITIES, <<"quantities">>).

-type services() :: #wh_services{}.
-export_type([services/0]).

%%%===================================================================
%%% Operations
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec empty() -> services().
empty() ->
    #wh_services{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> services().
new(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Account = get_account_definition(AccountDb, AccountId),
    ResellerId = get_reseller_id(AccountId),
    IsReseller = depreciated_is_reseller(Account),
    BillingId = depreciated_billing_id(Account),
    PvtTree = wh_json:get_value(<<"pvt_tree">>, Account, []),
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_created">>, wh_util:current_tstamp()}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
             ,{<<"pvt_type">>, <<"service">>}
             ,{<<"pvt_vsn">>, <<"1">>}
             ,{<<"pvt_account_id">>, AccountId}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_status">>, <<"good_standing">>}
             ,{<<"pvt_reseller">>, IsReseller}
             ,{<<"pvt_reseller_id">>, ResellerId}
             ,{<<"pvt_tree">>, PvtTree}
             ,{?QUANTITIES, wh_json:new()}
             ,{<<"billing_id">>, BillingId}
             ,{<<"plans">>, populate_service_plans(Account, ResellerId)}
            ],
    #wh_services{account_id=AccountId
                 ,jobj=wh_json:from_list(Props)
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                 ,dirty='true'
                 ,billing_id=BillingId
                 ,current_billing_id=BillingId
                 ,deleted=wh_json:is_true(<<"pvt_deleted">>, Account)
                }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_service_json(wh_json:object()) -> services().
from_service_json(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
    BillingId = wh_json:get_value(<<"billing_id">>, JObj, AccountId),
    #wh_services{account_id=AccountId
                 ,jobj=JObj
                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                 ,status=wh_json:get_ne_value(<<"pvt_status">>, JObj, <<"good_standing">>)
                 ,billing_id=BillingId
                 ,current_billing_id=BillingId
                 ,deleted=wh_json:is_true(<<"pvt_deleted">>, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> services().
fetch(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    %% TODO: if reseller populate cascade via merchant id
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            lager:debug("loaded account service doc ~s", [AccountId]),
            IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
            BillingId = wh_json:get_value(<<"billing_id">>, JObj, AccountId),
            #wh_services{account_id=AccountId
                         ,jobj=JObj
                         ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                         ,status=wh_json:get_ne_value(<<"pvt_status">>, JObj, <<"good_standing">>)
                         ,billing_id=BillingId
                         ,current_billing_id=BillingId
                         ,deleted=wh_json:is_true(<<"pvt_deleted">>, JObj)
                        };
        {'error', _R} ->
            lager:debug("unable to open account ~s services doc (creating new): ~p", [Account, _R]),
            new(AccountId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_service_plan(ne_binary(), services()) -> services().
add_service_plan(PlanId, #wh_services{jobj=JObj}=Services) ->
    ResellerId = wh_json:get_value(<<"pvt_reseller_id">>, JObj),
    Services#wh_services{jobj=wh_service_plans:add_service_plan(PlanId, ResellerId, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_service_plan(ne_binary(), services()) -> services().
delete_service_plan(PlanId, #wh_services{jobj=JObj}=Services) ->
    Services#wh_services{jobj=wh_service_plans:delete_service_plan(PlanId, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save(services()) -> services().
save(#wh_services{jobj=JObj
                  ,updates=UpdatedQuantities
                  ,account_id=AccountId
                  ,dirty=ForceDirty
                 }=Services) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Dirty = have_quantities_changed(UpdatedQuantities, CurrentQuantities) orelse ForceDirty,
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_dirty">>, Dirty}
             ,{<<"pvt_modified">>, wh_util:current_tstamp()}
             ,{?QUANTITIES, wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ],
    UpdatedJObj = wh_json:set_values(props:filter_undefined(Props), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {'ok', NewJObj} ->
            lager:debug("saved services for ~s", [AccountId]),
            IsReseller = wh_json:is_true(<<"pvt_reseller">>, JObj),
            _ = maybe_clean_old_billing_id(Services),
            BillingId=wh_json:get_value(<<"billing_id">>, NewJObj, AccountId),
            Services#wh_services{jobj=NewJObj
                                 ,cascade_quantities=cascade_quantities(AccountId, IsReseller)
                                 ,status=wh_json:get_ne_value(<<"pvt_status">>, NewJObj, <<"good_stainding">>)
                                 ,billing_id=BillingId
                                 ,current_billing_id=BillingId
                                 ,deleted=wh_json:is_true(<<"pvt_deleted">>, NewJObj)
                                };
        {'error', 'not_found'} ->
            lager:debug("service database does not exist, attempting to create"),
            'true' = couch_mgr:db_create(?WH_SERVICES_DB),
            save(Services);
        {'error', 'conflict'} ->
            lager:debug("services for ~s conflicted, merging changes and retrying", [AccountId]),
            {'ok', Existing} = couch_mgr:open_doc(?WH_SERVICES_DB, AccountId),
            save(Services#wh_services{jobj=Existing})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) -> wh_std_return().
delete(Account) ->
    AccountId = wh_util:format_account_id(Account, raw),
    %% TODO: support other bookkeepers, and just cancel subscriptions....
    _ = (catch braintree_customer:delete(AccountId)),
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            lager:debug("marking services for account ~s as deleted", [AccountId]),
            couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_values([{<<"pvt_deleted">>, 'true'}
                                                                    ,{<<"pvt_dirty">>, 'true'}
                                                                   ]
                                                                   ,JObj));
        {'error', 'not_found'} -> {'ok', wh_json:new()};
        {'error', _R}=E ->
            lager:debug("unable to mark service plan ~s as deleted: ~p", [AccountId, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_billing_id(api_binary(), ne_binary() | services()) -> 'undefined' | services().
set_billing_id('undefined', _) -> 'undefined';
set_billing_id(BillingId, #wh_services{billing_id=BillingId}) ->
    'undefined';
set_billing_id(BillingId, #wh_services{account_id=BillingId, jobj=ServicesJObj}=Services) ->
    Services#wh_services{jobj=wh_json:set_value(<<"billing_id">>, BillingId, ServicesJObj)
                         ,billing_id=BillingId
                         ,dirty='true'
                        };
set_billing_id(BillingId, #wh_services{jobj=ServicesJObj}=Services) ->
    PvtTree = wh_json:get_value(<<"pvt_tree">>, ServicesJObj, [BillingId]),
    try lists:last(PvtTree) of
        BillingId ->
            Services#wh_services{jobj=wh_json:set_value(<<"billing_id">>, BillingId, ServicesJObj)
                                 ,billing_id=BillingId
                                 ,dirty='true'
                                };
        _Else ->
            throw({'invalid_billing_id', <<"Requested billing id is not the parent of this account">>})
    catch
        {'EXIT', _} ->
            throw({'invalid_billing_id', <<"Unable to determine if billing id is valid">>})
    end;
set_billing_id(BillingId, AccountId) ->
    set_billing_id(BillingId, fetch(AccountId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_billing_id(ne_binary()) -> ne_binary().
get_billing_id(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _R} ->
            lager:debug("unable to open account ~s services: ~p", [AccountId, _R]),
            AccountId;
        {'ok', ServicesJObj} ->
            case wh_json:get_ne_value(<<"billing_id">>, ServicesJObj, AccountId) of
                AccountId -> AccountId;
                BillingId ->
                    lager:debug("following billing id ~s", [BillingId]),
                    get_billing_id(BillingId)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), ne_binary(), integer(), services()) -> services().
update(Category, Item, Quantity, Services) when not is_integer(Quantity) ->
    update(Category, Item, wh_util:to_integer(Quantity), Services);
update(Category, Item, Quantity, #wh_services{updates=JObj}=Services) when is_binary(Category), is_binary(Item) ->
    Services#wh_services{updates=wh_json:set_value([Category, Item], Quantity, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), services() | ne_binary()) -> number().
activation_charges(Category, Item, #wh_services{jobj=ServicesJObj}) ->
    Plans = wh_service_plans:from_service_json(ServicesJObj),
    wh_service_plans:activation_charges(Category, Item, Plans);
activation_charges(Category, Item, Account) ->
    activation_charges(Category, Item, fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(services(), wh_transactions:wh_transactions()) -> atom().
commit_transactions(#wh_services{billing_id=BillingId}, Activations) ->
    Bookkeeper = select_bookkeeper(BillingId),
    Bookkeeper:commit_transactions(BillingId, Activations).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec select_bookkeeper(ne_binary()) -> atom().
select_bookkeeper(BillingId) ->
    ResellerId = get_reseller_id(BillingId),
    MasterAccountId = whapps_util:get_master_account_id(),
    case ResellerId =:= MasterAccountId of
        'true' -> 'wh_bookkeeper_local';
        'false' ->
            whapps_config:get_atom(?WHS_CONFIG_CAT, <<"master_account_bookkeeper">>, 'wh_bookkeeper_local')
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec service_plan_json(ne_binary() | services()) -> wh_json:object().
service_plan_json(#wh_services{jobj=ServicesJObj}) ->
    Plans = wh_service_plans:from_service_json(ServicesJObj),
    wh_service_plans:public_json(Plans);
service_plan_json(Account) ->
    service_plan_json(fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(ne_binary() | services()) -> wh_json:object().
public_json(#wh_services{jobj=ServicesJObj
                         ,cascade_quantities=CascadeQuantities
                        }) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, ServicesJObj),
    InGoodStanding = try maybe_follow_billling_id(AccountId, ServicesJObj) of
                         'true' -> 'true'
                     catch
                         'throw':_ -> 'false'
                     end,
    Props = [{<<"account_quantities">>, wh_json:get_value(<<"quantities">>, ServicesJObj, wh_json:new())}
             ,{<<"cascade_quantities">>, CascadeQuantities}
             ,{<<"plans">>, wh_service_plans:plan_summary(ServicesJObj)}
             ,{<<"billing_id">>, wh_json:get_value(<<"billing_id">>, ServicesJObj, AccountId)}
             ,{<<"reseller">>, wh_json:is_true(<<"pvt_reseller">>, ServicesJObj)}
             ,{<<"reseller_id">>, wh_json:get_ne_value(<<"pvt_reseller_id">>, ServicesJObj)}
             ,{<<"dirty">>, wh_json:is_true(<<"pvt_dirty">>, ServicesJObj)}
             ,{<<"in_good_standing">>, InGoodStanding}
             ,{<<"items">>, wh_service_plans:public_json_items(ServicesJObj)}
            ],
    wh_json:from_list(Props);
public_json(Account) ->
    public_json(fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_reseller_id(api_binary()) -> api_binary().
find_reseller_id('undefined') ->
    case whapps_util:get_master_account_id() of
        {'error', _} -> 'undefined';
        {'ok', MasterAccountId} -> MasterAccountId
    end;
find_reseller_id(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            case wh_json:get_ne_value(<<"pvt_reseller_id">>, JObj) of
                'undefined' -> get_reseller_id(Account);
                ResellerId -> ResellerId
            end;
        {'error', _} -> get_reseller_id(Account)
    end.

%%%===================================================================
%%% Services functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Throws an error if the billing account is not in "good_standing",
%% used when update requests are made to kill them if there are
%% accounting issues.
%% @end
%%--------------------------------------------------------------------
-spec allow_updates(ne_binary()) -> boolean().
allow_updates(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    case couch_mgr:open_cache_doc(?WH_SERVICES_DB, AccountId) of
        {'error', _R} ->
            lager:debug("unable to open account ~s services: ~p", [AccountId, _R]),
            default_maybe_allow_updates(AccountId);
        {'ok', ServicesJObj} ->
            maybe_follow_billling_id(AccountId, ServicesJObj)
    end.

maybe_follow_billling_id(AccountId, ServicesJObj) ->
    case wh_json:get_ne_value(<<"billing_id">>, ServicesJObj, AccountId) of
        AccountId -> maybe_allow_updates(AccountId, ServicesJObj);
        BillingId ->
            lager:debug("following billing id ~s", [BillingId]),
            allow_updates(BillingId)
    end.

maybe_allow_updates(AccountId, ServicesJObj) ->
    Plans = wh_service_plans:plan_summary(ServicesJObj),
    case wh_util:is_empty(Plans) orelse wh_json:get_value(<<"pvt_status">>, ServicesJObj) of
        'true' ->
            lager:debug("allowing request for account with no service plans"),
            'true';
        <<"good_standing">> ->
            lager:debug("allowing request for account in good standing"),
            'true';
        Status ->
            %% TODO: support other bookkeepers
            case wh_bookkeeper_braintree:is_good_standing(AccountId) of
                'true' -> 'true';
                'false' ->
                    lager:debug("denying update request for services with status '~s'", [Status]),
                    Error = io_lib:format("Unable to continue due to billing account ~s status", [AccountId]),
                    throw({Status, wh_util:to_binary(Error)})
            end
    end.

default_maybe_allow_updates(AccountId) ->
    case whapps_config:get_is_true(?WHS_CONFIG_CAT, <<"default_allow_updates">>, 'true') of
        'true' -> 'true';
        'false' ->
            lager:debug("denying update request, ~s.default_allow_updates is false", [?WHS_CONFIG_CAT]),
            Error = io_lib:format("Service updates are disallowed by default for billing account ~s", [AccountId]),
            throw({<<"updates_disallowed">>, wh_util:to_binary(Error)})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(api_binary()) -> services().
reconcile('undefined') -> [];
reconcile(Account) ->
    lager:debug("reconcile all services for ~s", [Account]),
    ServiceModules = get_service_modules(),
    Services = lists:foldl(fun(M, S) ->
                                   M:reconcile(S)
                           end, fetch(Account), ServiceModules),
    save(Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile(api_binary(), text()) -> 'false' | services().
reconcile('undefined', _) -> 'false';
reconcile(Account, Module) ->
    timer:sleep(1000),
    lager:debug("reconcile ~s services for ~s", [Module, Account]),
    case get_service_module(Module) of
        'false' -> 'false';
        ServiceModule ->
            CurrentServices = fetch(Account),
            UpdatedServices = ServiceModule:reconcile(CurrentServices),
            maybe_save(UpdatedServices)
    end.

%%%===================================================================
%%% Access functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec account_id(services()) -> ne_binary().
account_id(#wh_services{account_id=AccountId}) ->
    AccountId.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity(ne_binary(), ne_binary(), services()) -> integer().
quantity(_, _, #wh_services{deleted='true'}) -> 0;
quantity(Category, Item, #wh_services{updates=UpdatedQuantities
                                      ,jobj=JObj
                                     }) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Quantities = wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities),
    wh_json:get_integer_value([Category, Item], Quantities, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_quantity(ne_binary(), ne_binary(), services()) -> integer().
update_quantity(_, _, #wh_services{deleted='true'}) -> 0;
update_quantity(Category, Item, #wh_services{updates=JObj}) ->
    wh_json:get_integer_value([Category, Item], JObj, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec category_quantity(ne_binary(), ne_binaries(), services()) -> integer().
category_quantity(_, _, #wh_services{deleted='true'}) -> 'true';
category_quantity(Category, Exceptions, #wh_services{updates=UpdatedQuantities
                                                     ,jobj=JObj
                                                    }) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    Quantities = wh_json:merge_jobjs(UpdatedQuantities, CurrentQuantities),
    lists:foldl(fun(Item, Sum) ->
                        case lists:member(Item, Exceptions) of
                            'true' -> Sum;
                            'false' ->
                                wh_json:get_integer_value([Category, Item], Quantities, 0) + Sum
                        end
                end, 0, wh_json:get_keys(Category, Quantities)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantity(ne_binary(), ne_binary(), services()) -> integer().
cascade_quantity(_, _, #wh_services{deleted='true'}) -> 0;
cascade_quantity(Category, Item, #wh_services{cascade_quantities=JObj}=Services) ->
    wh_json:get_integer_value([Category, Item], JObj, 0)
        + quantity(Category, Item, Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_category_quantity(ne_binary(), ne_binaries(), services()) -> integer().
cascade_category_quantity(_, _, #wh_services{deleted='true'}) -> 0;
cascade_category_quantity(Category, Exceptions, #wh_services{cascade_quantities=Quantities}=Services) ->
    lists:foldl(fun(Item, Sum) ->
                        case lists:member(Item, Exceptions) of
                            'true' -> Sum;
                            'false' ->
                                wh_json:get_integer_value([Category, Item], Quantities, 0) + Sum
                        end
                end, category_quantity(Category, Exceptions, Services)
                ,wh_json:get_keys(Category, Quantities)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_category(ne_binary(), services()) -> services().
reset_category(Category, #wh_services{updates=JObj}=Services) ->
    Services#wh_services{updates=wh_json:set_value(Category, wh_json:new(), JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% Helper function to know if an account is a reseller or not.
%% @end
%%--------------------------------------------------------------------
-spec is_reseller(ne_binary() | services()) -> boolean().
is_reseller(Account) ->
    Service = public_json(Account),
    wh_json:is_true(<<"reseller">>, Service).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calulate_charges(services(), any()) -> wh_json:object().
calulate_charges(Services, Transactions) ->
    TransactionCharges = calculate_transactions_charges(Transactions),
    PlansCharges = calculate_services_charges(Services),

    wh_json:merge_jobjs(TransactionCharges, PlansCharges).

-spec calculate_services_charges(services()) -> wh_json:object().
calculate_services_charges(#wh_services{jobj=ServiceJObj}=Services) ->
    case wh_service_plans:from_service_json(ServiceJObj) of
        [] -> wh_json:new();
        ServicePlans ->
            Items1 = wh_service_plans:create_items(Services, ServicePlans),
            case wh_service_plans:create_items(ServiceJObj) of
                {'error', _} -> wh_json:new();
                {'ok', Items2} ->
                    Changed = wh_service_items:get_udapted_items(Items1, Items2),
                    wh_service_items:public_json(Changed)
            end
    end.

-spec calculate_transactions_charges(any()) -> wh_json:object().
calculate_transactions_charges(Transactions) ->
    TransactionsJobj = wh_transactions:to_json(Transactions),
    lists:foldl(
        fun(TransactionJobj, Acc) ->
            Rate = wh_json:get_value(<<"pvt_amount">>, TransactionJobj, 0),
            Description = wh_json:get_value(<<"description">>, TransactionJobj, <<>>),
            wh_json:set_values([
                {<<"activation_charges">>, wht_util:units_to_dollars(Rate)}
                ,{<<"activation_charges_description">>, Description}
            ], Acc)
        end
        ,wh_json:from_list([
            {<<"activation_charges">>, 0}
            ,{<<"activation_charges_description">>, <<>>}
         ])
        ,TransactionsJobj
    ).

-spec get_changes(wh_json:object(), wh_json:object()) -> wh_proplist().
get_changes(OldQuantities, NewQuantities) ->
    Props = wh_json:recursive_to_proplist(NewQuantities),
    FlattenProps = flatten(Props),
    lists:foldl(
        fun({Keys, Value}, Acc) ->
            OldValue = wh_json:get_value(Keys, OldQuantities, 0),
            case Value - OldValue of
                I when I > 0 ->
                   [{Keys, I}];
                _ -> Acc
            end
        end
        ,[]
        ,FlattenProps
    ).


-spec flatten(wh_proplist()) ->  wh_proplist().
-spec flatten( wh_proplist(), list(), wh_proplist()) -> wh_proplist().
flatten(Props) ->
    flatten(Props, [], []).

flatten([], _, Acc) ->
    lists:reverse(Acc);
flatten([{Key, Value}|Tail], Path, Acc) when is_list(Value) ->
    Tmp = flatten(Value, [Key|Path], Acc),
    flatten(Tail, [], Tmp);
flatten([{Key, Value}|[]], Path, Acc) ->
    Tmp = lists:reverse([Key|Path]),
    [{Tmp, Value}|Acc];
flatten([{Key, Value}|Tail], Path, Acc) ->
    Tmp = lists:reverse([Key|Path]),
    flatten(Tail, Path, [{Tmp, Value}|Acc]);
flatten([Elem|Tail], Path, Acc) ->
    Tmp = flatten(Elem, Path, Acc),
    flatten(Tail, Path, lists:merge(Tmp, Acc)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_modules() -> atoms().
get_service_modules() ->
    case whapps_config:get(?WHS_CONFIG_CAT, <<"modules">>) of
        'undefined' ->
            Mods = [Mod
                    || P <- filelib:wildcard([code:lib_dir('whistle_services'), "/src/services/*.erl"])
                           ,begin
                                Name = wh_util:to_binary(filename:rootname(filename:basename(P))),
                                (Mod = wh_util:try_load_module(Name)) =/= 'false'
                            end
                   ],
            lager:debug("found service modules: ~p", [Mods]),
            Mods;
        Modules ->
            lager:debug("configured service modules: ~p", [Modules]),
            [Module || M <- Modules, (Module = wh_util:try_load_module(M)) =/= 'false']
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_module(text()) -> atom() | 'false'.
get_service_module(Module) when not is_binary(Module) ->
    get_service_module(wh_util:to_binary(Module));
get_service_module(<<"wh_service_", _/binary>>=Module) ->
    ServiceModules = get_service_modules(),
    case [M
          || M <- ServiceModules
                 ,wh_util:to_binary(M) =:= Module
         ]
    of
        [M] -> M;
        _Else -> 'false'
    end;
get_service_module(Module) ->
    get_service_module(<<"wh_service_", Module/binary>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantities(ne_binary(), boolean()) -> wh_json:object().

cascade_quantities(Account, 'false') ->
    do_cascade_quantities(Account, <<"services/cascade_quantities">>);
cascade_quantities(Account, 'true') ->
    do_cascade_quantities(Account, <<"services/reseller_quantities">>).

-spec do_cascade_quantities(ne_binary(), ne_binary()) -> wh_json:object().
do_cascade_quantities(Account, View) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    ViewOptions = ['group'
                   ,'reduce'
                   ,{'startkey', [AccountId]}
                   ,{'endkey', [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_SERVICES_DB, View, ViewOptions) of
        {'error', _} -> wh_json:new();
        {'ok', JObjs} ->
            lists:foldl(fun(JObj, J) ->
                                Key = wh_json:get_value(<<"key">>, JObj),
                                Value = wh_json:get_integer_value(<<"value">>, JObj),
                                wh_json:set_value(tl(Key), Value, J)
                        end, wh_json:new(), JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the billing id as it is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_billing_id(wh_json:object()) -> ne_binary().
depreciated_billing_id(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    wh_json:get_value(<<"billing_id">>, JObj, AccountId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine if pvt_reseller is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_is_reseller(wh_json:object()) -> boolean().
depreciated_is_reseller(JObj) ->
    wh_json:is_true(<<"pvt_reseller">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine what service plans are currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec populate_service_plans(wh_json:object(), ne_binary()) -> wh_json:object().
populate_service_plans(JObj, ResellerId) ->
    Plans = incorporate_default_service_plan(ResellerId, master_default_service_plan()),
    incorporate_depreciated_service_plans(Plans, JObj).

-spec default_service_plan_id(ne_binary()) -> api_binary().
default_service_plan_id(ResellerId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, ResellerId) of
        {'ok', JObj} -> wh_json:get_value(<<"default_service_plan">>, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s services: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec depreciated_default_service_plan_id(ne_binary()) -> api_binary().
depreciated_default_service_plan_id(ResellerId) ->
    ResellerDb = wh_util:format_account_id(ResellerId, 'encoded'),
    case couch_mgr:open_doc(ResellerDb, ResellerId) of
        {'ok', JObj} -> wh_json:get_value(<<"default_service_plan">>, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s account definition: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec master_default_service_plan() -> wh_json:object().
master_default_service_plan() ->
    case whapps_util:get_master_account_id() of
        {'error', _} -> wh_json:new();
        {'ok', MasterAccountId} ->
            incorporate_default_service_plan(MasterAccountId, wh_json:new())
    end.

-spec incorporate_default_service_plan(ne_binary(), wh_json:object()) -> wh_json:object().
incorporate_default_service_plan(ResellerId, JObj) ->
    case depreciated_default_service_plan_id(ResellerId) of
        'undefined' ->
            case default_service_plan_id(ResellerId) of
                'undefined' -> JObj;
                PlanId ->
                    Plan = wh_json:from_list([{<<"account_id">>, ResellerId}]),
                    wh_json:set_value(PlanId, Plan, JObj)
            end;
        PlanId ->
            Plan = wh_json:from_list([{<<"account_id">>, ResellerId}]),
            wh_json:set_value(PlanId, Plan, JObj)
    end.

-spec incorporate_depreciated_service_plans(wh_json:object(), wh_json:object()) -> wh_json:object().
incorporate_depreciated_service_plans(Plans, JObj) ->
    PlanIds = wh_json:get_value(<<"pvt_service_plans">>, JObj),
    ResellerId = wh_json:get_value(<<"pvt_reseller_id">>, JObj),
    case wh_util:is_empty(PlanIds) orelse wh_util:is_empty(ResellerId) of
        'true' -> Plans;
        'false' ->
            lists:foldl(fun(PlanId, P) ->
                                Plan = wh_json:from_list([{<<"account_id">>, ResellerId}]),
                                wh_json:set_value(PlanId, Plan, P)
                        end, Plans, PlanIds)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_reseller_id(ne_binaries() | ne_binary()) -> api_binary().
get_reseller_id([]) ->
    case whapps_util:get_master_account_id() of
        {'ok', MasterAccountId} -> MasterAccountId;
        {'error', _} -> 'undefined'
    end;
get_reseller_id([Parent|Ancestors]) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, Parent) of
        {'error', _R} ->
            lager:debug("failed to open services doc ~s durning reseller search: ~p", [Parent, _R]),
            get_reseller_id(Ancestors);
        {'ok', JObj} ->
            case wh_json:is_true(<<"pvt_reseller">>, JObj) of
                'false' -> get_reseller_id(Ancestors);
                'true' -> Parent
            end
    end;
get_reseller_id(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            PvtTree = lists:reverse(wh_json:get_value(<<"pvt_tree">>, JObj, [])),
            get_reseller_id(PvtTree);
        {'error', _R} ->
            lager:info("unable to open account defintion for ~s: ~p", [AccountId, _R]),
            get_reseller_id([])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_save(services()) -> services().
maybe_save(#wh_services{jobj=JObj
                        ,updates=UpdatedQuantities
                       }=Services) ->
    CurrentQuantities = wh_json:get_value(?QUANTITIES, JObj, wh_json:new()),
    case have_quantities_changed(UpdatedQuantities, CurrentQuantities) of
        'true' ->
            lager:debug("quantities have changed, saving dirty services"),
            save(Services);
        'false' ->
            lager:debug("no service quantity changes"),
            {'error', 'no_change'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec have_quantities_changed(wh_json:object(), wh_json:object()) -> boolean().
have_quantities_changed(Updated, Current) ->
    lists:any(fun(Key) -> wh_json:get_value(Key, Updated) =/= wh_json:get_value(Key, Current) end
              ,[[Category, Item]
                || Category <- wh_json:get_keys(Updated)
                       ,Item <- wh_json:get_keys(Category, Updated)
               ])
        orelse lists:any(fun(Key) -> wh_json:get_value(Key, Updated) =/= wh_json:get_value(Key, Current) end
                         ,[[Category, Item]
                           || Category <- wh_json:get_keys(Current)
                                  ,Item <- wh_json:get_keys(Category, Current)
                          ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_account_definition(ne_binary(), ne_binary()) -> wh_json:object().
get_account_definition(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'error', _R} ->
            lager:debug("unable to get account defintion for ~s: ~p", [AccountId, _R]),
            wh_json:new();
        {'ok', JObj} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_clean_old_billing_id(services()) -> services().
maybe_clean_old_billing_id(#wh_services{billing_id=BillingId
                                        ,current_billing_id=BillingId
                                       }=Services) ->
    Services;
maybe_clean_old_billing_id(#wh_services{current_billing_id=BillingId
                                        ,account_id=BillingId
                                        ,jobj=JObj
                                       }=Services) ->
    case wh_json:is_true(<<"pvt_reseller">>, JObj) of
        'true' -> Services;
        'false' ->
            _ = wh_service_sync:clean(BillingId),
            Services
    end;
maybe_clean_old_billing_id(#wh_services{}=Services) ->
    Services.
