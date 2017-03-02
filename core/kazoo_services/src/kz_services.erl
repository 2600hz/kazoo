%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_services).

-export([add_service_plan/2]).
-export([delete_service_plan/2]).
-export([service_plan_json/1]).
-export([public_json/1]).
-export([to_json/1]).

-export([new/0]).
-export([allow_updates/1]).
-export([from_service_json/1, from_service_json/2]).
-export([reconcile/1, reconcile/2
        ,reconcile_only/1, reconcile_only/2
        ,save_as_dirty/1
        ]).
-export([fetch/1, fetch_services_doc/2
        ,flush_services/0
        ]).
-export([update/4]).
-export([save/1]).
-export([delete/1]).
-export([list_categories/1]).
-export([list_items/2]).

-export([activation_charges/3]).
-export([commit_transactions/2]).
-export([charge_transactions/2]).
-export([select_bookkeeper/1]).
-export([check_bookkeeper/2]).
-export([set_billing_id/2]).
-export([get_billing_id/1]).
-export([find_reseller_id/1]).

-export([account_id/1
        ,account_name/1
        ,services_json/1
        ]).
-export([is_dirty/1]).
-export([quantity/3
        ,diff_quantities/1, diff_quantities/2
        ,diff_quantity/3
        ,have_quantities_changed/1
        ]).
-export([updated_quantity/3]).
-export([category_quantity/2, category_quantity/3]).
-export([cascade_quantity/3, cascade_quantities/1]).
-export([cascade_category_quantity/2, cascade_category_quantity/3]).
-export([reset_category/2]).
-export([get_service_module/1]).

-export([is_reseller/1]).
-export([get_reseller_id/1]).

-export([dry_run/1]).

-include("kazoo_services.hrl").

-define(STATUS_GOOD, kzd_services:status_good()).
-define(QUANTITIES_ACCOUNT, <<"account_quantities">>).
-define(QUANTITIES_CASCADE, <<"cascade_quantities">>).
-define(PLANS, <<"plans">>).
-define(SERVICE_MODULE_PREFIX, "kz_service_").
-define(SERVICE_MODULES, application:get_env(?APP, 'service_modules', default_service_modules())).

-record(kz_services, {account_id :: api_binary()
                     ,billing_id :: api_binary()
                     ,current_billing_id :: api_binary()
                     ,new_billing_id = 'false' :: boolean()
                     ,dirty = 'false' :: boolean()
                     ,deleted = 'false' :: boolean()
                     ,status = ?STATUS_GOOD :: ne_binary()
                     ,jobj = kz_json:new() :: kz_json:object()
                     ,updates = kz_json:new() :: kz_json:object()
                     ,cascade_quantities = kz_json:new() :: kz_json:object()
                     }).
-define(BASE_BACKOFF, 50).

-type services() :: #kz_services{}.
-type bookkeeper() :: 'kz_bookkeeper_braintree' |
                      'kz_bookkeeper_local' |
                      'kz_bookkeeper_http'.
-export_type([services/0
             ,bookkeeper/0
             ]).

%%%===================================================================
%%% Operations
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> services().
-spec new(ne_binary()) -> services().
new() ->
    #kz_services{}.

new(<<_/binary>> = AccountId) ->
    AccountJObj = get_account_definition(AccountId),

    JObj = base_service_object(AccountId, AccountJObj),

    BillingId = case ?SUPPORT_BILLING_ID of
                    'true' -> kzd_services:billing_id(JObj);
                    'false' -> AccountId
                end,
    IsReseller = kzd_services:is_reseller(JObj),

    #kz_services{account_id = AccountId
                ,jobj = JObj
                ,cascade_quantities = cascade_quantities(AccountId, IsReseller)
                ,dirty = 'true'
                ,billing_id = BillingId
                ,current_billing_id = BillingId
                ,deleted = kz_doc:is_soft_deleted(AccountJObj)
                }.

-spec base_service_object(ne_binary(), kz_json:object()) -> kzd_services:doc().
base_service_object(AccountId, AccountJObj) ->
    ResellerId = get_reseller_id(AccountId),
    BaseJObj = kz_doc:update_pvt_parameters(kz_json:new()
                                           ,kz_util:format_account_id(AccountId, 'encoded')
                                           ,[{'account_id', AccountId}
                                            ,{'crossbar_doc_vsn', <<"1">>}
                                            ,{'id', AccountId}
                                            ,{'type', kzd_services:type()}
                                            ]
                                           ),

    lists:foldl(fun({F, V}, J) -> F(J, V) end
               ,BaseJObj
               ,[{fun kzd_services:set_status/2, ?STATUS_GOOD}
                ,{fun kzd_services:set_is_reseller/2, depreciated_is_reseller(AccountJObj)}
                ,{fun kzd_services:set_reseller_id/2, ResellerId}
                ,{fun kzd_services:set_tree/2, kz_account:tree(AccountJObj)}
                ,{fun kzd_services:set_billing_id/2, depreciated_billing_id(AccountJObj)}
                ,{fun kzd_services:set_quantities/2, kz_json:new()}
                ,{fun kzd_services:set_plans/2, populate_service_plans(AccountJObj, ResellerId)}
                ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec from_service_json(kz_json:object()) -> services().
-spec from_service_json(kz_json:object(), boolean()) -> services().
from_service_json(JObj) ->
    from_service_json(JObj, 'true').

from_service_json(JObj, CalcUpdates) ->
    AccountId = kz_doc:account_id(JObj),
    BillingId = case ?SUPPORT_BILLING_ID of
                    'true' -> kzd_services:billing_id(JObj);
                    'false' -> AccountId
                end,
    Services = #kz_services{account_id = AccountId
                           ,jobj = JObj
                           ,status = kzd_services:status(JObj)
                           ,billing_id = BillingId
                           ,current_billing_id = BillingId
                           ,deleted = kz_doc:is_soft_deleted(JObj)
                           ,dirty = kzd_services:is_dirty(JObj)
                           },
    maybe_calc_updates(Services, CalcUpdates).

maybe_calc_updates(Services, 'true') ->
    Qs = cascade_quantities(account_id(Services), is_reseller(Services)),
    Services#kz_services{cascade_quantities = Qs};
maybe_calc_updates(Services, 'false') ->
    Services.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> services().
fetch(<<_/binary>> = Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),

    case fetch_cached_services(AccountId) of
        {'ok', Services} -> Services;
        {'error', 'not_found'} ->
            fetch_and_build(AccountId)
    end.

-spec fetch_and_build(ne_binary()) -> services().
fetch_and_build(AccountId) ->
    case fetch_services_doc(AccountId) of
        {'ok', JObj} ->
            Services = handle_fetch_result(AccountId, JObj),
            cache_services(AccountId, Services),
            Services;
        {'error', _R} ->
            lager:debug("unable to open account ~s services doc (creating new): ~p", [AccountId, _R]),
            new(AccountId)
    end.

-spec fetch_cached_services(ne_binary()) ->
                                   {'ok', services()} |
                                   {'error', 'not_found'}.
fetch_cached_services(AccountId) ->
    kz_cache:fetch_local(?CACHE_NAME, services_cache_key(AccountId)).

-spec cache_services(ne_binary(), services()) -> 'ok'.
cache_services(AccountId, Services) ->
    kz_cache:store_local(?CACHE_NAME
                        ,services_cache_key(AccountId)
                        ,Services
                        ,[{'origin', [{'db', ?KZ_SERVICES_DB, AccountId}]}]
                        ).

-spec flush_services() -> 'ok'.
flush_services() ->
    kz_cache:flush_local(?CACHE_NAME).

-spec services_cache_key(AccountId :: ne_binary()) ->
                                {?MODULE, ne_binary()}.
services_cache_key(AccountId) ->
    {?MODULE, AccountId}.

-spec fetch_services_doc(ne_binary()) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
-spec fetch_services_doc(ne_binary(), boolean()) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
fetch_services_doc(AccountId) ->
    %% TODO: if reseller populate cascade via merchant id
    fetch_services_doc(AccountId, 'false').

fetch_services_doc(AccountId, 'false') ->
    kz_datamgr:open_cache_doc(?KZ_SERVICES_DB, AccountId);
fetch_services_doc(AccountId, 'true') ->
    kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId).

-spec handle_fetch_result(ne_binary(), kz_json:object()) -> services().
handle_fetch_result(AccountId, JObj) ->
    lager:debug("loaded account service doc ~s", [AccountId]),
    IsReseller = kzd_services:is_reseller(JObj),
    BillingId = case ?SUPPORT_BILLING_ID of
                    'true' -> kzd_services:billing_id(JObj);
                    'false' -> AccountId
                end,
    #kz_services{account_id = AccountId
                ,jobj = JObj
                ,cascade_quantities = cascade_quantities(AccountId, IsReseller)
                ,status = kzd_services:status(JObj)
                ,billing_id = BillingId
                ,current_billing_id = BillingId
                ,deleted = kz_doc:is_soft_deleted(JObj)
                ,dirty = kzd_services:is_dirty(JObj)
                }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_service_plan(ne_binary(), services()) -> services().
add_service_plan(PlanId, #kz_services{jobj=JObj}=Services) ->
    ResellerId = kzd_services:reseller_id(JObj),
    Services#kz_services{jobj = kz_service_plans:add_service_plan(PlanId, ResellerId, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_service_plan(ne_binary(), services()) -> services().
delete_service_plan(PlanId, #kz_services{jobj=JObj}=Services) ->
    Services#kz_services{jobj = kz_service_plans:delete_service_plan(PlanId, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save_as_dirty(ne_binary() | services()) -> services().
-spec save_as_dirty(ne_binary() | services(), pos_integer()) -> services().
save_as_dirty(Account=?NE_BINARY) ->
    save_as_dirty(fetch(Account));
save_as_dirty(#kz_services{}=Services) ->
    save_as_dirty(Services, ?BASE_BACKOFF).

save_as_dirty(#kz_services{jobj = JObj
                          ,updates = _Updates
                          ,account_id = <<_/binary>> = AccountId
                          }=Services
             ,BackOff
             ) ->
    UpdatedJObj =
        lists:foldl(fun({F, V}, J) -> F(J, V) end
                   ,JObj
                   ,[{fun kz_doc:set_id/2, AccountId}
                    ,{fun kzd_services:set_is_dirty/2, 'true'}
                    ,{fun kz_doc:set_modified/2, kz_time:current_tstamp()}
                    ]
                   ),
    case kz_datamgr:save_doc(?KZ_SERVICES_DB, UpdatedJObj) of
        {'ok', SavedJObj} ->
            lager:debug("marked services as dirty for account ~s", [AccountId]),
            from_service_json(SavedJObj);
        {'error', 'not_found'} ->
            lager:debug("service database does not exist, attempting to create"),
            'true' = kz_datamgr:db_create(?KZ_SERVICES_DB),
            timer:sleep(BackOff),
            save_as_dirty(Services, BackOff);
        {'error', 'conflict'} ->
            lager:debug("conflict when saving, attempting mitigation"),
            save_conflicting_as_dirty(Services, BackOff)
    end.

-spec save_conflicting_as_dirty(services(), pos_integer()) -> services().
save_conflicting_as_dirty(#kz_services{account_id=AccountId}, BackOff) ->
    {'ok', Existing} = fetch_services_doc(AccountId, 'true'),
    NewServices = from_service_json(Existing),
    case is_dirty(NewServices) of
        'true' ->
            lager:debug("services doc for ~s saved elsewhere", [AccountId]),
            NewServices;
        'false' ->
            lager:debug("new services doc for ~s not dirty, marking it as so", [AccountId]),
            timer:sleep(BackOff + rand:uniform(?BASE_BACKOFF)),
            save_as_dirty(NewServices, BackOff*2)
    end.

-spec account_name(ne_binary()) -> api_binary().
account_name(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> kz_account:name(JObj);
        {'error', _} -> 'undefined'
    end.

-spec save(services()) -> services().
-spec save(services(), pos_integer()) -> services().
save(#kz_services{}=Services) ->
    save(Services, ?BASE_BACKOFF).

save(#kz_services{jobj = JObj
                 ,updates = UpdatedQuantities
                 ,account_id = AccountId
                 ,dirty = ForceDirty
                 }=Services
    ,BackOff
    ) ->
    CurrentQuantities = kzd_services:quantities(JObj),

    Dirty = have_quantities_changed(UpdatedQuantities, CurrentQuantities)
        orelse ForceDirty,

    Props = [{fun kz_doc:set_id/2, AccountId}
            ,{fun kzd_services:set_is_dirty/2, Dirty}
            ,{fun kz_doc:set_modified/2, kz_time:current_tstamp()}
            ,{fun kzd_services:set_quantities/2, kz_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ],
    UpdatedJObj = kz_json:set_values(props:filter_undefined(Props), JObj),

    case kz_datamgr:save_doc(?KZ_SERVICES_DB, UpdatedJObj) of
        {'ok', NewJObj} ->
            lager:debug("saved services for ~s: ~s", [AccountId, kz_json:encode(kzd_services:quantities(NewJObj))]),
            IsReseller = kzd_services:is_reseller(NewJObj),
            _ = maybe_clean_old_billing_id(Services),
            BillingId = case ?SUPPORT_BILLING_ID of
                            'true' -> kzd_services:billing_id(JObj);
                            'false' -> AccountId
                        end,
            Services#kz_services{jobj = NewJObj
                                ,cascade_quantities = cascade_quantities(AccountId, IsReseller)
                                ,status = kzd_services:status(NewJObj)
                                ,billing_id = BillingId
                                ,current_billing_id = BillingId
                                ,deleted = kz_doc:is_soft_deleted(NewJObj)
                                };
        {'error', 'not_found'} ->
            lager:debug("service database does not exist, attempting to create"),
            'true' = kz_datamgr:db_create(?KZ_SERVICES_DB),
            timer:sleep(BackOff),
            save(Services, BackOff);
        {'error', 'conflict'} ->
            lager:debug("services for ~s conflicted, merging changes and retrying", [AccountId]),
            timer:sleep(BackOff + rand:uniform(?BASE_BACKOFF)),
            {'ok', Existing} = kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId),
            save(Services#kz_services{jobj=Existing}, BackOff*2)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(ne_binary()) -> kz_std_return().
delete(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    %% TODO: support other bookkeepers, and just cancel subscriptions....
    _ = (catch braintree_customer:delete(AccountId)),
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId) of
        {'ok', JObj} ->
            lager:debug("marking services for account ~s as deleted", [AccountId]),
            kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values([{<<"pvt_deleted">>, 'true'}
                                                                    ,{<<"pvt_dirty">>, 'true'}
                                                                    ]
                                                                   ,JObj
                                                                   ));
        {'error', 'not_found'} -> {'ok', kz_json:new()};
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
-spec list_categories(services()) -> api_binaries().
list_categories(#kz_services{jobj=JObj
                            ,updates=Updates
                            ,cascade_quantities=CascadeQuantities}) ->
    Set = sets:union([
                      sets:from_list(kz_json:get_keys(kzd_services:quantities(JObj, kz_json:new())))
                     ,sets:from_list(kz_json:get_keys(Updates))
                     ,sets:from_list(kz_json:get_keys(CascadeQuantities))
                     ]),
    sets:to_list(Set).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec list_items(services(), ne_binary()) -> api_binaries().
list_items(#kz_services{jobj=JObj
                       ,updates=Updates
                       ,cascade_quantities=CascadeQuantities}, Category) ->
    Set = sets:union([
                      sets:from_list(kz_json:get_keys(kzd_services:category_quantities(JObj, Category, kz_json:new())))
                     ,sets:from_list(kz_json:get_keys(Category, Updates))
                     ,sets:from_list(kz_json:get_keys(Category, CascadeQuantities))
                     ]),
    sets:to_list(Set).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_billing_id(api_binary(), ne_binary() | services()) -> 'undefined' | services().
set_billing_id('undefined', _) -> 'undefined';
set_billing_id(BillingId, #kz_services{billing_id=BillingId}) ->
    'undefined';
set_billing_id(BillingId, #kz_services{account_id=BillingId
                                      ,jobj=ServicesJObj
                                      }=Services) ->
    Services#kz_services{jobj=kzd_services:set_billing_id(ServicesJObj, BillingId)
                        ,billing_id=BillingId
                        ,dirty='true'
                        };
set_billing_id(BillingId, #kz_services{jobj=ServicesJObj}=Services) ->
    PvtTree = kz_account:tree(ServicesJObj, [BillingId]),
    try lists:last(PvtTree) of
        BillingId ->
            Services#kz_services{jobj=kzd_services:set_billing_id(ServicesJObj, BillingId)
                                ,billing_id=BillingId
                                ,dirty='true'
                                };
        _Else ->
            throw({'invalid_billing_id', <<"Requested billing id is not the parent of this account">>})
    catch
        {'EXIT', _} ->
            throw({'invalid_billing_id', <<"Unable to determine if billing id is valid">>})
    end;
set_billing_id(BillingId, <<_/binary>> = AccountId) ->
    set_billing_id(BillingId, fetch(AccountId)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_billing_id(ne_binary() | services()) -> ne_binary().
get_billing_id(#kz_services{billing_id=BillingId}) -> BillingId;
get_billing_id(<<_/binary>> = Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    case fetch_services_doc(AccountId) of
        {'error', _R} ->
            lager:debug("unable to open account ~s services: ~p", [AccountId, _R]),
            AccountId;
        {'ok', ServicesJObj} ->
            case kzd_services:billing_id(ServicesJObj, AccountId) of
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
update(CategoryId, ItemId, Quantity, Services) when not is_integer(Quantity) ->
    update(CategoryId, ItemId, kz_term:to_integer(Quantity), Services);
update(CategoryId, ItemId, Quantity, #kz_services{updates=JObj}=Services)
  when is_binary(CategoryId),
       is_binary(ItemId) ->
    lager:debug("setting ~s.~s to ~p in updates", [CategoryId, ItemId, Quantity]),
    Services#kz_services{updates=kz_json:set_value([CategoryId, ItemId], Quantity, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec activation_charges(ne_binary(), ne_binary(), services() | ne_binary() | kz_service_plans:plans()) ->
                                number().
activation_charges(CategoryId, ItemId, #kz_services{jobj=ServicesJObj}) ->
    Plans = kz_service_plans:from_service_json(ServicesJObj),
    kz_service_plans:activation_charges(CategoryId, ItemId, Plans);
activation_charges(CategoryId, ItemId, Plans)
  when is_list(Plans) ->
    kz_service_plans:activation_charges(CategoryId, ItemId, Plans);
activation_charges(CategoryId, ItemId, Account=?NE_BINARY) ->
    activation_charges(CategoryId, ItemId, fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec commit_transactions(services(), kz_transactions:kz_transactions()) -> atom().
commit_transactions(#kz_services{billing_id=BillingId}, Activations) ->
    Bookkeeper = select_bookkeeper(BillingId),
    Transactions = [Activation
                    || Activation <- Activations
                           ,kz_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:commit_transactions(BillingId, Transactions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec charge_transactions(services(), kz_transactions:kz_transactions()) -> kz_json:objects().
charge_transactions(#kz_services{billing_id=BillingId}, Activations) ->
    Bookkeeper = select_bookkeeper(BillingId),
    Transactions = [kz_transaction:to_json(Activation)
                    || Activation <- Activations
                           ,kz_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:charge_transactions(BillingId, Transactions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec select_bookkeeper(services() | ne_binary()) -> bookkeeper().
select_bookkeeper(#kz_services{billing_id=BillingId}) ->
    select_bookkeeper(BillingId);
select_bookkeeper(BillingId) ->
    ResellerId = get_reseller_id(BillingId),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    case ResellerId =/= MasterAccountId of
        'true' -> 'kz_bookkeeper_local';
        'false' -> ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_bookkeeper(ne_binary(), integer()) -> boolean().
check_bookkeeper(BillingId, Amount) ->
    case select_bookkeeper(BillingId) of
        'kz_bookkeeper_local' ->
            case wht_util:current_balance(BillingId) of
                {'ok', Balance} -> Balance - Amount >= 0;
                {'error', _} -> false
            end;
        Bookkeeper ->
            CurrentStatus = current_service_status(BillingId),
            Bookkeeper:is_good_standing(BillingId, CurrentStatus)
    end.

-spec current_service_status(ne_binary()) -> ne_binary().
current_service_status(AccountId) ->
    {'ok', ServicesJObj} = fetch_services_doc(AccountId),
    kzd_services:status(ServicesJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec service_plan_json(ne_binary() | services()) -> kzd_service_plan:doc().
service_plan_json(#kz_services{jobj=ServicesJObj}) ->
    Plans = kz_service_plans:from_service_json(ServicesJObj),
    kz_service_plans:public_json(Plans);
service_plan_json(<<_/binary>> = Account) ->
    service_plan_json(fetch(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec public_json(ne_binary() | services()) -> kz_json:object().
public_json(#kz_services{jobj=ServicesJObj
                        ,cascade_quantities=CascadeQuantities
                        }) ->
    AccountId = kz_doc:account_id(ServicesJObj),
    InGoodStanding = try maybe_follow_billing_id(AccountId, ServicesJObj) of
                         'true' -> 'true'
                     catch
                         'throw':_ -> 'false'
                     end,
    Props = [{?QUANTITIES_ACCOUNT, kzd_services:quantities(ServicesJObj)}
            ,{?QUANTITIES_CASCADE, CascadeQuantities}
            ,{?PLANS, kz_service_plans:plan_summary(ServicesJObj)}
            ,{<<"billing_id">>, kzd_services:billing_id(ServicesJObj, AccountId)}
            ,{<<"reseller">>, kzd_services:is_reseller(ServicesJObj)}
            ,{<<"reseller_id">>, kzd_services:reseller_id(ServicesJObj)}
            ,{<<"dirty">>, kzd_services:is_dirty(ServicesJObj)}
            ,{<<"in_good_standing">>, InGoodStanding}
            ,{<<"items">>, kz_service_plans:public_json_items(ServicesJObj)}
            ],
    kz_json:from_list(Props);
public_json(<<_/binary>> = Account) ->
    public_json(fetch(Account)).

-spec to_json(services()) -> kz_json:object().
to_json(#kz_services{jobj=JObj
                    ,updates=UpdatedQuantities
                    ,cascade_quantities=CascadeQuantities
                    }
       ) ->
    CurrentQuantities = kzd_services:quantities(JObj),
    Props = [{fun kzd_services:set_quantities/2, kz_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ,{<<"cascade_quantities">>, CascadeQuantities}],
    kz_json:set_values(props:filter_undefined(Props), JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_reseller_id(api_binary()) -> api_binary().
find_reseller_id('undefined') ->
    case kapps_util:get_master_account_id() of
        {'error', _} -> 'undefined';
        {'ok', MasterAccountId} -> MasterAccountId
    end;
find_reseller_id(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case fetch_services_doc(AccountId) of
        {'ok', JObj} ->
            case kzd_services:reseller_id(JObj) of
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
-spec allow_updates(ne_binary() | services()) -> 'true'.
allow_updates(<<_/binary>> = Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case fetch_services_doc(AccountId) of
        {'error', _R} ->
            lager:debug("can't determine if account ~s can make updates: ~p", [AccountId, _R]),
            default_maybe_allow_updates(AccountId);
        {'ok', ServicesJObj} ->
            lager:debug("determining if account ~s is able to make updates", [AccountId]),
            maybe_follow_billing_id(AccountId, ServicesJObj)
    end;
allow_updates(#kz_services{jobj=ServicesJObj
                          ,account_id=AccountId
                          }) ->
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    maybe_follow_billing_id(AccountId, ServicesJObj).

-spec maybe_follow_billing_id(ne_binary(), kz_json:object()) -> 'true'.
maybe_follow_billing_id(AccountId, ServicesJObj) ->
    case kzd_services:billing_id(ServicesJObj, AccountId) of
        AccountId -> maybe_allow_updates(AccountId, ServicesJObj);
        BillingId ->
            lager:debug("following billing id ~s", [BillingId]),
            allow_updates(BillingId)
    end.

-spec maybe_allow_updates(ne_binary(), kz_json:object()) -> 'true'.
maybe_allow_updates(AccountId, ServicesJObj) ->
    StatusGood = ?STATUS_GOOD,
    Plans = kz_service_plans:plan_summary(ServicesJObj),
    case kz_term:is_empty(Plans)
        orelse kzd_services:status(ServicesJObj)
    of
        'true' ->
            lager:debug("allowing request for account ~s with no service plans"
                       ,[AccountId]
                       ),
            'true';
        StatusGood ->
            lager:debug("allowing request for account in good standing"),
            'true';
        Status ->
            lager:debug("checking local bookkeeper for account ~s in status ~s"
                       ,[AccountId, Status]
                       ),
            maybe_bookkeeper_allow_updates(AccountId, Status)
    end.

-spec maybe_bookkeeper_allow_updates(ne_binary(), ne_binary()) -> 'true'.
maybe_bookkeeper_allow_updates(AccountId, Status) ->
    Bookkeeper = select_bookkeeper(AccountId),
    case Bookkeeper:is_good_standing(AccountId, Status) of
        'true' -> spawn_move_to_good_standing(AccountId);
        'false' ->
            lager:debug("denying update request for services ~s due to status ~s", [AccountId, Status]),
            Reason = io_lib:format("Unable to continue due to billing account ~s status is ~s", [AccountId, Status]),
            Error = kz_json:from_list([{<<"error">>, <<"bad_status">>}
                                      ,{<<"message">>, kz_term:to_binary(Reason)}
                                      ]),
            throw({<<"account_billing_invalid">>, Error})
    end.

-spec default_maybe_allow_updates(ne_binary()) -> 'true'.
default_maybe_allow_updates(AccountId) ->
    case kapps_config:get_is_true(?WHS_CONFIG_CAT, <<"default_allow_updates">>, 'true') of
        'true' -> 'true';
        'false' ->
            lager:debug("denying update request, ~s.default_allow_updates is false", [?WHS_CONFIG_CAT]),
            Reason = io_lib:format("Service updates are disallowed by default for billing account ~s", [AccountId]),
            Error = kz_json:from_list([{<<"error">>, <<"updates_disallowed">>}
                                      ,{<<"message">>, kz_term:to_binary(Reason)}
                                      ]),
            throw({<<"account_billing_invalid">>, Error})
    end.

-spec spawn_move_to_good_standing(ne_binary()) -> 'true'.
spawn_move_to_good_standing(<<_/binary>> = AccountId) ->
    _ = kz_util:spawn(fun move_to_good_standing/1, [AccountId]),
    'true'.

-spec move_to_good_standing(ne_binary()) -> services().
move_to_good_standing(<<_/binary>> = AccountId) ->
    #kz_services{jobj=JObj}=Services = fetch(AccountId),
    lager:debug("moving account ~s services to good standing", [AccountId]),
    save(Services#kz_services{jobj = kzd_services:set_status(JObj, ?STATUS_GOOD)}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_only(api_ne_binary() | services()) -> 'false' | services().
-spec reconcile(api_ne_binary() | services()) -> 'false' | services().

reconcile_only('undefined') -> 'false';
reconcile_only(Account=?NE_BINARY) ->
    reconcile_only(fetch(Account));
reconcile_only(#kz_services{account_id=AccountId}=Services) ->
    lager:debug("reconcile all services for ~s", [AccountId]),
    Modules = get_service_modules(),
    lists:foldl(fun reconcile_module/2, Services, Modules).

-spec reconcile_module(atom(), services()) -> services().
reconcile_module(M, Services) ->
    M:reconcile(Services).

reconcile('undefined') -> 'false';
reconcile(<<_/binary>> = Account) ->
    save(reconcile_only(Account));
reconcile(#kz_services{}=Services) ->
    save(reconcile_only(Services)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reconcile_only(api_binary() | services(), text()) -> 'false' | services().
reconcile_only('undefined', _Module) -> 'false';
reconcile_only(<<_/binary>> = Account, Module) ->
    reconcile_only(fetch(Account), Module);
reconcile_only(#kz_services{account_id=AccountId}=CurrentServices, Module) ->
    lager:debug("reconcile ~s services for ~s", [Module, AccountId]),
    case get_service_module(Module) of
        'false' -> 'false';
        ServiceModule ->
            ServiceModule:reconcile(CurrentServices)
    end.

-spec reconcile(api_binary() | services(), text()) -> 'false' | services().
reconcile('undefined', _Module) -> 'false';
reconcile(<<_/binary>> = Account, Module) ->
    timer:sleep(?MILLISECONDS_IN_SECOND),
    maybe_save(reconcile_only(Account, Module));
reconcile(#kz_services{}=Services, Module) ->
    timer:sleep(?MILLISECONDS_IN_SECOND),
    maybe_save(reconcile_only(Services, Module)).

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
account_id(#kz_services{account_id='undefined'
                       ,jobj=JObj
                       }) ->
    lager:debug("services has no account id, looking in ~s", [kz_json:encode(JObj)]),
    kz_doc:account_id(JObj);
account_id(#kz_services{account_id=AccountId}) ->
    AccountId.

-spec services_json(services()) -> kzd_services:doc().
services_json(#kz_services{jobj=JObj}) ->
    JObj.

-spec is_dirty(services()) -> boolean().
is_dirty(#kz_services{dirty=IsDirty}) ->
    kz_term:is_true(IsDirty).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec quantity(ne_binary(), ne_binary(), services()) -> integer().
quantity(_, _, #kz_services{deleted='true'}) -> 0;
quantity(CategoryId, ItemId, #kz_services{updates=Updates
                                         ,jobj=JObj
                                         }) ->
    ItemQuantity = kzd_services:item_quantity(JObj, CategoryId, ItemId),
    kz_json:get_integer_value([CategoryId, ItemId], Updates, ItemQuantity).

-spec diff_quantities(services()) -> api_object().
diff_quantities(#kz_services{deleted='true'}) -> 'undefined';
diff_quantities(#kz_services{jobj=JObj
                            ,updates=Updates
                            }) ->
    kz_json:foldl(fun diff_cat_quantities/3, Updates, kzd_services:quantities(JObj)).

-spec diff_cat_quantities(ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
diff_cat_quantities(CategoryId, ItemsJObj, Updates) ->
    kz_json:foldl(fun(I, Q, Acc) ->
                          diff_item_quantities(I, Q, Acc, CategoryId)
                  end
                 ,Updates
                 ,ItemsJObj
                 ).

-spec diff_quantities(ne_binary(), services()) -> api_object().
diff_quantities(_CategoryId, #kz_services{deleted='true'}) -> 'undefined';
diff_quantities(CategoryId, #kz_services{jobj=JObj
                                        ,updates=Updates
                                        }) ->
    CatQuantities = kzd_services:category_quantities(JObj, CategoryId),
    UpdateQuantities = kz_json:get_value(CategoryId, Updates, kz_json:new()),
    kz_json:foldl(fun(Id, Q, Acc) ->
                          diff_item_quantities(Id, Q, Acc, CategoryId)
                  end
                 ,UpdateQuantities
                 ,CatQuantities
                 ).

-spec diff_item_quantities(ne_binary(), integer(), kz_json:object(), ne_binary()) ->
                                  kz_json:object().
diff_item_quantities(ItemId, ItemQuantity, Updates, CategoryId) ->
    UpdateQuantity = kz_json:get_integer_value([CategoryId, ItemId], Updates),
    maybe_update_diff([CategoryId, ItemId], ItemQuantity, UpdateQuantity, Updates).

maybe_update_diff(_Key, _ItemQuantity, 'undefined', Updates) ->
    lager:debug("no update for ~p", [_Key]),
    Updates;
maybe_update_diff(_Key, 0, 0, Updates) ->
    lager:debug("not updating ~p", [_Key]),
    Updates;
maybe_update_diff(Key, ItemQuantity, UpdateQuantity, Updates) ->
    lager:debug("updating ~p from ~p to ~p", [Key, ItemQuantity, UpdateQuantity]),
    kz_json:set_value(Key, UpdateQuantity - ItemQuantity, Updates).

-spec diff_quantity(ne_binary(), ne_binary(), services()) -> integer().
diff_quantity(_, _, #kz_services{deleted='true'}) -> 0;
diff_quantity(CategoryId, ItemId, #kz_services{jobj=JObj
                                              ,updates=Updates
                                              }) ->
    ItemQuantity = kzd_services:item_quantity(JObj, CategoryId, ItemId),
    UpdateQuantity = kz_json:get_integer_value([CategoryId, ItemId], Updates, 0),
    UpdateQuantity - ItemQuantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec updated_quantity(ne_binary(), ne_binary(), services()) -> integer().
updated_quantity(_, _, #kz_services{deleted='true'}) -> 0;
updated_quantity(CategoryId, ItemId, #kz_services{updates=JObj}) ->
    kz_json:get_integer_value([CategoryId, ItemId], JObj, 0).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec category_quantity(ne_binary(), services()) -> integer().
-spec category_quantity(ne_binary(), ne_binaries(), services()) -> integer().
category_quantity(CategoryId, Services) ->
    category_quantity(CategoryId, [], Services).

category_quantity(_CategoryId, _ItemExceptions, #kz_services{deleted='true'}) -> 0;
category_quantity(CategoryId, ItemExceptions, #kz_services{updates=UpdatedQuantities
                                                          ,jobj=JObj
                                                          }) ->
    CatQuantities = kzd_services:category_quantities(JObj, CategoryId),
    CatUpdates = kz_json:get_value(CategoryId, UpdatedQuantities, kz_json:new()),

    %% replaces CatQs values with CatUpdate
    Quantities = kz_json:merge_recursive(CatQuantities, CatUpdates),

    %% Removes ItemExceptions, if any
    QsMinusEx = kz_json:delete_keys(ItemExceptions, Quantities),

    kz_json:foldl(fun(_ItemId, ItemQuantity, Sum) ->
                          ItemQuantity + Sum
                  end
                 ,0
                 ,QsMinusEx
                 ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantity(ne_binary(), ne_binary(), services()) -> integer().
cascade_quantity(_, _, #kz_services{deleted='true'}) -> 0;
cascade_quantity(CategoryId, ItemId, #kz_services{cascade_quantities=JObj}=Services) ->
    kz_json:get_integer_value([CategoryId, ItemId], JObj, 0)
        + quantity(CategoryId, ItemId, Services).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_category_quantity(ne_binary(), services()) -> integer().
-spec cascade_category_quantity(ne_binary(), ne_binaries(), services()) -> integer().
cascade_category_quantity(CategoryId, Services) ->
    cascade_category_quantity(CategoryId, [], Services).

cascade_category_quantity(_, _, #kz_services{deleted='true'}) -> 0;
cascade_category_quantity(CategoryId, ItemExceptions, #kz_services{cascade_quantities=Quantities}=Services) ->
    CatQuantiies = kz_json:get_value(CategoryId, Quantities, kz_json:new()),
    QtysMinusEx = kz_json:delete_keys(ItemExceptions, CatQuantiies),

    kz_json:foldl(fun(_ItemId, ItemQuantity, Sum) ->
                          ItemQuantity + Sum
                  end
                 ,category_quantity(CategoryId, ItemExceptions, Services)
                 ,QtysMinusEx
                 ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_category(ne_binary(), services()) -> services().
reset_category(CategoryId, #kz_services{updates=JObj}=Services) ->
    Services#kz_services{updates=kz_json:set_value(CategoryId, kz_json:new(), JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% Helper function to know if an account is a reseller or not.
%% @end
%%--------------------------------------------------------------------
-spec is_reseller(ne_binary() | services() | kz_json:object()) -> boolean().
is_reseller(#kz_services{jobj=ServicesJObj}) ->
    kzd_services:is_reseller(ServicesJObj);
is_reseller(<<_/binary>> = Account) ->
    case fetch_services_doc(Account) of
        {'ok', ServicesJObj} -> kzd_services:is_reseller(ServicesJObj);
        _ -> 'false'
    end;
is_reseller(ServicesJObj) ->
    kzd_services:is_reseller(ServicesJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec dry_run(services()) -> kz_json:object().
dry_run(Services) ->
    ActivationsCharges = dry_run_activation_charges(Services),
    calculate_charges(Services, ActivationsCharges).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_charges(services(), kz_json:objects()) -> kz_json:object().
calculate_charges(Services, JObjs) ->
    case calculate_services_charges(Services) of
        {'no_plan', _NP} -> kz_json:new();
        {'ok', PlansCharges} ->
            calculate_transactions_charges(PlansCharges, JObjs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec calculate_services_charges(services()) ->
                                        {'no_plan' | 'ok', kz_json:object()}.
-spec calculate_services_charges(services(), kz_service_plans:plans()) ->
                                        {'error' | 'ok', kz_json:object()}.
calculate_services_charges(#kz_services{jobj=ServiceJObj}=Services) ->
    case kz_service_plans:from_service_json(ServiceJObj) of
        [] -> {'no_plan', kz_json:new()};
        ServicePlans ->
            calculate_services_charges(Services, ServicePlans)
    end.

calculate_services_charges(#kz_services{jobj=ServiceJObj
                                       ,updates=UpdatesJObj
                                       }=Service
                          ,ServicePlans
                          ) ->
    CurrentQuantities = kzd_services:quantities(ServiceJObj),
    UpdatedQuantities = kz_json:merge_jobjs(UpdatesJObj, CurrentQuantities),

    UpdatedServiceJObj = kzd_services:set_quantities(ServiceJObj, UpdatedQuantities),

    ExistingItems = kz_service_plans:create_items(ServiceJObj, ServicePlans),
    UpdatedItems = kz_service_plans:create_items(UpdatedServiceJObj, ServicePlans),
    Changed = kz_service_items:get_updated_items(UpdatedItems, ExistingItems),

    lager:debug("current items: ~s", [kz_json:encode(kz_service_items:public_json(ExistingItems))]),
    lager:debug("items after update: ~s", [kz_json:encode(kz_service_items:public_json(UpdatedItems))]),
    lager:debug("service diff quantities: ~s", [kz_json:encode(diff_quantities(Service))]),

    lager:debug("computed service charges"),
    {'ok', kz_service_items:public_json(Changed)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec calculate_transactions_charges(kz_json:object(), kz_json:objects()) ->
                                            kz_json:object().
calculate_transactions_charges(PlansCharges, JObjs) ->
    F = fun calculate_transactions_charge_fold/2,
    lists:foldl(F, PlansCharges, JObjs).

-spec calculate_transactions_charge_fold(kz_json:object(), kz_json:object()) ->
                                                kz_json:object().
calculate_transactions_charge_fold(JObj, PlanCharges) ->
    Amount = kz_json:get_value(<<"amount">>, JObj, 0),
    Quantity = kz_json:get_value(<<"activate_quantity">>, JObj, 0),
    SubTotal = kz_json:get_value(<<"activation_charges">>, PlanCharges, 0),

    case SubTotal + (Amount * Quantity) of
        Zero when Zero == 0 ->
            %% Works for 0.0 and 0. May compare to a threshold though…
            PlanCharges;
        Total ->
            CategoryId = kz_json:get_value(<<"category">>, JObj),
            ItemId = kz_json:get_value(<<"item">>, JObj),
            Props = [{<<"activation_charges">>, Total}
                    ,{[CategoryId, ItemId, <<"activation_charges">>], Amount}
                    ,{[CategoryId, ItemId, <<"activate_quantity">>], Quantity}
                    ],
            kz_json:set_values(Props, PlanCharges)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run_activation_charges(services()) -> kz_json:objects().
-spec dry_run_activation_charges(ne_binary(), kz_json:object()
                                ,services(), kz_json:objects()
                                ) -> kz_json:objects().
-spec dry_run_activation_charges(ne_binary(), ne_binary()
                                ,integer(), services()
                                ,kz_json:objects()
                                ) -> kz_json:objects().
dry_run_activation_charges(#kz_services{updates=Updates}=Services) ->
    kz_json:foldl(fun(CategoryId, CategoryJObj, Acc) ->
                          dry_run_activation_charges(CategoryId, CategoryJObj, Services, Acc)
                  end
                 ,[]
                 ,Updates
                 ).

dry_run_activation_charges(CategoryId, CategoryJObj, Services, JObjs) ->
    kz_json:foldl(fun(ItemId, Quantity, Acc1) ->
                          dry_run_activation_charges(CategoryId, ItemId, Quantity, Services, Acc1)
                  end
                 ,JObjs
                 ,CategoryJObj
                 ).

dry_run_activation_charges(CategoryId, ItemId, Quantity, #kz_services{jobj=JObj}=Services, JObjs) ->
    case kzd_services:item_quantity(JObj, CategoryId, ItemId) of
        Quantity -> JObjs;
        OldQuantity ->
            Plans = kz_service_plans:from_service_json(to_json(Services)),
            Charges = activation_charges(CategoryId, ItemId, Plans),
            ServicePlan = kz_service_plans:public_json(Plans),
            ItemPlan = get_item_plan(CategoryId, ItemId, ServicePlan),
            [kz_json:from_list([{<<"category">>, CategoryId}
                               ,{<<"item">>, kzd_item_plan:masquerade_as(ItemPlan, ItemId)}
                               ,{<<"amount">>, Charges}
                               ,{<<"quantity">>, Quantity}
                               ,{<<"activate_quantity">>, Quantity - OldQuantity}
                               ])
             | JObjs
            ]
    end.

-spec get_item_plan(ne_binary(), ne_binary(), kzd_service_plan:doc()) -> kz_json:object().
get_item_plan(CategoryId, ItemId, ServicePlan) ->
    case kzd_service_plan:item_plan(ServicePlan, CategoryId, ItemId, 'undefined') of
        'undefined' -> kzd_service_plan:category_plan(ServicePlan, CategoryId);
        Plan -> Plan
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_modules() -> atoms().
get_service_modules() ->
    case kapps_config:get(?WHS_CONFIG_CAT, <<"modules">>) of
        'undefined' -> ?SERVICE_MODULES;
        ConfModules ->
            lager:debug("configured service modules: ~p", [ConfModules]),
            [kz_term:to_atom(Mod, 'true') || Mod <- ConfModules]
    end.

-spec default_service_modules() -> atoms().
default_service_modules() ->
    ['kz_service_devices'
    ,'kz_service_ips'
    ,'kz_service_ledgers'
    ,'kz_service_limits'
    ,'kz_service_phone_numbers'
    ,'kz_service_ui_apps'
    ,'kz_service_users'
    ,'kz_service_whitelabel'
    ,'kz_service_billing'
    ,'kz_service_ratedeck'
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_service_module(text()) -> atom() | 'false'.
get_service_module(Module) when not is_binary(Module) ->
    get_service_module(kz_term:to_binary(Module));
get_service_module(<<?SERVICE_MODULE_PREFIX, _/binary>> = Module) ->
    ServiceModules = get_service_modules(),
    case [M
          || M <- ServiceModules,
             kz_term:to_binary(M) =:= Module
         ]
    of
        [M] -> M;
        _Else -> 'false'
    end;
get_service_module(Module) ->
    get_service_module(<<?SERVICE_MODULE_PREFIX, Module/binary>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cascade_quantities(services()) -> kz_json:object().
-spec cascade_quantities(ne_binary(), boolean()) -> kz_json:object().

cascade_quantities(#kz_services{cascade_quantities=JObj}) ->
    JObj.

cascade_quantities(<<_/binary>> = Account, 'false') ->
    lager:debug("computing cascade quantities"),
    do_cascade_quantities(Account, <<"services/cascade_quantities">>);
cascade_quantities(<<_/binary>> = Account, 'true') ->
    lager:debug("computing reseller cascade quantities"),
    do_cascade_quantities(Account, <<"services/reseller_quantities">>).

-spec do_cascade_quantities(ne_binary(), ne_binary()) -> kz_json:object().
do_cascade_quantities(<<_/binary>> = Account, <<_/binary>> = View) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ViewOptions = ['group'
                  ,'reduce'
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_SERVICES_DB, View, ViewOptions) of
        {'error', _} -> kz_json:new();
        {'ok', JObjs} ->
            lists:foldl(fun do_cascade_quantities_fold/2, kz_json:new(), JObjs)
    end.

-spec do_cascade_quantities_fold(kz_json:object(), kz_json:object()) ->
                                        kz_json:object().
-spec do_cascade_quantities_fold(kz_json:object(), kz_json:object(), kz_json:path()) ->
                                        kz_json:object().
do_cascade_quantities_fold(JObj, J) ->
    do_cascade_quantities_fold(JObj, J, kz_json:get_value(<<"key">>, JObj)).

do_cascade_quantities_fold(JObj, J, [_|Keys]) ->
    Value = kz_json:get_integer_value(<<"value">>, JObj),
    lager:debug("setting cascade quantity ~p: ~p", [Keys, Value]),
    kz_json:set_value(Keys, Value, J).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the billing id as it is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_billing_id(kz_json:object()) -> ne_binary().
depreciated_billing_id(JObj) ->
    AccountId = kz_doc:account_id(JObj),
    case ?SUPPORT_BILLING_ID of
        'true' -> kzd_services:billing_id(JObj, AccountId);
        'false' -> AccountId
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine if pvt_reseller is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec depreciated_is_reseller(kz_json:object()) -> boolean().
depreciated_is_reseller(JObj) ->
    kzd_services:is_reseller(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine what service plans are currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%--------------------------------------------------------------------
-spec populate_service_plans(kz_json:object(), ne_binary()) -> kz_json:object().
populate_service_plans(JObj, ResellerId) ->
    Plans = incorporate_default_service_plan(ResellerId, master_default_service_plan()),
    incorporate_depreciated_service_plans(Plans, JObj).

-spec default_service_plan_id(ne_binary()) -> api_binary().
default_service_plan_id(ResellerId) ->
    case kz_datamgr:open_doc(?KZ_SERVICES_DB, ResellerId) of
        {'ok', JObj} -> kz_json:get_value(<<"default_service_plan">>, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s services: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec depreciated_default_service_plan_id(ne_binary()) -> api_binary().
depreciated_default_service_plan_id(ResellerId) ->
    ResellerDb = kz_util:format_account_id(ResellerId, 'encoded'),
    case kz_datamgr:open_doc(ResellerDb, ResellerId) of
        {'ok', JObj} -> kz_json:get_value(<<"default_service_plan">>, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s account definition: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec master_default_service_plan() -> kz_json:object().
master_default_service_plan() ->
    case kapps_util:get_master_account_id() of
        {'error', _} -> kz_json:new();
        {'ok', MasterAccountId} ->
            incorporate_default_service_plan(MasterAccountId, kz_json:new())
    end.

-spec incorporate_default_service_plan(ne_binary(), kz_json:object()) -> kz_json:object().
incorporate_default_service_plan(ResellerId, JObj) ->
    case depreciated_default_service_plan_id(ResellerId) of
        'undefined' ->
            incorporate_only_default_service_plan(ResellerId, JObj);
        PlanId ->
            maybe_augment_with_plan(ResellerId, JObj, PlanId)
    end.

-spec incorporate_only_default_service_plan(ne_binary(), kz_json:object()) -> kz_json:object().
incorporate_only_default_service_plan(ResellerId, JObj) ->
    maybe_augment_with_plan(ResellerId, JObj, default_service_plan_id(ResellerId)).

maybe_augment_with_plan(_ResellerId, JObj, 'undefined') -> JObj;
maybe_augment_with_plan(ResellerId, JObj, PlanId) ->
    Plan = kz_json:from_list([{<<"account_id">>, ResellerId}]),
    kz_json:set_value(PlanId, Plan, JObj).

-spec incorporate_depreciated_service_plans(kz_json:object(), kz_json:object()) -> kz_json:object().
incorporate_depreciated_service_plans(Plans, JObj) ->
    PlanIds = kz_json:get_value(<<"pvt_service_plans">>, JObj),
    ResellerId = kzd_services:reseller_id(JObj),
    case kz_term:is_empty(PlanIds)
        orelse kz_term:is_empty(ResellerId)
    of
        'true' -> Plans;
        'false' ->
            lists:foldl(fun(PlanId, Ps) ->
                                maybe_augment_with_plan(ResellerId, Ps, PlanId)
                        end
                       ,Plans
                       ,PlanIds
                       )
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_reseller_id(ne_binaries() | ne_binary()) -> ne_binary().
get_reseller_id([]) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    MasterAccountId;
get_reseller_id([Parent|Ancestors]) ->
    case kz_datamgr:open_cache_doc(?KZ_SERVICES_DB, Parent, ['cache_failures']) of
        {'error', _R} ->
            lager:debug("failed to open services doc ~s durning reseller search: ~p", [Parent, _R]),
            get_reseller_id(Ancestors);
        {'ok', JObj} ->
            get_reseller_id(Parent, Ancestors, JObj)
    end;
get_reseller_id(<<_/binary>> = Account) ->
    case kz_account:fetch(Account) of
        {'ok', AccountJObj} ->
            get_reseller_id(lists:reverse(kz_account:tree(AccountJObj)));
        {'error', _R} ->
            %%            lager:info("unable to open account definition for ~s: ~p", [Account, _R]),
            get_reseller_id([])
    end.

-spec get_reseller_id(ne_binary(), ne_binaries(), kz_json:object()) -> api_binary().
get_reseller_id(Parent, Ancestors, JObj) ->
    case kzd_services:is_reseller(JObj) of
        'false' -> get_reseller_id(Ancestors);
        'true' -> Parent
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_save('false' | services()) -> 'false' |
                                          services() |
                                          {'error', 'no_change'}.
maybe_save('false') -> 'false';
maybe_save(#kz_services{jobj=JObj
                       ,updates=UpdatedQuantities
                       }=Services) ->
    CurrentQuantities = kzd_services:quantities(JObj),
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
-spec have_quantities_changed(services()) -> boolean().
-spec have_quantities_changed(kz_json:object(), kz_json:object()) -> boolean().
have_quantities_changed(#kz_services{jobj=JObj
                                    ,updates=UpdatedQuantities
                                    }) ->
    CurrentQuantities = kzd_services:quantities(JObj),
    have_quantities_changed(UpdatedQuantities, CurrentQuantities).

have_quantities_changed(Updated, Current) ->
    KeyNotSameFun = fun(Key) ->
                            kz_json:get_value(Key, Updated) =/= kz_json:get_value(Key, Current)
                    end,

    any_changed(KeyNotSameFun, Updated)
        orelse any_changed(KeyNotSameFun, Current).

-type changed_fun() :: fun((kz_json:path()) -> boolean()).

-spec any_changed(changed_fun(), kz_json:object()) -> boolean().
any_changed(KeyNotSameFun, Quantities) ->
    lists:any(KeyNotSameFun
             ,[[CategoryId, ItemId]
               || CategoryId <- kz_json:get_keys(Quantities),
                  ItemId <- kz_json:get_keys(CategoryId, Quantities)
              ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_account_definition(ne_binary()) -> kz_account:doc().
get_account_definition(<<_/binary>> = AccountId) ->
    case kz_account:fetch(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get account defintion for ~s: ~p", [AccountId, _R]),
            kz_json:new();
        {'ok', JObj} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_clean_old_billing_id(services()) -> services().
maybe_clean_old_billing_id(#kz_services{billing_id=BillingId
                                       ,current_billing_id=BillingId
                                       }=Services) ->
    Services;
maybe_clean_old_billing_id(#kz_services{current_billing_id=BillingId
                                       ,account_id=BillingId
                                       ,jobj=JObj
                                       }=Services) ->
    case kzd_services:is_reseller(JObj) of
        'true' -> Services;
        'false' ->
            _ = kz_service_sync:clean(BillingId),
            Services
    end;
maybe_clean_old_billing_id(#kz_services{}=Services) ->
    Services.
