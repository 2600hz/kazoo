%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
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
-export([fetch/1
        ,fetch_services_doc/1, fetch_services_doc/2
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
-export([get_billing_id/1, get_billing_id/2]).
-export([find_reseller_id/1]).

-export([account_id/1
        ,services_json/1
        ]).
-export([is_dirty/1]).
-export([quantity/3
        ,diff_quantities/1
        ,diff_quantity/3
        ,have_quantities_changed/1
        ]).
-export([updated_quantity/3]).
-export([category_quantity/2, category_quantity/3]).
-export([cascade_quantity/3, cascade_quantities/1]).
-export([cascade_category_quantity/2, cascade_category_quantity/3]).
-export([reset_category/2]).

-export([is_reseller/1]).
-export([get_reseller_id/1]).

-export([dry_run/1]).

-export([clean/1
        ,sync/1, sync/2
        ,mark_dirty/1
        ]).

-export([is_services/1]).

-include("services.hrl").

-define(QUANTITIES_ACCOUNT, <<"account_quantities">>).
-define(QUANTITIES_CASCADE, <<"cascade_quantities">>).
-define(PLANS, <<"plans">>).
-define(DEFAULT_PLAN, <<"default_service_plan">>).
-define(SERVICE_MODULE_PREFIX, "kz_service_").
-define(SERVICE_MODULES
       ,application:get_env(?APP, 'service_modules', default_service_modules())
       ).

-record(kz_services, {account_id :: kz_term:api_binary()
                     ,billing_id :: kz_term:api_binary()
                     ,current_billing_id :: kz_term:api_binary()
                     ,dirty = 'false' :: boolean()
                     ,deleted = 'false' :: boolean()
                     ,status = kzd_services:status_good() :: kz_term:ne_binary()
                     ,jobj = kz_json:new() :: kz_json:object()
                     ,updates = kz_json:new() :: kz_json:object()
                     ,cascade_quantities = kz_json:new() :: kz_json:object()
                     }).

-define(BASE_BACKOFF, 50).

-define(CACHE_KEY(AccountId), {?MODULE, AccountId}).

-type services() :: #kz_services{}.

-type bookkeeper() :: 'kz_bookkeeper_braintree' |
                      'kz_bookkeeper_local' |
                      'kz_bookkeeper_http'.

-export_type([services/0
             ,bookkeeper/0
             ]).

-define(SHOULD_ALLOW_UPDATES
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"default_allow_updates">>, 'true')
       ).

-define(DEFAULT_SERVICE_MODULES
       ,kapps_config:get_ne_binaries(?CONFIG_CAT, <<"modules">>)
       ).


-ifdef(TEST).
-export([current_billing_id/1]).
-export([is_deleted/1]).
-export([status/1]).
-export([get_service_module/1]).
-export([get_service_modules/0]).
-endif.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> services().
new() ->
    #kz_services{}.

-spec new(kz_term:ne_binary()) -> services().
new(?MATCH_ACCOUNT_RAW(AccountId)) ->
    AccountJObj = get_account_definition(AccountId),
    JObj = base_service_object(AccountId, AccountJObj),
    BillingId = depreciated_billing_id(JObj, AccountId),
    IsReseller = kzd_services:is_reseller(JObj),
    #kz_services{account_id = AccountId
                ,jobj = JObj
                ,cascade_quantities = cascade_quantities(AccountId, IsReseller)
                ,dirty = 'true'
                ,billing_id = BillingId
                ,current_billing_id = BillingId
                ,deleted = kz_doc:is_soft_deleted(AccountJObj)
                }.

-spec base_service_object(kz_term:ne_binary(), kz_json:object()) -> kzd_services:doc().
base_service_object(AccountId, AccountJObj) ->
    ResellerId = get_reseller_id(AccountId),
    BaseJObj = kz_doc:update_pvt_parameters(kz_json:new()
                                           ,kz_util:format_account_db(AccountId)
                                           ,[{'account_id', AccountId}
                                            ,{'crossbar_doc_vsn', <<"1">>}
                                            ,{'id', AccountId}
                                            ,{'type', kzd_services:type()}
                                            ]
                                           ),

    lists:foldl(fun({F, V}, J) -> F(J, V) end
               ,BaseJObj
               ,[{fun kzd_services:set_status/2, kzd_services:status_good()}
                ,{fun kzd_services:set_is_reseller/2, depreciated_is_reseller(AccountJObj)}
                ,{fun kzd_services:set_reseller_id/2, ResellerId}
                ,{fun kzd_services:set_tree/2, kzd_accounts:tree(AccountJObj)}
                ,{fun kzd_services:set_billing_id/2, depreciated_billing_id(AccountJObj)}
                ,{fun kzd_services:set_quantities/2, kz_json:new()}
                ,{fun kzd_services:set_plans/2, populate_service_plans(AccountJObj, ResellerId)}
                ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec from_service_json(kz_json:object()) -> services().
from_service_json(JObj) ->
    from_service_json(JObj, 'true').

-spec from_service_json(kz_json:object(), boolean()) -> services().
from_service_json(JObj, CalcUpdates) ->
    AccountId = kz_doc:account_id(JObj),
    BillingId = depreciated_billing_id(JObj, AccountId),
    Services = #kz_services{account_id = AccountId
                           ,jobj = JObj
                           ,status = kzd_services:status(JObj)
                           ,billing_id = BillingId
                           ,current_billing_id = BillingId
                           ,deleted = kz_doc:is_soft_deleted(JObj)
                           ,dirty = kzd_services:is_dirty(JObj)
                           },
    maybe_calc_updates(Services, CalcUpdates).

maybe_calc_updates(Services, 'false') -> Services;
maybe_calc_updates(Services, 'true') ->
    Qs = cascade_quantities(account_id(Services), is_reseller(Services)),
    Services#kz_services{cascade_quantities = Qs}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary()) -> services().
fetch(Account=?NE_BINARY) ->
    AccountId = kz_util:format_account_id(Account),
    case fetch_cached_services(AccountId) of
        {'ok', Services} -> Services;
        {'error', 'not_found'} ->
            fetch_and_build(AccountId)
    end.

-spec fetch_and_build(kz_term:ne_binary()) -> services().
fetch_and_build(AccountId) ->
    case fetch_services_doc(AccountId) of
        {'ok', JObj} ->
            Services = handle_fetch_result(AccountId, JObj),
            cache_services(AccountId, Services),
            Services;
        {'error', _R} ->
            ?LOG_DEBUG("unable to open account ~s services doc (creating new): ~p", [AccountId, _R]),
            new(AccountId)
    end.

-spec fetch_cached_services(kz_term:ne_binary()) -> {'ok', services()} |
                                                    {'error', 'not_found'}.
fetch_cached_services(?MATCH_ACCOUNT_RAW(AccountId)) ->
    kz_cache:fetch_local(?CACHE_NAME, ?CACHE_KEY(AccountId)).

-spec cache_services(kz_term:ne_binary(), services()) -> 'ok'.
-ifdef(TEST).
cache_services(?MATCH_ACCOUNT_RAW(_), #kz_services{}) -> 'ok'.
-else.
cache_services(AccountId, Services) ->
    Options = [{'origin', [{'db', ?KZ_SERVICES_DB, AccountId}]}],
    kz_cache:store_local(?CACHE_NAME, ?CACHE_KEY(AccountId), Services, Options).
-endif.

-spec flush_services() -> 'ok'.
flush_services() ->
    kz_cache:flush_local(?CACHE_NAME).

-spec fetch_services_doc(kz_term:ne_binary()) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
fetch_services_doc(?MATCH_ACCOUNT_RAW(AccountId)) ->
    %% TODO: if reseller populate cascade via merchant id
    fetch_services_doc(AccountId, 'false').

-ifdef(TEST).

-spec fetch_services_doc(kz_term:ne_binary(), boolean() | cache_failures) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
fetch_services_doc(?A_MASTER_ACCOUNT_ID, _NotFromCache)
  when is_boolean(_NotFromCache); _NotFromCache =:= cache_failures ->
    kz_json:fixture(?APP, "a_master_services.json");
fetch_services_doc(?A_RESELLER_ACCOUNT_ID, _NotFromCache)
  when is_boolean(_NotFromCache); _NotFromCache =:= cache_failures ->
    kz_json:fixture(?APP, "a_reseller_services.json");
fetch_services_doc(?A_SUB_ACCOUNT_ID, _NotFromCache)
  when is_boolean(_NotFromCache); _NotFromCache =:= cache_failures ->
    kz_json:fixture(?APP, "a_sub_services.json");
fetch_services_doc(?B_SUB_ACCOUNT_ID, _NotFromCache)
  when is_boolean(_NotFromCache); _NotFromCache =:= cache_failures ->
    {ok,ServicesJObj0} = kz_json:fixture(?APP, "a_sub_services.json"),
    ServicesJObj1 = kzd_services:set_is_dirty(ServicesJObj0, true),
    ServicesJObj2 = kzd_services:set_reseller_id(ServicesJObj1, undefined),
    {ok, kz_doc:set_id(ServicesJObj2, ?B_SUB_ACCOUNT_ID)};
fetch_services_doc(?UNRELATED_ACCOUNT_ID, _NotFromCache)
  when is_boolean(_NotFromCache); _NotFromCache =:= cache_failures ->
    {error, not_found};
fetch_services_doc(?WRONG_ACCOUNT_ID, _NotFromCache)
  when is_boolean(_NotFromCache); _NotFromCache =:= cache_failures ->
    {error, wrong};
fetch_services_doc(?MATCH_ACCOUNT_RAW(AccountId), _NotFromCache) ->
    try kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId) of
        {ok, _}=OK -> OK;
        {error, _} ->
            ?LOG_DEBUG("~n~n NO SERVICE DOC FOR AccountId: ~p~n~n", [AccountId]),
            kz_util:log_stacktrace(),
            {error, wrong}
            %% Not throwing since this is needed for one of the kapps_account_config test
            %% {error, _}=Error ->
            %% throw(Error)
    catch %% for tests that did not start kazoo_fixturedb
        error:badarg ->
            {error, wrong}
    end.
-else.

-spec fetch_services_doc(kz_term:ne_binary(), boolean() | cache_failures) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
fetch_services_doc(?MATCH_ACCOUNT_RAW(AccountId), cache_failures=Option) ->
    kz_datamgr:open_cache_doc(?KZ_SERVICES_DB, AccountId, [Option]);
fetch_services_doc(?MATCH_ACCOUNT_RAW(AccountId), 'false') ->
    kz_datamgr:open_cache_doc(?KZ_SERVICES_DB, AccountId);
fetch_services_doc(?MATCH_ACCOUNT_RAW(AccountId), 'true') ->
    kz_datamgr:open_doc(?KZ_SERVICES_DB, AccountId).
-endif.

-spec handle_fetch_result(kz_term:ne_binary(), kz_json:object()) -> services().
handle_fetch_result(AccountId, JObj) ->
    lager:debug("loaded account service doc ~s", [AccountId]),
    IsReseller = kzd_services:is_reseller(JObj),
    BillingId = depreciated_billing_id(JObj, AccountId),
    #kz_services{account_id = AccountId
                ,jobj = JObj
                ,cascade_quantities = cascade_quantities(AccountId, IsReseller)
                ,status = kzd_services:status(JObj)
                ,billing_id = BillingId
                ,current_billing_id = BillingId
                ,deleted = kz_doc:is_soft_deleted(JObj)
                ,dirty = kzd_services:is_dirty(JObj)
                }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_service_plan(kz_term:ne_binary(), services()) -> services().
add_service_plan(PlanId, #kz_services{jobj = JObj}=Services) ->
    ResellerId = kzd_services:reseller_id(JObj),
    UpdatedJObj = kz_service_plans:add_service_plan(PlanId, ResellerId, JObj),
    Services#kz_services{jobj = UpdatedJObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_service_plan(kz_term:ne_binary(), services()) -> services().
delete_service_plan(PlanId, #kz_services{jobj = JObj}=Services) ->
    Services#kz_services{jobj = kz_service_plans:delete_service_plan(PlanId, JObj)
                        }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save_as_dirty(kz_term:ne_binary() | services()) -> services().
save_as_dirty(Account=?NE_BINARY) ->
    save_as_dirty(fetch(Account));
save_as_dirty(#kz_services{}=Services) ->
    save_as_dirty(Services, ?BASE_BACKOFF).

-spec save_as_dirty(kz_term:ne_binary() | services(), pos_integer()) -> services().
save_as_dirty(#kz_services{jobj = JObj
                           %% ,updates = _Updates
                          ,account_id = ?MATCH_ACCOUNT_RAW(AccountId)
                          }=Services
             ,BackOff
             ) ->
    Updates = [{fun kz_doc:set_id/2, AccountId}
              ,{fun kzd_services:set_is_dirty/2, 'true'}
              ,{fun kz_doc:set_modified/2, kz_time:now_s()}
              ],
    UpdatedJObj = lists:foldl(fun({F, V}, J) -> F(J, V) end, JObj, Updates),
    case save_doc(UpdatedJObj) of
        {'ok', SavedJObj} ->
            lager:debug("marked services as dirty for account ~s", [AccountId]),
            from_service_json(SavedJObj);
        {'error', 'not_found'} ->
            lager:debug("service database does not exist, attempting to create"),
            'true' = kz_datamgr:db_create(?KZ_SERVICES_DB),
            timer:sleep(BackOff),
            save_as_dirty(Services, BackOff);
        {'error', 'conflict'} ->
            ?LOG_DEBUG("conflict when saving, attempting mitigation"),
            save_conflicting_as_dirty(Services, BackOff)
    end.

-spec save_conflicting_as_dirty(services(), pos_integer()) -> services().
save_conflicting_as_dirty(#kz_services{account_id = AccountId}, BackOff) ->
    {'ok', Existing} = fetch_services_doc(AccountId, 'true'),
    NewServices = from_service_json(Existing),
    case is_dirty(NewServices) of
        'true' ->
            ?LOG_DEBUG("services doc for ~s saved elsewhere", [AccountId]),
            NewServices;
        'false' ->
            ?LOG_DEBUG("new services doc for ~s not dirty, marking it as so", [AccountId]),
            timer:sleep(BackOff + rand:uniform(?BASE_BACKOFF)),
            save_as_dirty(NewServices, BackOff*2)
    end.

-spec save(services()) -> services().
save(#kz_services{}=Services) ->
    save(Services, ?BASE_BACKOFF).

-spec save(services(), pos_integer()) -> services().
save(#kz_services{jobj = JObj
                 ,updates = UpdatedQuantities
                 ,account_id = AccountId
                 ,dirty = ForceDirty
                 }=Services
    ,BackOff
    ) ->
    CurrentQuantities = kzd_services:quantities(JObj),
    Dirty = have_quantities_changed(UpdatedQuantities, CurrentQuantities) or ForceDirty,

    Props = [{fun kz_doc:set_id/2, AccountId}
            ,{fun kzd_services:set_is_dirty/2, Dirty}
            ,{fun kz_doc:set_modified/2, kz_time:now_s()}
            ,{fun kzd_services:set_quantities/2, kz_json:merge_jobjs(UpdatedQuantities, CurrentQuantities)}
            ],

    UpdatedJObj = kz_json:set_values(props:filter_undefined(Props), JObj),
    case save_doc(UpdatedJObj) of
        {'ok', NewJObj} ->
            ?LOG_DEBUG("saved services for ~s: ~s"
                      ,[AccountId, kz_json:encode(kzd_services:quantities(NewJObj))]
                      ),
            IsReseller = kzd_services:is_reseller(NewJObj),
            _ = maybe_clean_old_billing_id(Services),
            BillingId = depreciated_billing_id(JObj, AccountId),
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
            {ok, Existing} = fetch_services_doc(AccountId, true),
            save(Services#kz_services{jobj = Existing}, 2 * BackOff)
    end.

-ifdef(TEST).
save_doc(JObj) ->
    true = kz_json:is_json_object(JObj),
    case kz_doc:id(JObj) of
        ?B_SUB_ACCOUNT_ID -> {error, conflict};
        _Else -> {ok, JObj}
    end.
-else.
save_doc(JObj) ->
    kz_datamgr:save_doc(?KZ_SERVICES_DB, JObj).
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_term:ne_binary()) -> kz_term:std_return().
delete(Account) ->
    AccountId = kz_util:format_account_id(Account),
    %% TODO: support other bookkeepers, and just cancel subscriptions....
    _ = braintree:is_configured()
        andalso (catch braintree_customer:delete(AccountId)),
    case fetch_services_doc(AccountId, true) of
        {'ok', JObj} ->
            lager:debug("marking services for account ~s as deleted", [AccountId]),
            Values = [{?SERVICES_PVT_IS_DELETED, 'true'}
                     ,{?SERVICES_PVT_IS_DIRTY, 'true'}
                     ],
            save_doc(kz_json:set_values(Values, JObj));
        {'error', 'not_found'} -> {'ok', kz_json:new()};
        {'error', _R}=E ->
            lager:debug("unable to mark service plan ~s as deleted: ~p", [AccountId, _R]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec list_categories(services()) -> kz_term:api_ne_binaries().
list_categories(#kz_services{jobj = JObj
                            ,updates = Updates
                            ,cascade_quantities = CascadeQuantities
                            }) ->
    sets:to_list(
      sets:union([sets:from_list(kz_json:get_keys(kzd_services:quantities(JObj)))
                 ,sets:from_list(kz_json:get_keys(Updates))
                 ,sets:from_list(kz_json:get_keys(CascadeQuantities))
                 ])).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec list_items(services(), kz_term:ne_binary()) -> kz_term:api_ne_binaries().
list_items(#kz_services{jobj = JObj
                       ,updates = Updates
                       ,cascade_quantities = CascadeQuantities
                       }
          ,Category
          ) ->
    sets:to_list(
      sets:union([sets:from_list(kz_json:get_keys(kzd_services:category_quantities(JObj, Category)))
                 ,sets:from_list(kz_json:get_keys(Category, Updates))
                 ,sets:from_list(kz_json:get_keys(Category, CascadeQuantities))
                 ])).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_billing_id(kz_term:api_binary(), kz_term:ne_binary() | services()) -> 'undefined' | services().
set_billing_id('undefined', _) -> 'undefined';
set_billing_id(BillingId, #kz_services{billing_id = BillingId}) ->
    'undefined';
set_billing_id(BillingId, #kz_services{account_id = BillingId
                                      ,jobj = ServicesJObj
                                      }=Services) ->
    Services#kz_services{jobj = kzd_services:set_billing_id(ServicesJObj, BillingId)
                        ,billing_id = BillingId
                        ,dirty = 'true'
                        };
set_billing_id(BillingId, #kz_services{jobj = ServicesJObj
                                      }=Services) ->
    PvtTree = kzd_accounts:tree(ServicesJObj, [BillingId]),
    try lists:last(PvtTree) of
        BillingId ->
            Services#kz_services{jobj = kzd_services:set_billing_id(ServicesJObj, BillingId)
                                ,billing_id = BillingId
                                ,dirty = 'true'
                                };
        _Else ->
            throw({'invalid_billing_id', <<"Requested billing id is not the parent of this account">>})
    catch
        {'EXIT', _} ->
            throw({'invalid_billing_id', <<"Unable to determine if billing id is valid">>})
    end;
set_billing_id(BillingId, AccountId=?NE_BINARY) ->
    set_billing_id(BillingId, fetch(AccountId)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_billing_id(kz_term:ne_binary() | services()) -> kz_term:ne_binary().
get_billing_id(#kz_services{billing_id = BillingId}) -> BillingId;
get_billing_id(Account=?NE_BINARY) ->
    AccountId = kz_util:format_account_id(Account),
    ?LOG_DEBUG("determining if account ~s is able to make updates", [AccountId]),
    case fetch_services_doc(AccountId) of
        {'error', _R} ->
            ?LOG_DEBUG("unable to open account ~s services: ~p", [AccountId, _R]),
            AccountId;
        {'ok', ServicesJObj} ->
            case kzd_services:billing_id(ServicesJObj, AccountId) of
                AccountId -> AccountId;
                BillingId ->
                    ?LOG_DEBUG("following billing id ~s", [BillingId]),
                    get_billing_id(BillingId)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), kz_term:ne_binary(), integer(), services()) -> services().
update(CategoryId, ItemId, Quantity, Services) when not is_integer(Quantity) ->
    update(CategoryId, ItemId, kz_term:to_integer(Quantity), Services);
update(CategoryId, ItemId, Quantity, #kz_services{updates = JObj
                                                 }=Services)
  when is_binary(CategoryId),
       is_binary(ItemId) ->
    lager:debug("setting ~s.~s to ~p in updates", [CategoryId, ItemId, Quantity]),
    Services#kz_services{updates = kz_json:set_value([CategoryId, ItemId], Quantity, JObj)
                        }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec activation_charges(kz_term:ne_binary(), kz_term:ne_binary(), services() | kz_term:ne_binary() | kz_service_plans:plans()) ->
                                float().
activation_charges(CategoryId, ItemId, Plans)
  when is_list(Plans) ->
    kz_service_plans:activation_charges(CategoryId, ItemId, Plans);
activation_charges(CategoryId, ItemId, #kz_services{jobj = ServicesJObj}) ->
    Plans = kz_service_plans:from_service_json(ServicesJObj),
    activation_charges(CategoryId, ItemId, Plans);
activation_charges(CategoryId, ItemId, Account=?NE_BINARY) ->
    Services = fetch(Account),
    activation_charges(CategoryId, ItemId, Services).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec commit_transactions(services(), kz_transactions:kz_transactions()) -> ok | error.
commit_transactions(#kz_services{billing_id = BillingId}=Services, Activations) ->
    Bookkeeper = select_bookkeeper(Services),
    Transactions = [Activation
                    || Activation <- Activations,
                       kz_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:commit_transactions(BillingId, Transactions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec charge_transactions(services(), kz_transactions:kz_transactions()) -> kz_json:objects().
charge_transactions(#kz_services{billing_id = BillingId}=Services, Activations) ->
    Bookkeeper = select_bookkeeper(Services),
    Transactions = [kz_transaction:to_json(Activation)
                    || Activation <- Activations,
                       kz_transaction:amount(Activation) > 0
                   ],
    Bookkeeper:charge_transactions(BillingId, Transactions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec select_bookkeeper(services() | kz_term:ne_binary()) -> bookkeeper().
select_bookkeeper(#kz_services{billing_id = BillingId
                              ,account_id = AccountId
                              }
                 ) ->
    BillingIdReseller = get_reseller_id(BillingId),
    {'ok', MasterAccountId} = master_account_id(),
    case BillingIdReseller =:= MasterAccountId of
        true -> ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER;
        false ->
            case BillingIdReseller =:= get_reseller_id(AccountId) of
                'true' -> select_bookkeeper(AccountId);
                'false' -> 'kz_bookkeeper_local'
            end
    end;
select_bookkeeper(AccountId) ->
    ResellerId = get_reseller_id(AccountId),
    {'ok', MasterAccountId} = master_account_id(),
    case ResellerId =:= MasterAccountId of
        true -> ?KZ_SERVICE_MASTER_ACCOUNT_BOOKKEEPER;
        false ->
            case ?MAYBE_RESELLER_BOOKKEEPER_LOOKUP of
                'true' -> ?KZ_LOOKUP_BOOKKEEPER(ResellerId);
                'false' -> 'kz_bookkeeper_local'
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_bookkeeper(kz_term:ne_binary(), integer()) -> boolean().
check_bookkeeper(BillingId, Amount) ->
    case select_bookkeeper(BillingId) of
        'kz_bookkeeper_local' ->
            case current_balance(BillingId) of
                {'ok', Balance} -> Balance - Amount >= 0;
                {'error', _R} ->
                    ?LOG_DEBUG("error checking local bookkeeper balance: ~p", [_R]),
                    false
            end;
        Bookkeeper ->
            CurrentStatus = current_service_status(BillingId),
            Bookkeeper:is_good_standing(BillingId, CurrentStatus)
    end.

-spec current_service_status(kz_term:ne_binary()) -> kz_term:ne_binary().
current_service_status(AccountId) ->
    {'ok', ServicesJObj} = fetch_services_doc(AccountId),
    kzd_services:status(ServicesJObj).

-ifdef(TEST).
current_balance(?UNRELATED_ACCOUNT_ID) -> {ok, 100}.
-else.
current_balance(AccountId) ->
    wht_util:current_balance(AccountId).
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec service_plan_json(kz_term:ne_binary() | services()) -> kzd_service_plan:doc().
service_plan_json(#kz_services{jobj = ServicesJObj}) ->
    Plans = kz_service_plans:from_service_json(ServicesJObj),
    kz_service_plans:public_json(Plans);
service_plan_json(Account=?NE_BINARY) ->
    service_plan_json(fetch(Account)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(kz_term:ne_binary() | services()) -> kz_json:object().
public_json(#kz_services{jobj = ServicesJObj
                        ,cascade_quantities = CascadeQuantities
                        }) ->
    AccountId = kz_doc:account_id(ServicesJObj),
    InGoodStanding = try maybe_follow_billing_id(AccountId, ServicesJObj)
                     catch 'throw':_ -> 'false'
                     end,
    kz_json:from_list(
      [{?QUANTITIES_ACCOUNT, kzd_services:quantities(ServicesJObj)}
      ,{?QUANTITIES_CASCADE, CascadeQuantities}
      ,{?PLANS, kz_service_plans:plan_summary(ServicesJObj)}
      ,{<<"billing_id">>, kzd_services:billing_id(ServicesJObj, AccountId)}
      ,{<<"reseller">>, kzd_services:is_reseller(ServicesJObj)}
      ,{<<"reseller_id">>, kzd_services:reseller_id(ServicesJObj)}
      ,{<<"dirty">>, kzd_services:is_dirty(ServicesJObj)}
      ,{<<"in_good_standing">>, InGoodStanding}
      ,{<<"items">>, kz_service_plans:public_json_items(ServicesJObj)}
      ]);
public_json(Account=?NE_BINARY) ->
    public_json(fetch(Account)).

-spec to_json(services()) -> kz_json:object().
to_json(#kz_services{jobj = JObj
                    ,updates = UpdatedQuantities
                    ,cascade_quantities = CascadeQuantities
                    }
       ) ->
    NewQuantities = kz_json:merge_jobjs(UpdatedQuantities, kzd_services:quantities(JObj)),
    Props = props:filter_undefined(
              [{fun kzd_services:set_quantities/2, NewQuantities}
              ,{<<"cascade_quantities">>, CascadeQuantities}
              ]),
    kz_json:set_values(Props, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_reseller_id(kz_term:api_binary()) -> kz_term:api_binary().
find_reseller_id('undefined') ->
    case master_account_id() of
        {'error', _} -> 'undefined';
        {'ok', MasterAccountId} -> MasterAccountId
    end;
find_reseller_id(Account) ->
    AccountId = kz_util:format_account_id(Account),
    case fetch_services_doc(AccountId) of
        {'ok', JObj} ->
            case kzd_services:reseller_id(JObj) of
                'undefined' -> get_reseller_id(Account);
                ResellerId -> ResellerId
            end;
        {'error', _} -> get_reseller_id(Account)
    end.

-ifdef(TEST).
master_account_id() -> {ok, ?A_MASTER_ACCOUNT_ID}.
-else.
master_account_id() -> kapps_util:get_master_account_id().
-endif.

%%%=============================================================================
%%% Services functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Throws an error if the billing account is not in "good_standing",
%% used when update requests are made to kill them if there are
%% accounting issues.
%% @end
%%------------------------------------------------------------------------------
-spec allow_updates(kz_term:ne_binary() | services()) -> 'true'.
allow_updates(Account=?NE_BINARY) ->
    AccountId = kz_util:format_account_id(Account),
    case fetch_services_doc(AccountId) of
        {'error', _R} ->
            lager:debug("can't determine if account ~s can make updates: ~p", [AccountId, _R]),
            default_maybe_allow_updates(AccountId);
        {'ok', ServicesJObj} ->
            lager:debug("determining if account ~s is able to make updates", [AccountId]),
            maybe_follow_billing_id(AccountId, ServicesJObj)
    end;
allow_updates(#kz_services{jobj = ServicesJObj
                          ,account_id = AccountId
                          }) ->
    lager:debug("determining if account ~s is able to make updates", [AccountId]),
    maybe_follow_billing_id(AccountId, ServicesJObj).

-spec maybe_follow_billing_id(kz_term:ne_binary(), kz_json:object()) -> 'true'.
maybe_follow_billing_id(AccountId, ServicesJObj) ->
    case kzd_services:billing_id(ServicesJObj, AccountId) of
        AccountId -> maybe_allow_updates(AccountId, ServicesJObj);
        BillingId ->
            lager:debug("following billing id ~s", [BillingId]),
            allow_updates(BillingId)
    end.

-spec maybe_allow_updates(kz_term:ne_binary(), kz_json:object()) -> 'true'.
maybe_allow_updates(AccountId, ServicesJObj) ->
    StatusGood = kzd_services:status_good(),
    case kz_term:is_empty(kz_service_plans:plan_summary(ServicesJObj))
        orelse kzd_services:status(ServicesJObj)
    of
        'true' ->
            lager:debug("allowing request for account ~s with no service plans", [AccountId]),
            'true';
        StatusGood ->
            lager:debug("allowing request for account in good standing"),
            'true';
        Status ->
            lager:debug("checking bookkeeper for account ~s in status ~s", [AccountId, Status]),
            maybe_bookkeeper_allow_updates(AccountId, Status)
    end.

-spec maybe_bookkeeper_allow_updates(kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
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

-spec default_maybe_allow_updates(kz_term:ne_binary()) -> 'true'.
default_maybe_allow_updates(AccountId) ->
    case ?SHOULD_ALLOW_UPDATES of
        'true' -> 'true';
        'false' ->
            lager:debug("denying update request, ~s.default_allow_updates is false", [?CONFIG_CAT]),
            Reason = io_lib:format("Service updates are disallowed by default for billing account ~s", [AccountId]),
            Error = kz_json:from_list([{<<"error">>, <<"updates_disallowed">>}
                                      ,{<<"message">>, kz_term:to_binary(Reason)}
                                      ]),
            throw({<<"account_billing_invalid">>, Error})
    end.

-spec spawn_move_to_good_standing(kz_term:ne_binary()) -> 'true'.
spawn_move_to_good_standing(?MATCH_ACCOUNT_RAW(AccountId)) ->
    _ = kz_util:spawn(fun move_to_good_standing/1, [AccountId]),
    'true'.

-spec move_to_good_standing(kz_term:ne_binary()) -> services().
move_to_good_standing(?MATCH_ACCOUNT_RAW(AccountId)) ->
    #kz_services{jobj = JObj}=Services = fetch(AccountId),
    lager:debug("moving account ~s services to good standing", [AccountId]),
    save(Services#kz_services{jobj = kzd_services:set_status(JObj, kzd_services:status_good())
                             }).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_only(kz_term:api_ne_binary() | services()) -> 'false' | services().
reconcile_only('undefined') -> 'false';
reconcile_only(Account=?NE_BINARY) ->
    reconcile_only(fetch(Account));
reconcile_only(#kz_services{account_id=AccountId}=Services) ->
    lager:debug("reconcile all services for ~s", [AccountId]),
    Modules = get_service_modules(),
    lists:foldl(fun reconcile_module/2, Services, Modules).

-spec reconcile_module(module(), services()) -> services().
-ifdef(TEST).
reconcile_module(M, Services) ->
    {reconcile,1} = lists:keyfind(reconcile, 1, M:module_info(exports)),
    Services.
-else.
reconcile_module(M, Services) ->
    M:reconcile(Services).
-endif.

-spec reconcile(kz_term:api_ne_binary() | services()) -> 'false' | services().
reconcile('undefined') -> 'false';
reconcile(Account=?NE_BINARY) ->
    save(reconcile_only(Account));
reconcile(#kz_services{}=Services) ->
    save(reconcile_only(Services)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reconcile_only(kz_term:api_binary() | services(), kz_term:text()) -> 'false' | services().
reconcile_only('undefined', _Module) -> 'false';
reconcile_only(Account=?NE_BINARY, Module) ->
    reconcile_only(fetch(Account), Module);
reconcile_only(#kz_services{account_id = _AccountId}=CurrentServices, Module) ->
    lager:debug("reconcile ~s services for ~s", [Module, _AccountId]),
    case get_service_module(Module) of
        'false' -> 'false';
        ServiceModule -> reconcile_module(ServiceModule, CurrentServices)
    end.

-spec reconcile(kz_term:api_binary() | services(), kz_term:text()) -> 'false' | services().
reconcile('undefined', _Module) -> 'false';
reconcile(Account=?NE_BINARY, Module) ->
    maybe_save(reconcile_only(Account, Module));
reconcile(#kz_services{}=Services, Module) ->
    pause_between_service_reconciliation(),
    maybe_save(reconcile_only(Services, Module)).

-ifdef(TEST).
pause_between_service_reconciliation() -> ok.
-else.
pause_between_service_reconciliation() ->
    timer:sleep(?MILLISECONDS_IN_SECOND).
-endif.

%%%=============================================================================
%%% Access functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_id(services()) -> kz_term:ne_binary().
account_id(#kz_services{account_id = 'undefined'
                       ,jobj = JObj
                       }) ->
    lager:debug("services has no account id, looking in ~s", [kz_json:encode(JObj)]),
    kz_doc:account_id(JObj);
account_id(#kz_services{account_id = AccountId}) ->
    AccountId.

-spec services_json(services()) -> kzd_services:doc().
services_json(#kz_services{jobj = JObj}) ->
    JObj.

-spec is_dirty(services()) -> boolean().
is_dirty(#kz_services{dirty = IsDirty}) ->
    kz_term:is_true(IsDirty).

-spec is_services(any()) -> boolean().
is_services(#kz_services{}) -> true;
is_services(_) -> false.

-ifdef(TEST).
-spec current_billing_id(services()) -> kz_term:api_ne_binary().
current_billing_id(#kz_services{current_billing_id = CurrentBillingId}) ->
    CurrentBillingId.

-spec is_deleted(services()) -> boolean().
is_deleted(#kz_services{deleted = IsDeleted}) ->
    IsDeleted.

-spec status(services()) -> kz_term:ne_binary().
status(#kz_services{status = Status}) ->
    Status.
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec quantity(kz_term:ne_binary(), kz_term:ne_binary(), services()) -> integer().
quantity(_, _, #kz_services{deleted = 'true'}) -> 0;
quantity(CategoryId, ItemId, #kz_services{updates = Updates
                                         ,jobj = JObj
                                         }) ->
    ItemQuantity = kzd_services:item_quantity(JObj, CategoryId, ItemId),
    kz_json:get_integer_value([CategoryId, ItemId], Updates, ItemQuantity).

-spec diff_quantities(services()) -> kz_term:api_object().
diff_quantities(#kz_services{deleted = 'true'}) -> 'undefined';
diff_quantities(#kz_services{jobj = JObj
                            ,updates = Updates
                            }) ->
    kz_json:foldl(fun diff_cat_quantities/3, Updates, kzd_services:quantities(JObj)).

-spec diff_cat_quantities(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
diff_cat_quantities(CategoryId, ItemsJObj, Updates) ->
    kz_json:foldl(fun(I, Q, Acc) ->
                          diff_item_quantities(I, Q, Acc, CategoryId)
                  end
                 ,Updates
                 ,ItemsJObj
                 ).

-spec diff_item_quantities(kz_term:ne_binary(), integer(), kz_json:object(), kz_term:ne_binary()) ->
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
maybe_update_diff(_Key, ItemQuantity, ItemQuantity, Updates) ->
    lager:debug("same quantity for ~p, ignoring", [_Key]),
    Updates;
maybe_update_diff(Key, ItemQuantity, UpdateQuantity, Updates) ->
    lager:debug("updating ~p from ~p to ~p", [Key, ItemQuantity, UpdateQuantity]),
    kz_json:set_value(Key, UpdateQuantity - ItemQuantity, Updates).

-spec diff_quantity(kz_term:ne_binary(), kz_term:ne_binary(), services()) -> integer().
diff_quantity(_, _, #kz_services{deleted = 'true'}) -> 0;
diff_quantity(CategoryId, ItemId, #kz_services{jobj = JObj
                                              ,updates = Updates
                                              }) ->
    ItemQuantity = kzd_services:item_quantity(JObj, CategoryId, ItemId),
    UpdateQuantity = kz_json:get_integer_value([CategoryId, ItemId], Updates, 0),
    UpdateQuantity - ItemQuantity.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec updated_quantity(kz_term:ne_binary(), kz_term:ne_binary(), services()) -> integer().
updated_quantity(_, _, #kz_services{deleted = 'true'}) -> 0;
updated_quantity(CategoryId, ItemId, #kz_services{updates = JObj}) ->
    kz_json:get_integer_value([CategoryId, ItemId], JObj, 0).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category_quantity(kz_term:ne_binary(), services()) -> non_neg_integer().
category_quantity(CategoryId, Services) ->
    category_quantity(CategoryId, [], Services).

-spec category_quantity(kz_term:ne_binary(), kz_term:ne_binaries(), services()) -> non_neg_integer().
category_quantity(_CategoryId, _ItemExceptions, #kz_services{deleted = 'true'}) -> 0;
category_quantity(CategoryId, ItemExceptions, #kz_services{updates = UpdatedQuantities
                                                          ,jobj = JObj
                                                          }) ->
    CatQuantities = kzd_services:category_quantities(JObj, CategoryId),
    CatUpdates = kz_json:get_value(CategoryId, UpdatedQuantities, kz_json:new()),
    %% replaces CatQs values with CatUpdate
    Quantities = kz_json:merge(CatQuantities, CatUpdates),
    %% Removes ItemExceptions, if any
    QsMinusEx = kz_json:delete_keys(ItemExceptions, Quantities),
    sum_values(0, QsMinusEx).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_quantity(kz_term:ne_binary(), kz_term:ne_binary(), services()) -> non_neg_integer().
cascade_quantity(_, _, #kz_services{deleted = 'true'}) -> 0;
cascade_quantity(CategoryId, ItemId, #kz_services{cascade_quantities = JObj}=Services) ->
    kz_json:get_integer_value([CategoryId, ItemId], JObj, 0)
        + quantity(CategoryId, ItemId, Services).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_category_quantity(kz_term:ne_binary(), services()) -> non_neg_integer().
cascade_category_quantity(CategoryId, Services) ->
    cascade_category_quantity(CategoryId, [], Services).

-spec cascade_category_quantity(kz_term:ne_binary(), kz_term:ne_binaries(), services()) -> non_neg_integer().
cascade_category_quantity(_, _, #kz_services{deleted = 'true'}) -> 0;
cascade_category_quantity(CategoryId, ItemExceptions, #kz_services{cascade_quantities = Quantities
                                                                  }=Services) ->
    CatQuantities = kz_json:get_value(CategoryId, Quantities, kz_json:new()),
    sum_values(category_quantity(CategoryId, ItemExceptions, Services)
              ,kz_json:delete_keys(ItemExceptions, CatQuantities)
              ).

sum_values(Acc0, JObj) ->
    F = fun(_ItemId, ItemQuantity, Sum) -> ItemQuantity + Sum end,
    kz_json:foldl(F, Acc0, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reset_category(kz_term:ne_binary(), services()) -> services().
reset_category(CategoryId, #kz_services{updates = JObj}=Services) ->
    NewUpdates = kz_json:set_value(CategoryId, kz_json:new(), JObj),
    Services#kz_services{updates = NewUpdates
                        }.

%%------------------------------------------------------------------------------
%% @doc Helper function to know if an account is a reseller or not.
%% @end
%%------------------------------------------------------------------------------
-spec is_reseller(kz_term:ne_binary() | services() | kz_json:object()) -> boolean().
is_reseller(#kz_services{jobj = ServicesJObj}) ->
    is_reseller(ServicesJObj);
is_reseller(Account=?NE_BINARY) ->
    is_reseller(fetch(Account));
is_reseller(ServicesJObj) ->
    kzd_services:is_reseller(ServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dry_run(services()) -> kz_json:object().
dry_run(Services) ->
    ActivationsCharges = dry_run_activation_charges(Services),
    calculate_charges(Services, ActivationsCharges).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_charges(services(), kz_json:objects()) -> kz_json:object().
calculate_charges(Services, JObjs) ->
    case calculate_services_charges(Services) of
        {'no_plan', _NP} -> kz_json:new();
        {'ok', PlansCharges} ->
            calculate_transactions_charges(PlansCharges, JObjs)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec calculate_services_charges(services()) ->
                                        {'no_plan' | 'ok', kz_json:object()}.
calculate_services_charges(#kz_services{jobj = ServiceJObj}=Services) ->
    case kz_service_plans:from_service_json(ServiceJObj) of
        [] -> {'no_plan', kz_json:new()};
        ServicePlans ->
            calculate_services_charges(Services, ServicePlans)
    end.

-spec calculate_services_charges(services(), kz_service_plans:plans()) ->
                                        {'error' | 'ok', kz_json:object()}.
calculate_services_charges(#kz_services{jobj = ServiceJObj
                                       ,updates = UpdatesJObj
                                       }=Service
                          ,ServicePlans
                          ) ->
    CurrentQuantities = kzd_services:quantities(ServiceJObj),
    UpdatedQuantities = kz_json:merge(CurrentQuantities, UpdatesJObj),

    UpdatedServiceJObj = kzd_services:set_quantities(ServiceJObj, UpdatedQuantities),

    ExistingItems = kz_service_plans:create_items(ServiceJObj, ServicePlans),
    lager:debug("current items: ~s", [kz_json:encode(kz_service_items:public_json(ExistingItems))]),
    UpdatedItems = kz_service_plans:create_items(UpdatedServiceJObj, ServicePlans),
    lager:debug("items after update: ~s", [kz_json:encode(kz_service_items:public_json(UpdatedItems))]),
    Changed = kz_service_items:get_updated_items(UpdatedItems, ExistingItems),
    lager:debug("service diff quantities: ~s", [kz_json:encode(diff_quantities(Service))]),

    lager:debug("computed service charges"),
    {'ok', kz_service_items:public_json(Changed)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec calculate_transactions_charges(kz_json:object(), kz_json:objects()) ->
                                            kz_json:object().
calculate_transactions_charges(PlansCharges, JObjs) ->
    lists:foldl(fun calculate_transactions_charge_fold/2, PlansCharges, JObjs).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec dry_run_activation_charges(services()) -> kz_json:objects().
dry_run_activation_charges(#kz_services{updates = Updates}=Services) ->
    kz_json:foldl(fun(CategoryId, CategoryJObj, Acc) ->
                          dry_run_activation_charges(CategoryId, CategoryJObj, Services, Acc)
                  end
                 ,[]
                 ,Updates
                 ).

-spec dry_run_activation_charges(kz_term:ne_binary(), kz_json:object()
                                ,services(), kz_json:objects()
                                ) -> kz_json:objects().
dry_run_activation_charges(CategoryId, CategoryJObj, Services, JObjs) ->
    kz_json:foldl(fun(ItemId, Quantity, Acc1) ->
                          dry_run_activation_charges(CategoryId, ItemId, Quantity, Services, Acc1)
                  end
                 ,JObjs
                 ,CategoryJObj
                 ).

-spec dry_run_activation_charges(kz_term:ne_binary(), kz_term:ne_binary()
                                ,integer(), services()
                                ,kz_json:objects()
                                ) -> kz_json:objects().
dry_run_activation_charges(CategoryId, ItemId, Quantity, #kz_services{jobj = JObj}=Services, JObjs) ->
    case kzd_services:item_quantity(JObj, CategoryId, ItemId) of
        Quantity -> JObjs;
        OldQuantity ->
            Plans = kz_service_plans:from_service_json(to_json(Services)),
            Charges = activation_charges(CategoryId, ItemId, Plans),
            ServicePlan = kz_service_plans:public_json(Plans),
            ItemPlan = get_item_plan(CategoryId, ItemId, ServicePlan),
            [kz_json:from_list(
               [{<<"category">>, CategoryId}
               ,{<<"item">>, kzd_item_plan:masquerade_as(ItemPlan, ItemId)}
               ,{<<"amount">>, Charges}
               ,{<<"quantity">>, Quantity}
               ,{<<"activate_quantity">>, Quantity - OldQuantity}
               ])
             | JObjs
            ]
    end.

-spec get_item_plan(kz_term:ne_binary(), kz_term:ne_binary(), kzd_service_plan:doc()) -> kz_json:object().
get_item_plan(CategoryId, ItemId, ServicePlan) ->
    case kzd_service_plan:item_plan(ServicePlan, CategoryId, ItemId, 'undefined') of
        'undefined' -> kzd_service_plan:category_plan(ServicePlan, CategoryId);
        Plan -> Plan
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_service_modules() -> kz_term:atoms().
get_service_modules() ->
    case ?DEFAULT_SERVICE_MODULES of
        [_|_]=ConfModules ->
            lager:debug("configured service modules: ~p", [ConfModules]),
            [kz_term:to_atom(Mod, 'true') || Mod <- ConfModules];
        _ ->
            ConfModules = ?SERVICE_MODULES,
            kapps_config:set_default(?CONFIG_CAT, <<"modules">>, ConfModules),
            lager:info("set default service modules: ~p", [ConfModules]),
            ConfModules
    end.

-spec default_service_modules() -> kz_term:atoms().
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

-spec get_service_module(kz_term:text()) -> module() | 'false'.
get_service_module(Module) when not is_binary(Module) ->
    get_service_module(kz_term:to_binary(Module));
get_service_module(<<?SERVICE_MODULE_PREFIX,_/binary>> = Module) ->
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_quantities(services()) -> kz_json:object().
cascade_quantities(#kz_services{cascade_quantities = JObj}) -> JObj.

-spec cascade_quantities(kz_term:ne_binary(), boolean()) -> kz_json:object().
cascade_quantities(Account=?NE_BINARY, 'false') ->
    lager:debug("computing cascade quantities for ~s", [Account]),
    do_cascade_quantities(Account, <<"services/cascade_quantities">>);
cascade_quantities(Account=?NE_BINARY, 'true') ->
    lager:debug("computing reseller cascade quantities for ~s", [Account]),
    do_cascade_quantities(Account, <<"services/reseller_quantities">>).

-spec do_cascade_quantities(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
do_cascade_quantities(Account, View) ->
    case cascade_results(View, kz_util:format_account_id(Account)) of
        {'error', _} -> kz_json:new();
        {'ok', JObjs} ->
            lists:foldl(fun do_cascade_quantities_fold/2, kz_json:new(), JObjs)
    end.

do_cascade_quantities_fold(JObj, AccJObj) ->
    [_|Keys] = kz_json:get_value(<<"key">>, JObj),
    Value = kz_json:get_integer_value(<<"value">>, JObj),
    lager:debug("setting cascade quantity ~p: ~p", [Keys, Value]),
    kz_json:set_value(Keys, Value, AccJObj).

-ifdef(TEST).
-define(ITEM(Category, Item, Quantity)
       ,kz_json:from_list(
          [{<<"key">>, [?A_RESELLER_ACCOUNT_ID, Category, Item]}
          ,{<<"value">>, Quantity}
          ])).

cascade_results(<<"services/cascade_quantities">>, ?A_SUB_ACCOUNT_ID) ->
    {ok, []};
cascade_results(<<"services/cascade_quantities">>, ?B_SUB_ACCOUNT_ID) ->
    {ok, []};
cascade_results(<<"services/reseller_quantities">>, ?UNRELATED_ACCOUNT_ID) ->
    {ok, []};
cascade_results(<<"services/reseller_quantities">>, ?A_RESELLER_ACCOUNT_ID) ->
    {ok, [?ITEM(<<"billing">>, <<"devices">>, 42)
         ,?ITEM(<<"billing">>, <<"fax">>, 1)
         ,?ITEM(<<"billing">>, <<"localDIDs">>, 7)
         ,?ITEM(<<"branding">>, <<"whitelabel">>, 10)
         ,?ITEM(<<"devices">>, <<"mobile">>, 40)
         ,?ITEM(<<"devices">>, <<"sip_device">>, 2)
         ,?ITEM(<<"ledgers">>, <<"per-minute-voip">>, 913840)
         ,?ITEM(<<"limits">>, <<"twoway_trunks">>, 404)
         ,?ITEM(<<"number_carriers">>, <<"knm_bandwidth2">>, 7)
         ,?ITEM(<<"number_services">>, <<"dash_e911">>, 2)
         ,?ITEM(<<"number_services">>, <<"failover">>, 3)
         ,?ITEM(<<"phone_numbers">>, <<"did_us">>, 7)
         ,?ITEM(<<"users">>, <<"admin">>, 1)
         ,?ITEM(<<"users">>, <<"user">>, 1)
         ]};
cascade_results(View, AccountId) ->
    ViewOptions = ['group'
                  ,'reduce'
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_SERVICES_DB, View, ViewOptions) of
        {ok, _}=OK -> OK;
        {error, _}=Error ->
            ?LOG_DEBUG("~n~n NO VIEW FOR AccountId: ~p View: ~p~n~n", [AccountId, View]),
            kz_util:log_stacktrace(),
            Error
            %% Not throwing since this is needed for one of the kapps_account_config test
            %% throw(Error)
    end.
-else.
cascade_results(View, AccountId) ->
    ViewOptions = ['group'
                  ,'reduce'
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    kz_datamgr:get_results(?KZ_SERVICES_DB, View, ViewOptions).
-endif.

%%------------------------------------------------------------------------------
%% @doc determine the billing id as it is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%------------------------------------------------------------------------------
-spec depreciated_billing_id(kz_json:object()) -> kz_term:ne_binary().
depreciated_billing_id(JObj) ->
    depreciated_billing_id(JObj, kz_doc:account_id(JObj)).

depreciated_billing_id(JObj, AccountId) ->
    case ?SUPPORT_BILLING_ID of
        'true' -> kzd_services:billing_id(JObj, AccountId);
        'false' -> AccountId
    end.

%%------------------------------------------------------------------------------
%% @doc determine if pvt_reseller is currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%------------------------------------------------------------------------------
-spec depreciated_is_reseller(kz_json:object()) -> boolean().
depreciated_is_reseller(JObj) ->
    kzd_services:is_reseller(JObj).

%%------------------------------------------------------------------------------
%% @doc determine what service plans are currently set on the account
%% definition as this will be depreciated in the future.
%% @end
%%------------------------------------------------------------------------------
-spec populate_service_plans(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
populate_service_plans(JObj, ResellerId) ->
    Plans = incorporate_default_service_plan(ResellerId, master_default_service_plan()),
    incorporate_depreciated_service_plans(Plans, JObj).

-spec default_service_plan_id(kz_term:ne_binary()) -> kz_term:api_binary().
default_service_plan_id(ResellerId) ->
    case fetch_services_doc(ResellerId, true) of
        {'ok', JObj} -> kz_json:get_value(?DEFAULT_PLAN, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s services: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec depreciated_default_service_plan_id(kz_term:ne_binary()) -> kz_term:api_binary().
depreciated_default_service_plan_id(ResellerId) ->
    case fetch_account(ResellerId) of
        {'ok', JObj} -> kz_json:get_value(?DEFAULT_PLAN, JObj);
        {'error', _R} ->
            lager:debug("unable to open reseller ~s account definition: ~p", [ResellerId, _R]),
            'undefined'
    end.

-spec master_default_service_plan() -> kz_json:object().
master_default_service_plan() ->
    case master_account_id() of
        {'error', _} -> kz_json:new();
        {'ok', MasterAccountId} ->
            incorporate_default_service_plan(MasterAccountId, kz_json:new())
    end.

-spec incorporate_default_service_plan(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
incorporate_default_service_plan(ResellerId, JObj) ->
    case depreciated_default_service_plan_id(ResellerId) of
        'undefined' ->
            incorporate_only_default_service_plan(ResellerId, JObj);
        PlanId ->
            maybe_augment_with_plan(ResellerId, JObj, PlanId)
    end.

-spec incorporate_only_default_service_plan(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
incorporate_only_default_service_plan(ResellerId, JObj) ->
    maybe_augment_with_plan(ResellerId, JObj, default_service_plan_id(ResellerId)).

maybe_augment_with_plan(_ResellerId, JObj, 'undefined') -> JObj;
maybe_augment_with_plan(ResellerId, JObj, PlanId) ->
    Plan = kz_json:from_list(
             [{<<"account_id">>, ResellerId}
             ]),
    kz_json:set_value(PlanId, Plan, JObj).

-spec incorporate_depreciated_service_plans(kz_json:object(), kz_json:object()) -> kz_json:object().
incorporate_depreciated_service_plans(Plans, JObj) ->
    PlanIds = kz_json:get_value(?SERVICES_PVT_PLANS, JObj),
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_reseller_id(kz_term:ne_binaries() | kz_term:ne_binary()) -> kz_term:ne_binary().
get_reseller_id([]) ->
    {'ok', MasterAccountId} = master_account_id(),
    MasterAccountId;
get_reseller_id([Parent|Ancestors]) ->
    case fetch_services_doc(Parent, cache_failures) of
        {'error', _R} ->
            lager:debug("failed to open services doc ~s durning reseller search: ~p", [Parent, _R]),
            get_reseller_id(Ancestors);
        {'ok', ServicesJObj} ->
            get_reseller_id(Parent, Ancestors, ServicesJObj)
    end;
get_reseller_id(Account=?NE_BINARY) ->
    case fetch_account(Account) of
        {'ok', AccountJObj} ->
            get_reseller_id(lists:reverse(kzd_accounts:tree(AccountJObj)));
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [Account, _R]),
            get_reseller_id([])
    end.

-spec get_reseller_id(kz_term:ne_binary(), kz_term:ne_binaries(), kz_json:object()) -> kz_term:api_binary().
get_reseller_id(Parent, Ancestors, ServicesJObj) ->
    case kzd_services:is_reseller(ServicesJObj) of
        'false' -> get_reseller_id(Ancestors);
        'true' -> Parent
    end.

-ifdef(TEST).
fetch_account(?A_MASTER_ACCOUNT_ID) -> kz_json:fixture(?APP, "a_master_account.json");
fetch_account(?A_RESELLER_ACCOUNT_ID) -> kz_json:fixture(?APP, "a_reseller_account.json");
fetch_account(?A_SUB_ACCOUNT_ID) -> kz_json:fixture(?APP, "a_sub_account.json");
fetch_account(?B_SUB_ACCOUNT_ID) -> kz_json:fixture(?APP, "a_sub_account.json");
fetch_account(?UNRELATED_ACCOUNT_ID) -> kz_json:fixture(?APP, "unrelated_account.json");
%% Line below is needed for one of the kapps_account_config test
fetch_account(Account) -> kzd_accounts:fetch(Account).
-else.
fetch_account(Account) -> kzd_accounts:fetch(Account).
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_save('false' | services()) -> 'false' | services().
maybe_save('false') -> 'false';
maybe_save(#kz_services{jobj = JObj
                       ,updates = UpdatedQuantities
                       }=Services) ->
    CurrentQuantities = kzd_services:quantities(JObj),
    case have_quantities_changed(UpdatedQuantities, CurrentQuantities) of
        'true' ->
            lager:debug("quantities have changed, saving dirty services"),
            save(Services);
        'false' ->
            lager:debug("no service quantity changes"),
            Services
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec have_quantities_changed(services()) -> boolean().
have_quantities_changed(#kz_services{jobj = JObj
                                    ,updates = UpdatedQuantities
                                    }) ->
    CurrentQuantities = kzd_services:quantities(JObj),
    have_quantities_changed(UpdatedQuantities, CurrentQuantities).

-spec have_quantities_changed(kz_json:object(), kz_json:object()) -> boolean().
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_account_definition(kz_term:ne_binary()) -> kzd_accounts:doc().
get_account_definition(?MATCH_ACCOUNT_RAW(AccountId)) ->
    case fetch_account(AccountId) of
        {'error', _R} ->
            lager:debug("unable to get account defintion for ~s: ~p", [AccountId, _R]),
            kz_json:new();
        {'ok', JObj} -> JObj
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_clean_old_billing_id(services()) -> services().
maybe_clean_old_billing_id(#kz_services{billing_id = BillingId
                                       ,current_billing_id = BillingId
                                       }=Services) ->
    Services;
maybe_clean_old_billing_id(#kz_services{current_billing_id = BillingId
                                       ,account_id = BillingId
                                       ,jobj = JObj
                                       }=Services) ->
    case kzd_services:is_reseller(JObj) of
        'true' -> Services;
        'false' ->
            _ = clean(BillingId),
            Services
    end;
maybe_clean_old_billing_id(#kz_services{}=Services) ->
    Services.


-spec clean(kz_term:ne_binary()) -> kz_term:std_return().
clean(Account) ->
    AccountId = kz_util:format_account_id(Account),
    case ?MODULE:fetch_services_doc(AccountId, 'true') of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} ->
            immediate_sync(AccountId, kz_doc:set_soft_deleted(ServicesJObj, 'true'))
    end.

-spec immediate_sync(kz_term:ne_binary(), kzd_services:doc()) -> kz_term:std_return().
immediate_sync(AccountId, ServicesJObj) ->
    case kz_service_plans:create_items(ServicesJObj) of
        {'error', 'no_plans'}=E -> E;
        {'ok', ServiceItems} ->
            %% TODO: support other bookkeepers...
            try kz_bookkeeper_braintree:sync(ServiceItems, AccountId) of
                _ -> {'ok', ServicesJObj}
            catch
                'throw':{_, R} -> {'error', R};
                _E:R -> {'error', R}
            end
    end.

-spec sync(kz_term:ne_binary()) -> kz_term:std_return().
sync(Account) ->
    AccountId = kz_util:format_account_id(Account),
    kz_util:put_callid(<<AccountId/binary, "-sync">>),
    case ?MODULE:fetch_services_doc(AccountId, 'true') of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} ->
            sync(AccountId, ServicesJObj)
    end.

-spec sync(kz_term:ne_binary(), kz_json:object()) -> kz_term:std_return().
sync(AccountId, ServicesJObj) ->
    case get_billing_id(AccountId, ServicesJObj) of
        AccountId -> maybe_sync_services(AccountId, ServicesJObj);
        BillingId ->
            io:format("Account ~s is configured to use the credit card of ~s, following billing tree~n"
                     ,[AccountId, BillingId]),
            lager:debug("account ~s is configured to use the credit card of ~s, following billing tree"
                       ,[AccountId, BillingId]),
            sync(BillingId)
    end.

-spec maybe_sync_services(kz_term:ne_binary(), kzd_services:doc()) -> kz_term:std_return().
maybe_sync_services(AccountId, ServicesJObj) ->
    case kz_service_plans:create_items(ServicesJObj) of
        {'error', 'no_plans'} ->
            lager:debug("no services plans found"),
            _ = maybe_sync_transactions(AccountId, ServicesJObj),
            _ = mark_clean_and_status(kzd_services:status_good(), ServicesJObj),
            maybe_sync_reseller(AccountId, ServicesJObj);
        {'ok', ServiceItems} ->
            sync_services(AccountId, ServicesJObj, ServiceItems)
    end.

-spec sync_services(kz_term:ne_binary(), kzd_services:doc(), kz_service_items:items()) -> kz_term:std_return().
sync_services(AccountId, ServicesJObj, ServiceItems) ->
    try sync_services_bookkeeper(AccountId, ServicesJObj, ServiceItems) of
        'ok' ->
            _ = mark_clean_and_status(kzd_services:status_good(), ServicesJObj),
            io:format("synchronization with bookkeeper complete (good-standing)~n"),
            lager:debug("synchronization with bookkeeper complete (good-standing)"),
            maybe_sync_reseller(AccountId, ServicesJObj);
        'delinquent' ->
            _ = mark_clean_and_status(kzd_services:status_delinquent(), ServicesJObj),
            io:format("synchronization with bookkeeper complete (delinquent)~n"),
            lager:debug("synchronization with bookkeeper complete (delinquent)"),
            maybe_sync_reseller(AccountId, ServicesJObj);
        'retry' ->
            io:format("synchronization with bookkeeper complete (retry)~n"),
            lager:debug("synchronization with bookkeeper complete (retry)"),
            {'error', 'retry'}
    catch
        'throw':{Reason, _}=_R ->
            lager:info("bookkeeper error: ~p", [_R]),
            _ = mark_clean_and_status(kz_term:to_binary(Reason), ServicesJObj),
            maybe_sync_reseller(AccountId, ServicesJObj);
        _E:R ->
            lager:info("unable to sync services(~p): ~p", [_E, R]),
            kz_util:log_stacktrace(),
            {'error', R}
    end.

-spec sync_services_bookkeeper(kz_term:ne_binary(), kz_json:object(), kz_service_items:items()) -> 'ok' | 'delinquent' | 'retry'.
sync_services_bookkeeper(AccountId, ServicesJObj, ServiceItems) ->
    Bookkeeper = ?MODULE:select_bookkeeper(AccountId),
    lager:debug("attempting to sync with bookkeeper ~s", [Bookkeeper]),
    Result = Bookkeeper:sync(ServiceItems, AccountId),
    maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper),
    Result.

-spec maybe_sync_transactions(kz_term:ne_binary(), kzd_services:doc()) -> 'ok'.
maybe_sync_transactions(AccountId, ServicesJObj) ->
    Bookkeeper = ?MODULE:select_bookkeeper(AccountId),
    maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper).

-spec maybe_sync_transactions(kz_term:ne_binary(), kzd_services:doc(), atom()) -> 'ok'.
maybe_sync_transactions(AccountId, ServicesJObj, Bookkeeper) ->
    case kzd_services:transactions(ServicesJObj) of
        [] -> 'ok';
        Trs ->
            Transactions = maybe_delete_topup_transaction(AccountId, Trs),
            sync_transactions(AccountId, ServicesJObj, Bookkeeper, Transactions)
    end.

-spec maybe_delete_topup_transaction(kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_delete_topup_transaction(AccountId, Transactions) ->
    NonTopup = lists:filter(
                 fun(J) ->
                         kz_json:get_integer_value(<<"pvt_code">>, J) =/= ?CODE_TOPUP
                 end, Transactions
                ),
    case NonTopup of
        Transactions -> Transactions;
        _Other ->
            case kz_topup:should_topup(AccountId) of
                'true' -> Transactions;
                'false' -> NonTopup
            end
    end.

-spec sync_transactions(kz_term:ne_binary(), kzd_services:doc(), atom(), kz_json:objects()) ->
                               'ok'.
sync_transactions(AccountId, ServicesJObj, Bookkeeper, Transactions) ->
    BillingId = kzd_services:billing_id(ServicesJObj),
    FailedTransactions = Bookkeeper:charge_transactions(BillingId, Transactions),
    case kz_datamgr:save_doc(?KZ_SERVICES_DB
                            ,kzd_services:set_transactions(ServicesJObj, FailedTransactions)
                            )
    of
        {'error', _E} ->
            lager:warning("failed to clean pending transactions ~p", [_E]);
        {'ok', _} ->
            handle_topup_transactions(AccountId, Transactions, FailedTransactions)
    end.

-spec handle_topup_transactions(kz_term:ne_binary(), kz_json:objects(), kz_json:objects() | integer()) -> 'ok'.
handle_topup_transactions(Account, JObjs, Failed) when is_list(Failed) ->
    case did_topup_failed(Failed) of
        'true' -> 'ok';
        'false' -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(_, [], _) -> 'ok';
handle_topup_transactions(Account, [JObj|JObjs]=List, Retry) when Retry > 0 ->
    case kz_json:get_integer_value(<<"pvt_code">>, JObj) of
        ?CODE_TOPUP ->
            Amount = kz_json:get_value(<<"pvt_amount">>, JObj),
            Transaction = kz_transaction:credit(Account, Amount),
            Transaction1 = kz_transaction:set_reason(wht_util:topup(), Transaction),
            case kz_transaction:save(Transaction1) of
                {'ok', _} -> 'ok';
                {'error', 'conflict'} ->
                    lager:warning("did not write top up transaction for account ~s already exist for today", [Account]);
                {'error', _E} ->
                    lager:error("failed to write top up transaction ~p , for account ~s (amount: ~p), retrying ~p..."
                               ,[_E, Account, Amount, Retry]
                               ),
                    handle_topup_transactions(Account, List, Retry-1)
            end;
        _ -> handle_topup_transactions(Account, JObjs, 3)
    end;
handle_topup_transactions(Account, _, _) ->
    lager:error("failed to write top up transaction for account ~s too many retries", [Account]).

-spec did_topup_failed(kz_json:objects()) -> boolean().
did_topup_failed(JObjs) ->
    lists:any(fun has_topup_code/1, JObjs).

-spec has_topup_code(kz_json:object()) -> boolean().
has_topup_code(JObj) ->
    ?CODE_TOPUP =:= kz_json:get_integer_value(<<"pvt_code">>, JObj).

-spec maybe_sync_reseller(kz_term:ne_binary(), kzd_services:doc()) -> kz_term:std_return().
maybe_sync_reseller(AccountId, ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj, AccountId) of
        AccountId -> {'ok', ServicesJObj};
        ResellerId ->
            lager:debug("marking reseller ~s as dirty", [ResellerId]),
            mark_dirty(ResellerId)
    end.

-spec get_billing_id(kz_term:ne_binary(), kzd_services:doc()) -> kz_term:ne_binary().
get_billing_id(AccountId, ServicesJObj) ->
    case kzd_services:is_reseller(ServicesJObj) of
        'true' -> AccountId;
        'false' ->
            case ?SUPPORT_BILLING_ID of
                'true' -> kzd_services:billing_id(ServicesJObj, AccountId);
                'false' -> AccountId
            end
    end.

-spec mark_dirty(kz_term:ne_binary() | kzd_services:doc()) -> kz_term:std_return().
mark_dirty(?MATCH_ACCOUNT_RAW(AccountId)) ->
    case ?MODULE:fetch_services_doc(AccountId, 'true') of
        {'error', _}=E -> E;
        {'ok', ServicesJObj} -> mark_dirty(ServicesJObj)
    end;
mark_dirty(ServicesJObj) ->
    Values = [{?SERVICES_PVT_IS_DIRTY, 'true'}
             ,{?SERVICES_PVT_MODIFIED, kz_time:now_s()}
             ],
    kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values(Values, ServicesJObj)).

-spec mark_clean_and_status(kz_term:ne_binary(), kzd_services:doc()) -> kz_term:std_return().
mark_clean_and_status(Status, ServicesJObj) ->
    lager:debug("marking services clean with status ~s", [Status]),
    Values = [{?SERVICES_PVT_IS_DIRTY, 'false'}
             ,{?SERVICES_PVT_STATUS, Status}
             ],
    kz_datamgr:save_doc(?KZ_SERVICES_DB, kz_json:set_values(Values, ServicesJObj)).
