%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_tests).

-include_lib("eunit/include/eunit.hrl").
-include("services.hrl").

-define(CAT, <<"phone_numbers">>).
-define(ITEM, <<"did_us">>).

-record(state, {services :: kz_services:services()
               ,services_jobj :: kz_json:object()
               ,service_plan_jobj :: kzd_service_plan:plan()
               ,account_plan :: kzd_service_plan:plan()
               ,no_overrides :: boolean()
               }).


phone_number_services_test_() ->
    services_tests({"example_account_services.json", "example_service_plan_1.json"}).

services_reseller_test_() ->
    services_tests(?A_RESELLER_ACCOUNT_ID).

services_tests(Init) ->
    {'foreach'
    ,fun () -> init(Init) end
    ,fun (#state{}) -> ok end
    ,[fun services_json_to_record/1
     ,fun services_record_to_json/1
     ,fun service_plan_json_to_plans/1
     ,fun increase_quantities/1
     ]
    }.

services_master_test_() ->
    no_plans_tests(?A_MASTER_ACCOUNT_ID).

services_sub_test_() ->
    no_plans_tests(?A_SUB_ACCOUNT_ID).

no_plans_tests(?MATCH_ACCOUNT_RAW(AccountId)) ->
    {ok, ServicesJObj} = kz_services:fetch_services_doc(AccountId),
    [?_assertEqual([], kz_service_plans:from_service_json(ServicesJObj))
    ,?_assertEqual(kz_json:new(), kzd_services:plan_overrides(ServicesJObj, undefined))
    ].

init(?MATCH_ACCOUNT_RAW(AccountId)) ->
    {ok, ServicesJObj} = kz_services:fetch_services_doc(AccountId),
    [PlanId] = kzd_services:plan_ids(ServicesJObj),
    ServicePlan = kz_service_plans:from_service_json(ServicesJObj),
    ServicePlanJObj = kz_service_plans:public_json(ServicePlan),
    Overrides = kzd_services:plan_overrides(ServicesJObj, PlanId),
    #state{services_jobj = ServicesJObj
          ,services = kz_services:from_service_json(ServicesJObj)
          ,service_plan_jobj = ServicePlanJObj
          ,account_plan = kzd_service_plan:merge_overrides(ServicePlanJObj, Overrides)
          ,no_overrides = kz_json:is_empty(Overrides)
          };

init({ServicesFixture, ServicePlanFixture}) ->
    {ok,ServicePlanJObj} = kz_json:fixture(?APP, ServicePlanFixture),
    {ok,ServicesJObj} = kz_json:fixture(?APP, ServicesFixture),
    Overrides = kzd_services:plan_overrides(ServicesJObj, kz_doc:id(ServicePlanJObj)),
    #state{services_jobj = ServicesJObj
          ,services = kz_services:from_service_json(ServicesJObj, false)
          ,service_plan_jobj = ServicePlanJObj
          ,account_plan = kzd_service_plan:merge_overrides(ServicePlanJObj, Overrides)
          ,no_overrides = kz_json:is_empty(Overrides)
          }.

services_json_to_record(#state{services = Services
                              ,services_jobj = JObj
                              }) ->
    [{"Verify account id is set properly"
     ,?_assertEqual(kz_doc:account_id(JObj), kz_services:account_id(Services))
     }
    ,{"Verify the dirty flag is set properly"
     ,?_assertEqual(kzd_services:is_dirty(JObj), kz_services:is_dirty(Services))
     }
    ,{"Verify the billing id"
     ,?_assertEqual(kzd_services:billing_id(JObj), kz_services:get_billing_id(Services))
     }
     | quantity_checks(JObj, Services, false)
    ].

services_record_to_json(#state{services = Services}) ->
    ServicesJObj = kz_services:to_json(Services),
    [{"Verify account id is set properly"
     ,?_assertEqual(kz_doc:account_id(ServicesJObj), kz_services:account_id(Services))
     }
    ,{"Verify the dirty flag is set properly"
     ,?_assertEqual(kzd_services:is_dirty(ServicesJObj), kz_services:is_dirty(Services))
     }
    ,{"Verify the billing id"
     ,?_assertEqual(kzd_services:billing_id(ServicesJObj), kz_services:get_billing_id(Services))
     }
     | quantity_checks(ServicesJObj, Services, false)
    ].

quantity_checks(ServicesJObj, Services, IsTestingDeleted) ->
    [category_checks(Category, CategoryJObj, Services, IsTestingDeleted)
     || {Category, CategoryJObj} <- kz_json:to_proplist(kzd_services:quantities(ServicesJObj))
    ].

category_checks(Category, CategoryJObj, Services, IsTestingDeleted) ->
    [item_check(Category, Item, Quantity, Services, IsTestingDeleted)
     || {Item, Quantity} <- kz_json:to_proplist(CategoryJObj)
    ].

item_check(Category, Item, Quantity, Services, false) ->
    {iolist_to_binary(io_lib:format("Verify ~s.~s is ~p", [Category, Item, Quantity]))
    ,?_assertEqual(Quantity, kz_services:quantity(Category, Item, Services))
    };
item_check(Category, Item, _, Services, true) ->
    {iolist_to_binary(io_lib:format("Verify ~s.~s is ~p", [Category, Item, 0]))
    ,?_assertEqual(0, kz_services:quantity(Category, Item, Services))
    }.

clean_discount(PlanJObj) ->
    Path = [<<"plan">>, <<"phone_numbers">>, <<"did_us">>, <<"discounts">>
           ,<<"cumulative">>, <<"rate">>],
    kz_json:delete_key(Path, PlanJObj).

service_plan_json_to_plans(#state{service_plan_jobj = ServicePlan
                                 ,account_plan = AccountPlan
                                 ,services = Services
                                 ,no_overrides = false
                                 }) ->
    [{"Verify plan from file matches services plan"
     ,?_assert(kz_json:are_equal(clean_discount(ServicePlan), clean_discount(AccountPlan)))
     }
    ,?_assertEqual(undefined, kzd_service_plan:account_id(AccountPlan))
    ,?_assertEqual(<<"968dc36503bcb05f798d9530016f311f">>, kz_doc:id(AccountPlan))
    ,?_assertEqual(<<"c0705d7984ea0160110a451b25a4406b">>, kz_services:account_id(Services))
    ,{"Verify cumulative discount rate from service plan"
     ,?_assertEqual(0.5, rate(cumulative_discount(did_us_item(ServicePlan))))
     }
    ,{"Verify cumulative discount rate was overridden"
     ,?_assertEqual(5.0, rate(cumulative_discount(did_us_item(AccountPlan))))
     }
    ,?_assert(not kz_json:are_equal(ServicePlan, kz_services:service_plan_json(Services)))
    ];

service_plan_json_to_plans(#state{service_plan_jobj = ServicePlan
                                 ,account_plan = AccountPlan
                                 ,services = Services
                                 ,no_overrides = true
                                 }) ->
    AccountId = kz_services:account_id(Services),
    [{"Verify plan from file matches services plan"
     ,?_assertEqual(kz_doc:account_id(ServicePlan), kzd_service_plan:account_id(AccountPlan))
     }
    ,?_assertEqual(undefined, kzd_service_plan:account_id(AccountPlan))
    ,{"Verify cumulative discount rate from service plan (set as int)"
     ,?_assertEqual(5.0, rate(cumulative_discount(did_us_item(ServicePlan))))
     }
    ,{"Verify cumulative discount rate was not overridden"
     ,?_assertEqual(undefined, did_us_item(AccountPlan))
     }
    ,{"Since ?A_RESELLER_ACCOUNT_ID does not have overrides, verify none were applied"
     ,?_assert(kz_json:are_equal(ServicePlan, kz_services:service_plan_json(Services)))
     }
    ,?_assert(kz_json:are_equal(ServicePlan, kz_services:service_plan_json(AccountId)))
    ,?_assertError(function_clause, kz_services:service_plan_json(undefined))
    ].

did_us_item(Plan) ->
    kzd_service_plan:item(Plan, ?CAT, ?ITEM).

cumulative_discount(Item) ->
    kzd_item_plan:cumulative_discount(Item).

rate(JObj) ->
    kz_json:get_float_value(<<"rate">>, JObj).

increase_quantities(#state{account_plan = _AccountPlan
                          ,services = Services
                          }) ->
    ItemQuantity = kz_services:quantity(?CAT, ?ITEM, Services),
    Increment = 1.0 * rand:uniform(10), %% To make sure this turns into an int
    UpdatedServices = kz_services:update(?CAT, ?ITEM, ItemQuantity + Increment, Services),
    UpdatedItemQuantity = kz_services:quantity(?CAT, ?ITEM, UpdatedServices),
    DiffItemQuantity = kz_services:diff_quantity(?CAT, ?ITEM, UpdatedServices),
    [{"Verify base quantity on the services doc"
     ,?_assertEqual(9, ItemQuantity)
     }
    ,{"Verify incrementing the quantity"
     ,?_assertEqual(ItemQuantity + round(Increment), UpdatedItemQuantity)
     }
    ,{"Verify the diff of the quantity"
     ,?_assertEqual(round(Increment), DiffItemQuantity)
     }
     | category_quantities(Services, UpdatedServices, Increment)
    ].

category_quantities(CurrentServices, UpdatedServices, Increment) ->
    CategoryQuantity = kz_services:category_quantity(?CAT, CurrentServices),
    UpdatedCategoryQuantity = kz_services:category_quantity(?CAT, UpdatedServices),

    TollFreeQuantity = kz_services:quantity(?CAT, <<"toll_free">>, UpdatedServices),
    DIDUSQuantity = kz_services:quantity(?CAT, ?ITEM, UpdatedServices),

    MinusTollFree = kz_services:category_quantity(?CAT, [<<"toll_free">>], UpdatedServices),
    MinusDIDUS = kz_services:category_quantity(?CAT, [?ITEM], UpdatedServices),

    [{"Verify base category quantities"
     ,?_assertEqual(10, CategoryQuantity)
     }
    ,{"Verify updated category quantities"
     ,?_assertEqual(CategoryQuantity + round(Increment), UpdatedCategoryQuantity)
     }
    ,{"Verify updated category quantities minus toll_free numbers"
     ,?_assertEqual(MinusTollFree, UpdatedCategoryQuantity-TollFreeQuantity)
     }
    ,{"Verify updated category quantities minus did_us numbers"
     ,?_assertEqual(MinusDIDUS, UpdatedCategoryQuantity-DIDUSQuantity)
     }
    ].

find_reseller_id_test_() ->
    [?_assertEqual(?A_MASTER_ACCOUNT_ID, kz_services:find_reseller_id(undefined))
    ,?_assertEqual(?A_MASTER_ACCOUNT_ID, kz_services:find_reseller_id(?A_MASTER_ACCOUNT_ID))
    ,?_assertEqual(?A_MASTER_ACCOUNT_ID, kz_services:find_reseller_id(?A_RESELLER_ACCOUNT_ID))
    ,?_assertEqual(?A_RESELLER_ACCOUNT_ID, kz_services:find_reseller_id(?A_SUB_ACCOUNT_ID))
    ,{"Verify missing info on service doc can be recovered from account doc"
     ,?_assertEqual(?A_RESELLER_ACCOUNT_ID, kz_services:find_reseller_id(?B_SUB_ACCOUNT_ID))
     }
    ].

new_empty_test_() ->
    Services = kz_services:new(),
    [?_assertEqual(undefined, kz_services:account_id(Services))
    ,?_assertEqual(undefined, kz_services:get_billing_id(Services))
    ,?_assertEqual(false, kz_services:is_dirty(Services))
    ,?_assertEqual(false, kz_services:is_deleted(Services))
    ,?_assertEqual(<<"good_standing">>, kz_services:status(Services))
    ,?_assert(kz_json:is_empty(kz_services:services_json(Services)))
    ,?_assertEqual([], kz_services:list_categories(Services))
    ,?_assertEqual([]
                  ,lists:flatmap(fun (Cat) -> kz_services:list_items(Services, Cat) end
                                ,kz_services:list_categories(Services)
                                )
                  )
    ].

fetch_reseller_test_() ->
    AccountId = ?A_RESELLER_ACCOUNT_ID,
    Categories = [<<"ledgers">>,<<"users">>,<<"limits">>,
                  <<"phone_numbers">>,<<"branding">>,<<"ips">>,
                  <<"number_carriers">>,<<"ui_apps">>,
                  <<"billing">>,<<"number_services">>,
                  <<"devices">>,<<"ratedeck_name">>],
    Items = [<<"per-minute-voip">>,<<"admin">>,<<"user">>,
             <<"twoway_trunks">>,<<"outbound_trunks">>,
             <<"inbound_trunks">>,<<"tollfree_us">>,
             <<"did_us">>,<<"whitelabel">>,<<"dedicated">>,
             <<"knm_inventory">>,<<"knm_local">>,
             <<"knm_bandwidth2">>,<<"fax">>,<<"localDIDs">>,
             <<"devices">>,<<"dash_e911">>,<<"local">>,
             <<"failover">>,<<"mobile">>,<<"sip_device">>],
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    Plans = kzd_services:plans(ServicesJObj),
    [?_assertEqual(AccountId, kz_services:account_id(Services))
    ,?_assertEqual(AccountId, kz_services:get_billing_id(Services))
    ,?_assertEqual(AccountId, kzd_services:billing_id(ServicesJObj))
    ,?_assertEqual(AccountId, kz_services:current_billing_id(Services))
    ,?_assertEqual(?A_MASTER_ACCOUNT_ID, kzd_services:reseller_id(ServicesJObj))
    ,?_assert(kzd_services:is_reseller(ServicesJObj))
    ,?_assert(kz_services:is_reseller(ServicesJObj))
    ,?_assert(kz_services:is_reseller(AccountId))
    ,?_assert(not kz_services:is_dirty(Services))
    ,?_assert(not kzd_services:is_dirty(ServicesJObj))
    ,?_assert(not kz_services:is_deleted(Services))
    ,?_assert(not kzd_services:is_deleted(ServicesJObj))
    ,?_assertEqual(<<"good_standing">>, kz_services:status(Services))
    ,?_assertEqual(kzd_services:status_good(), kzd_services:status(ServicesJObj))
    ,?_assertEqual([?A_MASTER_ACCOUNT_ID], kzd_services:tree(ServicesJObj))
    ,?_assertEqual(undefined, kzd_services:reason(ServicesJObj))
    ,?_assertEqual(undefined, kzd_services:reason_code(ServicesJObj))
    ,?_assertEqual(kzd_services:type(), kzd_services:type(ServicesJObj))
    ,?_assert(not kz_json:is_empty(Plans))
    ,?_assertEqual([?A_MASTER_PLAN_ID], kz_json:get_keys(Plans))
    ,?_assertEqual([?A_MASTER_PLAN_ID], kzd_services:plan_ids(ServicesJObj))
    ,?_assert(not kz_json:is_empty(kzd_services:plan(ServicesJObj, ?A_MASTER_PLAN_ID)))
    ,?_assertEqual(?A_MASTER_ACCOUNT_ID, kzd_services:plan_account_id(ServicesJObj, ?A_MASTER_PLAN_ID))
    ,?_assert(kz_json:is_empty(kzd_services:plan_overrides(ServicesJObj, ?A_MASTER_PLAN_ID)))
    ,?_assert(not kz_json:is_empty(kzd_services:quantities(ServicesJObj)))
    ,?_assert(not kz_json:is_empty(kzd_services:category_quantities(ServicesJObj, <<"phone_numbers">>)))
    ,?_assertEqual(9, kzd_services:item_quantity(ServicesJObj, <<"phone_numbers">>, <<"did_us">>))
    ,?_assertEqual([], kzd_services:transactions(ServicesJObj))
    ,?_assertEqual(Categories, kz_services:list_categories(Services))
    ,?_assertEqual(Items
                  ,lists:flatmap(fun (Cat) -> kz_services:list_items(Services, Cat) end
                                ,kz_services:list_categories(Services)
                                )
                  )
    ].

new_unrelated_test_() ->
    AccountId = ?UNRELATED_ACCOUNT_ID,
    Services = kz_services:fetch(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    Plans = kzd_services:plans(ServicesJObj),
    [?_assertEqual(AccountId, kz_services:account_id(Services))
    ,?_assertEqual(AccountId, kz_services:get_billing_id(Services))
    ,?_assertEqual(AccountId, kzd_services:billing_id(ServicesJObj))
    ,?_assertEqual(AccountId, kz_services:current_billing_id(Services))
    ,?_assertEqual(?A_MASTER_ACCOUNT_ID, kzd_services:reseller_id(ServicesJObj))
    ,?_assert(kzd_services:is_reseller(ServicesJObj))
    ,?_assert(kz_services:is_reseller(ServicesJObj))
    ,?_assert(kz_services:is_reseller(AccountId))

     %% The dirtyness need not be reflected in the JSON version,
     %% so these 2 being different should be OK.
    ,?_assert(kz_services:is_dirty(Services))
    ,?_assert(not kzd_services:is_dirty(ServicesJObj))

    ,?_assert(not kz_services:is_deleted(Services))
    ,?_assert(not kzd_services:is_deleted(ServicesJObj))
    ,?_assertEqual(<<"good_standing">>, kz_services:status(Services))
    ,?_assertEqual(kzd_services:status_good(), kzd_services:status(ServicesJObj))
    ,?_assertEqual([?A_MASTER_ACCOUNT_ID], kzd_services:tree(ServicesJObj))
    ,?_assertEqual(undefined, kzd_services:reason(ServicesJObj))
    ,?_assertEqual(undefined, kzd_services:reason_code(ServicesJObj))
    ,?_assertEqual(kzd_services:type(), kzd_services:type(ServicesJObj))
    ,?_assert(kz_json:is_empty(Plans))
    ,?_assertEqual([], kz_json:get_keys(Plans))
    ,?_assertEqual([], kzd_services:plan_ids(ServicesJObj))
    ,?_assert(kz_json:is_empty(kzd_services:quantities(ServicesJObj)))
    ,?_assert(kz_json:is_empty(kzd_services:category_quantities(ServicesJObj, <<"phone_numbers">>)))
    ,?_assertEqual(0, kzd_services:item_quantity(ServicesJObj, <<"phone_numbers">>, <<"did_us">>))
    ,?_assertEqual([], kzd_services:transactions(ServicesJObj))
    ,?_assertEqual([], kz_services:list_categories(Services))
    ,?_assertEqual([]
                  ,lists:flatmap(fun (Cat) -> kz_services:list_items(Services, Cat) end
                                ,kz_services:list_categories(Services)
                                )
                  )
    ].

add_delete_service_plan_test_() ->
    PlanId = ?A_MASTER_PLAN_ID,
    Services0 = kz_services:fetch(?UNRELATED_ACCOUNT_ID),
    Services1 = kz_services:add_service_plan(PlanId, Services0),
    Services2 = kz_services:delete_service_plan(PlanId, Services1),
    [?_assertEqual([], kzd_services:plan_ids(kz_services:services_json(Services0)))
    ,?_assertEqual([PlanId], kzd_services:plan_ids(kz_services:services_json(Services1)))
    ,?_assertEqual([], kzd_services:plan_ids(kz_services:services_json(Services2)))
    ].

add_delete_save_service_plan_test_() ->
    PlanId = ?A_MASTER_PLAN_ID,
    Services0 = kz_services:fetch(?UNRELATED_ACCOUNT_ID),
    Services1 = kz_services:add_service_plan(PlanId, Services0),
    Services2 = kz_services:delete_service_plan(PlanId, Services1),
    Saved0 = kz_services:save(Services0),
    Saved1 = kz_services:save(Services1),
    Saved2 = kz_services:save(Services2),
    [?_assert(kz_json:are_equal(kz_services:services_json(Services0)
                               ,kz_services:services_json(Services2)
                               )
             )
    ,?_assert(kz_json:are_equal(kz_services:services_json(Services1)
                               ,kz_json:delete_key(?SERVICES_PVT_IS_DIRTY, kz_services:services_json(Saved1))
                               )
             )
    ,?_assert(kzd_services:is_dirty(kz_services:services_json(Saved1)))
    ,?_assert(kz_json:are_equal(kz_services:services_json(Saved0)
                               ,kz_services:services_json(Saved2)
                               )
             )
    ].

add_delete_save_as_dirty_test_() ->
    PlanId = ?A_MASTER_PLAN_ID,
    AccountId = ?UNRELATED_ACCOUNT_ID,
    Services0 = kz_services:fetch(AccountId),
    Services1 = kz_services:add_service_plan(PlanId, Services0),
    Services2 = kz_services:delete_service_plan(PlanId, Services1),
    Saved0 = kz_services:save_as_dirty(AccountId),
    Saved1 = kz_services:save_as_dirty(Services1),
    Saved2 = kz_services:save_as_dirty(Services2),
    [?_assert(kz_json:are_equal(kz_services:services_json(Services0)
                               ,kz_services:services_json(Services2)
                               )
             )
    ,?_assert(kz_json:are_equal(kz_services:services_json(Services1)
                               ,kz_json:delete_key(?SERVICES_PVT_IS_DIRTY, kz_services:services_json(Saved1))
                               )
             )
    ,?_assert(kzd_services:is_dirty(kz_services:services_json(Saved1)))
    ,?_assert(kz_json:are_equal(kz_services:services_json(Saved0)
                               ,kz_services:services_json(Saved2)
                               )
             )
    ].

save_as_dirty_conflict_test_() ->
    AccountId = ?B_SUB_ACCOUNT_ID,
    {ok, ServicesJObj0} = kz_services:fetch_services_doc(AccountId),
    Services = kz_services:save_as_dirty(AccountId),
    ServicesJObj = kz_services:services_json(Services),
    [?_assert(kz_json:are_equal(kz_json:delete_key(<<"pvt_modified">>, ServicesJObj0)
                               ,kz_json:delete_key(<<"pvt_modified">>, ServicesJObj)
                               )
             )
    ,?_assert(kzd_services:is_dirty(ServicesJObj))
    ].

delete_test_() ->
    AccountId = ?A_RESELLER_ACCOUNT_ID,
    {ok, ServicesJObj0} = kz_services:fetch_services_doc(AccountId, true),
    {ok, ServicesJObj} = kz_services:delete(AccountId),
    Services = kz_services:from_service_json(ServicesJObj),
    Keys = [?SERVICES_PVT_IS_DIRTY, ?SERVICES_PVT_IS_DELETED],
    [?_assert(kz_json:are_equal(kz_json:delete_keys(Keys, ServicesJObj0)
                               ,kz_json:delete_keys(Keys, ServicesJObj)
                               )
             )
    ,?_assert(kzd_services:is_dirty(ServicesJObj))
    ,?_assert(kzd_services:is_deleted(ServicesJObj))
    ,?_assertEqual({ok,kz_json:new()}, kz_services:delete(?UNRELATED_ACCOUNT_ID))
    ,?_assertMatch({error,_}, kz_services:delete(?WRONG_ACCOUNT_ID))
    ,?_assertEqual(undefined, kz_services:diff_quantities(Services))
    ,?_assertEqual(0, kz_services:diff_quantity(?CAT, ?ITEM, Services))
    ,?_assertEqual(0, kz_services:updated_quantity(?CAT, ?ITEM, Services))
    ,?_assertEqual(0, kz_services:category_quantity(?CAT, Services))
    ,?_assertEqual(0, kz_services:cascade_quantity(?CAT, ?ITEM, Services))
    ,?_assertEqual(0, kz_services:cascade_category_quantity(?CAT, Services))
    ,?_assert(not kz_json:is_empty(kz_services:cascade_quantities(Services)))
     | quantity_checks(ServicesJObj, Services, true)
    ].

reset_category_test_() ->
    Services = kz_services:fetch(?A_RESELLER_ACCOUNT_ID),
    ServicesWithNewQ = kz_services:update(?CAT, ?ITEM, 42, Services),
    ServicesNowReset = kz_services:reset_category(?CAT, ServicesWithNewQ),
    ServicesWithZeroQ = kz_services:update(?CAT, ?ITEM, 0, ServicesNowReset),
    [?_assertEqual(9, kz_services:quantity(?CAT, ?ITEM, Services))
    ,?_assertEqual(0, kz_services:updated_quantity(?CAT, ?ITEM, Services))
    ,?_assert(kz_json:are_equal(kz_json:new(), kz_services:diff_quantities(Services)))
    ,?_assertEqual(10, kz_services:category_quantity(?CAT, Services))
    ,?_assertEqual(16, kz_services:cascade_quantity(?CAT, ?ITEM, Services))
    ,?_assertEqual(17, kz_services:cascade_category_quantity(?CAT, Services))
    ,?_assertEqual(kz_json:new(), kz_services:dry_run(Services))
     %% ServicesWithNewQ
    ,?_assertEqual(42, kz_services:quantity(?CAT, ?ITEM, ServicesWithNewQ))
    ,?_assertEqual(42, kz_services:updated_quantity(?CAT, ?ITEM, ServicesWithNewQ))
    ,?_assert(kz_json:are_equal(kz_json:decode(<<"{\"phone_numbers\":{\"did_us\":33}}">>)
                               ,kz_services:diff_quantities(ServicesWithNewQ)
                               )
             )
    ,?_assertEqual(43, kz_services:category_quantity(?CAT, ServicesWithNewQ))
    ,?_assertEqual(49, kz_services:cascade_quantity(?CAT, ?ITEM, ServicesWithNewQ))
    ,?_assertEqual(50, kz_services:cascade_category_quantity(?CAT, ServicesWithNewQ))
    ,?_assert(kz_json:are_equal(dry_run(newq), kz_services:dry_run(ServicesWithNewQ)))
     %% ServicesNowReset
    ,?_assertEqual(9, kz_services:quantity(?CAT, ?ITEM, ServicesNowReset))
    ,?_assertEqual(0, kz_services:updated_quantity(?CAT, ?ITEM, ServicesNowReset))
    ,?_assert(kz_json:are_equal(kz_json:decode(<<"{\"phone_numbers\":{}}">>)
                               ,kz_services:diff_quantities(ServicesNowReset)
                               )
             )
    ,?_assertEqual(10, kz_services:category_quantity(?CAT, ServicesNowReset))
    ,?_assertEqual(16, kz_services:cascade_quantity(?CAT, ?ITEM, ServicesNowReset))
    ,?_assertEqual(17, kz_services:cascade_category_quantity(?CAT, ServicesNowReset))
    ,?_assertEqual(kz_json:new(), kz_services:dry_run(ServicesNowReset))
     %% ServicesWithZeroQ
    ,?_assertEqual(0, kz_services:quantity(?CAT, ?ITEM, ServicesWithZeroQ))
    ,?_assertEqual(0, kz_services:updated_quantity(?CAT, ?ITEM, ServicesWithZeroQ))
    ,?_assert(kz_json:are_equal(kz_json:decode(<<"{\"phone_numbers\":{\"did_us\":-9}}">>)
                               ,kz_services:diff_quantities(ServicesWithZeroQ)
                               )
             )
    ,?_assertEqual(1, kz_services:category_quantity(?CAT, ServicesWithZeroQ))
    ,?_assertEqual(7, kz_services:cascade_quantity(?CAT, ?ITEM, ServicesWithZeroQ))
    ,?_assertEqual(8, kz_services:cascade_category_quantity(?CAT, ServicesWithZeroQ))
    ,?_assert(kz_json:are_equal(dry_run(zeroq), kz_services:dry_run(ServicesWithZeroQ)))
    ].

dry_run(newq) ->
    kz_json:decode(
      <<"{"
        "\"phone_numbers\": {"
        "\"did_us\": {"
        "\"category\": \"phone_numbers\","
        "\"item\": \"did_us\","
        "\"name\": \"US DID\","
        "\"quantity\": 42,"
        "\"rate\": 3.0,"
        "\"single_discount\": true,"
        "\"single_discount_rate\": 3.0,"
        "\"cumulative_discount\": 2,"
        "\"cumulative_discount_rate\": 5.0,"
        "\"activation_charge\": 42.0,"
        "\"minimum\": 0,"
        "\"exceptions\": [],"
        "\"activate_quantity\": 33,"
        "\"activation_charges\": 42.0"
        "}"
        "},"
        "\"activation_charges\": 1386.0"
        "}"
      >>);
dry_run(zeroq) ->
    kz_json:decode(
      <<"{"
        "\"phone_numbers\": {"
        "\"did_us\": {"
        "\"activate_quantity\": -9,"
        "\"activation_charges\": 42.0"
        "}"
        "},"
        "\"activation_charges\": -378.0"
        "}"
      >>).

set_billing_id_test_() ->
    MA = ?A_MASTER_ACCOUNT_ID,
    RA = ?A_RESELLER_ACCOUNT_ID,
    SA = ?A_SUB_ACCOUNT_ID,
    ServicesReseller = kz_services:fetch(RA),
    [?_assertEqual(RA, kz_services:get_billing_id(ServicesReseller))
    ,?_assertEqual(RA, kz_services:get_billing_id(RA))
    ,?_assertEqual(SA, kz_services:get_billing_id(SA))
    ,?_assertEqual(MA, kz_services:get_billing_id(MA))
    ,?_assertEqual(?UNRELATED_ACCOUNT_ID, kz_services:get_billing_id(?UNRELATED_ACCOUNT_ID))
    ,?_assertEqual(undefined, kz_services:set_billing_id(undefined, ServicesReseller))
    ,?_assertEqual(undefined, kz_services:set_billing_id(RA, ServicesReseller))
    ,?_assertEqual(MA
                  ,kz_services:get_billing_id(kz_services:set_billing_id(MA, ServicesReseller))
                  )
    ,?_assertThrow({'invalid_billing_id', <<"Requested billing id is not the parent of this account">>}
                  ,kz_services:set_billing_id(SA, ServicesReseller)
                  )
    ,?_assertEqual(RA, kz_services:get_billing_id(kz_services:set_billing_id(RA, SA)))
    ,?_assertThrow({'invalid_billing_id', <<"Requested billing id is not the parent of this account">>}
                  ,kz_services:set_billing_id(MA, SA)
                  )
    ].

activation_charges_test_() ->
    RA = ?A_RESELLER_ACCOUNT_ID,
    [?_assertEqual(42.0, kz_services:activation_charges(<<"phone_numbers">>, <<"did_us">>, RA))
    ,?_assertEqual(10.0, kz_services:activation_charges(<<"number_services">>, <<"port">>, RA))
    ,?_assertEqual(0.0, kz_services:activation_charges(<<"ips">>, <<"dedicated">>, RA))
    ,?_assertEqual(0.0, kz_services:activation_charges(<<"ips">>, <<"blabla">>, RA))
    ].

select_bookkeeper_test_() ->
    Local = kz_bookkeeper_local,
    [?_assertEqual(Local, kz_services:select_bookkeeper(?A_MASTER_ACCOUNT_ID))
    ,?_assertEqual(Local, kz_services:select_bookkeeper(?A_RESELLER_ACCOUNT_ID))
    ,?_assertEqual(Local, kz_services:select_bookkeeper(?A_SUB_ACCOUNT_ID))
    ,?_assertEqual(Local, kz_services:select_bookkeeper(?UNRELATED_ACCOUNT_ID))
    ,?_assertEqual(Local, kz_services:select_bookkeeper(kz_services:fetch(?A_SUB_ACCOUNT_ID)))
    ,?_assertEqual(Local, kz_services:select_bookkeeper(kz_services:fetch(?UNRELATED_ACCOUNT_ID)))
    ].

public_private_json_test_() ->
    test_public_private_json(?A_RESELLER_ACCOUNT_ID)
        ++ test_public_private_json(?A_SUB_ACCOUNT_ID).

test_public_private_json(AccountId) ->
    PubJObj = kz_services:public_json(AccountId),
    JObj = kz_services:to_json(kz_services:fetch(AccountId)),
    [?_assert(kz_json:is_true(<<"in_good_standing">>, PubJObj))
    ,?_assertEqual(<<"good_standing">>, kz_json:get_value(?SERVICES_PVT_STATUS, JObj))
    ,?_assert(kz_json:is_json_object(kz_json:get_value(<<"items">>, PubJObj)))
    ]
        ++ [assert_same(Key, PubJObj, JObj)
            || Key <- [{<<"account_quantities">>, <<"quantities">>}
                      ,<<"billing_id">>
                      ,<<"cascade_quantities">>
                      ,<<"plans">>
                      ,{<<"dirty">>, ?SERVICES_PVT_IS_DIRTY}
                      ,{<<"reseller">>, ?SERVICES_PVT_IS_RESELLER}
                      ,{<<"reseller_id">>, ?SERVICES_PVT_RESELLER_ID}
                      ]
           ]
        ++ [{"Ensuring private key " ++ kz_term:to_list(Key) ++ " is set"
            ,?_assert(kz_json:is_defined(Key, JObj))
            }
            || Key <- [<<"_id">>
                      ,<<"pvt_type">>
                      ,<<"pvt_account_id">>
                      ,<<"pvt_account_db">>
                      ,?SERVICES_PVT_IS_DIRTY
                      ,?SERVICES_PVT_IS_RESELLER
                      ,?SERVICES_PVT_MODIFIED
                      ,?SERVICES_PVT_RESELLER_ID
                      ,?SERVICES_PVT_REV
                      ,?SERVICES_PVT_STATUS
                      ,?SERVICES_PVT_TREE
                      ]
           ]
        ++ case AccountId =:= ?A_RESELLER_ACCOUNT_ID of
               false -> [];
               true ->
                   [?_assert(kz_json:are_equal(kz_json:get_value([<<"items">>, <<"billing">>, <<"_all">>], PubJObj), items_billing_all()))
                   ,?_assert(kz_json:are_equal(kz_json:get_value([<<"items">>, <<"phone_numbers">>, <<"did_us">>], PubJObj), items_phone_numbers_did_us()))
                   ]
           end.

assert_same(Key=?NE_BINARY, PubJObj, JObj) ->
    assert_same({Key, Key}, PubJObj, JObj);
assert_same({PubKey, Key}, PubJObj, JObj) ->
    PubValue = kz_json:get_value(PubKey, PubJObj),
    {"Ensure public " ++ kz_term:to_list(PubKey) ++ " matches " ++ kz_term:to_list(Key)
    ,case kz_json:is_json_object(PubValue) of
         true -> ?_assert(kz_json:are_equal(PubValue, kz_json:get_value(Key, JObj)));
         false -> ?_assertEqual(PubValue, kz_json:get_value(Key, JObj))
     end
    }.

items_billing_all() ->
    kz_json:decode(
      <<"{"
        "\"activation_charge\": 0.0,"
        "\"category\": \"billing\","
        "\"cumulative_discount\": 0,"
        "\"cumulative_discount_rate\": 0.0,"
        "\"exceptions\": ["
        "],"
        "\"item\": \"_all\","
        "\"minimum\": 0,"
        "\"quantity\": 50,"
        "\"rate\": 24.99,"
        "\"single_discount\": true,"
        "\"single_discount_rate\": 0.0"
        "}"
      >>).

items_phone_numbers_did_us() ->
    kz_json:decode(
      <<"{"
        "\"activation_charge\": 42.0,"
        "\"category\": \"phone_numbers\","
        "\"cumulative_discount\": 2,"
        "\"cumulative_discount_rate\": 5.0,"
        "\"exceptions\": ["
        "],"
        "\"item\": \"did_us\","
        "\"minimum\": 0,"
        "\"name\": \"US DID\","
        "\"quantity\": 9,"
        "\"rate\": 3.0,"
        "\"single_discount\": true,"
        "\"single_discount_rate\": 3.0"
        "}"
      >>).

allow_updates_test_() ->
    [?_assert(kz_services:allow_updates(?A_MASTER_ACCOUNT_ID))
    ,?_assert(kz_services:allow_updates(?A_RESELLER_ACCOUNT_ID))
    ,?_assert(kz_services:allow_updates(?A_SUB_ACCOUNT_ID))
    ,?_assert(kz_services:allow_updates(?B_SUB_ACCOUNT_ID))
    ,?_assert(kz_services:allow_updates(?UNRELATED_ACCOUNT_ID))
    ,?_assert(kz_services:allow_updates(kz_services:fetch(?A_RESELLER_ACCOUNT_ID)))
    ,?_assert(kz_services:allow_updates(kz_services:fetch(?A_SUB_ACCOUNT_ID)))
    ,?_assert(kz_services:allow_updates(kz_services:fetch(?B_SUB_ACCOUNT_ID)))
    ,?_assert(kz_services:allow_updates(kz_services:fetch(?UNRELATED_ACCOUNT_ID)))
    ].

reconcile_test_() ->
    [?_assert(not kz_services:reconcile_only(undefined))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?A_RESELLER_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?A_SUB_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?B_SUB_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?UNRELATED_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(kz_services:fetch(?UNRELATED_ACCOUNT_ID))))
    ,?_assert(not kz_services:reconcile(undefined))
    ,?_assert(kz_services:is_services(kz_services:reconcile(?A_RESELLER_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile(?A_SUB_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile(?UNRELATED_ACCOUNT_ID)))
    ,?_assert(kz_services:is_services(kz_services:reconcile(kz_services:fetch(?UNRELATED_ACCOUNT_ID))))
    ,?_assert(not kz_services:reconcile_only(undefined, kz_service_ledgers))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?UNRELATED_ACCOUNT_ID, <<"ledgers">>)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?UNRELATED_ACCOUNT_ID, kz_service_ips)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(kz_services:fetch(?UNRELATED_ACCOUNT_ID), kz_service_ips)))
    ,?_assert(kz_services:is_services(kz_services:reconcile_only(?A_RESELLER_ACCOUNT_ID, kz_service_devices)))
    ,?_assert(not kz_services:reconcile_only(?UNRELATED_ACCOUNT_ID, not_a_service_module))
    ,?_assert(not kz_services:reconcile_only(?UNRELATED_ACCOUNT_ID, <<"not_a_service_module">>))
    ,?_assert(not kz_services:reconcile(undefined, kz_service_ledgers))
    ,?_assert(kz_services:is_services(kz_services:reconcile(?UNRELATED_ACCOUNT_ID, <<"ledgers">>)))
    ,?_assert(kz_services:is_services(kz_services:reconcile(?UNRELATED_ACCOUNT_ID, kz_service_ips)))
    ,?_assert(kz_services:is_services(kz_services:reconcile(kz_services:fetch(?UNRELATED_ACCOUNT_ID), kz_service_ips)))
    ,?_assert(kz_services:is_services(kz_services:reconcile(?A_RESELLER_ACCOUNT_ID, kz_service_devices)))
    ,?_assert(not kz_services:reconcile(?UNRELATED_ACCOUNT_ID, not_a_service_module))
    ,?_assert(not kz_services:reconcile(?UNRELATED_ACCOUNT_ID, <<"not_a_service_module">>))
    ,?_assert(not kz_services:is_services(kz_json:new()))
    ].

modules_test_() ->
    Modules = kz_services:get_service_modules(),
    [?_assert(lists:all(fun is_atom/1, Modules))
    ,?_assertEqual(length(Modules), length(lists:usort(Modules)))
    ,?_assertEqual(10, length(Modules))
    ,?_assertEqual(kz_service_ledgers, kz_services:get_service_module(ledgers))
    ]
        ++ [[?_assertEqual(M, kz_services:get_service_module(M))
            ,?_assert(is_of_behaviour(kz_gen_service, M))
            ]
            || M <- Modules
           ].

is_of_behaviour(Behaviour, Module) ->
    {behaviour, Behaviours} =
        lists:keyfind(behaviour, 1, Module:module_info(attributes)),
    lists:member(Behaviour, Behaviours).

check_bookkeeper_test_() ->
    [?_assert(kz_services:check_bookkeeper(?UNRELATED_ACCOUNT_ID, 10))
    ].

transactions_test_() ->
    AccountId = ?A_RESELLER_ACCOUNT_ID,
    Services = kz_services:fetch(AccountId),
    Cost = kz_services:activation_charges(<<"number_services">>, <<"port">>, Services),
    Transaction0 = kz_transaction:debit(AccountId, wht_util:dollars_to_units(Cost)),
    Transaction = kz_transaction:set_reason(wht_util:number_activation(), Transaction0),
    JObj = kz_transaction:to_json(Transaction),
    PubJObj = kz_transaction:to_public_json(Transaction),
    [?_assertEqual(ok, kz_services:commit_transactions(Services, [Transaction]))
    ,?_assertEqual([], kz_services:charge_transactions(Services, [Transaction]))
    ,?_assertEqual(10.0, kz_json:get_value(<<"amount">>, PubJObj))
    ,?_assertEqual(100000, kz_json:get_value(<<"pvt_amount">>, JObj))
    ]
        ++ [assert_same(Key, PubJObj, JObj)
            || Key <- [{<<"code">>, <<"pvt_code">>}
                      ,{<<"created">>, <<"pvt_created">>}
                      ,{<<"reason">>, <<"pvt_reason">>}
                      ,{<<"type">>, <<"pvt_type">>}
                      ,{<<"version">>, <<"pvt_vsn">>}
                      ]
           ].
