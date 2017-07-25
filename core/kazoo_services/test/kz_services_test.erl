%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_services_test).

-export([fixture/1]).

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
    ServicePlanJObj = fixture(ServicePlanFixture),
    ServicesJObj = fixture(ServicesFixture),
    Overrides = kzd_services:plan_overrides(ServicesJObj, kz_doc:id(ServicePlanJObj)),
    #state{services_jobj = ServicesJObj
          ,services = kz_services:from_service_json(ServicesJObj, false)
          ,service_plan_jobj = ServicePlanJObj
          ,account_plan = kzd_service_plan:merge_overrides(ServicePlanJObj, Overrides)
          ,no_overrides = kz_json:is_empty(Overrides)
          }.

-spec fixture(nonempty_string()) -> binary() | kz_json:object().
fixture(Filename) ->
    Path = filename:join([code:lib_dir(?APP), "test", Filename]),
    ?LOG_DEBUG("reading fixture ~s", [Path]),
    {ok, Bin} = file:read_file(Path),
    case lists:suffix(".json", Filename) of
        false -> Bin;
        true -> kz_json:decode(Bin)
    end.

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
     | quantity_checks(JObj, Services)
    ].

services_record_to_json(#state{services = Services}) ->
    JObj = kz_services:to_json(Services),
    [{"Verify account id is set properly"
     ,?_assertEqual(kz_doc:account_id(JObj), kz_services:account_id(Services))
     }
    ,{"Verify the dirty flag is set properly"
     ,?_assertEqual(kzd_services:is_dirty(JObj), kz_services:is_dirty(Services))
     }
    ,{"Verify the billing id"
     ,?_assertEqual(kzd_services:billing_id(JObj), kz_services:get_billing_id(Services))
     }
     | quantity_checks(JObj, Services)
    ].

quantity_checks(JObj, Services) ->
    [category_checks(Category, CategoryJObj, Services)
     || {Category, CategoryJObj} <- kz_json:to_proplist(kzd_services:quantities(JObj))
    ].

category_checks(Category, CategoryJObj, Services) ->
    [item_check(Category, Item, Quantity, Services)
     || {Item, Quantity} <- kz_json:to_proplist(CategoryJObj)
    ].

item_check(Category, Item, Quantity, Services) ->
    {iolist_to_binary(io_lib:format("Verify ~s.~s is ~p", [Category, Item, Quantity]))
    ,?_assertEqual(Quantity, kz_services:quantity(Category, Item, Services))
    }.

service_plan_json_to_plans(#state{service_plan_jobj = ServicePlan
                                 ,account_plan = AccountPlan
                                 ,services = Services
                                 ,no_overrides = false
                                 }) ->
    [{"Verify plan from file matches services plan"
     ,?_assertEqual(kz_doc:account_id(ServicePlan), kzd_service_plan:account_id(AccountPlan))
     }
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
    [{"Verify plan from file matches services plan"
     ,?_assertEqual(kz_doc:account_id(ServicePlan), kzd_service_plan:account_id(AccountPlan))
     }
    ,{"Verify cumulative discount rate from service plan (set as int)"
     ,?_assertEqual(5.0, rate(cumulative_discount(did_us_item(ServicePlan))))
     }
    ,{"Verify cumulative discount rate was not overridden"
     ,?_assertEqual(undefined, did_us_item(AccountPlan))
     }
    ,{"Since ?A_RESELLER_ACCOUNT_ID does not have overrides, verify none were applied"
     ,?_assert(kz_json:are_equal(ServicePlan, kz_services:service_plan_json(Services)))
     }
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
    Increment = rand:uniform(10),
    UpdatedServices = kz_services:update(?CAT, ?ITEM, ItemQuantity + Increment, Services),
    UpdatedItemQuantity = kz_services:quantity(?CAT, ?ITEM, UpdatedServices),
    DiffItemQuantity = kz_services:diff_quantity(?CAT, ?ITEM, UpdatedServices),
    [{"Verify base quantity on the services doc"
     ,?_assertEqual(9, ItemQuantity)
     }
    ,{"Verify incrementing the quantity"
     ,?_assertEqual(ItemQuantity + Increment, UpdatedItemQuantity)
     }
    ,{"Verify the diff of the quantity"
     ,?_assertEqual(Increment, DiffItemQuantity)
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
     ,?_assertEqual(CategoryQuantity + Increment, UpdatedCategoryQuantity)
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
    ,?_assertEqual(kz_json:new(), kz_services:services_json(Services))
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
    [?_assertEqual(AccountId, kz_services:account_id(Services))
    ,?_assertEqual(AccountId, kz_services:get_billing_id(Services))
    ,?_assertEqual(false, kz_services:is_dirty(Services))
    ,?_assertEqual(false, kz_services:is_deleted(Services))
    ,?_assertEqual(<<"good_standing">>, kz_services:status(Services))
    ,?_assert(not kz_json:is_empty(kz_services:services_json(Services)))
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
    [?_assertEqual(AccountId, kz_services:account_id(Services))
    ,?_assertEqual(AccountId, kz_services:get_billing_id(Services))
    ,?_assertEqual(true, kz_services:is_dirty(Services))
    ,?_assertEqual(false, kz_services:is_deleted(Services))
    ,?_assertEqual(<<"good_standing">>, kz_services:status(Services))
    ,?_assert(not kz_json:is_empty(kz_services:services_json(Services)))
    ,?_assertEqual([], kz_services:list_categories(Services))
    ,?_assertEqual([]
                  ,lists:flatmap(fun (Cat) -> kz_services:list_items(Services, Cat) end
                                ,kz_services:list_categories(Services)
                                )
                  )
    ].

add_service_plan_test_() ->
    PlanId = ?A_MASTER_PLAN_ID,
    Services0 = kz_services:fetch(?UNRELATED_ACCOUNT_ID),
    Services1 = kz_services:add_service_plan(PlanId, Services0),
    Services2 = kz_services:delete_service_plan(PlanId, Services0),
    [?_assertEqual([], kzd_services:plan_ids(kz_services:services_json(Services0)))
    ,?_assertEqual([PlanId], kzd_services:plan_ids(kz_services:services_json(Services1)))
    ,?_assertEqual([], kzd_services:plan_ids(kz_services:services_json(Services2)))
    ].
