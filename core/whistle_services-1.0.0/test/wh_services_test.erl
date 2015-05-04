%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services_test).

-include_lib("eunit/include/eunit.hrl").

-record(state, {service_plan_jobj :: kzd_service_plan:plan()
                ,services :: wh_services:services()
                ,services_jobj :: wh_json:object()
               }).

services_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun services_json_to_record/1
       ,fun services_record_to_json/1
       ,fun service_plan_json_to_plans/1
       ,fun service_plans_to_json/1
      ]
    }.

init() ->
    lists:foldl(fun init_fold/2
                ,#state{}
                ,[fun read_service_plan/1
                  ,fun read_services/1
                 ]).

init_fold(F, State) ->
    F(State).

stop(#state{}=_State) ->
    'ok'.

read_service_plan(State) ->
    ServicePlan1 = filename:join([priv_dir(), "example_service_plan_1.json"]),
    JObj = read_json(ServicePlan1),

    State#state{service_plan_jobj=JObj}.

read_services(State) ->
    Services = filename:join([priv_dir(), "example_account_services.json"]),

    JObj = read_json(Services),
    State#state{services_jobj=JObj
                ,services=wh_services:from_service_json(JObj, 'false')
               }.

priv_dir() ->
    {'ok', AppDir} = file:get_cwd(),
    filename:join([AppDir, "priv"]).

read_json(Path) ->
    {'ok', JSON} = file:read_file(Path),
    wh_json:decode(JSON).

services_json_to_record(#state{services=Services
                               ,services_jobj=JObj
                              }) ->
    [{"Verify account id is set properly"
      ,?_assertEqual(wh_doc:account_id(JObj)
                     ,wh_services:account_id(Services)
                    )
     }
     ,{"Verify the dirty flag is set properly"
       ,?_assertEqual(kzd_services:is_dirty(JObj)
                      ,wh_services:is_dirty(Services)
                     )
      }
     ,{"Verify the billing id"
       ,?_assertEqual(kzd_services:billing_id(JObj)
                      ,wh_services:get_billing_id(Services)
                     )
      }
     | quantity_checks(Services, JObj)
    ].

services_record_to_json(#state{services=Services}) ->
    JObj = wh_services:to_json(Services),
    [{"Verify account id is set properly"
      ,?_assertEqual(wh_doc:account_id(JObj)
                     ,wh_services:account_id(Services)
                    )
     }
     ,{"Verify the dirty flag is set properly"
       ,?_assertEqual(kzd_services:is_dirty(JObj)
                      ,wh_services:is_dirty(Services)
                     )
      }
     ,{"Verify the billing id"
       ,?_assertEqual(kzd_services:billing_id(JObj)
                      ,wh_services:get_billing_id(Services)
                     )
      }
     | quantity_checks(Services, JObj)
    ].

quantity_checks(Services, JObj) ->
    {Tests, _} = wh_json:foldl(fun category_checks/3
                               ,{[], Services}
                               ,kzd_services:quantities(JObj)
                              ),
    Tests.

category_checks(Category, CategoryJObj, Acc) ->
    wh_json:foldl(fun(K, V, Acc1) ->
                          item_checks(Category, K, V, Acc1)
                  end
                  ,Acc
                  ,CategoryJObj
                 ).

item_checks(Category, Item, Quantity, {Tests, Services}) ->
    {[item_check(Category, Item, Quantity, Services) | Tests]
     ,Services
    }.

item_check(Category, Item, Quantity, Services) ->
    {iolist_to_binary(io_lib:format("Verify ~s.~s is ~p", [Category, Item, Quantity]))
     ,?_assertEqual(Quantity, wh_services:quantity(Category, Item, Services))
    }.

service_plan_json_to_plans(#state{service_plan_jobj=ServicePlan
                                  ,services_jobj=Services
                                 }) ->
    Overrides = kzd_services:plan_overrides(Services, wh_doc:id(ServicePlan)),
    AccountPlan = kzd_service_plan:merge_overrides(ServicePlan, Overrides),

    [{"Verify plan from file matches services plan"
      ,?_assertEqual(wh_doc:account_id(ServicePlan)
                     ,kzd_service_plan:account_id(AccountPlan)
                    )
     }
     ,{"Verify cumulative discount rate from service plan"
       ,?_assertEqual(0.5, rate(cumulative_discount(did_us_item(ServicePlan))))
      }
     ,{"Verify cumulative discount rate was overridden"
       ,?_assertEqual(5.0, rate(cumulative_discount(did_us_item(AccountPlan))))
      }
    ].

service_plans_to_json(_State) ->
    [].

did_us_item(Plan) ->
    kzd_service_plan:item(Plan, <<"phone_numbers">>, <<"did_us">>).

cumulative_discount(Item) ->
    kzd_item_plan:cumulative_discount(Item).

rate(JObj) ->
    wh_json:get_float_value(<<"rate">>, JObj).
