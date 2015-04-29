%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_services_test).

-include_lib("eunit/include/eunit.hrl").

-record(state, {service_plan :: wh_json:object()
                ,services :: wh_services:services()
                ,services_jobj :: wh_json:object()
               }).

services_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun services_json_to_record/1
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

    State#state{service_plan=read_json(ServicePlan1)}.

read_services(State) ->
    Services = filename:join([priv_dir(), "example_account_services.json"]),

    JObj = read_json(Services),
    State#state{services_jobj=JObj
                ,services=wh_services:from_service_json(JObj, 'false')
               }.

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

priv_dir() ->
    {'ok', AppDir} = file:get_cwd(),
    filename:join([AppDir, "priv"]).
