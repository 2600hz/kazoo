%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_port_request_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").


n_x(X, Ret) ->
    lists:nth(X, maps:get(ok, Ret)).

pn_x(X, Ret) ->
    knm_number:phone_number(n_x(X, Ret)).

base() ->
    [{assign_to, ?RESELLER_ACCOUNT_ID}
    ,{dry_run, false}
    ,{<<"auth_by_account">>
     ,kz_account:set_allow_number_additions(?RESELLER_ACCOUNT_DOC, false)
     }
    ].

transition_port_from_port_in_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    #{ok := [N]} = knm_numbers:create([?TEST_PORT_IN_NUM], Options),
    PN = knm_number:phone_number(N),
    [{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?PORT_IN_MODULE_NAME, knm_phone_number:module_name(PN))
     }
    ,{"Verify local number is not billable"
     ,?_assertEqual(false, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is not marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_from_available_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    #{ok := [N]} = knm_numbers:create([?TEST_AVAILABLE_NUM], Options),
    PN = knm_number:phone_number(N),
    [{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the local carrier module is being used"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PN))
     }
    ,{"Verify local number is not billable"
     ,?_assertEqual(false, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is not marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_in_not_specifying1_test_() ->
    Options1 = [{auth_by,?MASTER_ACCOUNT_ID} | base()],
    Options2 = [{auth_by,?KNM_DEFAULT_AUTH_BY} | base()],
    Num = ?TEST_PORT_IN_NUM,
    Error = kz_json:from_list(
              %% Was not instructed Num is being ported_in,
              %% thus the wrong target state is picked and fails.
              [{<<"code">>, 400},
               {<<"error">>, <<"invalid_state_transition">>},
               {<<"cause">>, <<"from port_in to reserved">>},
               {<<"message">>, <<"invalid state transition">>}
              ]),
    [{"Verify cannot create in_service number if not specified as ported_in"
     ,?_assertMatch(#{ko := #{Num := Error}}, knm_numbers:create([Num], Options1))
     }
    ,{"Verify cannot create in_service number if not specified as ported_in even as sudo"
     ,?_assertMatch(#{ko := #{Num := Error}}, knm_numbers:create([Num], Options2))
     }
    ].

transition_port_in_not_specifying2_test_() ->
    Options1 = [{auth_by,?MASTER_ACCOUNT_ID} | base()],
    Options2 = [{auth_by,?KNM_DEFAULT_AUTH_BY} | base()],
    Num = ?TEST_AVAILABLE_NUM,
    Ret1 = knm_numbers:create([Num], Options1),
    Ret2 = knm_numbers:create([Num], Options2),
    [{"Verify number create has nothing to do with ports"
     ,?_assertNotEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(pn_x(1, Ret1)))
     }
    ,?_assertNotEqual(?NUMBER_STATE_PORT_IN, knm_phone_number:state(pn_x(1, Ret1)))
    ,{"Verify number create has nothing to do with ports and is not ported_in"
     ,?_assertEqual(false, knm_phone_number:ported_in(pn_x(1, Ret1)))
     }
    ,{"Verify number create has nothing to do with ports"
     ,?_assertNotEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(pn_x(1, Ret2)))
     }
    ,?_assertNotEqual(?NUMBER_STATE_PORT_IN, knm_phone_number:state(pn_x(1, Ret2)))
    ,{"Verify number create has nothing to do with ports and is not ported_in"
     ,?_assertEqual(false, knm_phone_number:ported_in(pn_x(1, Ret2)))
     }
    ].
