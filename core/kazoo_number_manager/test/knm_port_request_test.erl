%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_port_request_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").


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
    {ok, N} = knm_number:create(?TEST_PORT_IN_NUM, Options),
    PN = knm_number:phone_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history"
     ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the configured port in carrier module is being used"
     ,?_assertEqual(?PORT_IN_MODULE_NAME, knm_phone_number:module_name(PN))
     }
    ,{"Verify local number is not billable"
     ,?_assertEqual(false, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_from_port_in_with_different_module_configured_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, N} = knm_number:create(?TEST_PORT_IN2_NUM, Options),
    PN = knm_number:phone_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history"
     ,?_assertEqual([?RESELLER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the configured port in module name is being used"
     ,?_assertEqual(<<"knm_telnyx">>, knm_phone_number:module_name(PN))
     }
    ,{"Verify number is billable"
     ,?_assertEqual(true, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_from_available_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, N} = knm_number:create(?TEST_AVAILABLE_NUM, Options),
    PN = knm_number:phone_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the configured port in module name is being used"
     ,?_assertEqual(<<"knm_bandwidth2">>, knm_phone_number:module_name(PN))
     }
    ,{"Verify number is billable"
     ,?_assertEqual(true, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_from_available_not_specifying_test_() ->
    Options1 = [{auth_by,?MASTER_ACCOUNT_ID} | base()],
    Options2 = [{auth_by,?KNM_DEFAULT_AUTH_BY} | base()],
    Num = ?TEST_AVAILABLE_NUM,
    {ok, N1} = knm_number:create(Num, Options1),
    PN1 = knm_number:phone_number(N1),
    {ok, N2} = knm_number:create(Num, Options2),
    PN2 = knm_number:phone_number(N2),
    [?_assert(knm_phone_number:is_dirty(PN1))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN1))
    ,{"Verify number create has nothing to do with ports and is not ported_in"
     ,?_assertEqual(false, knm_phone_number:ported_in(PN1))
     }
    ,?_assert(knm_phone_number:is_dirty(PN2))
    ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN2))
    ,{"Verify number create has nothing to do with ports and is not ported_in"
     ,?_assertEqual(false, knm_phone_number:ported_in(PN2))
     }
    ].

transition_port_from_not_found_test_() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, N} = knm_number:create(?TEST_CREATE_NUM, Options),
    PN = knm_number:phone_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify phone number is assigned to reseller account"
     ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify new phone number was authorized by master account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:auth_by(PN))
     }
    ,{"Verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN))
     }
    ,{"Verify reserve history is empty"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,{"Verify the configured port in module name is being used"
     ,?_assertEqual(<<"knm_vitelity">>, knm_phone_number:module_name(PN))
     }
    ,{"Verify number is billable"
     ,?_assertEqual(true, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

is_force_outbound_test_() ->
    {ok, ?RESELLER_ACCOUNT_ID, Props1} = knm_number:lookup_account(?TEST_PORT_IN_NUM),
    {error, {not_in_service, ?RESELLER_ACCOUNT_ID}} = knm_number:lookup_account(?TEST_TELNYX_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props2} = knm_number:lookup_account(?TEST_VITELITY_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props3} = knm_number:lookup_account(?TEST_IN_SERVICE_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props4} = knm_number:lookup_account(?TEST_IN_SERVICE_MDN),
    {ok, ?RESELLER_ACCOUNT_ID, Props5} = knm_number:lookup_account(?TEST_IN_SERVICE_BAD_CARRIER_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props6} = knm_number:lookup_account(?TEST_NEW_PORT_NUM),
    {ok, ?RESELLER_ACCOUNT_ID, Props7} = knm_number:lookup_account(?TEST_PORT_IN3_NUM),
    [{"knm_local + port_in --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props1))
     }
    ,{"knm_vitelity + in_service --> false"
     ,?_assert(not knm_number_options:should_force_outbound(Props2))
     }
    ,{"knm_local + in_service --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props3))
     }
    ,{"knm_mdn + in_service --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props4))
     }
    ,{"knm_pacwest + in_service --> false"
     ,?_assert(not knm_number_options:should_force_outbound(Props5))
     }
    ,{"pending port request --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props6))
     }
    ,{"knm_bandwidth2 + port_in --> true"
     ,?_assert(knm_number_options:should_force_outbound(Props7))
     }
    ].
