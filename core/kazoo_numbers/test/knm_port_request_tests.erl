%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_port_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [transition_port_from_port_in()
    ,transition_port_from_port_in_with_different_module_configured()
    ,transition_port_from_available()
    ,transition_port_from_available_not_specifying()
    ,transition_port_from_not_found()
    ].

base() ->
    [{assign_to, ?RESELLER_ACCOUNT_ID}
    ,{dry_run, false}
    ].

transition_port_from_port_in() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, PN} = knm_number:create(?TEST_PORT_IN_NUM, Options),
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

transition_port_from_port_in_with_different_module_configured() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, PN} = knm_number:create(?TEST_PORT_IN2_NUM, Options),
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
      %% TODO: set number_manager.port_in_module_name to knm_telnyx after fixture have save feature
      %% ,?_assertEqual(<<"knm_telnyx">>, knm_phone_number:module_name(PN))
     ,?_assertEqual(?PORT_IN_MODULE_NAME, knm_phone_number:module_name(PN))
     }
    ,{"Verify number is billable"
      %% See TODO above
      %% ,?_assertEqual(true, knm_carriers:is_number_billable(PN))
     ,?_assertEqual(false, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_from_available() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, PN} = knm_number:create(?TEST_AVAILABLE_NUM, Options),
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
      %% TODO: set number_manager.port_in_module_name to knm_telnyx after fixture have save feature
      %% ,?_assertEqual(<<"knm_bandwidth2">>, knm_phone_number:module_name(PN))
     ,?_assertEqual(?PORT_IN_MODULE_NAME, knm_phone_number:module_name(PN))
     }
    ,{"Verify number is billable"
      %% See TODO above
      %% ,?_assertEqual(true, knm_carriers:is_number_billable(PN))
     ,?_assertEqual(false, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].

transition_port_from_available_not_specifying() ->
    Options1 = [{auth_by,?MASTER_ACCOUNT_ID} | base()],
    Options2 = [{auth_by,?KNM_DEFAULT_AUTH_BY} | base()],
    Num = ?TEST_AVAILABLE_NUM,
    {ok, PN1} = knm_number:create(Num, Options1),
    {ok, PN2} = knm_number:create(Num, Options2),
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

transition_port_from_not_found() ->
    Options = [{auth_by, ?MASTER_ACCOUNT_ID}
              ,{ported_in, true}
               |base()
              ],
    {ok, PN} = knm_number:create(?TEST_CREATE_NUM, Options),
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
      %% TODO: set number_manager.port_in_module_name to knm_telnyx after fixture have save feature
      %% ,?_assertEqual(<<"knm_vitelity">>, knm_phone_number:module_name(PN))
     ,?_assertEqual(?PORT_IN_MODULE_NAME, knm_phone_number:module_name(PN))
     }
    ,{"Verify number is billable"
      %% See TODO above
      %% ,?_assertEqual(true, knm_carriers:is_number_billable(PN))
     ,?_assertEqual(false, knm_carriers:is_number_billable(PN))
     }
    ,{"Verify number is marked as ported_in"
     ,?_assertEqual(true, knm_phone_number:ported_in(PN))
     }
    ].
