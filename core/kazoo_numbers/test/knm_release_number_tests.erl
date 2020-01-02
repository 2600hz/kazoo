%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_release_number_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0).

db_dependant() ->
    [release_unknown_number()
    ,release_available_number()
    ,release_in_service_bad_carrier_number()
    ,release_in_service_mdn_number()
    ,release_in_service_numbers()
    ,delete_in_service()
    ].

release_unknown_number() ->
    [{"verify missing numbers return errors"
     ,?_assertMatch({error, not_found}, knm_number:release(?TEST_CREATE_NUM))
     }
    ].

release_available_number() ->
    {ok, PN} = knm_number:release(?TEST_AVAILABLE_NUM),
    {error, Error} = knm_number:release(?TEST_TELNYX_NUM),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify releasing available local number results in deletion"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN))
     }
    ,{"verify number assignment"
     ,?_assertEqual(undefined, knm_phone_number:assigned_to(PN))
     }
    ,{"Verify error code for releasing available number"
     ,?_assertEqual(400, knm_errors:code(Error))
     }
    ,{"Verify error for releasing available number"
     ,?_assertEqual(<<"invalid_state_transition">>, knm_errors:error(Error))
     }
    ].

release_in_service_bad_carrier_number() ->
    {ok, PN} = knm_number:release(?TEST_IN_SERVICE_BAD_CARRIER_NUM),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"verify number state is changed"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PN))
     }
    ,{"verify available number is unassigned"
     ,?_assertEqual(undefined, knm_phone_number:assigned_to(PN))
     }
    ,{"verify reserve history is empty now"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ].

release_in_service_mdn_number() ->
    {ok, PN} = knm_number:release(?TEST_IN_SERVICE_MDN, knm_number_options:mdn_options()),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"verify number state is changed"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN))
     }
    ,{"verify number assignment"
     ,?_assertEqual(undefined, knm_phone_number:assigned_to(PN))
     }
    ,{"verify reserve history is empty now"
     ,?_assertEqual([], knm_phone_number:reserve_history(PN))
     }
    ,?_assertEqual(?CARRIER_MDN, knm_phone_number:module_name(PN))
    ].

release_in_service_numbers() ->
    DefaultAuth = knm_number_options:default(),
    ResellerAuth = [{auth_by, ?RESELLER_ACCOUNT_ID}],
    MasterAuth = [{auth_by, ?MASTER_ACCOUNT_ID}],
    ResellerMDNAuth = [{auth_by, ?RESELLER_ACCOUNT_ID}|knm_number_options:mdn_options()],
    MasterMDNAuth = [{auth_by, ?MASTER_ACCOUNT_ID}|knm_number_options:mdn_options()],
    SimpleHistory = [?RESELLER_ACCOUNT_ID],
    DeeperHistory = [?RESELLER_ACCOUNT_ID, ?MASTER_ACCOUNT_ID],
    [release_in_service(?TEST_IN_SERVICE_NUM, DefaultAuth, SimpleHistory)
    ,release_in_service(?TEST_IN_SERVICE_NUM, MasterAuth, SimpleHistory)
    ,release_in_service(?TEST_IN_SERVICE_NUM, ResellerAuth, SimpleHistory)
    ,release_in_service(?TEST_IN_SERVICE_WITH_HISTORY_NUM, DefaultAuth, DeeperHistory)
    ,release_in_service(?TEST_IN_SERVICE_WITH_HISTORY_NUM, MasterAuth, DeeperHistory)
    ,release_in_service(?TEST_IN_SERVICE_WITH_HISTORY_NUM, ResellerAuth, DeeperHistory)
    ,release_in_service(?TEST_IN_SERVICE_MDN, DefaultAuth, SimpleHistory)
    ,release_in_service(?TEST_IN_SERVICE_MDN, MasterMDNAuth, SimpleHistory)
    ,release_in_service(?TEST_IN_SERVICE_MDN, ResellerMDNAuth, SimpleHistory)
    ].

release_in_service(Num, Options, PreHistory) ->
    {ok, PN0} = knm_number:get(Num, Options),
    {ok, PN} = knm_number:release(Num, Options),
    [?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN0))
    ,?_assertEqual(PreHistory, knm_phone_number:reserve_history(PN0))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN0))
    ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN0))
    ,?_assert(knm_phone_number:is_dirty(PN))
    ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN))
     |
     case PreHistory of
         [?RESELLER_ACCOUNT_ID] ->
             [{"verify number state is changed"
              ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN))
              }
             ,{"verify number assignment"
              ,?_assertEqual(undefined, knm_phone_number:assigned_to(PN))
              }
             ,{"verify reserve history is empty now"
              ,?_assertEqual(tl(PreHistory), knm_phone_number:reserve_history(PN))
              }
             ];
         [?RESELLER_ACCOUNT_ID, ?MASTER_ACCOUNT_ID] ->
             [{"verify number state is moved to RESERVED"
              ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(PN))
              }
             ,{"verify number is assigned to prev account"
              ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:assigned_to(PN))
              }
             ,{"verify reserve history is unwound"
              ,?_assertEqual([?MASTER_ACCOUNT_ID], knm_phone_number:reserve_history(PN))
              }
             ]
     end
    ].

delete_in_service() ->
    DefaultAuth = knm_number_options:default(),
    MasterAuth = [{auth_by, ?MASTER_ACCOUNT_ID}],
    MasterMDNAuth = [{auth_by, ?MASTER_ACCOUNT_ID}|knm_number_options:mdn_options()],
    [delete_in_service(?TEST_IN_SERVICE_NUM, DefaultAuth)
    ,delete_in_service(?TEST_IN_SERVICE_NUM, MasterAuth)
    ,delete_in_service(?TEST_IN_SERVICE_WITH_HISTORY_NUM, DefaultAuth)
    ,delete_in_service(?TEST_IN_SERVICE_WITH_HISTORY_NUM, MasterAuth)
    ,delete_in_service(?TEST_IN_SERVICE_MDN, DefaultAuth)
    ,delete_in_service(?TEST_IN_SERVICE_MDN, MasterMDNAuth)
    ].

delete_in_service(Num, Options) ->
    {ok, PN0} = knm_number:get(Num, Options),
    {ok, PN} = knm_number:delete(Num, Options),
    [?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN0))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN0))
    ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN0))
    ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN))
    ,{"verify number state is changed"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN))
     }
    ].
