%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_release_number_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

release_unknown_number_test_() ->
    [{"verfiy missing numbers return errors"
     ,?_assertMatch({error, not_found}, knm_number:release(?TEST_CREATE_NUM))
     }
    ].

release_available_number_test_() ->
    {ok, N} = knm_number:release(?TEST_AVAILABLE_NUM),
    PN = knm_number:phone_number(N),
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

release_in_service_bad_carrier_number_test_() ->
    {ok, N} = knm_number:release(?TEST_IN_SERVICE_BAD_CARRIER_NUM),
    PN = knm_number:phone_number(N),
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

release_in_service_mdn_number_test_() ->
    {ok, N} = knm_number:release(?TEST_IN_SERVICE_MDN, knm_number_options:mdn_options()),
    PN = knm_number:phone_number(N),
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

release_in_service_numbers_test_() ->
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
    {ok, N0} = knm_number:get(Num, Options),
    PN0 = knm_number:phone_number(N0),
    {ok, N} = knm_number:release(Num, Options),
    PN = knm_number:phone_number(N),
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

delete_in_service_test_() ->
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
    {ok, N0} = knm_number:get(Num, Options),
    PN0 = knm_number:phone_number(N0),
    {ok, N} = knm_number:delete(Num, Options),
    PN = knm_number:phone_number(N),
    [?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PN0))
    ,?_assertEqual(?RESELLER_ACCOUNT_ID, knm_phone_number:assigned_to(PN0))
    ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN0))
    ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(PN))
    ,{"verify number state is changed"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PN))
     }
    ].
