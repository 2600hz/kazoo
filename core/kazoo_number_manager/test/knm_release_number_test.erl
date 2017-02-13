%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_release_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

release_unknown_number_test_() ->
    [{"verfiy missing numbers return errors"
     ,?_assertMatch({'error', 'not_found'}, knm_number:release(?TEST_CREATE_NUM))
     }
    ].

release_available_number_test_() ->
    {'error', Error} = knm_number:release(?TEST_AVAILABLE_NUM),
    [{"Verify error code for releasing available number"
     ,?_assertEqual(400, knm_errors:code(Error))
     }
    ,{"Verify error for releasing available number"
     ,?_assertEqual(<<"invalid_state_transition">>, knm_errors:error(Error))
     }
    ].

release_in_service_bad_carrier_number_test_() ->
    {'ok', Released} = knm_number:release(?TEST_IN_SERVICE_BAD_CARRIER_NUM),
    PhoneNumber = knm_number:phone_number(Released),
    [{"verify number state is changed"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PhoneNumber))
     }
    ,{"verify reserve history is empty now"
     ,?_assertEqual([], knm_phone_number:reserve_history(PhoneNumber))
     }
    ].

release_in_service_number_test_() ->
    {'ok', Released} = knm_number:release(?TEST_IN_SERVICE_NUM),
    PhoneNumber = knm_number:phone_number(Released),
    [{"verify number state is changed"
     ,?_assertNotEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PhoneNumber))
     }
    ,{"verify reserve history is empty now"
     ,?_assertEqual([], knm_phone_number:reserve_history(PhoneNumber))
     }
    ].

release_with_history_test_() ->
    {'ok', Unwound} = knm_number:release(?TEST_IN_SERVICE_WITH_HISTORY_NUM),
    PhoneNumber = knm_number:phone_number(Unwound),
    [{"verify number state is moved to RESERVED"
     ,?_assertEqual(?NUMBER_STATE_RESERVED, knm_phone_number:state(PhoneNumber))
     }
    ,{"verify reserve history is unwound"
     ,?_assertEqual([?MASTER_ACCOUNT_ID], knm_phone_number:reserve_history(PhoneNumber))
     }
    ,{"verify number is assigned to prev account"
     ,?_assertEqual(?MASTER_ACCOUNT_ID, knm_phone_number:assigned_to(PhoneNumber))
     }
    ].

release_for_hard_delete_test_() ->
    {'ok', Deleted} = knm_number:release(?TEST_IN_SERVICE_NUM, [{'should_delete', 'true'}]),
    PhoneNumber = knm_number:phone_number(Deleted),
    [{"verify number state is moved to DELETED"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PhoneNumber))
     }
    ].

release_mdn_test_() ->
    BaseOptions = knm_number_options:mdn_options(),
    {'ok', Deleted} = knm_number:release(?TEST_IN_SERVICE_MDN, BaseOptions),
    PhoneNumber = knm_number:phone_number(Deleted),
    [{"verify number state is moved to DELETED"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(PhoneNumber))
     }
    ,?_assertEqual(?CARRIER_MDN, knm_phone_number:module_name(PhoneNumber))
    ].
