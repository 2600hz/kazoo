%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

available_as_owner_test_() ->
    available_as(?RESELLER_ACCOUNT_ID).

available_as_parent_test_() ->
    available_as(?MASTER_ACCOUNT_ID).

available_as_rando_test_() ->
    available_as(kz_binary:rand_hex(16)).

available_as(AuthAccountId) ->
    case knm_number:get(?TEST_AVAILABLE_NUM, [{'auth_by', AuthAccountId}]) of
        {'ok', Number} -> available_tests(Number);
        {'error', Error} -> unavailable_tests(Error)
    end.

unavailable_tests(ErrorJObj) ->
    [{"verify unavailable number error code"
     ,?_assertEqual(403, knm_errors:code(ErrorJObj))
     }
    ,{"verify unavailable number error"
     ,?_assertEqual(<<"forbidden">>, knm_errors:error(ErrorJObj))
     }
    ].

available_tests(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    [{"Verify available phone number"
     ,?_assertEqual(?TEST_AVAILABLE_NUM, knm_phone_number:number(PhoneNumber))
     }
    ,{"Verify available number module"
     ,?_assertEqual(?CARRIER_LOCAL, knm_phone_number:module_name(PhoneNumber))
     }
    ,{"Verify available number state"
     ,?_assertEqual(?NUMBER_STATE_AVAILABLE, knm_phone_number:state(PhoneNumber))
     }
    ].

get_unreconcilable_number_test_() ->
    [{"Verify non-reconcilable numbers result in errors"
     ,?_assertMatch({'error', 'not_reconcilable'}, knm_number:get(<<"1000">>))
     }
    ].

get_not_found_test_() ->
    [?_assertEqual({error, not_found}, knm_number:get(<<"4156301234">>))
    ].

mdn_transitions_test_() ->
    Num = ?TEST_IN_SERVICE_MDN,
    DefaultOptions = [{assign_to, ?MASTER_ACCOUNT_ID}
                      |knm_number_options:mdn_options()
                     ],
    {ok, N1} = knm_number:move(Num, ?MASTER_ACCOUNT_ID, DefaultOptions),
    {ok, N2} = knm_number:release(Num, DefaultOptions),
    {ok, N3} = knm_number:reconcile(Num, DefaultOptions),
    {ok, N4} = knm_number:create(?TEST_CREATE_NUM, [{module_name,?CARRIER_MDN}|DefaultOptions]),
    [{"Verify MDN can move from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(knm_number:phone_number(N1)))
     }
    ,{"Verify releasing MDN results in deletion"
     ,?_assertEqual(?NUMBER_STATE_DELETED, knm_phone_number:state(knm_number:phone_number(N2)))
     }
    ,{"Verify MDN can reconcile from in_service to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(knm_number:phone_number(N3)))
     }
    ,{"Verify MDN cannot be reserved"
     ,?_assertMatch({error,_}, knm_number:reserve(Num, knm_number_options:default()))
     }
    ,{"Verify MDN creation forces state to in_service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(knm_number:phone_number(N4)))
     }
    ,{"Verify MDN creation creates local feature"
     ,?_assertEqual([?FEATURE_LOCAL], knm_phone_number:features_list(knm_number:phone_number(N4)))
     }
    ].

is_mdn_for_mdn_run_test_() ->
    Run = {mdn_run, true},
    Base = [{auth_by,?MASTER_ACCOUNT_ID}],
    Sudo = knm_number_options:default(),
    Fs = [{fun knm_phone_number:update_doc/2, kz_json:from_list([{<<"*">>,42}])}],
    [{"Verify an mdn_run && knm_mdn number can be updated"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, [Run|Base]))
     }
    ,{"Verify an mdn_run && !knm_mdn number cannot be updated"
     ,?_assertMatch({error,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, [Run|Base]))
     }
    ,{"Verify a !mdn_run && knm_mdn number cannot be updated"
     ,?_assertMatch({error,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, Base))
     }
    ,{"Verify a !mdn_run && !knm_mdn number can be updated"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, Base))
     }
    ,{"Verify sudo can update mdn_run && knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, [Run|Sudo]))
     }
    ,{"Verify sudo can update mdn_run && !knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, [Run|Sudo]))
     }
    ,{"Verify sudo can update !mdn_run && knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_MDN, Fs, Sudo))
     }
    ,{"Verify sudo can update !mdn_run && !knm_mdn number"
     ,?_assertMatch({ok,_}, knm_number:update(?TEST_IN_SERVICE_NUM, Fs, Sudo))
     }
    ].
