%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_release_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

release_number_test_() ->
    Tests = [fun release_unknown_number/1
             ,fun release_available_number/1
             ,fun release_in_service_number/1
             ,fun release_with_history/1
             ,fun release_for_hard_delete/1
            ],
    lists:foldl(fun(F, Acc) ->
                        F(Acc)
                end, [], Tests
               ).

release_unknown_number(Tests) ->
    [{"verfiy missing numbers return errors"
      ,?_assertMatch(
          {'error', 'not_found'}
          ,knm_number:delete(?TEST_CREATE_NUM)
         )
     }
     | Tests
    ].

release_available_number(Tests) ->
    {'error', Error} = knm_number:delete(?TEST_AVAILABLE_NUM),

    [{"Verify error code for releasing available number"
      ,?_assertEqual(400, knm_errors:code(Error))
     }
     ,{"Verify error for releasing available number"
       ,?_assertEqual(<<"invalid_state_transition">>, knm_errors:error(Error))
      }
     | Tests
    ].

release_in_service_number(Tests) ->
    {'ok', Released} = knm_number:delete(?TEST_IN_SERVICE_NUM),
    PhoneNumber = knm_number:phone_number(Released),

    [{"verify number state is changed"
      ,?_assertEqual(knm_config:released_state()
                     ,knm_phone_number:state(PhoneNumber)
                    )
     }
     ,{"verify reserve history is empty now"
       ,?_assertEqual([]
                      ,knm_phone_number:reserve_history(PhoneNumber)
                     )
      }
     | Tests
    ].

release_with_history(Tests) ->
    {'ok', Unwound} = knm_number:delete(?TEST_IN_SERVICE_WITH_HISTORY_NUM),
    PhoneNumber = knm_number:phone_number(Unwound),

    [{"verify number state is moved to RESERVED"
      ,?_assertEqual(?NUMBER_STATE_RESERVED
                    ,knm_phone_number:state(PhoneNumber)
                    )
     }
     ,{"verify reserve history is unwound"
       ,?_assertEqual([?MASTER_ACCOUNT_ID]
                      ,knm_phone_number:reserve_history(PhoneNumber)
                     )
      }
     ,{"verify number is assigned to prev account"
       ,?_assertEqual(?MASTER_ACCOUNT_ID
                      ,knm_phone_number:assigned_to(PhoneNumber)
                     )
      }
     | Tests
    ].

release_for_hard_delete(Tests) ->
    {'ok', Deleted} = knm_number:delete(?TEST_IN_SERVICE_NUM, [{'should_delete', 'true'}]),
    PhoneNumber = knm_number:phone_number(Deleted),

    [{"verify number state is moved to DELETED"
      ,?_assertEqual(?NUMBER_STATE_DELETED
                    ,knm_phone_number:state(PhoneNumber)
                    )
     }
     | Tests
    ].
