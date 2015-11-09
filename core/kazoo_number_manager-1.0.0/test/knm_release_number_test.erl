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
    {'ok', Deleted} = knm_number:delete(?TEST_IN_SERVICE_NUM),
    ?debugFmt("del ~p~n", [Deleted]),
    Tests.
