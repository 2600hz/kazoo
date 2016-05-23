%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_move_number_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_number_manager/include/knm.hrl").

move_number_test_() ->
    TestFuns = [fun move_to_child/1],
    lists:foldl(fun gen_tests/2, [], TestFuns).

gen_tests(F, A) ->
    F(A).

move_to_child(Tests) ->
    {'ok', Number} = knm_number:move(?TEST_AVAILABLE_NUM, ?CHILD_ACCOUNT_ID),
    PhoneNumber = knm_number:phone_number(Number),

    [{"verify assigned_to is child account"
      ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(PhoneNumber))
     }
     | Tests
    ].
