%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_move_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

move_to_child_test_() ->
    {'ok', Number} = knm_number:move(?TEST_AVAILABLE_NUM, ?CHILD_ACCOUNT_ID),
    PhoneNumber = knm_number:phone_number(Number),
    [{"verify assigned_to is child account"
     ,?_assertEqual(?CHILD_ACCOUNT_ID, knm_phone_number:assigned_to(PhoneNumber))
     }
    ,{"verify number is in service"
     ,?_assertEqual(?NUMBER_STATE_IN_SERVICE, knm_phone_number:state(PhoneNumber))
     }
    ].
