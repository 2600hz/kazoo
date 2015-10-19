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
    [{"verfiy missing numbers return errors"
      ,?_assertMatch(
          {'error', 'not_found'}
          ,knm_number:delete(?TEST_CREATE_NUM)
         )
     }
     | release_existing_number()
    ].

release_existing_number() ->
    {'ok', Deleted} = knm_number:delete(?TEST_EXISTING_NUM),

    PN = knm_number:phone_number(Deleted),
    [{"Verify number is in DISCONNECTED state"
      ,knm_phone_number:state(PN)
     }
    ].
