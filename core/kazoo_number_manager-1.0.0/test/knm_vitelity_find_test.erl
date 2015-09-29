%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_vitelity_find_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

find_test_() ->
    [tollfree_tests()
     ,local_number_tests()
     ,local_prefix_tests()
    ].

tollfree_tests() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_vitelity">>]}
              ],
    Limit = 1,
    Results = knm_carriers:find(?TEST_CREATE_TOLL, Limit, Options),

    [{"Verify tollfree result size"
      ,?_assertEqual(Limit, length(Results))
     }
    ].

local_number_tests() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_vitelity">>]}
              ],
    Limit = 1,
    Results = knm_carriers:find(<<"+19875559876">>, Limit, Options),

    [{"Verify local number search result size"
      ,?_assertEqual(Limit, length(Results))
     }
    ].

local_prefix_tests() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_vitelity">>]}
              ],
    Limit = 2,
    Results = knm_carriers:find(<<"987">>, Limit, Options),

    [{"Verify local prefix search result size"
      ,?_assertEqual(Limit, length(Results))
     }
    ].
