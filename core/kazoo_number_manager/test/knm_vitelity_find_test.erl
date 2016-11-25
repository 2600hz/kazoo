%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_vitelity_find_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

find_test_() ->
    [tollfree_tests()
    ,local_number_tests()
    ,local_prefix_tests()
    ].

tollfree_tests() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_vitelity">>]}
              ,{'quantity', 1}
              ],
    <<"+1", Num/binary>> = ?TEST_CREATE_TOLL,
    [Result] = knm_carriers:find(Num, Options),
    [Result] = knm_carriers:find(Num, [{'tollfree','true'}|Options]),

    [{"Verify found number"
     ,?_assertEqual(?TEST_CREATE_TOLL, kz_json:get_value(<<"number">>, Result))
     }
    ,{"Verify activation charge found"
     ,?_assertEqual(1.0, kz_json:get_value(<<"activation_charge">>, Result))
     }
    ].

local_number_tests() ->
    Limit = 1,
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_vitelity">>]}
              ,{'quantity', Limit}
              ],
    Results = knm_carriers:find(<<"9875559876">>, Options),
    [{"Verify local number search result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].

local_prefix_tests() ->
    Limit = 2,
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_vitelity">>]}
              ,{'quantity', Limit}
              ],
    Results = knm_carriers:find(<<"987">>, Options),
    [{"Verify local prefix search result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].
