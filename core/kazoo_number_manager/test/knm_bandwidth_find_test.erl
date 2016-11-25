%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_bandwidth_find_test).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

find_test_() ->
    [npan_tests()
    ,area_code_tests()
    ].

npan_tests() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth">>]}
              ],
    Limit = 1,
    Prefix = <<"+14158867900">>,
    Results = knm_carriers:find(Prefix, [{'quantity',Limit}|Options]),
    [{"Verify area code result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].

area_code_tests() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth">>]}
              ],
    Limit = 15,
    Prefix = <<"412">>,
    Results = knm_carriers:find(Prefix, [{'quantity',Limit}|Options]),
    [{"Verify area code result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].
