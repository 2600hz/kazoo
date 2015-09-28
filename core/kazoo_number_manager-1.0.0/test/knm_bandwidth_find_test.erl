%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_bandwidth_find_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

find_test_() ->
    [npan_tests()
     ,area_code_tests()
    ].

npan_tests() ->
    [].
    %% Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
    %%            ,{<<"carriers">>, [<<"knm_bandwidth">>]}
    %%           ],
    %% Limit = 10,
    %% knm_carriers:find(?START_BLOCK, Limit, Options).

area_code_tests() ->
    Options = [{<<"account_id">>, ?RESELLER_ACCOUNT_ID}
               ,{<<"carriers">>, [<<"knm_bandwidth">>]}
              ],
    Limit = 15,
    Results = knm_carriers:find(<<"412">>, Limit, Options),

    [{"Verify area code result size"
      ,?_assertEqual(Limit, length(Results))
     }
    ].
