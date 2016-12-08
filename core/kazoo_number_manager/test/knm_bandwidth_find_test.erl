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

api_test_() ->
    {'ok', Pid} = knm_search:start_link(),
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth">>]}
              ,{'query_id', <<"QID">>}
              ],
    X = [npan_tests(Options)
        ,area_code_tests(Options)
        ],
    _ = gen_server:stop(Pid),
    X.

npan_tests(Options) ->
    Limit = 1,
    Prefix = <<"+14158867900">>,
    Results = knm_search:find([{'quantity',Limit}
                              ,{prefix, Prefix}
                              ,{query_id, <<"QID-", Prefix/binary>>}
                               |Options]),
    [{"Verify area code result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].

area_code_tests(Options) ->
    Limit = 15,
    Prefix = <<"412">>,
    Results = knm_search:find([{'quantity',Limit}
                              ,{prefix, Prefix}
                              ,{query_id, <<"QID-", Prefix/binary>>}
                               |Options]),
    [{"Verify area code result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].
