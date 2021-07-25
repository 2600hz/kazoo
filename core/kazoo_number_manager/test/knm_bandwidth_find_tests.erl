%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2021, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth_find_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> {'ok', Pid} = knm_search:start_link(), Pid end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [npan_tests(Options)
             ,area_code_tests(Options)
             ]
     end
    }.

npan_tests(Options) ->
    Limit = 1,
    Prefix = <<"+14158867900">>,
    Results = knm_search:find([{'quantity',Limit}
                              ,{prefix, Prefix}
                              ,{query_id, <<"QID-", Prefix/binary>>}
                               | Options
                              ]),
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
                               | Options
                              ]),
    [{"Verify area code result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].
