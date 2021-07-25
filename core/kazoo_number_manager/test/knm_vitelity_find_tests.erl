%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2021, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_vitelity_find_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

find_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_vitelity">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> {'ok', Pid} = knm_search:start_link(), Pid end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [tollfree_tests(Options)
             ,local_number_tests(Options)
             ,local_prefix_tests(Options)
             ]
     end
    }.

tollfree_tests(Options0) ->
    <<"+1", Num/binary>> = ?TEST_CREATE_TOLL,
    Options = [{'prefix', Num}
              ,{'quantity', 1}
               | Options0
              ],
    [Result] = knm_search:find(Options),
    [{"Verify found number"
     ,?_assertEqual(?TEST_CREATE_TOLL, kz_json:get_value(<<"number">>, Result))
     }
    ,?_assertEqual([Result], knm_search:find([{'tollfree','true'}|Options]))
    ].

local_number_tests(Options0) ->
    Limit = 1,
    Prefix = <<"9875559876">>,
    Options = [{'prefix', Prefix}
              ,{'quantity', Limit}
               | Options0
              ],
    Results = knm_search:find(Options),
    [{"Verify local number search result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].

local_prefix_tests(Options0) ->
    Limit = 2,
    Prefix = <<"987">>,
    Options = [{'prefix', Prefix}
              ,{'quantity', Limit}
               | Options0
              ],
    Results = knm_search:find(Options),
    [{"Verify local prefix search result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].
