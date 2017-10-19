%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_bandwidth_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () ->
             case knm_search:start_link() of
                 {ok, Pid} -> Pid;
                 {error, {already_started, Pid}} -> Pid
             end
     end
    ,fun (Pid) -> ok = gen_server:stop(Pid) end
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
