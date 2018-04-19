%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth2_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").

api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth2">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> {'ok', Pid} = knm_search:start_link(), Pid end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [find_numbers(Options)
             ,find_tollfree_numbers(Options)
             ,acquire_number()
             ]
     end
    }.

find_numbers(Options) ->
    Limit = 2,
    Prefix = <<"973">>,
    MatchPrefix =
        fun (JObj) ->
                Size = byte_size(Prefix),
                Number = kz_json:get_value(<<"number">>, JObj),
                case Number of
                    <<"+1", Prefix:Size/binary, _/binary>> -> 'true';
                    _Else -> 'false'
                end
        end,
    Results = knm_search:find([{'quantity',Limit}
                              ,{'prefix', Prefix}
                              ,{'query_id', <<"QID-", Prefix/binary>>}
                               |Options
                              ]),
    [{"Verify found numbers"
     ,?_assertEqual(Limit, length(Results))
     }
    ,{"Verify results match queried prefix"
     ,?_assertEqual('true', lists:all(MatchPrefix, Results))
     }
    ].

find_tollfree_numbers(Options) ->
    Limit = 15,
    Prefix = <<"855">>,
    MatchPrefix =
        fun (JObj) ->
                Size = byte_size(Prefix),
                Number = kz_json:get_value(<<"number">>, JObj),
                case Number of
                    <<"+1", Prefix:Size/binary, _/binary>> -> 'true';
                    _Else -> 'false'
                end
        end,
    Results = knm_search:find([{'quantity',Limit}
                              ,{'prefix', Prefix}
                              ,{'query_id', <<"QID-", Prefix/binary>>}
                               |Options]),
    [{"Verify found numbers"
     ,?_assertEqual(Limit, length(Results))
     }
    ,{"Verify results match queried prefix"
     ,?_assertEqual('true', lists:all(MatchPrefix, Results))
     }
    ].

acquire_number() ->
    Num = <<"+19734096113">>,
    PN = knm_phone_number:from_number(Num),
    N = knm_number:set_phone_number(knm_number:new(), PN),
    Result = knm_bandwidth2:acquire_number(N),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify number is still one inputed"
     ,?_assertEqual(Num, knm_phone_number:number(knm_number:phone_number(Result)))
     }
    ].
