%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth2_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

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
    Result = knm_bandwidth2:acquire_number(PN),
    [?_assert(knm_phone_number:is_dirty(PN))
    ,{"Verify number is still one inputed"
     ,?_assertEqual(Num, knm_phone_number:number(Result))
     }
    ].
