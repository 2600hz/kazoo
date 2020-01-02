%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_voxbone_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knm.hrl").


api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_voxbone">>]}
              ,{'query_id', <<"QID">>}
              ],
    {setup
    ,fun () -> case knm_search:start_link() of
                   {'ok', Pid} -> Pid;
                   {'error', {'already_started', Pid}} -> Pid
               end
     end
    ,fun gen_server:stop/1
    ,fun (_ReturnOfSetup) ->
             [find_numbers(Options)
             ]
     end
    }.

find_numbers(Options) ->
    Limit = 15,
    Prefix = <<"256">>,
    Results = knm_search:find([{'quantity',Limit}
                              ,{'prefix', Prefix}
                              ,{'query_id', <<"QID-", Prefix/binary>>}
                              ,{'country', <<"US">>}
                               |Options
                              ]),
    MatchPrefix =
        fun (JObj) ->
                Size = byte_size(Prefix),
                Number = kz_json:get_value(<<"number">>, JObj),
                case Number of
                    <<"+1", Prefix:Size/binary, _/binary>> -> 'true';
                    _Else -> 'false'
                end
        end,
    [{"Verify found numbers"
     ,?_assertEqual(Limit, length(Results))
     }
    ,{"Verify results match queried prefix"
     ,?_assertEqual('true', lists:all(MatchPrefix, Results))
     }
    ].
