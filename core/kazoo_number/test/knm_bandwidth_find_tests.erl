%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth_find_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

api_test_() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_bandwidth">>]}
              ,{'query_id', <<"QID">>}
              ],
    {'setup'
    ,fun () -> case knm_search:start_link() of
                   {'ok', Pid} -> Pid;
                   {'error', {'already_started', Pid}} -> Pid
               end
     end
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
                              ,{'prefix', Prefix}
                              ,{'query_id', <<"QID-", Prefix/binary>>}
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
                              ,{'prefix', Prefix}
                              ,{'query_id', <<"QID-", Prefix/binary>>}
                               | Options
                              ]),
    [{"Verify area code result size"
     ,?_assertEqual(Limit, length(Results))
     }
    ].
