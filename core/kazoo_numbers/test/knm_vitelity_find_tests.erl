%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_vitelity_find_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

-export([db_dependant/0
        ,setup/1
        ,cleanup/1
        ]).

knm_number_test_() ->
    knm_test_util:start_db(fun db_dependant/0, fun setup/1, fun cleanup/1).

db_dependant() ->
    [find()
    ].

setup(TestState) ->
    Pid = case knm_search:start_link() of
              {'ok', P} -> P;
              {'error', {'already_started', P}} -> P
          end,
    TestState#{search_pid => Pid}.

cleanup(#{search_pid := Pid}) ->
    gen_server:stop(Pid).

find() ->
    Options = [{'account_id', ?RESELLER_ACCOUNT_ID}
              ,{'carriers', [<<"knm_vitelity">>]}
              ,{'query_id', <<"QID">>}
              ],
    [tollfree_tests(Options)
    ,local_number_tests(Options)
    ,local_prefix_tests(Options)
    ].

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
