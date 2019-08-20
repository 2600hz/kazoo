%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_nomorobo_tests).

-include_lib("eunit/include/eunit.hrl").

nomorobo_branch_test_() ->
    ScoreBranch = [{  0, <<"0">>}
                  ,{ 1, <<"0">>}
                  ,{ 2, <<"0">>}
                  ,{ 3, <<"3">>}
                  ,{ 4, <<"3">>}
                  ,{ 5, <<"3">>}
                  ,{ 6, <<"6">>}
                  ,{ 7, <<"6">>}
                  ,{ 8, <<"6">>}
                  ,{ 9, <<"6">>}
                  ,{10, <<"10">>}
                  ],
    Keys = cf_nomorobo:nomorobo_branches({'branch_keys', [<<"0">>, <<"10">>, <<"3">>, <<"6">>]}),

    [?_assertEqual(Branch, cf_nomorobo:nomorobo_branch(Score, Keys))
     || {Score, Branch} <- ScoreBranch
    ].
