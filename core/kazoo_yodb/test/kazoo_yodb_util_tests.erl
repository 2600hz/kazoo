%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_yodb_util_tests).

-include_lib("eunit/include/eunit.hrl").

prev_year_test_() ->
    Year3 = 1776,
    Year3_bin = <<"1776">>,
    Year3_yod = <<"account%2F0a%2F93%2F6fc79bdb4a8c38e6089ab44ad030-1776">>,
    Year2 = kazoo_yodb_util:prev_year(Year3),
    Year2_bin = kazoo_yodb_util:prev_year(Year3_bin),
    Year2_yod = kazoo_yodb_util:prev_year(Year3_yod),
    Year1 = kazoo_yodb_util:prev_year(Year2),

    [?_assertEqual(1775, Year2)
    ,?_assertEqual(1775, Year2_bin)
    ,?_assertEqual(1775, Year2_yod)
    ,?_assertEqual(1774, Year1)
    ].

prev_year_yod_test_() ->
    Year3 = <<"account%2F0a%2F93%2F6fc79bdb4a8c38e6089ab44ad030-1776">>,
    Year2 = kazoo_yodb_util:prev_year(Year3),

    [?_assertEqual(1775, Year2)].

split_account_yod_test_() ->
    DB = <<"account%2F0a%2F93%2F6fc79bdb4a8c38e6089ab44ad030-2014">>,
    ID = <<"account/0a/93/6fc79bdb4a8c38e6089ab44ad030-2014">>,

    {DbAccountId, DbYear} = kazoo_yodb_util:split_account_yod(DB),
    {IdAccountId, IdYear} = kazoo_yodb_util:split_account_yod(ID),

    [?_assertEqual(2014, DbYear)
    ,?_assertEqual(DbYear, IdYear)

    ,?_assertEqual(<<"0a936fc79bdb4a8c38e6089ab44ad030">>, DbAccountId)
    ,?_assertEqual(DbAccountId, IdAccountId)
    ].

get_yodb_suffix_test_() ->
    Year = 1776,
    Year_bin = <<"1776">>,

    [?_assertEqual(Year, kazoo_yodb_util:get_yodb_suffix(Year))
    ,?_assertEqual(Year, kazoo_yodb_util:get_yodb_suffix(Year_bin))
    ].
