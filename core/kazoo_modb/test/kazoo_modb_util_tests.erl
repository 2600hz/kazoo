%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_modb_util_tests).

-include_lib("eunit/include/eunit.hrl").

prev_year_month_test_() ->
    {Year3, Month3} = {1776, 3},
    {Year2, Month2} = kazoo_modb_util:prev_year_month(Year3, Month3),
    {Year1, Month1} = kazoo_modb_util:prev_year_month(Year2, Month2),
    {Year12, Month12} = kazoo_modb_util:prev_year_month(Year1, Month1),
    {Year11, Month11} = kazoo_modb_util:prev_year_month(Year12, Month12),

    [?_assertEqual({1776, 3}, {Year3, Month3})
    ,?_assertEqual({1776, 2}, {Year2, Month2})
    ,?_assertEqual({1776, 1}, {Year1, Month1})
    ,?_assertEqual({1775, 12}, {Year12, Month12})
    ,?_assertEqual({1775, 11}, {Year11, Month11})
    ].

split_account_mod_test() ->
    DB = <<"account%2F0a%2F93%2F6fc79bdb4a8c38e6089ab44ad030-201405">>,
    ID = <<"account/0a/93/6fc79bdb4a8c38e6089ab44ad030-201405">>,

    {DbAccountId, DbYear, DbMonth} = kazoo_modb_util:split_account_mod(DB),
    {IdAccountId, IdYear, IdMonth} = kazoo_modb_util:split_account_mod(ID),

    [?_assertEqual(2014, DbYear)
    ,?_assertEqual(DbYear, IdYear)

    ,?_assertEqual(5, DbMonth)
    ,?_assertEqual(DbMonth, IdMonth)

    ,?_assertEqual(<<"0a936fc79bdb4a8c38e6089ab44ad030">>, DbAccountId)
    ,?_assertEqual(DbAccountId, IdAccountId)
    ].

get_year_month_sequence_test() ->
    AccountId = <<"a05ced59126846892856c1fecbc3d5d9">>,
    AccountDb = kz_util:format_account_db(AccountId),
    ?assertEqual( [ <<AccountDb/binary, "-201311">>
                  , <<AccountDb/binary, "-201312">>
                  , <<AccountDb/binary, "-201401">>
                  ]
                , kazoo_modb:get_year_month_sequence(AccountId, {2013, 11}, {2014, 1})
                ).
