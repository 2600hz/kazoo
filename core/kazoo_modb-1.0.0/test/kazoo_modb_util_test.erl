%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(kazoo_modb_util_test).

-include_lib("eunit/include/eunit.hrl").

prev_year_month_test() ->
    {Year3, Month3} = {1776, 3},
    {Year2, Month2} = kazoo_modb_util:prev_year_month(Year3, Month3),
    {Year1, Month1} = kazoo_modb_util:prev_year_month(Year2, Month2),
    {Year12, Month12} = kazoo_modb_util:prev_year_month(Year1, Month1),
    {Year11, Month11} = kazoo_modb_util:prev_year_month(Year12, Month12),

    ?assertEqual({1776, 3}, {Year3, Month3}),
    ?assertEqual({1776, 2}, {Year2, Month2}),
    ?assertEqual({1776, 1}, {Year1, Month1}),
    ?assertEqual({1775, 12}, {Year12, Month12}),
    ?assertEqual({1775, 11}, {Year11, Month11}).

split_account_mod_test() ->
    DB = <<"account%2F0a%2F93%2F6fc79bdb4a8c38e6089ab44ad030-201405">>,
    ID = <<"account/0a/93/6fc79bdb4a8c38e6089ab44ad030-201405">>,

    {DbAccountId, DbYear, DbMonth} = kazoo_modb_util:split_account_mod(DB),
    {IdAccountId, IdYear, IdMonth} = kazoo_modb_util:split_account_mod(ID),

    ?assertEqual(2014, DbYear),
    ?assertEqual(DbYear, IdYear),

    ?assertEqual(5, DbMonth),
    ?assertEqual(DbMonth, IdMonth),

    ?assertEqual(<<"0a936fc79bdb4a8c38e6089ab44ad030">>, DbAccountId),
    ?assertEqual(DbAccountId, IdAccountId).
