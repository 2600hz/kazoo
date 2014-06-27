%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600HZ, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(kazoo_modb_util).

-include("kazoo_modb.hrl").

-export([prev_year_month/1, prev_year_month/2
         ,prev_year_month_mod/1
         ,split_account_mod/1
        ]).

-spec prev_year_month(ne_binary()) -> {wh_year(), wh_month()}.
-spec prev_year_month(wh_year(), wh_month()) -> {wh_year(), wh_month()}.
prev_year_month(AccountMod) ->
    {_AccountId, Year, Month} = kazoo_modb_util:split_account_mod(AccountMod),
    prev_year_month(Year, Month).

prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

-spec prev_year_month_mod(ne_binary()) -> ne_binary().
prev_year_month_mod(AccountModb) ->
    {AccountId, Year, Month} = split_account_mod(AccountModb),
    {PrevYear, PrevMonth} = prev_year_month(Year, Month),
    wh_util:format_account_id(AccountId, PrevYear, PrevMonth).

-spec split_account_mod(ne_binary()) -> {ne_binary(), wh_year(), wh_month()}.
split_account_mod(<<Account:32/binary, "-", Year:4/binary, Month:2/binary>>) ->
    {wh_util:format_account_id(Account, 'raw')
     ,wh_util:to_integer(Year)
     ,wh_util:to_integer(Month)
    };
split_account_mod(<<Account:42/binary, "-", Year:4/binary, Month:2/binary>>) ->
    {wh_util:format_account_id(Account, 'raw')
     ,wh_util:to_integer(Year)
     ,wh_util:to_integer(Month)
    };
split_account_mod(<<Account:48/binary, "-",  Year:4/binary, Month:2/binary>>) ->
    {wh_util:format_account_id(Account, 'raw')
     ,wh_util:to_integer(Year)
     ,wh_util:to_integer(Month)
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prev_year_month_test() ->
    {Year3, Month3} = {1776, 3},
    {Year2, Month2} = prev_year_month(Year3, Month3),
    {Year1, Month1} = prev_year_month(Year2, Month2),
    {Year12, Month12} = prev_year_month(Year1, Month1),
    {Year11, Month11} = prev_year_month(Year12, Month12),

    ?assertEqual({1776, 3}, {Year3, Month3}),
    ?assertEqual({1776, 2}, {Year2, Month2}),
    ?assertEqual({1776, 1}, {Year1, Month1}),
    ?assertEqual({1775, 12}, {Year12, Month12}),
    ?assertEqual({1775, 11}, {Year11, Month11}).

split_account_mod_test() ->
    DB = <<"account%2F0a%2F93%2F6fc79bdb4a8c38e6089ab44ad030-201405">>,
    ID = <<"account/0a/93/6fc79bdb4a8c38e6089ab44ad030-201405">>,

    {DbAccountId, DbYear, DbMonth} = split_account_mod(DB),
    {IdAccountId, IdYear, IdMonth} = split_account_mod(ID),

    ?assertEqual(2014, DbYear),
    ?assertEqual(DbYear, IdYear),

    ?assertEqual(5, DbMonth),
    ?assertEqual(DbMonth, IdMonth),

    ?assertEqual(<<"0a936fc79bdb4a8c38e6089ab44ad030">>, DbAccountId),
    ?assertEqual(DbAccountId, IdAccountId).

-endif.
