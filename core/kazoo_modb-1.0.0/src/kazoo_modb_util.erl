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

-export([prev_year_month/2]).

-spec prev_year_month(wh_year(), wh_month()) -> {wh_year(), wh_month()}.
prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

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

-endif.
