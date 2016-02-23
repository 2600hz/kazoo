%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600HZ, INC
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
-spec prev_year_month(ne_binary() | wh_year(), ne_binary() | wh_month()) ->
                             {wh_year(), wh_month()}.
prev_year_month(AccountMod) ->
    {_AccountId, Year, Month} = ?MODULE:split_account_mod(AccountMod),
    prev_year_month(Year, Month).

prev_year_month(<<_/binary>> = Year, Month) ->
    prev_year_month(wh_util:to_integer(Year), Month);
prev_year_month(Year, <<_/binary>> = Month) ->
    prev_year_month(Year, wh_util:to_integer(Month));
prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

prev_year_month_bin(Y, M) ->
    {Year, Month} = prev_year_month(Y, M),
    {wh_util:to_binary(Year), wh_util:pad_month(Month)}.

-spec prev_year_month_mod(ne_binary()) -> ne_binary().
prev_year_month_mod(?MATCH_MODB_SUFFIX_RAW(AccountId, Year, Month)) ->
    {PrevYear, PrevMonth} = prev_year_month_bin(Year, Month),
    ?MATCH_MODB_SUFFIX_RAW(AccountId, PrevYear, PrevMonth);
prev_year_month_mod(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month)) ->
    {PrevYear, PrevMonth} = prev_year_month_bin(Year, Month),
    ?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, PrevYear, PrevMonth);
prev_year_month_mod(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month)) ->
    {PrevYear, PrevMonth} = prev_year_month_bin(Year, Month),
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, PrevYear, PrevMonth);
prev_year_month_mod(?MATCH_MODB_SUFFIX_encoded(A, B, Rest, Year, Month)) ->
    {PrevYear, PrevMonth} = prev_year_month_bin(Year, Month),
    ?MATCH_MODB_SUFFIX_encoded(A, B, Rest, PrevYear, PrevMonth).

-spec split_account_mod(ne_binary()) -> {ne_binary(), wh_year(), wh_month()}.
split_account_mod(?MATCH_MODB_SUFFIX_RAW(Account,Year,Month)) ->
    {wh_util:format_account_id(Account, 'raw')
     ,wh_util:to_integer(Year)
     ,wh_util:to_integer(Month)
    };
split_account_mod(?MATCH_MODB_SUFFIX_UNENCODED(Account,Year,Month)) ->
    {wh_util:format_account_id(Account, 'raw')
     ,wh_util:to_integer(Year)
     ,wh_util:to_integer(Month)
    };
split_account_mod(?MATCH_MODB_SUFFIX_ENCODED(Account,Year,Month)) ->
    {wh_util:format_account_id(Account, 'raw')
     ,wh_util:to_integer(Year)
     ,wh_util:to_integer(Month)
    }.
