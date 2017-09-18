%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz, INC
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
        ,modb_id/0, modb_id/1, modb_id/2, modb_id/3
        ]).

-spec prev_year_month(ne_binary()) -> {kz_year(), kz_month()}.
-spec prev_year_month(ne_binary() | kz_year(), ne_binary() | kz_month()) ->
                             {kz_year(), kz_month()}.
prev_year_month(AccountMod) ->
    {_AccountId, Year, Month} = split_account_mod(AccountMod),
    prev_year_month(Year, Month).

prev_year_month(<<_/binary>> = Year, Month) ->
    prev_year_month(kz_term:to_integer(Year), Month);
prev_year_month(Year, <<_/binary>> = Month) ->
    prev_year_month(Year, kz_term:to_integer(Month));
prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

prev_year_month_bin(Y, M) ->
    {Year, Month} = prev_year_month(Y, M),
    {kz_term:to_binary(Year), kz_date:pad_month(Month)}.

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

-spec split_account_mod(ne_binary()) -> {ne_binary(), kz_year(), kz_month()}.
split_account_mod(?MATCH_MODB_SUFFIX_RAW(Account,Year,Month)) ->
    {kz_util:format_account_id(Account, 'raw')
    ,kz_term:to_integer(Year)
    ,kz_term:to_integer(Month)
    };
split_account_mod(?MATCH_MODB_SUFFIX_UNENCODED(Account,Year,Month)) ->
    {kz_util:format_account_id(Account, 'raw')
    ,kz_term:to_integer(Year)
    ,kz_term:to_integer(Month)
    };
split_account_mod(?MATCH_MODB_SUFFIX_ENCODED(Account,Year,Month)) ->
    {kz_util:format_account_id(Account, 'raw')
    ,kz_term:to_integer(Year)
    ,kz_term:to_integer(Month)
    }.

%% @doc
%% Equivalent to `modb_id/3`
-spec modb_id() -> ne_binary().
modb_id() ->
    {Year, Month, _} = erlang:date(),
    modb_id(Year, Month, kz_binary:rand_hex(16)).

%% @doc
%% Equivalent to `modb_id/3`
-spec modb_id(non_neg_integer() | ne_binary()) -> ne_binary().
modb_id(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    modb_id(Year, Month, kz_binary:rand_hex(16));
modb_id(Id) when is_binary(Id) ->
    {Year, Month, _} = erlang:date(),
    modb_id(Year, Month, Id).

%% @doc
%% Equivalent to `modb_id/3`
-spec modb_id(ne_binary() | kz_year(), ne_binary() | kz_month()) -> ne_binary().
modb_id(Year, Month) ->
    modb_id(Year, Month, kz_binary:rand_hex(16)).

%% @doc
%% Format a document id prefix with year and month
-spec modb_id(ne_binary() | kz_year(), ne_binary() | kz_month(), ne_binary()) -> ne_binary().
modb_id(Year, Month, Id) ->
    <<(kz_term:to_binary(Year))/binary
      ,(kz_date:pad_month(Month))/binary
      ,"-"
      ,(Id)/binary
    >>.
