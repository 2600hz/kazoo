%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_modb_util).

-include("kazoo_modb.hrl").

-export([prev_year_month/1
        ,prev_year_month/2
        ,prev_year_month_mod/1
        ]).
-export([split_account_mod/1]).
-export([modb_id/0
        ,modb_id/1
        ,modb_id/2
        ,modb_id/3
        ]).
-export([db_list/1]).
-export([get_modb_suffix/1]).

-spec prev_year_month(kz_term:ne_binary()) -> {kz_time:year(), kz_time:month()}.
prev_year_month(AccountMod) ->
    {_AccountId, Year, Month} = split_account_mod(AccountMod),
    prev_year_month(Year, Month).

-spec prev_year_month(kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary() | kz_time:month()) ->
          {kz_time:year(), kz_time:month()}.
prev_year_month(<<_/binary>> = Year, Month) ->
    prev_year_month(kz_term:to_integer(Year), Month);
prev_year_month(Year, <<_/binary>> = Month) ->
    prev_year_month(Year, kz_term:to_integer(Month));
prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

prev_year_month_bin(Y, M) ->
    {Year, Month} = prev_year_month(Y, M),
    {kz_term:to_binary(Year), kz_date:pad_month(Month)}.

-spec prev_year_month_mod(kz_term:ne_binary()) -> kz_term:ne_binary().
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

-spec split_account_mod(kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_time:year(), kz_time:month()}.
split_account_mod(?MATCH_MODB_SUFFIX_RAW(Account,Year,Month)) ->
    {kzs_util:format_account_id(Account)
    ,kz_term:to_integer(Year)
    ,kz_term:to_integer(Month)
    };
split_account_mod(?MATCH_MODB_SUFFIX_UNENCODED(Account,Year,Month)) ->
    {kzs_util:format_account_id(Account)
    ,kz_term:to_integer(Year)
    ,kz_term:to_integer(Month)
    };
split_account_mod(?MATCH_MODB_SUFFIX_ENCODED(Account,Year,Month)) ->
    {kzs_util:format_account_id(Account)
    ,kz_term:to_integer(Year)
    ,kz_term:to_integer(Month)
    }.

%% @equiv modb_id(Year, Month, kz_binary:rand_hex(16))
-spec modb_id() -> kz_term:ne_binary().
modb_id() ->
    {Year, Month, _} = erlang:date(),
    modb_id(Year, Month, kz_binary:rand_hex(16)).

%%------------------------------------------------------------------------------
%% @doc Generates MODB prefix document ID.
%% If argument is a timestamp, generates will convert it to date time and
%% then calls `modb_id(Year, Month, kz_binary:rand_hex(16))'.
%%
%% If the argument is a binary will generate an ID for current month.
%% @end
%%------------------------------------------------------------------------------
-spec modb_id(non_neg_integer() | kz_term:ne_binary()) -> kz_term:ne_binary().
modb_id(Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    modb_id(Year, Month, kz_binary:rand_hex(16));
modb_id(Id) when is_binary(Id) ->
    {Year, Month, _} = erlang:date(),
    modb_id(Year, Month, Id).

%% @equiv modb_id(Year, Month, kz_binary:rand_hex(16))
-spec modb_id(kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary() | kz_time:month()) -> kz_term:ne_binary().
modb_id(Year, Month) ->
    modb_id(Year, Month, kz_binary:rand_hex(16)).

%%------------------------------------------------------------------------------
%% @doc Format a document id prefix with year and month
%% @end
%%------------------------------------------------------------------------------
-spec modb_id(kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary() | kz_time:month(), kz_term:ne_binary()) -> kz_term:ne_binary().
modb_id(Year, Month, Id) ->
    list_to_binary([kz_term:to_binary(Year)
                   ,kz_date:pad_month(Month)
                   ,"-"
                   ,kz_term:to_binary(Id)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_list(kz_term:ne_binary()) -> kz_term:ne_binaries().
db_list(Account) ->
    db_list(kazoo_modb:get_modb(Account), []).

-spec db_list(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
db_list(MODb, MODbs) ->
    case kz_datamgr:db_exists(MODb) of
        'false' -> MODbs;
        'true' ->
            db_list(prev_year_month_mod(MODb), [MODb|MODbs])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_modb_suffix(kz_term:ne_binary() | pos_integer()) -> {kz_term:api_integer(), kz_term:api_integer()}.
get_modb_suffix(<<YearBin:4/binary, MonthBin:2/binary>>) ->
    {kz_term:safe_cast(YearBin, 'undefined', fun kz_term:to_integer/1)
    ,kz_term:safe_cast(MonthBin, 'undefined', fun kz_term:to_integer/1)
    };
get_modb_suffix(<<YearBin:4/binary, MonthBin:1/binary>>) ->
    {kz_term:safe_cast(YearBin, 'undefined', fun kz_term:to_integer/1)
    ,kz_term:safe_cast(MonthBin, 'undefined', fun kz_term:to_integer/1)
    };
get_modb_suffix(Suffix) when is_integer(Suffix) ->
    get_modb_suffix(kz_term:to_binary(Suffix));
get_modb_suffix(_) ->
    {'undefined', 'undefined'}.
