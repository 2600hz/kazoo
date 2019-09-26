%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_yodb_util).

-include("kazoo_yodb.hrl").

-export([prev_year/1
        ,prev_year_yod/1
        ]).
-export([split_account_yod/1]).
-export([yodb_id/0
        ,yodb_id/1
        ,yodb_id/2
        ]).
-export([db_list/1]).
-export([get_yodb_suffix/1]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prev_year(kz_term:ne_binary() | kz_time:year()) -> kz_time:year().
prev_year(<<_:4/binary>> = Year) ->
    prev_year(kz_term:to_integer(Year));
prev_year(<<AccountYod/binary>>) ->
    {_AccountId, Year} = split_account_yod(AccountYod),
    prev_year(Year);
prev_year(Year) ->
    Year - 1.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prev_year_bin(kz_term:ne_binary() | kz_term:year()) -> kz_term:ne_binary().
prev_year_bin(Y) ->
    Year = prev_year(Y),
    kz_term:to_binary(Year).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prev_year_yod(kz_term:ne_binary()) -> kz_term:ne_binary().
prev_year_yod(?MATCH_YODB_SUFFIX_RAW(AccountId, Year)) ->
    PrevYear = prev_year_bin(Year),
    ?MATCH_YODB_SUFFIX_RAW(AccountId, PrevYear);
prev_year_yod(?MATCH_YODB_SUFFIX_UNENCODED(A, B, Rest, Year)) ->
    PrevYear = prev_year_bin(Year),
    ?MATCH_YODB_SUFFIX_UNENCODED(A, B, Rest, PrevYear);
prev_year_yod(?MATCH_YODB_SUFFIX_ENCODED(A, B, Rest, Year)) ->
    PrevYear = prev_year_bin(Year),
    ?MATCH_YODB_SUFFIX_ENCODED(A, B, Rest, PrevYear);
prev_year_yod(?MATCH_YODB_SUFFIX_encoded(A, B, Rest, Year)) ->
    PrevYear = prev_year_bin(Year),
    ?MATCH_YODB_SUFFIX_encoded(A, B, Rest, PrevYear).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec split_account_yod(kz_term:ne_binary()) -> {kz_term:ne_binary(), kz_time:year()}.
split_account_yod(?MATCH_YODB_SUFFIX_RAW(Account, Year)) ->
    {kzs_util:format_account_id(Account)
    ,kz_term:to_integer(Year)
    };
split_account_yod(?MATCH_YODB_SUFFIX_UNENCODED(Account, Year)) ->
    {kzs_util:format_account_id(Account)
    ,kz_term:to_integer(Year)
    };
split_account_yod(?MATCH_YODB_SUFFIX_ENCODED(Account, Year)) ->
    {kzs_util:format_account_id(Account)
    ,kz_term:to_integer(Year)
    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec yodb_id() -> kz_term:ne_binary().
yodb_id() ->
    {Year, _, _} = erlang:date(),
    yodb_id(Year, kz_binary:rand_hex(16)).

%%------------------------------------------------------------------------------
%% @doc Generates YODB prefix document ID.
%% If argument is a timestamp, generates will convert it to date time and
%% then calls `yodb_id(Year, Month, kz_binary:rand_hex(16))'.
%%
%% If the argument is a binary will generate an ID for current month.
%% @end
%%------------------------------------------------------------------------------
-spec yodb_id(non_neg_integer() | kz_term:ne_binary() | kz_time:year()) -> kz_term:ne_binary().
yodb_id(Timestamp)  when is_integer(Timestamp)
                         andalso Timestamp > 10000 ->
    {{Year, _, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    yodb_id(Year, kz_binary:rand_hex(16));
yodb_id(<<_binary:4/binary>>=Year) ->
    yodb_id(kz_term:to_integer(Year), kz_binary:rand_hex(16));
yodb_id(Id) when is_binary(Id) ->
    {Year, _, _} = erlang:date(),
    yodb_id(Year, Id);
yodb_id(Year) ->
    yodb_id(Year, kz_binary:rand_hex(16)).

%%------------------------------------------------------------------------------
%% @doc Format a document id prefix with year
%% @end
%%------------------------------------------------------------------------------
-spec yodb_id(kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary()) -> kz_term:ne_binary().
yodb_id(Year, Id) ->
    list_to_binary([kz_term:to_binary(Year)
                   ,"-"
                   ,kz_term:to_binary(Id)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_list(kz_term:ne_binary()) -> kz_term:ne_binaries().
db_list(Account) ->
    db_list(kazoo_yodb:get_yodb(Account), []).

-spec db_list(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
db_list(YODb, YODbs) ->
    case kz_datamgr:db_exists(YODb) of
        'false' -> YODbs;
        'true' ->
            db_list(prev_year_yod(YODb), [YODb|YODbs])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_yodb_suffix(kz_term:ne_binary() | pos_integer()) -> kz_term:api_integer().
get_yodb_suffix(<<YearBin:4/binary>>) ->
    kz_term:safe_cast(YearBin, 'undefined', fun kz_term:to_integer/1);
get_yodb_suffix(Suffix) when is_integer(Suffix) ->
    get_yodb_suffix(kz_term:to_binary(Suffix));
get_yodb_suffix(_) ->
    'undefined'.
