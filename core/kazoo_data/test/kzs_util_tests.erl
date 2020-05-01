%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_util_tests).

%% For format_account_* tests
-export([format_account_id_raw/1
        ,format_account_id_encoded/1
        ,format_account_id_unencoded/1
        ,format_account_mod_id_from_year_month/1
        ,format_account_mod_id_from_now/1
        ,format_account_modb_raw/1
        ,format_account_modb_encoded/1
        ,format_account_modb_unencoded/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

account_formats_test_() ->
    AccountId = <<A:2/binary, B:2/binary, Rest:28/binary>> = kz_binary:rand_hex(16),
    AccountDbUn = list_to_binary(["account/", A, "/", B, "/", Rest]),
    AccountDbEn = list_to_binary(["account%2F", A, "%2F", B, "%2F", Rest]),

    {Y, M, _} = erlang:date(),
    TS = kz_time:now_s(),
    Year = kz_term:to_binary(Y),
    Month = kz_date:pad_month(M),

    MODbId = list_to_binary([AccountId, "-", Year, Month]),
    MODbEn = list_to_binary([AccountDbEn, "-", Year, Month]),
    MODbUn = list_to_binary([AccountDbUn, "-", Year, Month]),

    Formats = [AccountId, AccountDbUn, AccountDbEn
              ,MODbId, MODbEn, MODbUn
              ],
    %% Note: the whole point of exporting some of these is so that function_clause can be caught
    Funs = [{fun kzs_util:format_account_id/1, AccountId}
           ,{fun ?MODULE:format_account_id_raw/1, AccountId}
           ,{fun ?MODULE:format_account_id_encoded/1, AccountDbEn}
           ,{fun ?MODULE:format_account_id_unencoded/1, AccountDbUn}
           ,{fun kzs_util:format_account_db/1, AccountDbEn}
           ,{fun kzs_util:format_account_mod_id/1, MODbEn}
           ,{fun ?MODULE:format_account_mod_id_from_year_month/1, MODbEn}
           ,{fun ?MODULE:format_account_mod_id_from_now/1, MODbEn}
           ,{fun kzs_util:format_account_modb/1, MODbId}
           ,{fun ?MODULE:format_account_modb_raw/1, MODbId}
           ,{fun ?MODULE:format_account_modb_encoded/1, MODbEn}
           ,{fun ?MODULE:format_account_modb_unencoded/1, MODbUn}
           ],
    [{format_title(Fun, Format, Expected)
     ,format_assert(Fun, Format, Expected)
     }
     || {Fun, Expected} <- Funs,
        Format <- Formats
    ] ++
        [?_assertEqual('undefined', kzs_util:format_account_id('undefined'))
        ,?_assertEqual(<<"accounts">>, kzs_util:format_account_id(<<"accounts">>))
        ,?_assertEqual(MODbEn, kzs_util:format_account_id(AccountDbEn, TS))
        ,?_assertEqual(MODbEn, kzs_util:format_account_mod_id(AccountDbEn, TS))
        ,?_assertEqual('undefined', kzs_util:format_account_id('undefined', Year, Month))
        ,?_assertEqual(MODbEn, kzs_util:format_account_id(AccountDbEn, Year, Month))
        ,?_assertEqual(MODbEn, kzs_util:format_account_id(AccountDbEn, Year, M))
        ,?_assertEqual(?KZ_TASKS_DB, kzs_util:format_account_id(?KZ_TASKS_DB))
        ,?_assertEqual(<<"bla">>, kzs_util:format_account_id(<<"bla">>))
        ].

format_assert(Fun, Format, Expected) ->
    Matchable = format_title(Fun, Format, Expected),
    case {is_simple_modb_converter(Matchable), Format} of
        {'true', ?MATCH_ACCOUNT_RAW(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {'true', ?MATCH_ACCOUNT_ENCODED(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {'true', ?MATCH_ACCOUNT_UNENCODED(_)} -> ?_assertException('error', 'function_clause', Fun(Format));
        {_Else, Format} -> ?_assertEqual(Expected, Fun(Format))
    end.

format_title(Fun, Format, Expected) ->
    lists:flatten(
      io_lib:format("~p converting ~s to ~s", [Fun, Format, Expected])
     ).

is_simple_modb_converter("#Fun<kz_util.format_account_modb.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<"?MODULE_STRING".format_account_modb_raw.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<"?MODULE_STRING".format_account_modb_encoded.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<"?MODULE_STRING".format_account_modb_unencoded.1>"++_) -> 'true';

%% OTP 21+ changed format
is_simple_modb_converter("fun kzs_util:format_account_modb/1"++_) -> 'true';
is_simple_modb_converter("fun "?MODULE_STRING":format_account_modb_raw/1"++_) -> 'true';
is_simple_modb_converter("fun "?MODULE_STRING":format_account_modb_encoded/1"++_) -> 'true';
is_simple_modb_converter("fun "?MODULE_STRING":format_account_modb_unencoded/1"++_) -> 'true';

is_simple_modb_converter(_Else) -> 'false'.

format_account_id_raw(F) -> kzs_util:format_account_id(F).
format_account_id_encoded(F) -> kzs_util:format_account_db(F).
format_account_id_unencoded(F) -> kzs_util:format_account_id(F, 'unencoded').
format_account_mod_id_from_year_month(F) ->
    {Year, Month, _} = erlang:date(),
    kzs_util:format_account_mod_id(F, Year, Month).
format_account_mod_id_from_now(F) ->
    kzs_util:format_account_mod_id(F, os:timestamp()).
format_account_modb_raw(F) -> kzs_util:format_account_modb(F, 'raw').
format_account_modb_encoded(F) -> kzs_util:format_account_modb(F, 'encoded').
format_account_modb_unencoded(F) -> kzs_util:format_account_modb(F, 'unencoded').
