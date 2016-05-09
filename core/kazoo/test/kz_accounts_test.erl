%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_accounts_test).

-include_lib("kazoo/include/kz_types.hrl").

-include_lib("eunit/include/eunit.hrl").

%% For format_account_* tests
-export([ format_account_id_raw/1
        , format_account_id_encoded/1
        , format_account_id_unencoded/1
        , format_account_mod_id_from_year_month/1
        , format_account_mod_id_from_now/1
        , format_account_modb_raw/1
        , format_account_modb_encoded/1
        , format_account_modb_unencoded/1
        ]).

account_formats_test_() ->
    AccountId = <<A:2/binary, B:2/binary, Rest:28/binary>> = kz_term:rand_hex_binary(16),
    AccountDbUn = list_to_binary(["account/", A, "/", B, "/", Rest]),
    AccountDbEn = list_to_binary(["account%2F", A, "%2F", B, "%2F", Rest]),

    {Y, M, _} = erlang:date(),
    Now = os:timestamp(),
    Year = kz_term:to_binary(Y),
    Month = kz_time:pad_month(M),

    MODbId = list_to_binary([AccountId, "-", Year, Month]),
    MODbEn = list_to_binary([AccountDbEn, "-", Year, Month]),
    MODbUn = list_to_binary([AccountDbUn, "-", Year, Month]),

    Formats = [AccountId, AccountDbUn, AccountDbEn
               ,MODbId, MODbEn, MODbUn
              ],
    %% Note: the whole point of exporting some of these is so that function_clause can be caught
    Funs = [{fun kz_accounts:format_account_id/1, AccountId}
            ,{fun ?MODULE:format_account_id_raw/1, AccountId}
            ,{fun ?MODULE:format_account_id_encoded/1, AccountDbEn}
            ,{fun ?MODULE:format_account_id_unencoded/1, AccountDbUn}
            ,{fun kz_accounts:format_account_db/1, AccountDbEn}
            ,{fun kz_accounts:format_account_mod_id/1, MODbEn}
            ,{fun ?MODULE:format_account_mod_id_from_year_month/1, MODbEn}
            ,{fun ?MODULE:format_account_mod_id_from_now/1, MODbEn}
            ,{fun kz_accounts:format_account_modb/1, MODbId}
            ,{fun ?MODULE:format_account_modb_raw/1, MODbId}
            ,{fun ?MODULE:format_account_modb_encoded/1, MODbEn}
            ,{fun ?MODULE:format_account_modb_unencoded/1, MODbUn}
           ],
    [{format_title(Fun, Format, Expected)
      ,format_assert(Fun, Format, Expected)
     }
     || {Fun, Expected} <- Funs,
        Format <- Formats
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

is_simple_modb_converter("#Fun<kz_accounts.format_account_modb.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_accounts_test.format_account_modb_raw.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_accounts_test.format_account_modb_encoded.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_accounts_test.format_account_modb_unencoded.1>"++_) -> 'true';
is_simple_modb_converter(_Else) -> 'false'.

format_account_id_raw(F) -> kz_accounts:format_account_id(F, 'raw').
format_account_id_encoded(F) -> kz_accounts:format_account_id(F, 'encoded').
format_account_id_unencoded(F) -> kz_accounts:format_account_id(F, 'unencoded').
format_account_mod_id_from_year_month(F) ->
    {Year, Month, _} = erlang:date(),
    kz_accounts:format_account_mod_id(F, Year, Month).
format_account_mod_id_from_now(F) ->
    kz_accounts:format_account_mod_id(F, os:timestamp()).
format_account_modb_raw(F) -> kz_accounts:format_account_modb(F, 'raw').
format_account_modb_encoded(F) -> kz_accounts:format_account_modb(F, 'encoded').
format_account_modb_unencoded(F) -> kz_accounts:format_account_modb(F, 'unencoded').
