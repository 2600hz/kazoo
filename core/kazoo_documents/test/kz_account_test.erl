%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_account_test).

-include_lib("kazoo/include/kz_types.hrl").

-include_lib("eunit/include/eunit.hrl").

%% For format_* tests
-export([ format_id_raw/1
        , format_id_encoded/1
        , format_id_unencoded/1
        , format_mod_id_from_year_month/1
        , format_mod_id_from_now/1
        , format_modb_raw/1
        , format_modb_encoded/1
        , format_modb_unencoded/1
        ]).

-define(ID, <<"_id">>).
-define(TREE, <<"pvt_tree">>).

-define(MASTER_ACCOUNT_ID, <<"1">>).
-define(MASTER_ACCOUNT, kz_json:from_list([{?TREE, []}
                                           ,{?ID, ?MASTER_ACCOUNT_ID}
                                          ])).

-define(SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_ACCOUNT, kz_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID]}
                                        ,{?ID, ?SUB_ACCOUNT_ID}
                                       ])).

-define(SUB_SUB_ACCOUNT_ID, <<"2">>).
-define(SUB_SUB_ACCOUNT, kz_json:from_list([{?TREE, [?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID]}
                                            ,{?ID, ?SUB_SUB_ACCOUNT_ID}
                                           ])).

parent_account_id_test() ->
    ?assertEqual('undefined', kz_account:parent_account_id(?MASTER_ACCOUNT)),
    ?assertEqual(?MASTER_ACCOUNT_ID, kz_account:parent_account_id(?SUB_ACCOUNT)),
    ?assertEqual(?SUB_ACCOUNT_ID, kz_account:parent_account_id(?SUB_SUB_ACCOUNT)).

tree_test() ->
    ?assertEqual([], kz_account:tree(?MASTER_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID], kz_account:tree(?SUB_ACCOUNT)),
    ?assertEqual([?MASTER_ACCOUNT_ID, ?SUB_ACCOUNT_ID], kz_account:tree(?SUB_SUB_ACCOUNT)).

trial_time_test_() ->
    Now = kz_time:current_tstamp(),
    Passed = kz_account:set_trial_expiration(kz_account:new(), Now - 10000),
    Active = kz_account:set_trial_expiration(kz_account:new(), Now + 10000),

    [{"testing expired trial accounts are computed as such"
      ,?_assertEqual('true', kz_account:trial_has_expired(Passed, Now))
     }
     ,{"testing current trial accounts are computed as such"
       ,?_assertEqual('false', kz_account:trial_has_expired(Active, Now))
      }
     ,{"testing that current trial accounts have proper time left computed"
       ,?_assertEqual(10000, kz_account:trial_time_left(Active, Now))
      }
     ,{"testing that expired trial accounts have proper time since expiration computed"
       ,?_assertEqual(-10000, kz_account:trial_time_left(Passed, Now))
      }
    ].


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
    Funs = [{fun kz_account:format_id/1, AccountId}
            ,{fun ?MODULE:format_id_raw/1, AccountId}
            ,{fun ?MODULE:format_id_encoded/1, AccountDbEn}
            ,{fun ?MODULE:format_id_unencoded/1, AccountDbUn}
            ,{fun kz_account:format_db/1, AccountDbEn}
            ,{fun kz_account:format_mod_id/1, MODbEn}
            ,{fun ?MODULE:format_mod_id_from_year_month/1, MODbEn}
            ,{fun ?MODULE:format_mod_id_from_now/1, MODbEn}
            ,{fun kz_account:format_modb/1, MODbId}
            ,{fun ?MODULE:format_modb_raw/1, MODbId}
            ,{fun ?MODULE:format_modb_encoded/1, MODbEn}
            ,{fun ?MODULE:format_modb_unencoded/1, MODbUn}
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

is_simple_modb_converter("#Fun<kz_account.format_modb.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_account_test.format_modb_raw.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_account_test.format_modb_encoded.1>"++_) -> 'true';
is_simple_modb_converter("#Fun<kz_account_test.format_modb_unencoded.1>"++_) -> 'true';
is_simple_modb_converter(_Else) -> 'false'.

format_id_raw(F) -> kz_account:format_id(F, 'raw').
format_id_encoded(F) -> kz_account:format_id(F, 'encoded').
format_id_unencoded(F) -> kz_account:format_id(F, 'unencoded').
format_mod_id_from_year_month(F) ->
    {Year, Month, _} = erlang:date(),
    kz_account:format_mod_id(F, Year, Month).
format_mod_id_from_now(F) ->
    kz_account:format_mod_id(F, os:timestamp()).
format_modb_raw(F) -> kz_account:format_modb(F, 'raw').
format_modb_encoded(F) -> kz_account:format_modb(F, 'encoded').
format_modb_unencoded(F) -> kz_account:format_modb(F, 'unencoded').
