%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(kazoo_modb_maintenance).

-export([delete_modbs/1
        ,archive_modbs/0
        ,archive_modbs/1
        ]).
-export([verify_rollups/0
        ,verify_rollups/1
        ]).
-export([fix_rollup/1]).
-export([rollup_accounts/0
        ,rollup_account/1
        ,rollup_account/2
        ]).

-include("kazoo_modb.hrl").

-spec delete_modbs(ne_binary()) -> 'ok' | 'no_return'.
-spec delete_modbs(ne_binary() | kz_year(), ne_binary() | kz_month()) -> 'ok' | 'no_return'.
delete_modbs(Period) ->
    Regex = <<"(2[0-9]{3})(0[1-9]|1[0-2])">>,
    case re:run(Period, Regex, [{'capture', 'all', 'binary'}]) of
        {'match', [_Full, Year, Month]} ->
            delete_modbs(Year, Month);
        'nomatch' ->
            io:format("period '~s' does not match YYYYMM format~n", [Period])
    end.

delete_modbs(?NE_BINARY=Year, Month) ->
    delete_modbs(kz_term:to_integer(Year), Month);
delete_modbs(Year, ?NE_BINARY=Month) ->
    delete_modbs(Year, kz_term:to_integer(Month));
delete_modbs(Year, Month) when is_integer(Year),
                               is_integer(Month),
                               Year > 2000,
                               Year < 2999,
                               Month > 0,
                               Month < 13 ->
    case erlang:date() of
        {Year, Month, _} ->
            io:format("request to delete the current MODB (~p~p) denied~n", [Year, Month]);
        {CurrYear, CurrMonth, _} when (CurrYear * 12 + CurrMonth) > (Year * 12 + Month) ->
            io:format("deleting all MODBs equal to or older than ~p/~p~n", [Year, Month]),
            delete_older_modbs(Year, Month, kapps_util:get_all_account_mods());
        {_CurrYear, _CurrMonth, _} ->
            io:format("request to delete future MODBs (~p~p) denied~n", [Year, Month])
    end.

-spec delete_older_modbs(kz_year(), kz_month(), ne_binaries()) -> 'no_return'.
delete_older_modbs(Year, Month, AccountModbs) ->
    Months = (Year * 12) + Month,
    _ = [delete_modb(AccountModb) || AccountModb <- AccountModbs, should_delete(AccountModb, Months)],
    'no_return'.

-spec should_delete(ne_binary(), pos_integer()) -> boolean().
should_delete(AccountModb, Months) ->
    {_AccountId, ModYear, ModMonth} = kazoo_modb_util:split_account_mod(AccountModb),
    ((ModYear * 12) + ModMonth) =< Months.

-spec delete_modb(ne_binary()) -> 'ok'.
delete_modb(?MATCH_MODB_SUFFIX_UNENCODED(_,_,_) = AccountModb) ->
    delete_modb(kz_util:format_account_db(AccountModb));
delete_modb(?MATCH_MODB_SUFFIX_ENCODED(_,_,_) = AccountModb) ->
    'ok' = kz_datamgr:db_archive(AccountModb),
    _Deleted = kz_datamgr:db_delete(AccountModb),
    io:format("    deleted: ~p~n", [_Deleted]),
    timer:sleep(5 * ?MILLISECONDS_IN_SECOND).

-spec archive_modbs() -> 'no_return'.
-spec archive_modbs(text()) -> 'no_return'.
archive_modbs() ->
    do_archive_modbs(kapps_util:get_all_account_mods(), 'undefined').
archive_modbs(AccountId) ->
    do_archive_modbs(kapps_util:get_account_mods(AccountId), kz_term:to_binary(AccountId)).

-spec do_archive_modbs(ne_binaries(), api_binary()) -> 'no_return'.
do_archive_modbs(MODbs, AccountId) ->
    kz_util:put_callid(?MODULE),
    lists:foreach(fun kazoo_modb:maybe_archive_modb/1, MODbs),
    Keep = kapps_config:get_integer(?CONFIG_CAT, <<"active_modbs">>, 6),
    From = case AccountId =:= 'undefined' of 'true' -> <<"all">>; 'false' -> AccountId end,
    io:format("archived ~s MODbs more than ~b months old~n", [From, Keep]),
    'no_return'.

-spec verify_rollups() -> 'ok'.
-spec verify_rollups(ne_binary()) -> 'ok'.
-spec verify_rollups(ne_binary(), kz_year(), kz_month()) -> 'ok'.
-spec verify_rollups(ne_binary(), kz_year(), kz_month(), ne_binary(), kz_json:object(), kz_std_return()) -> 'ok'.
verify_rollups() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    _ = lists:foldr(fun verify_db_rollup/2, {1, Total}, Accounts),
    'ok'.

-spec verify_db_rollup(ne_binary(), {pos_integer(), pos_integer()}) ->
                              {pos_integer(), pos_integer()}.
verify_db_rollup(AccountDb, {Current, Total}) ->
    io:format("verify rollup accounts (~p/~p) '~s'~n"
             ,[Current, Total, AccountDb]),
    verify_rollups(AccountDb),
    {Current+1, Total}.

verify_rollups(Account) ->
    {Y, M, _} = erlang:date(),
    verify_rollups(Account, Y, M).

verify_rollups(AccountDb, Year, Month) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    kazoo_modb:maybe_create(kazoo_modb:get_modb(AccountId, Year, Month)),
    case kazoo_modb:open_doc(AccountDb, <<"monthly_rollup">>, Year, Month) of
        {'ok', JObj} ->
            PrevBalance = wht_util:previous_balance(AccountId, kz_term:to_binary(Year), kz_time:pad_month(Month)),
            verify_rollups(AccountDb, Year, Month, AccountId, JObj, PrevBalance);
        {'error', 'not_found'} ->
            io:format("    account ~s : no modb @ ~p-~p~n", [AccountId, Year, Month]);
        Else ->
            io:format("    account ~s : error getting monthly rollup ~p~n", [AccountId, Else])
    end.

verify_rollups(AccountDb, Year, Month, AccountId, JObj, {'ok', Balance}) ->
    case rollup_balance(JObj) of
        Balance ->
            io:format("    account ~s : rollup for ~p-~p confirmed~n", [AccountId, Year, Month]),
            {PYear, PMonth} = kazoo_modb_util:prev_year_month(Year, Month),
            verify_rollups(AccountDb, PYear, PMonth);
        _RollupBalance ->
            io:format("    account ~s : has a discrepancy! rollup/balance ~p/~p~n"
                     ,[AccountId, _RollupBalance, Balance]
                     )
    end;
verify_rollups(AccountDb, Year, Month, AccountId, _JObj, {'error', _R}) ->
    io:format("    account ~s : error getting balance for ~b-~b: ~p~n"
             ,[AccountId, Year, Month, _R]
             ),
    {PYear, PMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    verify_rollups(AccountDb, PYear, PMonth).



-spec fix_rollup(ne_binary()) -> 'ok'.
fix_rollup(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    {Y, M, _} = erlang:date(),
    {PYear, PMonth} =  kazoo_modb_util:prev_year_month(Y, M),
    kazoo_modb:maybe_create(kazoo_modb:get_modb(AccountId, PYear, PMonth)),
    case wht_util:previous_balance(AccountId, kz_term:to_binary(PYear), kz_time:pad_month(PMonth)) of
        {'error', _R} ->
            io:format("account ~s : error getting balance for ~b-~b: ~p~n", [AccountId, PYear, PMonth, _R]);
        {'ok', Balance} ->
            AccountMODb = kazoo_modb:get_modb(AccountId),
            io:format("rolling up ~p credits to ~s~n", [Balance, AccountMODb]),
            wht_util:rollup(AccountMODb, Balance)
    end.

-spec rollup_accounts() -> 'ok'.
rollup_accounts() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = length(Accounts),
    _ = lists:foldr(fun rollup_account_fold/2, {1, Total}, Accounts),
    'ok'.

-spec rollup_account_fold(ne_binary(), {pos_integer(), pos_integer()}) ->
                                 {pos_integer(), pos_integer()}.
rollup_account_fold(AccountDb, {Current, Total}) ->
    io:format("rollup accounts (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = rollup_account(AccountDb),
    {Current + 1, Total}.

-spec rollup_account(ne_binary()) -> 'ok'.
rollup_account(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case wht_util:current_balance(AccountId) of
        {'ok', Balance} -> rollup_account(AccountId, Balance);
        {'error', _R} ->
            io:format("failed to get current balance for ~s: ~p~n", [AccountId, _R])
    end.

-spec rollup_account(ne_binary(), integer()) -> 'ok'.
rollup_account(AccountId, Balance) ->
    AccountMODb = kazoo_modb:get_modb(AccountId),
    io:format("rolling up ~p credits to ~s~n", [Balance, AccountMODb]),
    wht_util:rollup(AccountMODb, Balance).

-spec rollup_balance(kz_json:object()) -> integer().
rollup_balance(JObj) ->
    Balance = kz_json:get_integer_value(<<"pvt_amount">>, JObj, 0),
    case kz_doc:type(JObj) of
        <<"credit">> -> Balance;
        <<"debit">> -> Balance * -1
    end.
