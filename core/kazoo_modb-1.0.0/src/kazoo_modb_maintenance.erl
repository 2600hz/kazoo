%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(kazoo_modb_maintenance).

-export([delete_modbs/1
         ,archive_modbs/0, archive_modbs/1
        ]).
-export([verify_rollups/0
         ,verify_rollups/1
        ]).
-export([fix_rollup/1]).

-include("kazoo_modb.hrl").

-spec delete_modbs(ne_binary()) -> 'ok' | 'no_return'.
-spec delete_modbs(ne_binary() | wh_year(), ne_binary() | wh_month()) -> 'ok' | 'no_return'.
delete_modbs(Period) ->
    Regex = <<"(2[0-9]{3})(0[1-9]|1[0-2])">>,

    case re:run(Period, Regex, [{'capture', 'all', 'binary'}]) of
        {'match', [_Full, Year, Month]} ->
            delete_modbs(Year, Month);
        'nomatch' ->
            io:format("period '~s' does not match YYYYMM format~n", [Period])
    end.

delete_modbs(<<_/binary>> = Year, Month) ->
    delete_modbs(wh_util:to_integer(Year), Month);
delete_modbs(Year, <<_/binary>> = Month) ->
    delete_modbs(Year, wh_util:to_integer(Month));
delete_modbs(Year, Month) when is_integer(Year),
                               is_integer(Month),
                               Year > 2000 andalso Year < 2999,
                               Month > 0 andalso Month < 13 ->
    case erlang:date() of
        {Year, Month, _} ->
            io:format("request to delete the current MODB (~p~p) denied~n", [Year, Month]);
        {CurrYear, CurrMonth, _} when (CurrYear * 12 + CurrMonth) > (Year * 12 + Month) ->
            io:format("deleting all MODBs equal to or older than ~p/~p~n", [Year, Month]),
            delete_older_modbs(Year, Month, whapps_util:get_all_account_mods());
        {_CurrYear, _CurrMonth, _} ->
            io:format("request to delete future MODBs (~p~p) denied~n", [Year, Month])
    end.

-spec delete_older_modbs(wh_year(), wh_month(), ne_binaries()) -> 'no_return'.
delete_older_modbs(Year, Month, AccountModbs) ->
    Months = (Year * 12) + Month,
    _ = [delete_modb(AccountModb) || AccountModb <- AccountModbs, should_delete(AccountModb, Months)],
    'no_return'.

-spec should_delete(ne_binary(), pos_integer()) -> boolean().
should_delete(AccountModb, Months) ->
    {_AccountId, ModYear, ModMonth} = kazoo_modb_util:split_account_mod(AccountModb),
    ((ModYear * 12) + ModMonth) =< Months.

-spec delete_modb(ne_binary()) -> 'ok'.
delete_modb(AccountModb) ->
    io:format("  deleting '~s'~n", [AccountModb]),
    _Deleted = couch_mgr:db_delete(AccountModb),
    io:format("    deleted: ~p~n", [_Deleted]),
    timer:sleep(5000).

-spec archive_modbs() -> 'no_return'.
-spec archive_modbs(text()) -> 'no_return'.
archive_modbs() ->
    do_archive_modbs(whapps_util:get_all_account_mods(), 'undefined').
archive_modbs(AccountId) ->
    do_archive_modbs(whapps_util:get_account_mods(AccountId), wh_util:to_binary(AccountId)).

-spec do_archive_modbs(ne_binaries(), api_binary()) -> 'no_return'.
do_archive_modbs(MODbs, AccountId) ->
    wh_util:put_callid(?MODULE),
    _ = [kazoo_modb:maybe_archive_modb(MODb) || MODb <- MODbs],

    Keep = whapps_config:get_integer(?CONFIG_CAT, <<"active_modbs">>, 6),
    From = case AccountId =:= 'undefined' of 'true' -> <<"all">>; 'false' -> AccountId end,
    io:format("archived and removed ~s MODbs more than ~b months old~n", [From, Keep]),
    'no_return'.

-spec verify_rollups() -> 'ok'.
-spec verify_rollups(ne_binary()) -> 'ok'.
-spec verify_rollups(ne_binary(), integer(), integer()) -> 'ok'.
verify_rollups() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    lists:foldr(
        fun(AccountDb, Current) ->
            io:format("\e[46mverify rollup accounts (~p/~p) '~s'\e[0m~n"
                      ,[Current, Total, AccountDb]),
            verify_rollups(AccountDb),
            Current+1
        end
        ,1
        ,Accounts),
    'ok'.

verify_rollups(Account) ->
    {Y, M, _} = erlang:date(),
    verify_rollups(Account, Y, M).

verify_rollups(Account, Year, Month) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    {PYear, PMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    Balance = wht_util:previous_balance(Account
                                        ,wh_util:to_binary(PYear)
                                        ,wh_util:pad_month(PMonth)),
    io:format("account ~s trying... @ ~p-~p ~n"
                              ,[AccountId, Year, Month]),
    case kazoo_modb:open_doc(Account, <<"monthly_rollup">>, Year, Month) of
        {'ok', JObj} ->
            RollupBalance = rollup_balance(JObj),
            case Balance =:= RollupBalance of
                'true' ->
                    io:format("account ~s \e[35m@ ~p-~p OK!\e[0m balance/rollup ~p/~p~n"
                              ,[AccountId, Year, Month, Balance, RollupBalance]),
                    verify_rollups(Account, PYear, PMonth);
                'false' ->
                    io:format("\e[41maccount ~s has a discrepancy! rollup/balance ~p/~p \e[0m ~n"
                              ,[AccountId, RollupBalance, Balance])
            end;
        {'error', 'not_found'} ->
            io:format("\e[33maccount ~s : no modb @ ~p-~p \e[0m ~n", [AccountId, Year, Month]);
        Else ->
            io:format("\e[41maccount ~s: error getting monthly rollup ~p \e[0m ~n", [AccountId, Else])
    end.

-spec fix_rollup(ne_binary()) -> 'ok'.
fix_rollup(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    {Y, M, _} = erlang:date(),
    {PYear, PMonth} =  kazoo_modb_util:prev_year_month(Y, M),
    Balance = wht_util:previous_balance(AccountId
                                        ,wh_util:to_binary(PYear)
                                        ,wh_util:pad_month(PMonth)
                                       ),
    AccountMODb = kazoo_modb:get_modb(AccountId),
    lager:debug("rolling up ~p credits to ~s", [Balance, AccountMODb]),
    wht_util:rollup(AccountMODb, Balance).

-spec rollup_balance(wh_json:object()) -> integer().
rollup_balance(JObj) ->
    Balance = wh_json:get_integer_value(<<"pvt_amount">>, JObj, 0),
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"credit">> -> Balance;
        <<"debit">> -> Balance * -1
    end.
