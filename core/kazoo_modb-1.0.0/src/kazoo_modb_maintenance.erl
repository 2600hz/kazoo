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
-export`([fix_rollup/1]).

-include("kazoo_modb.hrl").

delete_modbs(Period) ->
    {Year, Month, _} = erlang:date(),
    case Period =:= wh_util:to_binary(io_lib:format("~w~2..0w",[Year, Month])) of
        'true'
        -> io:format("Current month DB annihilation seems to be way to unwisely. Do it manually if you still want to. :)\n");
        'false' ->
            delete_all_modbs_by_period(Period)
    end.

delete_all_modbs_by_period(<<"20",_:4/binary>> = Period) ->
    {'ok', DbsList} = couch_mgr:db_info(),
    lists:foreach(fun(DbName) -> maybe_delete_modb(DbName, Period) end, DbsList);
delete_all_modbs_by_period(_) ->
    io:format("Wrong period format. Should be: YYYYMM\n").

maybe_delete_modb(<<_:42/binary, "-", Period/binary>> = MODbName, Period) ->
    io:format("Will be deleted ~p\n", [MODbName]),
    couch_mgr:db_delete(wh_util:format_account_id(MODbName,'encoded')),
    timer:sleep(5000);
maybe_delete_modb(DbName, _Period) ->
    io:format("Skipping ~p\n", [DbName]).

archive_modbs() ->
    do_archive_modbs(whapps_util:get_all_account_mods(), 'undefined').
archive_modbs(AccountId) ->
    do_archive_modbs(whapps_util:get_account_mods(AccountId), AccountId).

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
    {PYear, PMonth} = prev_year_month(Year, Month),
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

fix_rollup(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    {Y, M, _} = erlang:date(),
    {PYear, PMonth} = prev_year_month(Year, Month),
    Balance = wht_util:previous_balance(AccountId
                                        ,wh_util:to_binary(PYear)
                                        ,wh_util:pad_month(PMonth)),
    AccountMODb = kazoo_modb:get_modb(AccountId),
    lager:debug("rolling up ~p credits to ~s", [Balance, AccountMODb]),
    rollup(AccountMODb, Balance).

-spec prev_year_month(wh_year(), wh_month()) -> {wh_year(), wh_month()}.
prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

-spec rollup_balance(wh_json:object()) -> integer().
rollup_balance(JObj) ->
    Balance = wh_json:get_integer_value(<<"pvt_amount">>, JObj, 0),
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"credit">> -> Balance;
        <<"debit">> -> Balance * -1
    end.
