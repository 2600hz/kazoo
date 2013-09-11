%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%% Utility module for V3 Kazoo Migration
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_v3_migrate_lib).

%% API
-export([get_prev_n_month_date_list/2
         ,get_prev_n_month_date_list/3
         ,get_next_n_month_date_list/2
         ,get_next_n_month_date_list/3
         ,get_prev_n_months/3
         ,get_prev_n_months/4
         ,get_next_n_months/3
         ,get_next_n_months/4
         ,prev_month/2
         ,next_month/2
         ,generate_test_accounts/3
         ,get_test_account_details/1
         ,delete_test_accounts/0
        ]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_prev_n_month_date_list(wh_datetime(), pos_integer()) -> wh_proplist().
get_prev_n_month_date_list({{Year, Month, _}, _}, NumMonths) ->
    get_prev_n_month_date_list(Year, Month, NumMonths).

-spec get_prev_n_month_date_list(wh_year(), wh_month(), pos_integer()) -> wh_proplist().
get_prev_n_month_date_list(Year, Month, NumMonths) ->
    SortDirection = 'DESC',
    DateRange = get_prev_n_months(Year, Month, NumMonths, SortDirection),
    lists:foldl(fun({NextYear, NextMonth}, Acc) ->
                        build_month_date_list(NextYear, NextMonth, SortDirection, Acc) 
                end, [], DateRange).

-spec get_next_n_month_date_list(wh_datetime(), pos_integer()) -> wh_proplist().
-spec get_next_n_month_date_list(wh_year(), wh_month(), pos_integer()) -> wh_proplist().
get_next_n_month_date_list({{Year, Month, _}, _}, NumMonths) ->
    get_next_n_month_date_list(Year, Month, NumMonths).

get_next_n_month_date_list(Year, Month, NumMonths) ->
    SortDirection = 'ASC',
    DateRange = get_next_n_months(Year, Month, NumMonths, SortDirection),
    lists:foldl(fun({NextYear, NextMonth}, Acc) -> 
                        build_month_date_list(NextYear, NextMonth, SortDirection, Acc) 
                end, [], DateRange).

-spec build_month_date_list(wh_year(), wh_month(), atom(), list()) -> any().
build_month_date_list(Year, Month, 'DESC', Acc) ->
    [{Year, Month, Day}
     || Day <- lists:seq(calendar:last_day_of_the_month(Year, Month), 1, -1)
    ] ++ Acc;
build_month_date_list(Year, Month, 'ASC', Acc) ->
    [{Year, Month, Day}
     || Day <- lists:seq(1, calendar:last_day_of_the_month(Year, Month))
    ] ++ Acc.

-spec get_test_account_details(pos_integer()) -> api_binaries().
get_test_account_details(NumAccounts) ->
    [{<<"v3migratetest", (wh_util:to_binary(io_lib:format("~3..0B",[X])))/binary>>
          , <<"v3migratetest",(wh_util:to_binary(io_lib:format("~3..0B", [X])))/binary,".realm.com">>
          , <<"v3testuser", (wh_util:to_binary(io_lib:format("~3..0B", [X])))/binary, "-user">>
          , <<"v3password">>
     } || X <- lists:seq(1, NumAccounts)].

-spec generate_test_accounts(pos_integer(), pos_integer(), pos_integer()) -> 'ok'.
generate_test_accounts(NumAccounts, NumMonths, NumCdrs) ->
    CdrJObjFixture = wh_json:load_fixture_from_file('cdr', "fixtures/cdr.json"),
    lists:foreach(fun(AccountDetail) ->
                          generate_test_account(AccountDetail
                                                ,NumMonths
                                                ,NumCdrs
                                                ,CdrJObjFixture)
                  end, get_test_account_details(NumAccounts)).

-spec generate_test_account({ne_binary(),ne_binary(), ne_binary(), ne_binary()}
                            ,pos_integer(), pos_integer()
                            ,wh_json:object()
                           ) -> 'ok' | {'error', any()}.
generate_test_account({AccountName, AccountRealm, User, Pass}, NumMonths, NumCdrs, CdrJObjFixture) ->
    crossbar_maintenance:create_account(AccountName, AccountRealm, User, Pass),
    case get_account_by_realm(AccountRealm) of
        {'ok', AccountDb} ->
            DateRange = get_prev_n_month_date_list(calendar:universal_time(), NumMonths),
            lists:foreach(fun(Date) -> 
                                  generate_test_account_cdrs(AccountDb, CdrJObjFixture, Date, NumCdrs) 
                          end, DateRange);
        {'multiples', AccountDbs} ->
            lager:debug("found multiple DBS for Account Name: ~p", [AccountDbs]),
            {'error', 'multiple_account_dbs'};
        {'error', Reason} ->
            lager:debug("failed to find account: ~p [~s]", [Reason, AccountName]),
            {'error', Reason}
    end.

-spec get_account_by_realm(ne_binary()) -> 
                                  {'ok', account_db()} | {'multiples', any()} | {'error', any()}.
get_account_by_realm(AccountRealm) ->
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_realm">>, [{'key', AccountRealm}]) of
        {'ok', [JObj]} ->
            AccountDb = wh_json:get_value([<<"value">>, <<"account_db">>], JObj),
            _AccountId = wh_util:format_account_id(AccountDb, 'raw'),
            {'ok', AccountDb};
        {'ok', []} ->
            {'error', 'not_found'};
        {'ok', [_|_]=JObjs} ->
            AccountDbs = [wh_json:get_value([<<"value">>, <<"account_db">>], JObj) || JObj <- JObjs],
            {'multiples', AccountDbs};
        _E ->
            lager:debug("error while fetching accounts by realm: ~p", [_E]),
            {'error', 'not_found'}
    end.

-spec generate_test_account_cdrs(ne_binary(), wh_json:object(), wh_date(), pos_integer()) -> 'ok'.
generate_test_account_cdrs(_, _, _, 0) -> 'ok';
generate_test_account_cdrs(AccountDb, CdrJObjFixture, Date, NumCdrs) ->
    DateTime = {Date, {random:uniform(23), random:uniform(59), random:uniform(59)}},
    CreatedAt = calendar:datetime_to_gregorian_seconds(DateTime),
    Props = [{<<"call_id">>, <<(couch_mgr:get_uuid())/binary>>}
             ,{<<"timestamp">>, CreatedAt}
             ,{<<"pvt_created">>, CreatedAt}
             ,{<<"pvt_modified">>, CreatedAt}
            ],
    Doc = wh_json:set_values(Props, CdrJObjFixture),
    case couch_mgr:save_doc(AccountDb, Doc) of
        {'error',_}=_E -> lager:debug("cdr Save Failed: ~p", [_E]);
        {'ok', _} -> 'ok'
    end,
    generate_test_account_cdrs(AccountDb, CdrJObjFixture, Date, NumCdrs - 1).

-spec delete_test_accounts() -> 'ok' | wh_std_return().
delete_test_accounts() ->
    case whapps_util:get_all_accounts() of
        {'error', _E} -> lager:debug("error retrieving accounts: ~p", [_E]);
        [] -> 'ok';
        Accounts ->
            [maybe_delete_test_account(AccountDb) || AccountDb <- Accounts],
            'ok'
    end.

-spec maybe_get_migrate_account(account_db()) -> 'false' | wh_json:object().
maybe_get_migrate_account(AccountDb) ->
    case couch_mgr:get_results(AccountDb, <<"account/listing_by_realm">>, ['include_docs']) of
        {'error', _} -> 'false';
        [] -> 'false';
        {'ok', Results} ->
            [wh_json:get_value(<<"doc">>, Result)
             || Result <- Results
                    ,matches_realm(wh_json:get_value(<<"key">>, Result))]
    end.

-spec matches_realm(ne_binary()) -> boolean().
matches_realm(<<"migratetest", _:3/binary, ".realm.com">>) -> 'true';
matches_realm(<<"v3migratetest", _:3/binary, ".realm.com">>) -> 'true';
matches_realm(Realm) ->
    lager:debug("realm does not match migrate pattern: ~p", [Realm]),
    'false'.

-spec maybe_delete_test_account(account_db()) -> 'ok' | {'error', any()}.
maybe_delete_test_account(AccountDb) ->
    case maybe_get_migrate_account(AccountDb) of
        'false' -> 'ok';
        [] -> lager:debug("account_db is not a migrate test: ~p", [AccountDb]);
        [_Account] ->
            NumMonthsToShard = whapps_config:get_integer(?CONFIG_CAT, <<"v3_migrate_num_months">>, 4),
            AccountId = wh_util:format_account_id(AccountDb, 'raw'),
            {{CurrentYear, CurrentMonth, _}, _} = calendar:universal_time(),
            Months = get_prev_n_months(CurrentYear, CurrentMonth, NumMonthsToShard),
            AccountId = wh_util:format_account_id(AccountDb, 'raw'),
            [delete_account_database(AccountId, {Year, Month})
             || {Year, Month} <- Months],
            couch_mgr:del_doc(<<"accounts">>, AccountId),
            couch_mgr:db_delete(AccountDb)
    end.

-spec delete_account_database(account_id(), {wh_year(), wh_month()}) ->
                                     'ok' | {'error', any()}.
delete_account_database(AccountId, {Year, Month}) ->
    AccountMODb = wh_util:format_account_id(AccountId, Year, Month),
    couch_mgr:db_delete(AccountMODb).

-spec get_prev_n_months(wh_year(), wh_month(), pos_integer()) -> wh_proplist().
get_prev_n_months(Year, Month, NumMonths) when Month =< 12, Month > 0 ->
    get_prev_n_months(Year, Month, NumMonths, 'ASC').

-spec get_prev_n_months(wh_year(), wh_month(), pos_integer(), atom()) -> wh_proplist().
get_prev_n_months(Year, Month, NumMonths, 'ASC') when Month =< 12, Month > 0 ->
    lists:reverse(prev_n_months(Year, Month, NumMonths, []));
get_prev_n_months(Year, Month, NumMonths, 'DESC') when Month =< 12, Month > 0 ->
    prev_n_months(Year, Month, NumMonths, []).


-spec get_next_n_months(wh_year(), wh_month(), pos_integer()) -> wh_proplist().
get_next_n_months(Year, Month, NumMonths) when Month =< 12, Month > 0 ->
    get_next_n_months(Year, Month, NumMonths, 'ASC').

-spec get_next_n_months(wh_year(), wh_month(), pos_integer(), atom()) -> wh_proplist().
get_next_n_months(Year, Month, NumMonths, 'ASC') when Month =< 11, Month > 0 ->
    next_n_months(Year, Month, NumMonths, []);
get_next_n_months(Year, Month, NumMonths, 'DESC') when Month =< 12, Month > 0 ->
    lists:reverse(next_n_months(Year, Month, NumMonths, [])).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
prev_n_months(_, _, 0, Acc) ->
    Acc;
prev_n_months(CurrentYear, 1, NumMonths, Acc) ->
    prev_n_months(CurrentYear - 1, 12, NumMonths - 1, [{CurrentYear, 1} | Acc]);
prev_n_months(CurrentYear, Month, NumMonths, Acc) ->
    prev_n_months(CurrentYear, Month -1, NumMonths - 1, [{CurrentYear, Month} | Acc]).

next_n_months(_, _, 0, Acc) ->
    Acc;
next_n_months(Year, 12, NumMonths, Acc) ->
    next_n_months(Year + 1, 1, NumMonths - 1, [{Year, 12} | Acc]);
next_n_months(Year, Month, NumMonths, Acc) ->
    next_n_months(Year, Month + 1, NumMonths - 1, [{Year, Month} | Acc]).

-spec prev_month(wh_year(), wh_month()) -> {wh_year(), wh_month()}.
prev_month(Year, 1) ->
    {Year - 1, 12};
prev_month(Year, Month) ->
    {Year, Month - 1}.

-spec next_month(wh_year(), wh_month()) -> {wh_year(), wh_month()}.
next_month(Year, 12) ->
    {Year + 1, 1};
next_month(Year, Month) ->
    {Year, Month + 1}.
