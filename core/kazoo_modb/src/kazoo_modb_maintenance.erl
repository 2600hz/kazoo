%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_modb_maintenance).

-export([delete_modbs/1
        ,delete_modbs/2
        ]).
-export([archive_modbs/0
        ,archive_modbs/1
        ]).
-export([current_rollups/0]).
-export([verify_rollups/0
        ,verify_rollups/1
        ,verify_all_rollups/0
        ]).
-export([fix_rollup/1
        ,fix_rollup/3
        ]).
-export([rollup_accounts/0
        ,rollup_account/1
        ,rollup_account/2
        ]).
-export([dump_transactions/3]).

-export([register_views/0]).

-include("kazoo_modb.hrl").

-define(HEADER_MAP, [{<<"_id">>, <<"ID">>}
                    ,{<<"_rev">>, <<"Revision">>}
                    ,{<<"description">>, <<"Description">>}
                    ,{<<"pvt_account_db">>, <<"Account DB">>}
                    ,{<<"pvt_account_id">>, <<"Amount">>}
                    ,{<<"pvt_amount">>, <<"Amount">>}
                    ,{<<"pvt_code">>, <<"Transaction Code">>}
                    ,{<<"pvt_created">>, <<"Created">>}
                    ,{<<"pvt_modified">>, <<"Modified">>}
                    ,{<<"pvt_reason">>, <<"Reason">>}
                    ,{<<"pvt_type">>, <<"Type">>}
                    ,{<<"pvt_vsn">>, <<"Version">>}
                    ]).

-spec delete_modbs(kz_term:ne_binary()) -> 'ok' | 'no_return'.
delete_modbs(Period) ->
    delete_modbs(Period, 'true').

-spec delete_modbs(kz_term:ne_binary(), boolean() | kz_term:ne_binary()) -> 'ok' | 'no_return'.
delete_modbs(Period, ShouldArchive) ->
    Regex = <<"(2[0-9]{3})(0[1-9]|1[0-2])">>,
    case re:run(Period, Regex, [{'capture', 'all', 'binary'}]) of
        {'match', [_Full, Year, Month]} ->
            delete_modbs(Year, Month, kz_term:is_true(ShouldArchive));
        'nomatch' ->
            io:format("period '~s' does not match YYYYMM format~n", [Period])
    end.

-spec delete_modbs(kz_term:ne_binary() | kz_time:year(), kz_term:ne_binary() | kz_time:month(), boolean()) -> 'ok' | 'no_return'.
delete_modbs(<<_/binary>> = Year, Month, ShouldArchive) ->
    delete_modbs(kz_term:to_integer(Year), Month, ShouldArchive);
delete_modbs(Year, <<_/binary>> = Month, ShouldArchive) ->
    delete_modbs(Year, kz_term:to_integer(Month), ShouldArchive);
delete_modbs(Year, Month, ShouldArchive) when is_integer(Year),
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
            delete_older_modbs(Year, Month, kapps_util:get_all_account_mods(), ShouldArchive);
        {_CurrYear, _CurrMonth, _} ->
            io:format("request to delete future MODBs (~p~p) denied~n", [Year, Month])
    end.

-spec delete_older_modbs(kz_time:year(), kz_time:month(), kz_term:ne_binaries(), boolean()) -> 'no_return'.
delete_older_modbs(Year, Month, AccountModbs, ShouldArchive) ->
    Months = (Year * 12) + Month,
    _ = [delete_modb(AccountModb, ShouldArchive) || AccountModb <- AccountModbs, should_delete(AccountModb, Months)],
    'no_return'.

-spec should_delete(kz_term:ne_binary(), pos_integer()) -> boolean().
should_delete(AccountModb, Months) ->
    {_AccountId, ModYear, ModMonth} = kazoo_modb_util:split_account_mod(AccountModb),
    ((ModYear * 12) + ModMonth) =< Months.

-spec delete_modb(kz_term:ne_binary(), boolean()) -> 'ok'.
delete_modb(?MATCH_MODB_SUFFIX_UNENCODED(_,_,_) = AccountModb, ShouldArchive) ->
    delete_modb(kz_util:format_account_db(AccountModb), ShouldArchive);
delete_modb(?MATCH_MODB_SUFFIX_ENCODED(_,_,_) = AccountModb, ShouldArchive) ->
    'ok' = case ShouldArchive of
               'true' -> kz_datamgr:db_archive(AccountModb);
               'false' -> io:format("deleting database ~s~n", [AccountModb])
           end,
    _Deleted = kz_datamgr:db_delete(AccountModb),
    io:format("    deleted: ~p~n", [_Deleted]),
    timer:sleep(5 * ?MILLISECONDS_IN_SECOND).

-spec archive_modbs() -> 'no_return'.
archive_modbs() ->
    do_archive_modbs(kapps_util:get_all_account_mods(), 'undefined').

-spec archive_modbs(kz_term:text()) -> 'no_return'.
archive_modbs(AccountId) ->
    do_archive_modbs(kapps_util:get_account_mods(AccountId), kz_term:to_binary(AccountId)).

-spec do_archive_modbs(kz_term:ne_binaries(), kz_term:api_binary()) -> 'no_return'.
do_archive_modbs(MODbs, AccountId) ->
    kz_util:put_callid(?MODULE),
    lists:foreach(fun kazoo_modb:maybe_archive_modb/1, MODbs),
    Keep = kapps_config:get_integer(?CONFIG_CAT, <<"active_modbs">>, 6),
    From = case AccountId =:= 'undefined' of 'true' -> <<"all">>; 'false' -> AccountId end,
    io:format("archived ~s MODbs more than ~b months old~n", [From, Keep]),
    'no_return'.

-spec current_rollups() -> 'ok'.
current_rollups() ->
    Accounts = kapps_util:get_all_accounts(),
    current_rollups(Accounts).

-spec current_rollups(kz_term:ne_binaries()) -> 'ok'.
current_rollups([]) -> 'ok';
current_rollups([Account|Accounts]) ->
    {Year, Month, _} = erlang:date(),
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Remaining = length(Accounts),
    _ = case kazoo_modb:open_doc(AccountId, <<"monthly_rollup">>, Year, Month) of
            {'ok', JObj} ->
                {{_Y, _M, _D}, {_H, _Min, _S}} = calendar:gregorian_seconds_to_datetime(kz_doc:created(JObj)),
                io:format("[~p] account ~s has rollup (created ~p/~p/~p ~p:~p:~p) for ~p-~p with balance ~p~n"
                         ,[Remaining, AccountId, _Y, _M, _D, _H, _Min, _S, Year, Month, rollup_balance(JObj)]
                         );
            {'error', 'not_found'} ->
                io:format("[~p] account ~s has no rollup for ~p-~p~n", [Remaining, AccountId, Year, Month]);
            Else ->
                io:format("[~p] account ~s error getting monthly rollup ~p~n", [Remaining, AccountId, Else])
        end,
    current_rollups(Accounts).

-type rollup_option() :: {'all_rollups', 'true'}.
-type rollup_options() :: [rollup_option()].

-spec verify_rollups() -> 'ok'.
verify_rollups() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    _ = lists:foldr(fun verify_db_rollup/2, {1, Total, []}, Accounts),
    'ok'.

-spec verify_all_rollups() -> 'ok'.
verify_all_rollups() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    _ = lists:foldr(fun verify_db_rollup/2, {1, Total, [{'all_rollups', 'true'}]}, Accounts),
    'ok'.

-type rollup_acc() :: {pos_integer(), pos_integer(), rollup_options()}.
-spec verify_db_rollup(kz_term:ne_binary(), rollup_acc()) -> rollup_acc().
verify_db_rollup(AccountDb, {Current, Total, Options}) ->
    io:format("verify rollup accounts (~p/~p) '~s'~n"
             ,[Current, Total, AccountDb]
             ),
    verify_rollups(AccountDb, Options),
    {Current+1, Total, Options}.

-spec verify_rollups(kz_term:ne_binary()) -> 'ok'.
verify_rollups(Account) ->
    verify_rollups(Account, [{'all_rollups', 'true'}]).

-spec verify_rollups(kz_term:ne_binary(), rollup_options()) -> 'ok'.
verify_rollups(Account, Options) ->
    {Year, Month, _} = erlang:date(),
    verify_rollups(Account, Year, Month, Options).

-spec verify_rollups(kz_term:ne_binary(), kz_time:year(), kz_time:month(), rollup_options()) -> 'ok'.
verify_rollups(AccountDb, Year, Month, Options) when is_list(Options) ->
    AllRollups = props:get_value('all_rollups', Options),
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    kazoo_modb:maybe_create(kazoo_modb:get_modb(AccountId, Year, Month)),
    case kazoo_modb:open_doc(AccountDb, <<"monthly_rollup">>, Year, Month) of
        {'ok', JObj} when AllRollups ->
            _ = verify_monthly_rollup(AccountDb, Year, Month, JObj),
            {PYear, PMonth} = kazoo_modb_util:prev_year_month(Year, Month),
            verify_rollups(AccountDb, PYear, PMonth, Options);
        {'ok', JObj} ->
            verify_monthly_rollup(AccountDb, Year, Month, JObj);
        {'error', 'not_found'} ->
            io:format("    account ~s has no rollup for ~p-~p~n", [AccountId, Year, Month]);
        Else ->
            io:format("    account ~s error getting monthly rollup ~p~n", [AccountId, Else])
    end.

-spec verify_monthly_rollup(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_json:object()) -> 'ok'.
verify_monthly_rollup(AccountDb, Year, Month, MonthlyJObj) ->
    {PYear, PMonth} =  kazoo_modb_util:prev_year_month(Year, Month),
    PrevBalance = wht_util:previous_balance(AccountDb, PYear, PMonth),
    verify_rollups(AccountDb, Year, Month, MonthlyJObj, PrevBalance).

-spec verify_rollups(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_json:object(), kz_term:std_return()) -> 'ok'.
verify_rollups(AccountDb, Year, Month, JObj, {'ok', Balance}) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    case rollup_balance(JObj) of
        Balance ->
            io:format("    account ~s rollup for ~p-~p confirmed~n", [AccountId, Year, Month]);
        _RollupBalance ->
            io:format("    account ~s rollup for ~p-~p balance is ~p but previous month balance was ~p~n"
                     ,[AccountId, Year, Month, _RollupBalance, Balance]
                     )
    end;
verify_rollups(AccountDb, Year, Month, _JObj, {'error', _R}) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    io:format("    account ~s : error getting balance for ~b-~b: ~p~n"
             ,[AccountId, Year, Month, _R]
             ).

-spec fix_rollup(kz_term:ne_binary()) -> 'ok'.
fix_rollup(Account) ->
    {Year, Month, _} = erlang:date(),
    fix_rollup(Account, Year, Month).

-spec fix_rollup(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
fix_rollup(Account, Year, Month) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    Y = kz_term:to_integer(Year),
    M = kz_term:to_integer(Month),
    {PYear, PMonth} =  kazoo_modb_util:prev_year_month(Y, M),
    kazoo_modb:maybe_create(kazoo_modb:get_modb(AccountId, PYear, PMonth)),
    case wht_util:previous_balance(AccountId, PYear, PMonth) of
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

-spec rollup_account_fold(kz_term:ne_binary(), {pos_integer(), pos_integer()}) ->
                                 {pos_integer(), pos_integer()}.
rollup_account_fold(AccountDb, {Current, Total}) ->
    io:format("rollup accounts (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = rollup_account(AccountDb),
    {Current + 1, Total}.

-spec rollup_account(kz_term:ne_binary()) -> 'ok'.
rollup_account(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case wht_util:current_balance(AccountId) of
        {'ok', Balance} -> rollup_account(AccountId, Balance);
        {'error', _R} ->
            io:format("failed to get current balance for ~s: ~p~n", [AccountId, _R])
    end.

-spec rollup_account(kz_term:ne_binary(), integer()) -> 'ok'.
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

-spec dump_transactions(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return' | 'ok'.
dump_transactions(Account, Year, Month) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    View = <<"transactions/by_timestamp">>,
    ViewOptions = ['include_docs'
                  ,{'year', kz_term:to_binary(Year)}
                  ,{'month', kz_date:pad_month(Month)}
                  ],
    case kazoo_modb:get_results(AccountId, View, ViewOptions) of
        {'ok', JObjs} ->
            CsvOptions = [{'transform_fun', fun fix_value_map/2}
                         ,{'header_map', ?HEADER_MAP}
                         ],
            Docs = [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            io:format("~s~n", [kz_csv:from_jobjs(Docs, CsvOptions)]),
            'no_return';
        {'error', _R} ->
            io:format("unable to get transactions for ~s (~p-~p): ~p", [AccountId, Month, Year, _R])
    end.

-spec fix_value_map(kz_term:ne_binary(), kz_json:term()) -> kz_json:term().
fix_value_map(<<"pvt_amount">> = Key, Value) -> {Key, wht_util:units_to_dollars(Value)};
fix_value_map(<<"pvt_modified">> = Key, Value) -> {Key, kz_time:rfc1036(Value)};
fix_value_map(<<"pvt_created">> = Key, Value) -> {Key, kz_time:rfc1036(Value)};
fix_value_map(Key, Value) -> {Key, Value}.

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('modb', 'kazoo_modb').
