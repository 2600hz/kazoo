%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_ledgers_maintenance).

-export([current_rollups/0]).
-export([verify_rollups/0
        ,verify_rollups/1
        ,verify_all_rollups/0
        ]).
-export([fix_rollups/1
        ,fix_rollup/3
        ]).
-export([rollup_accounts/0
        ,rollup_account/1
        ]).

-include("kazoo_ledgers.hrl").

-type rollup_option() :: {'all_rollups', 'true'}.
-type rollup_options() :: [rollup_option()].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
    _ = case kz_ledgers:get_monthly_rollup(Account, Year, Month) of
            {'ok', Ledger} ->
                Created = kz_ledger:created(Ledger),
                {{_Y, _M, _D}, {_H, _Min, _S}} = calendar:gregorian_seconds_to_datetime(Created),
                io:format("[~p] account ~s has rollup (created ~p/~p/~p ~p:~p:~p) for ~p-~p with balance ~p~n"
                         ,[Remaining, AccountId, _Y, _M, _D, _H, _Min, _S, Year, Month, kz_ledger:unit_amount(Ledger)]
                         );
            {'error', 'not_found'} ->
                io:format("[~p] account ~s has no rollup for ~p-~p~n", [Remaining, AccountId, Year, Month]);
            Else ->
                io:format("[~p] account ~s error getting monthly rollup ~p~n", [Remaining, AccountId, Else])
        end,
    current_rollups(Accounts).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_rollups() -> 'ok'.
verify_rollups() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    _ = lists:foldr(fun verify_db_rollup/2, {1, Total, []}, Accounts),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_rollups(kz_term:ne_binary()) -> 'ok'.
verify_rollups(Account) ->
    verify_rollups(Account, [{'all_rollups', 'true'}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_rollups(kz_term:ne_binary() | kz_term:ne_binaries(), rollup_options()) -> 'ok'.
verify_rollups(?NE_BINARY=Account, Options) ->
    case props:get_value('all_rollups', Options) of
        'false' ->
            {Year, Month, _} = erlang:date(),
            verify_rollups(Account, Year, Month);
        'true' ->
            verify_rollups(kazoo_modb_util:db_list(Account))
    end;
verify_rollups([], _Options) -> 'ok';
verify_rollups([MODb|MODbs], Options) ->
    {Account, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
    _ = verify_rollups(Account, Year, Month),
    verify_rollups(MODbs, Options).

-spec verify_rollups(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
verify_rollups(AccountDb, Year, Month) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    case kz_ledgers:get_monthly_rollup(AccountDb, Year, Month) of
        {'ok', Ledger} ->
            verify_monthly_rollup(AccountDb, Year, Month, Ledger);
        {'error', 'not_found'} ->
            io:format("    account ~s ~p-~p has no rollup~n", [AccountId, Year, Month]);
        Else ->
            io:format("    account ~s ~p-~p error getting monthly rollup ~p~n"
                     ,[AccountId, Year, Month, Else]
                     )
    end.

-spec verify_monthly_rollup(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_ledger:ledger()) -> 'ok'.
verify_monthly_rollup(AccountDb, Year, Month, Ledger) ->
    {PreviousYear, PreviousMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    case kz_currency:past_available_units(AccountDb, PreviousYear, PreviousMonth) of
        {'error', _Reason} ->
            AccountId = kz_util:format_account_id(AccountDb, 'raw'),
            io:format("    account ~s ~p-~p : error getting balance for ~p-~p: ~p~n"
                     ,[AccountId, Year, Month, PreviousYear, PreviousMonth, _Reason]
                     );
        {'ok', AvailableUnits} ->
            verify_rollups(AccountDb, Year, Month, Ledger, AvailableUnits)
    end.

-spec verify_rollups(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_ledger:ledger(), kz_currency:units()) -> 'ok'.
verify_rollups(AccountDb, Year, Month, Ledger, AvailableUnits) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    case kz_ledger:amount(Ledger) of
        AvailableUnits ->
            io:format("    account ~s ~p-~p rollup confirmed~n", [AccountId, Year, Month]);
        _RollupUnits ->
            io:format("    account ~s ~p-~p rollup balance is ~p but previous month balance was ~p~n"
                     ,[AccountId, Year, Month, _RollupUnits, AvailableUnits]
                     )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_rollups(kz_term:ne_binary()|kz_term:ne_binaries()) -> 'ok'.
fix_rollups(?NE_BINARY=Account) ->
    [MODb|MODbs] = kazoo_modb_util:db_list(Account),
    {_, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
    maybe_replace_rollup(Account, Year, Month, 0),
    fix_rollups(MODbs);
fix_rollups([]) -> 'ok';
fix_rollups([MODb|MODbs]) ->
    {Account, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
    _ = fix_rollup(Account, Year, Month),
    fix_rollups(MODbs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_rollup(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
fix_rollup(Account, Year, Month) when not is_integer(Year) ->
    fix_rollup(Account, kz_term:to_integer(Year), Month);
fix_rollup(Account, Year, Month) when not is_integer(Month) ->
    fix_rollup(Account, Year, kz_term:to_integer(Month));
fix_rollup(Account, Year, Month) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    {PreviousYear, PreviousMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    case kz_currency:past_available_units(AccountId, PreviousYear, PreviousMonth) of
        {'error', _R} ->
            io:format("account ~s : error getting balance for ~p-~p: ~p~n"
                     ,[AccountId, PreviousYear, PreviousMonth, _R]
                     );
        {'ok', AvailableUnits} ->
            {'ok', _} = maybe_replace_rollup(Account, Year, Month, AvailableUnits),
            'ok'
    end.

-spec maybe_replace_rollup(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_currency:units()) ->
                                  {'ok', kz_ledger:ledger()} | {'error', any()}.
maybe_replace_rollup(Account, Year, Month, AvailableUnits) ->
    case kz_ledgers:get_monthly_rollup(Account, Year, Month) of
        {'error', _} ->
            io:format("account ~s ~p-~p rollup missing, creating for ~p units~n"
                     ,[Account, Year, Month, AvailableUnits]
                     ),
            kz_ledgers:rollup(Account, Year, Month, AvailableUnits);
        {'ok', Ledger}=Ok ->
            case kz_ledger:amount(Ledger) =:= AvailableUnits of
                'true' -> Ok;
                'false' ->
                    io:format("account ~s ~p-~p incorrect, updating with ~p units~n"
                             ,[Account, Year, Month, AvailableUnits]
                             ),
                    Setters = [{fun kz_ledger:set_unit_amount/2, AvailableUnits}
                              ,{fun kz_ledger:set_ledger_type/2, AvailableUnits}
                              ],
                    kz_ledger:save(kz_ledger:setters(Ledger, Setters))
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollup_accounts() -> 'ok'.
rollup_accounts() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = length(Accounts),
    _ = lists:foldr(fun rollup_account_fold/2, {1, Total}, Accounts),
    'ok'.

-spec rollup_account_fold(kz_term:ne_binary(), {pos_integer(), pos_integer()}) ->
                                 {pos_integer(), pos_integer()}.
rollup_account_fold(Account, {Current, Total}) ->
    io:format("rollup accounts (~p/~p) '~s'~n", [Current, Total, Account]),
    _ = rollup_account(Account),
    {Current + 1, Total}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollup_account(kz_term:ne_binary()) -> 'ok'.
rollup_account(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    kz_ledgers:rollup(AccountId).
