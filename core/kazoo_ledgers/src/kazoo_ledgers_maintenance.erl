%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_ledgers_maintenance).

-export([current_rollovers/0]).
-export([verify_rollovers/0
        ,verify_rollovers/1
        ,verify_all_rollovers/0
        ]).
-export([fix_rollovers/1
        ,fix_rollover/3
        ]).
-export([rollover_accounts/0
        ,rollover_account/1
        ]).

-include("kazoo_ledgers.hrl").

-type rollover_option() :: {'all_rollovers', 'true'}.
-type rollover_options() :: [rollover_option()].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec current_rollovers() -> 'ok'.
current_rollovers() ->
    Accounts = kapps_util:get_all_accounts(),
    current_rollovers(Accounts).

-spec current_rollovers(kz_term:ne_binaries()) -> 'ok'.
current_rollovers([]) -> 'ok';
current_rollovers([Account|Accounts]) ->
    {Year, Month, _} = erlang:date(),
    AccountId = kzs_util:format_account_id(Account),
    Remaining = length(Accounts),
    _ = case kz_ledgers:get_monthly_rollover(Account, Year, Month) of
            {'ok', Ledger} ->
                Created = kz_ledger:created(Ledger),
                {{_Y, _M, _D}, {_H, _Min, _S}} = calendar:gregorian_seconds_to_datetime(Created),
                io:format("[~p] account ~s has rollover (created ~p/~p/~p ~p:~p:~p) for ~p-~p with balance ~p~n"
                         ,[Remaining, AccountId, _Y, _M, _D, _H, _Min, _S, Year, Month, kz_ledger:unit_amount(Ledger)]
                         );
            {'error', 'not_found'} ->
                io:format("[~p] account ~s has no rollover for ~p-~p~n", [Remaining, AccountId, Year, Month]);
            Else ->
                io:format("[~p] account ~s error getting monthly rollover ~p~n", [Remaining, AccountId, Else])
        end,
    current_rollovers(Accounts).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_rollovers() -> 'ok'.
verify_rollovers() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    _ = lists:foldr(fun verify_db_rollover/2, {1, Total, []}, Accounts),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_all_rollovers() -> 'ok'.
verify_all_rollovers() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = erlang:length(Accounts),
    _ = lists:foldr(fun verify_db_rollover/2, {1, Total, [{'all_rollovers', 'true'}]}, Accounts),
    'ok'.

-type rollover_acc() :: {pos_integer(), pos_integer(), rollover_options()}.
-spec verify_db_rollover(kz_term:ne_binary(), rollover_acc()) -> rollover_acc().
verify_db_rollover(AccountDb, {Current, Total, Options}) ->
    io:format("verify rollover accounts (~p/~p) '~s'~n"
             ,[Current, Total, AccountDb]
             ),
    verify_rollovers(AccountDb, Options),
    {Current+1, Total, Options}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_rollovers(kz_term:ne_binary()) -> 'ok'.
verify_rollovers(Account) ->
    verify_rollovers(Account, [{'all_rollovers', 'true'}]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_rollovers(kz_term:ne_binary(), rollover_options()) -> 'ok'.
verify_rollovers(?NE_BINARY=Account, Options) ->
    case props:get_is_true('all_rollovers', Options, 'false') of
        'false' ->
            {Year, Month, _} = erlang:date(),
            verify_rollovers(Account, Year, Month);
        'true' ->
            verify_all_rollovers(kazoo_modb_util:db_list(Account), Options)
    end.

-spec verify_all_rollovers(kz_term:ne_binaries(), rollover_options()) -> 'ok'.
verify_all_rollovers([], _Options) -> 'ok';
verify_all_rollovers([MODb|MODbs], Options) ->
    {Account, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
    _ = verify_rollovers(Account, Year, Month),
    verify_all_rollovers(MODbs, Options).

-spec verify_rollovers(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
verify_rollovers(AccountDb, Year, Month) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    case kz_ledgers:get_monthly_rollover(AccountDb, Year, Month) of
        {'ok', Ledger} ->
            verify_monthly_rollover(AccountDb, Year, Month, Ledger);
        {'error', 'not_found'} ->
            io:format("    account ~s ~p-~p has no rollover~n", [AccountId, Year, Month]);
        Else ->
            io:format("    account ~s ~p-~p error getting monthly rollover ~p~n"
                     ,[AccountId, Year, Month, Else]
                     )
    end.

-spec verify_monthly_rollover(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_ledger:ledger()) -> 'ok'.
verify_monthly_rollover(AccountDb, Year, Month, Ledger) ->
    {PreviousYear, PreviousMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    case kz_currency:past_available_units(AccountDb, PreviousYear, PreviousMonth) of
        {'error', _Reason} ->
            AccountId = kzs_util:format_account_id(AccountDb),
            io:format("    account ~s ~p-~p : error getting balance for ~p-~p: ~p~n"
                     ,[AccountId, Year, Month, PreviousYear, PreviousMonth, _Reason]
                     );
        {'ok', AvailableUnits} ->
            verify_rollovers(AccountDb, Year, Month, Ledger, AvailableUnits)
    end.

-spec verify_rollovers(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_ledger:ledger(), kz_currency:units()) -> 'ok'.
verify_rollovers(AccountDb, Year, Month, Ledger, AvailableUnits) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    case kz_ledger:amount(Ledger) of
        AvailableUnits ->
            io:format("    account ~s ~p-~p rollover confirmed~n", [AccountId, Year, Month]);
        _RolloverUnits ->
            io:format("    account ~s ~p-~p rollover balance is ~p but previous month balance was ~p~n"
                     ,[AccountId, Year, Month, _RolloverUnits, AvailableUnits]
                     )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_rollovers(kz_term:ne_binary()|kz_term:ne_binaries()) -> 'ok'.
fix_rollovers(?NE_BINARY=Account) ->
    [MODb|MODbs] = kazoo_modb_util:db_list(Account),
    {_, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
    _ = maybe_replace_rollover(Account, Year, Month, 0),
    fix_rollovers(MODbs);
fix_rollovers([]) -> 'ok';
fix_rollovers([MODb|MODbs]) ->
    {Account, Year, Month} = kazoo_modb_util:split_account_mod(MODb),
    _ = fix_rollover(Account, Year, Month),
    fix_rollovers(MODbs).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_rollover(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'ok'.
fix_rollover(Account, Year, Month) when not is_integer(Year) ->
    fix_rollover(Account, kz_term:to_integer(Year), Month);
fix_rollover(Account, Year, Month) when not is_integer(Month) ->
    fix_rollover(Account, Year, kz_term:to_integer(Month));
fix_rollover(Account, Year, Month) ->
    AccountId = kzs_util:format_account_id(Account),
    {PreviousYear, PreviousMonth} = kazoo_modb_util:prev_year_month(Year, Month),
    case kz_currency:past_available_units(AccountId, PreviousYear, PreviousMonth) of
        {'error', _R} ->
            io:format("account ~s : error getting balance for ~p-~p: ~p~n"
                     ,[AccountId, PreviousYear, PreviousMonth, _R]
                     );
        {'ok', AvailableUnits} ->
            {'ok', _} = maybe_replace_rollover(Account, Year, Month, AvailableUnits),
            'ok'
    end.

-spec maybe_replace_rollover(kz_term:ne_binary(), kz_time:year(), kz_time:month(), kz_currency:units()) ->
          {'ok', kz_ledger:ledger()} |
          {'error', any()}.
maybe_replace_rollover(Account, Year, Month, AvailableUnits) ->
    case kz_ledgers:get_monthly_rollover(Account, Year, Month) of
        {'error', _} ->
            io:format("account ~s ~p-~p rollover missing, creating for ~p units~n"
                     ,[Account, Year, Month, AvailableUnits]
                     ),
            kz_ledgers:rollover(Account, Year, Month, AvailableUnits);
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
-spec rollover_accounts() -> 'ok'.
rollover_accounts() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = length(Accounts),
    _ = lists:foldr(fun rollover_account_fold/2, {1, Total}, Accounts),
    'ok'.

-spec rollover_account_fold(kz_term:ne_binary(), {pos_integer(), pos_integer()}) ->
          {pos_integer(), pos_integer()}.
rollover_account_fold(Account, {Current, Total}) ->
    io:format("rollover accounts (~p/~p) '~s'~n", [Current, Total, Account]),
    _ = rollover_account(Account),
    {Current + 1, Total}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollover_account(kz_term:ne_binary()) -> 'ok'.
rollover_account(Account) ->
    AccountId = kzs_util:format_account_id(Account),
    {'ok', _} = kz_ledgers:rollover(AccountId),
    io:format("account ~s rolled up~n", [AccountId]).
