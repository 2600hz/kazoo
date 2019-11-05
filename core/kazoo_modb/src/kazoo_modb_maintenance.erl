%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_modb_maintenance).

-export([delete_modbs/1
        ,delete_modbs/2
        ]).
-export([archive_modbs/0
        ,archive_modbs/1
        ]).
-export([register_views/0]).

-include("kazoo_modb.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_modbs(kz_term:ne_binary()) -> 'ok' | 'no_return'.
delete_modbs(Period) ->
    delete_modbs(Period, 'true').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec archive_modbs() -> 'no_return'.
archive_modbs() ->
    do_archive_modbs(kapps_util:get_all_account_mods(), 'undefined').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec archive_modbs(kz_term:text()) -> 'no_return'.
archive_modbs(AccountId) ->
    do_archive_modbs(kapps_util:get_account_mods(AccountId), kz_term:to_binary(AccountId)).

-spec do_archive_modbs(kz_term:ne_binaries(), kz_term:api_binary()) -> 'no_return'.
do_archive_modbs(MODbs, AccountId) ->
    kz_log:put_callid(?MODULE),
    lists:foreach(fun kazoo_modb:maybe_archive_modb/1, MODbs),
    Keep = kapps_config:get_integer(?CONFIG_CAT, <<"active_modbs">>, 6),
    From = case AccountId =:= 'undefined' of 'true' -> <<"all">>; 'false' -> AccountId end,
    io:format("archived ~s MODbs more than ~b months old~n", [From, Keep]),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('kazoo_modb').
