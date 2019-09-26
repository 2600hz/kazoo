%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_yodb_maintenance).

-export([delete_yodbs/1
        ,delete_yodbs/2
        ]).
-export([archive_yodbs/0
        ,archive_yodbs/1
        ]).
-export([register_views/0]).

-include("kazoo_yodb.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_yodbs(kz_term:ne_binary()) -> 'ok' | 'no_return'.
delete_yodbs(Period) ->
    delete_yodbs(Period, 'true').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_yodbs(kz_term:year() | kz_term:ne_binary(), boolean() | kz_term:ne_binary()) -> 'ok' | 'no_return'.
delete_yodbs(<<_/binary>> = Period, ShouldArchive) ->
    Regex = <<"(2[0-9]{3})">>,
    case re:run(Period, Regex, [{'capture', 'all', 'binary'}]) of
        {'match', [_Full, Year]} ->
            delete_yodbs(kz_term:to_integer(Year), kz_term:is_true(ShouldArchive));
        'nomatch' ->
            io:format("period '~s' does not match YYYY format~n", [Period])
    end;
delete_yodbs(Year, ShouldArchive) when is_integer(Year),
                                       Year > 2000,
                                       Year < 2999 ->
    case erlang:date() of
        {Year, _, _} ->
            io:format("request to delete the current YODB (~p) denied~n", [Year]);
        {CurrYear, _, _} when CurrYear > Year ->
            io:format("deleting all YODBs equal to or older than ~p~n", [Year]),
            delete_older_yodbs(Year, kapps_util:get_all_account_yods(), ShouldArchive);
        {_CurrYear, _, _} ->
            io:format("request to delete future YODBs (~p) denied~n", [Year])
    end.

-spec delete_older_yodbs(kz_time:year(), kz_term:ne_binaries(), boolean()) -> 'no_return'.
delete_older_yodbs(Year, AccountYodbs, ShouldArchive) ->
    _ = [delete_yodb(AccountYodb, ShouldArchive) || AccountYodb <- AccountYodbs, should_delete(AccountYodb, Year)],
    'no_return'.

-spec should_delete(kz_term:ne_binary(), kz_term:year()) -> boolean().
should_delete(AccountYodb, Year) ->
    {_AccountId, ModYear} = kazoo_yodb_util:split_account_yod(AccountYodb),
    ModYear =< Year.

-spec delete_yodb(kz_term:ne_binary(), boolean()) -> 'ok'.
delete_yodb(?MATCH_YODB_SUFFIX_UNENCODED(_,_) = AccountYodb, ShouldArchive) ->
    delete_yodb(kzs_util:format_account_db(AccountYodb), ShouldArchive);
delete_yodb(?MATCH_YODB_SUFFIX_ENCODED(_,_) = AccountYodb, ShouldArchive) ->
    'ok' = case ShouldArchive of
               'true' -> kz_datamgr:db_archive(AccountYodb);
               'false' -> io:format("deleting database ~s~n", [AccountYodb])
           end,
    _Deleted = kz_datamgr:db_delete(AccountYodb),
    io:format("    deleted: ~p~n", [_Deleted]),
    timer:sleep(5 * ?MILLISECONDS_IN_SECOND).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec archive_yodbs() -> 'no_return'.
archive_yodbs() ->
    do_archive_yodbs(kapps_util:get_all_account_yods(), 'undefined').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec archive_yodbs(kz_term:text()) -> 'no_return'.
archive_yodbs(AccountId) ->
    do_archive_yodbs(kapps_util:get_account_yods(AccountId), kz_term:to_binary(AccountId)).

-spec do_archive_yodbs(kz_term:ne_binaries(), kz_term:api_binary()) -> 'no_return'.
do_archive_yodbs(YODbs, AccountId) ->
    kz_log:put_callid(?MODULE),
    lists:foreach(fun kazoo_yodb:maybe_archive_yodb/1, YODbs),
    Keep = kapps_config:get_integer(?CONFIG_CAT, <<"active_yodbs">>, 6),
    From = case AccountId =:= 'undefined' of 'true' -> <<"all">>; 'false' -> AccountId end,
    io:format("archived ~s YODbs more than ~b months old~n", [From, Keep]),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder('kazoo_yodb').
