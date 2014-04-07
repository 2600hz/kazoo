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
