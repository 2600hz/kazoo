%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%% Migration worker module used for migrating logic for a specific 
%%% account.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_v3_migrate_worker).

%% API
-export([migrate_account_cdrs/3]).

-include("cdr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec migrate_account_cdrs(account_db(), wh_proplist(), wh_date()) -> 'ok'.
migrate_account_cdrs(AccountDb, DbMigrateDateList, FSArchiveStartDate) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    MaxArchiveMonths = 48,
    lists:foreach(fun(DbMigrateDate) -> 
                          migrate_cdrs_for_date(AccountId
                                                ,AccountDb
                                                ,DbMigrateDate)
                  end, DbMigrateDateList),
    start_fs_archive(AccountId
                     ,AccountDb
                     ,FSArchiveStartDate
                     ,MaxArchiveMonths).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_fs_archive(account_id(), account_db(), {wh_year(), wh_month()}, pos_integer()) -> any().
start_fs_archive(AccountId, AccountDb, FSArchiveStartDate, ArchiveIterationsLeft) ->
    _ = maybe_add_migrate_design_doc(AccountDb),
    archive_cdrs(AccountId, AccountDb, FSArchiveStartDate, ArchiveIterationsLeft).

-spec archive_cdrs(account_id(), account_db(), {wh_year(), wh_month()}, pos_integer()) -> any().
archive_cdrs(_, _, _, 0) ->
    lager:debug("cdr_v3_migrate_worker: file system archiver reached maximum archive depth");
archive_cdrs(AccountId, AccountDb, {Year, Month}, ArchiveIterationsLeft) ->
    lager:debug("Max Archive Iterations Left: ~p", [ArchiveIterationsLeft]),
    case count_cdrs_left_to_migrate(AccountDb) > 0 of
        'false' -> 'ok';
        'true' -> archive_cdrs_for_date(AccountId
                                        ,AccountDb
                                        ,Year,
                                        Month
                                        ,ArchiveIterationsLeft)
    end.

-spec archive_cdrs_for_date(account_id()
                            ,account_db(),
                            wh_year()
                            ,wh_month()
                            ,pos_integer()) -> 'ok'.
archive_cdrs_for_date(AccountId, AccountDb, Year, Month, ArchiveIterationsLeft) ->
    StartTime = calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({{Year, Month, calendar:last_day_of_the_month(Year, Month)}, {23, 59, 59}}),
    ViewOptions = [{'startkey', StartTime}
                   ,{'endkey', EndTime}
                  ],
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'error', _E} -> 
            lager:error("failed to lookup cdrs for ~s: ~p", [AccountDb, _E]);
        {'ok', []} -> lager:debug("No results from view with view options ~p", [ViewOptions]);
        {'ok', Cdrs} -> 
            lager:debug("Got some cdrs back for the view options: ~p", [ViewOptions]),
            lists:foreach(fun(CdrDoc) -> 
                                  archive_cdr(AccountId
                                              ,AccountDb
                                              ,CdrDoc
                                              ,Year
                                              ,Month
                                             )
                          end, get_docs_from_view_results(AccountDb, Cdrs, []))
    end,
    NextDate = cdr_v3_migrate_lib:get_prev_month(Year, Month),
    archive_cdrs(AccountId, AccountDb, NextDate, ArchiveIterationsLeft - 1).

-spec archive_cdr(account_id(), account_db(), wh_json:object(), wh_year(), wh_month()) -> 'ok'.
archive_cdr(AccountId, AccountDb, Cdr, Year, Month) ->
    DocId = wh_json:get_value(<<"_id">>, Cdr),
    FileName = <<(DocId)/binary, ".json">>,
    ArchivePath = list_to_binary([code:priv_dir('cdr')
                                  ,"/archive_data/"
                                  ,AccountId
                                  ,"/"
                                  ,wh_util:to_binary(Year)
                                  ,wh_util:pad_month(Month)
                                  ,"/"
                                 ]),
    case filelib:ensure_dir(ArchivePath) of
        'ok' ->
    FilePath = list_to_binary([ArchivePath, FileName]),
    case file:write_file(FilePath, io_lib:fwrite("~s", [wh_json:encode(Cdr)])) of
        {'error', _E} -> lager:error("Error writing file: ~p", [_E]);
        'ok' -> 'ok'
    end;
{'error', _E} -> lager:error("Error creating directory: ~p", [_E])
    end,
    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"pvt_deleted">>, true, Cdr)).

-spec count_cdrs_left_to_migrate(account_db()) -> integer().
count_cdrs_left_to_migrate(AccountDb) ->
    Opts = [{'reduce', 'true'}],
    case couch_mgr:get_results(AccountDb, <<"cdrmigrate/cdr_migrate_count">>, Opts) of
        {'error', _}=_E ->
            lager:error("failed to lookup cdrs for ~s: ~p", [AccountDb, _E]);
        {'ok', []} -> 0;
        {'ok', [JObj|_]} ->
            abs(wh_json:get_integer_value(<<"value">>, JObj, 0))
    end.

-spec maybe_add_migrate_design_doc(ne_binary()) -> 'ok' | {'error', 'not_found'}.
maybe_add_migrate_design_doc(AccountDb) ->
    case couch_mgr:open_doc(AccountDb, <<"_design/cdrmigrate">>) of
        {'error', 'not_found'} ->
            couch_mgr:load_doc_from_file(AccountDb
                                         ,'cdr'
                                         ,<<"views/cdrmigrate.json">>);
        {'ok', _ } -> 'ok'
    end.

-spec migrate_cdrs_for_date(account_id(), account_db(), wh_date()) -> any().
migrate_cdrs_for_date(AccountId, AccountDb, {Year, Month, _}=Date) ->
    ViewOptions = create_view_options(Date),
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} -> 
            lager:error("failed view ~s: ~p", [AccountDb, _E]), [];
        {'ok', Cdrs} -> 
            lists:foreach(fun(CdrDoc) -> 
                                  copy_cdr_to_account_mod(AccountId
                                                          ,AccountDb
                                                          ,CdrDoc
                                                          ,Year
                                                          ,Month
                                                         ) 
                          end, get_docs_from_view_results(AccountDb, Cdrs, []))
    end.
    
-spec get_docs_from_view_results(account_db()
                                 ,wh_json:object()
                                 ,wh_proplist()) -> wh_proplist().
get_docs_from_view_results(_, [], Acc) -> Acc;
get_docs_from_view_results(AccountDb, [NextCdr|RestCdrs], Acc) ->
    CdrId = wh_json:get_value(<<"id">>, NextCdr),
    case couch_mgr:open_doc(AccountDb, CdrId) of
        {'ok', JObj} -> 
            JObj1 = wh_json:delete_key(<<"_rev">>, JObj),
            get_docs_from_view_results(AccountDb, RestCdrs, [JObj1 | Acc]);
        {'error', _E} -> lager:error("migrate worker: error ~p", [_E])
    end.
    

-spec copy_cdr_to_account_mod(account_id()
                              ,account_db()
                              ,ne_binary()
                              ,wh_year()
                              ,wh_month()) -> any().
copy_cdr_to_account_mod(AccountId, AccountDb, CdrDoc, Year, Month) ->
    AccountMODb = wh_util:format_account_id(AccountId, Year, Month),
    MODDocId = cdr_util:get_cdr_doc_id(Year, Month),
    JObj = wh_json:set_values([{<<"_id">>, MODDocId}
                               ,{<<"pvt_account_id">>, AccountId}
                               ,{<<"pvt_account_db">>, AccountMODb}
                              ], CdrDoc),
    case cdr_util:save_cdr(AccountMODb, JObj) of
        {'error', _}=_E -> lager:error("could not migrate cdr ~p", [_E]);
        'ok' -> 'ok'
    end,
    couch_mgr:save_doc(AccountDb
                       ,wh_json:set_value(<<"pvt_deleted">>, 'true', CdrDoc)).

-spec create_view_options(wh_datetime()) -> wh_proplist().
create_view_options(Date) ->
    StartTime = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}),
    [{<<"startkey">>, StartTime}, {<<"endkey">>, EndTime}].
