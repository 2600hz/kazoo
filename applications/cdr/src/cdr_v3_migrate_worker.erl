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
migrate_account_cdrs(AccountDb, DbMigrateDateList, ArchiveBatchSize) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    lists:foreach(fun(DbMigrateDate) ->
                          migrate_cdrs_for_date(AccountId
                                                ,AccountDb
                                                ,DbMigrateDate)
                  end, DbMigrateDateList),
    start_fs_archive(AccountId
                     ,AccountDb
                     ,ArchiveBatchSize).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_fs_archive(account_id(), account_db(), pos_integer()) -> any().
start_fs_archive(AccountId, AccountDb, ArchiveBatchSize) ->
    lager:debug("starting account filesystem archive"),
    archive_cdr_batch(AccountId, AccountDb, ArchiveBatchSize).

-spec archive_cdr_batch(account_id()
                        ,account_db()
                        ,pos_integer()) -> 'ok'.
archive_cdr_batch(AccountId, AccountDb, ArchiveBatchSize) ->
    ViewOptions = [{'limit', ArchiveBatchSize}, 'include_docs'],
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'error', _R} ->
            lager:error("cdr archive error ~s: ~p", [AccountDb, _R]);
        {'ok', []} -> lager:debug("done here");
        {'ok', JObjs} ->
            lager:debug("Received JObjs: ~p", [length(JObjs)]),
            archive(AccountId, AccountDb, JObjs),
            archive_cdr_batch(AccountId, AccountDb, ArchiveBatchSize)
    end.

-spec archive(account_id(), account_db(), wh_proplist()) -> 'ok'.
archive(AccountId, AccountDb, JObjs) ->
    BatchData = convert_batch(AccountId, JObjs, []),
    ArchivePath = list_to_binary([code:priv_dir('cdr')
                                  ,"/archive_data/"
                                  ,AccountId
                                  ,"/"
                                 ]),
    [write_cdrs_to_file(Batch, ArchivePath, AccountDb)
     || Batch <- BatchData].

write_cdrs_to_file({FileName, CdrDocs}, ArchivePath, AccountDb) ->
    FilePath = list_to_binary([ArchivePath, FileName]),
    case {filelib:ensure_dir(ArchivePath), filelib:is_dir(FilePath)} of
        {'ok', 'true'} ->
            CsvData = csv_util:json_objs_to_csv(CdrDocs, 'false'),
            case file:write_file(FilePath, CsvData, ['append']) of
                {'error', _E} -> lager:error("error appending file: ~p", [_E]);
                'ok' -> soft_delete_cdrs(AccountDb, CdrDocs)
            end;
        {'ok', 'false'} ->
            CsvData = csv_util:json_objs_to_csv(CdrDocs),
            case file:write_file(FilePath, CsvData) of
                {'error', _E} -> lager:error("error writing file: ~p", [_E]);
                'ok' -> soft_delete_cdrs(AccountDb,CdrDocs)
            end;
        {'error', _E} ->
            lager:error("error creating directory: ~p", [_E])
    end.

convert_batch(_, [], Acc) -> Acc;
convert_batch(AccountId, [Cdr | RestCdrs], Acc) ->
    JObj = wh_json:get_value([<<"doc">>], Cdr),
    CreatedAt = wh_json:get_value(<<"pvt_created">>, JObj),
    CdrJObj1 = wh_json:public_fields(JObj),
    FileName = get_csv_batch_filename(CreatedAt),
    %% TODO - Checkout using dict: for data structure
    case props:is_defined(FileName, Acc) of
        'true' ->
            Data = props:get_value(FileName, Acc),
            convert_batch(AccountId
                          ,RestCdrs
                          ,props:set_value(FileName
                                           ,[CdrJObj1 | Data]
                                           ,Acc)
                         );
        'false' ->
            convert_batch(AccountId
                          ,RestCdrs
                          ,props:set_value(FileName
                                           ,[CdrJObj1]
                                           ,Acc)
                         )
    end.

get_csv_batch_filename(Timestamp) ->
    {{Year, Month, _},_} = calendar:gregorian_seconds_to_datetime(Timestamp),
    <<(wh_util:to_binary(Year))/binary
      ,(wh_util:to_binary(wh_util:pad_month(Month)))/binary
      ,".csv">>.

soft_delete_cdrs(_, []) -> 'ok';
soft_delete_cdrs(AccountDb, [JObj | RestJObjs]) ->
    UpdatedDoc = wh_json:set_value(<<"pvt_deleted">>, true, JObj),
    couch_mgr:save_doc(AccountDb, UpdatedDoc),
    soft_delete_cdrs(AccountDb, RestJObjs).

-spec migrate_cdrs_for_date(account_id(), account_db(), wh_date()) -> any().
migrate_cdrs_for_date(AccountId, AccountDb, {Year, Month, _}=Date) ->
    ViewOptions = create_view_options(Date),
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} ->
            lager:error("failed view ~s: ~p", [AccountDb, _E]), [];
        {'ok', Cdrs} ->
            [copy_cdr_to_account_mod(AccountId
                                     ,AccountDb
                                     ,wh_json:get_value([<<"doc">>], CdrDoc)
                                     ,Year
                                     ,Month
                                    )
             || CdrDoc <- Cdrs
            ]
    end.

-spec copy_cdr_to_account_mod(account_id()
                              ,account_db()
                              ,wh_json:object()
                              ,wh_year()
                              ,wh_month()) -> any().
copy_cdr_to_account_mod(AccountId, AccountDb, CdrDoc, Year, Month) ->
    AccountMODb = wh_util:format_account_id(AccountId, Year, Month),
    MODDocId = cdr_util:get_cdr_doc_id(Year, Month),
    JObj = wh_json:delete_key(<<"_rev">>, CdrDoc),
    JObj1 = wh_json:set_values([{<<"_id">>, MODDocId}
                               ,{<<"pvt_account_id">>, AccountId}
                               ,{<<"pvt_account_db">>, AccountMODb}
                              ], JObj),
    case cdr_util:save_cdr(AccountMODb, JObj1) of
        {'error', _}=_E -> lager:error("could not migrate cdr ~p", [_E]);
        'ok' ->
            couch_mgr:save_doc(AccountDb
                               ,wh_json:set_value(<<"pvt_deleted">>, 'true', CdrDoc))
    end.

-spec create_view_options(wh_datetime()) -> wh_proplist().
create_view_options(Date) ->
    StartTime = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}),
    [{<<"startkey">>, StartTime}, {<<"endkey">>, EndTime}, 'include_docs'].
