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
    _ = maybe_add_migrate_design_doc(AccountDb),
    archive_cdrs(AccountId, AccountDb, ArchiveBatchSize).

-spec archive_cdrs(account_id(), account_db(), pos_integer()) -> any().
archive_cdrs(AccountId, AccountDb, ArchiveBatchSize) ->
    case count_cdrs_left_to_migrate(AccountDb) > 0 of
        'false' -> 'ok';
        'true' -> archive_cdr_batch(AccountId
                                     ,AccountDb
                                     ,ArchiveBatchSize)
    end.

-spec archive_cdr_batch(account_id()
                        ,account_db()
                        ,pos_integer()) -> 'ok'.
archive_cdr_batch(AccountId, AccountDb, ArchiveBatchSize) ->
    ViewOptions = [{'limit', ArchiveBatchSize}, 'include_docs'],
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'error', _R} ->
            lager:error("cdr archive error ~s: ~p", [AccountDb, _R]);
        {'ok', JObjs} ->
            lager:debug("Received JObjs: ~p", [length(JObjs)]),
            [archive(AccountId, AccountDb, CdrDocs) || CdrDocs <- chunk_list(JObjs, 100)]
    end,
    archive_cdrs(AccountId, AccountDb, ArchiveBatchSize).

-spec archive(account_id(), account_db(), wh_proplist()) -> 'ok'.
archive(AccountId, AccountDb, JObjs) ->
    CdrDocs = extract_docs_from_cdr_chunk(JObjs, []),
    Now = calendar:universal_time(),
    Seconds = calendar:datetime_to_gregorian_seconds(Now),
    FileName = <<"archive-",(wh_util:to_binary(Seconds))/binary, ".csv">>,
    ArchivePath = list_to_binary([code:priv_dir('cdr')
                                  ,"/archive_data/"
                                  ,AccountId
                                  ,"/"
                                 ]),
    case filelib:ensure_dir(ArchivePath) of
        'ok' ->
            FilePath = list_to_binary([ArchivePath, FileName]),
            CsvData = csv_util:json_objs_to_csv(CdrDocs),
            case file:write_file(FilePath, CsvData) of
                {'error', _E} -> lager:error("Error writing file: ~p", [_E]);
                'ok' -> 'ok'
            end;
        {'error', _E} -> lager:error("Error creating directory: ~p", [_E])
    end,
    soft_delete_cdrs(AccountDb, CdrDocs).

extract_docs_from_cdr_chunk([], Acc) -> Acc;
extract_docs_from_cdr_chunk([Cdr | CdrChunk], Acc) ->
    Doc = wh_json:get_value([<<"doc">>], Cdr),
    CdrDoc = wh_json:delete_key(<<"custom_channel_vars">>, Doc),
    extract_docs_from_cdr_chunk(CdrChunk, [CdrDoc | Acc]).

soft_delete_cdrs(_, []) -> 'ok';
soft_delete_cdrs(AccountDb, [JObj | RestJObjs]) ->
    UpdatedDoc = wh_json:set_value(<<"pvt_deleted">>, true, JObj),
    couch_mgr:save_doc(AccountDb, UpdatedDoc),
    soft_delete_cdrs(AccountDb, RestJObjs).

-spec chunk_list(list(), pos_integer()) -> wh_proplist().
chunk_list(List, Cs) ->
    Noc = fun(X) -> Y = length(List)/X , %% X == size of one chunk
                    case Y == (Z = trunc(Y)) of %% 5 == (Z = trunc(5.0)) == true
                        true  -> Z-1 ; %% -> 5-1 -> 4
                        false -> Z %% Y == e.g. 4.5 -> 4
                    end
          end ,
    [ begin lists:sublist(List, (Cs*Elem)+1, Cs)
      end || Elem <- lists:seq(0,Noc(Cs)) ] .

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
            [copy_cdr_to_account_mod(AccountId
                                     ,AccountDb
                                     ,wh_json:get_value([<<"doc">>], CdrDoc)
                                     ,Year
                                     ,Month
                                    )
             || CdrDoc <- Cdrs]
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
        'ok' -> 'ok'
    end,
    couch_mgr:save_doc(AccountDb
                       ,wh_json:set_value(<<"pvt_deleted">>, 'true', CdrDoc)).

-spec create_view_options(wh_datetime()) -> wh_proplist().
create_view_options(Date) ->
    StartTime = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    EndTime = calendar:datetime_to_gregorian_seconds({Date, {23,59,59}}),
    [{<<"startkey">>, StartTime}, {<<"endkey">>, EndTime}, 'include_docs'].
