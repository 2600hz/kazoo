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
    [migrate_cdrs_for_date(AccountId, AccountDb, Date)
     || Date <- DbMigrateDateList
    ],
    archive_cdr_batch(AccountId
                      ,AccountDb
                      ,ArchiveBatchSize).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec archive_cdr_batch(account_id()
                        ,account_db()
                        ,pos_integer()) -> 'ok'.
archive_cdr_batch(AccountId, AccountDb, ArchiveBatchSize) ->
    ViewOptions = [{'limit', ArchiveBatchSize}, 'include_docs'],
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'error', _R} ->
            lager:error("cdr_migrator: error ~s: ~p", [AccountDb, _R]);
        {'ok', []} -> 
            lager:debug("cdr_migrator: done archiving");
        {'ok', JObjs} ->
            case archive(AccountId, AccountDb, JObjs) of
                {'error', _E} -> 
                    lager:error("migrate archive error: ~p", [_E]);
                'ok' -> 
                    archive_cdr_batch(AccountId, AccountDb, ArchiveBatchSize)
            end
    end.

-spec archive(account_id(), account_db(), wh_proplist()) -> 'ok'.
archive(AccountId, AccountDb, JObjs) ->
    BatchDict = convert_batch(AccountId, JObjs, orddict:new()),
    ArchivePath = list_to_binary([code:priv_dir('cdr')
                                  ,"/archive_data/accounts/"
                                  ,AccountId
                                  ,"/"
                                 ]),
    case write_cdrs_to_file(ArchivePath
                            ,AccountDb
                            ,BatchDict
                            ,orddict:fetch_keys(BatchDict)) of
        {'error', _E} ->
            lager:error("cdr_migrator: error writing to file: ~p", [_E]),
            {'error', _E};
        'ok' -> 
            lager:debug("cdr_migrator: cdrs written to file"),
            soft_delete_cdrs(AccountDb, JObjs)
    end.

-spec write_cdrs_to_file(ne_binary(), account_db(), orddict:orddict(), list()) ->
                                'ok' | {'error', any()}.
write_cdrs_to_file(_, _, _, []) -> 'ok';
write_cdrs_to_file(ArchivePath, AccountDb, Dict, [FileNameKey | RestKeys]) ->
    try orddict:fetch(FileNameKey, Dict) of
        Cdrs -> FilePath = list_to_binary([ArchivePath, FileNameKey]),
                case {filelib:ensure_dir(ArchivePath), filelib:is_file(FilePath)} of
                    {'ok', 'true'} ->
                        JsonData = wh_json:encode(Cdrs),
                        case file:write_file(FilePath, JsonData, ['append']) of
                            {'error', _E} -> lager:error("error appending: ~p", [_E]);
                            'ok' ->
                                write_cdrs_to_file(ArchivePath
                                                   ,AccountDb
                                                   ,Dict
                                                   ,RestKeys)
                        end;
                    {'ok', 'false'} ->
                        JsonData = wh_json:encode(Cdrs),
                        case file:write_file(FilePath, JsonData) of
                            {'error', _E} -> lager:error("error writings: ~p", [_E]);
                            'ok' ->
                                write_cdrs_to_file(ArchivePath
                                                   ,AccountDb
                                                   ,Dict
                                                   ,RestKeys)
                        end;
                    {{'error', _E}, _} ->
                        lager:error("error creating directory: ~p", [_E]),
                        {'error', _E}
                end
    catch
        error:function_clause -> 'undefined'
    end.

-spec convert_batch(account_id(), wh_json:objects(), any()) -> any().
convert_batch(_, [], DictAcc) -> DictAcc;
convert_batch(AccountId, [Cdr | RestCdrs], DictAcc) ->
    JObj = wh_json:get_value([<<"doc">>], Cdr),
    CreatedAt = wh_json:get_value(<<"pvt_created">>, JObj),
    CdrJObj1 = wh_json:public_fields(JObj),
    FileName = get_json_batch_filename(CreatedAt),
    convert_batch(AccountId
                  ,RestCdrs
                  ,orddict:update(FileName
                                  ,fun (Old) -> [CdrJObj1 | Old] end
                                  ,[CdrJObj1]
                                  ,DictAcc)).

-spec get_json_batch_filename(pos_integer()) -> ne_binary().
get_json_batch_filename(Timestamp) ->
    {{Year, Month, _},_} = calendar:gregorian_seconds_to_datetime(Timestamp),
    <<(wh_util:to_binary(Year))/binary
      ,(wh_util:to_binary(wh_util:pad_month(Month)))/binary
      ,".json">>.

-spec soft_delete_cdrs(account_db(), wh_json:objects()) -> 'ok'.
soft_delete_cdrs(AccountDb, JObjs) ->
    case couch_mgr:save_docs(AccountDb
                        ,[wh_json:set_value(<<"pvt_deleted">>
                                                ,true
                                            ,wh_json:get_value([<<"doc">>], JObj)) 
                          || JObj <- JObjs
                         ]) of
        {'error', _E} ->
            lager:error("cdr_migrator: error soft deleting");
        {'ok', _} ->
            lager:debug("cdr_migrator: soft deleted batch of docs")
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
    CallId = wh_json:get_value(<<"call_id">>, CdrDoc),
    MODDocId = cdr_util:get_cdr_doc_id(Year, Month, CallId),
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
