%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_maintenance).

-include("fax.hrl").

%%==============================================================================
%% API functions
%%==============================================================================

-export([migrate/0, migrate/1, migrate/2]).
-export([migrate_outbound_faxes/0, migrate_outbound_faxes/1]).
-export([refresh_views/0]).
-export([flush/0]).

-export([restart_job/1 , update_job/2]).
-export([account_jobs/1, account_jobs/2]).
-export([faxbox_jobs/1, faxbox_jobs/2]).
-export([pending_jobs/0, active_jobs/0]).

-export([load_smtp_attachment/2]).

-define(DEFAULT_MIGRATE_OPTIONS, [{'allow_old_modb_creation', 'true'}]).
-define(OVERRIDE_DOCS, ['override_existing_document'
                       ,{'transform', fun(_, B) -> kz_json:set_value(<<"folder">>, <<"outbox">>, B) end}
                        |?DEFAULT_MIGRATE_OPTIONS
                       ]
       ).
-define(DEFAULT_BATCH_SIZE, 100).

-spec migrate() -> 'ok'.
migrate() ->
    Accounts = kapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_faxes_fold(A, C, Total, ?DEFAULT_MIGRATE_OPTIONS) end, 1, Accounts),
    migrate_outbound_faxes(),
    'ok'.

-spec migrate(kz_term:ne_binaries() | kz_term:ne_binary()) -> 'ok'.
migrate([]) -> 'ok';
migrate(<<"override_existing_documents">>) ->
    Accounts = kapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_faxes_fold(A, C, Total, ?OVERRIDE_DOCS) end, 1, Accounts),
    'ok';
migrate([Account|Accounts]) ->
    _ = migrate_faxes(Account, ?DEFAULT_MIGRATE_OPTIONS),
    migrate(Accounts);
migrate(Account) ->
    migrate_faxes(Account, ?DEFAULT_MIGRATE_OPTIONS).

-spec migrate(kz_term:ne_binaries() | kz_term:ne_binary(), kz_term:ne_binary() | kz_term:proplist()) -> 'ok'.
migrate([], _) -> 'ok';
migrate(Accounts, <<"override_existing_documents">>) ->
    migrate(Accounts, ?OVERRIDE_DOCS);
migrate(Accounts, Option) when is_binary(Option)->
    migrate(Accounts, ?DEFAULT_MIGRATE_OPTIONS);
migrate([Account|Accounts], Options) when is_list(Options) ->
    _ = migrate_faxes(Account, Options),
    migrate(Accounts, Options);
migrate(Account, Options) when is_list(Options) ->
    migrate_faxes(Account, Options).

migrate_faxes_fold(AccountDb, Current, Total, Options) ->
    io:format("migrating faxes in database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = migrate_faxes(AccountDb, Options),
    Current + 1.

-spec migrate_faxes(atom() | string() | binary(),  kz_term:proplist()) -> 'ok'.
migrate_faxes(Account, Options) when not is_binary(Account) ->
    migrate_faxes(kz_term:to_binary(Account), Options);
migrate_faxes(Account, Options) ->
    migrate_private_media(Account),
    recover_private_media(Account),
    migrate_faxes_to_modb(Account, Options).


-spec migrate_private_media(kz_term:ne_binary()) -> 'ok'.
migrate_private_media(Account) ->
    AccountDb = case kz_datamgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> kz_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"private_media">>}],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs3}->
            _ = [maybe_migrate_private_media(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E3])
    end.

-spec maybe_migrate_private_media(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_migrate_private_media(AccountDb, JObj) ->
    DocId = kz_doc:id(JObj),
    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'ok', Doc } ->
            MediaType = kz_json:get_value(<<"media_type">>, Doc),
            migrate_private_media(AccountDb, Doc, MediaType);
        {'error', Error} ->
            io:format("document ~s not found in database ~s : ~p~n", [DocId, AccountDb, Error])
    end.

-spec migrate_private_media(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
migrate_private_media(AccountDb, Doc, <<"tiff">>) ->
    {'ok', _} = kz_datamgr:ensure_saved(AccountDb, kz_doc:set_type(Doc, <<"fax">>)),
    'ok';
migrate_private_media(_AccountDb, _JObj, _MediaType) -> 'ok'.


-spec recover_private_media(kz_term:ne_binary()) -> 'ok'.
recover_private_media(Account) ->
    AccountDb = case kz_datamgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> kz_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"fax">>}],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs3}->
            _ = [maybe_recover_private_media(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch fax docs in db ~s: ~p~n", [AccountDb, E3])
    end.

-spec maybe_recover_private_media(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_recover_private_media(AccountDb, JObj) ->
    {'ok', Doc } = kz_datamgr:open_doc(AccountDb, kz_doc:id(JObj)),
    recover_private_media(AccountDb, Doc, kz_json:get_value(<<"media_type">>, Doc)).

-spec recover_private_media(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
recover_private_media(_AccountDb, _Doc, <<"tiff">>) ->
    'ok';
recover_private_media(AccountDb, Doc, _MediaType) ->
    {'ok', _ } = kz_datamgr:ensure_saved(AccountDb, kz_doc:set_type(Doc, <<"private_media">>)),
    'ok'.

-spec migrate_faxes_to_modb(kz_term:ne_binary(),  kz_term:proplist()) -> 'ok'.
migrate_faxes_to_modb(Account, Options) ->
    AccountDb = case kz_datamgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> kz_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"fax">>}],
    case kz_datamgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> io:format("no fax docs in db for fax migration ~s~n", [AccountDb]);
        {'ok', JObjs3}->
            _ = [maybe_migrate_fax_to_modb(AccountDb, JObj, Options) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch fax docs in db ~s: ~p~n", [AccountDb, E3])
    end.

-spec maybe_migrate_fax_to_modb(kz_term:ne_binary(), kz_json:object(),  kz_term:proplist()) -> 'ok'.
maybe_migrate_fax_to_modb(AccountDb, JObj, Options) ->
    DocId = kz_doc:id(JObj),
    case kz_datamgr:open_doc(AccountDb, DocId) of
        {'ok', Doc} ->
            case kz_doc:attachments(Doc) of
                'undefined' ->
                    case kapps_config:get_is_true(?CONFIG_CAT, <<"delete_empty_faxes">>, 'false') of
                        'true' ->
                            io:format("deleting no attachments fax doc ~s from ~s~n",[DocId, AccountDb]),
                            kz_datamgr:del_doc(AccountDb, Doc);
                        'false' -> 'ok'
                    end;
                _Attachments ->
                    migrate_fax_to_modb(AccountDb, DocId, Doc, Options)
            end;
        {'error', E} ->
            io:format("unable to get document ~s for fax migration : ~p",[DocId, E])
    end.

-spec migrate_fax_to_modb(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(),  kz_term:proplist()) -> 'ok'.
migrate_fax_to_modb(AccountDb, DocId, JObj, Options) ->
    Timestamp = kz_doc:created(JObj, kz_time:now_s()),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    AccountMODb = kazoo_modb:get_modb(AccountDb, Year, Month),
    FaxMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    FaxId = list_to_binary([kz_term:to_binary(Year)
                           ,kz_date:pad_month(Month)
                           ,"-"
                           ,DocId
                           ]),
    io:format("moving doc ~s/~s to ~s/~s~n",[AccountDb, DocId, AccountMODb, FaxId]),
    case kazoo_modb:move_doc(AccountDb, DocId, FaxMODb, FaxId, Options) of
        {'ok', _JObj} -> io:format("document ~s moved to ~s~n",[DocId, FaxId]);
        {'error', Error} -> io:format("error ~p moving document ~s to ~s~n",[Error, DocId, FaxId])
    end.


%%------------------------------------------------------------------------------
%% @doc Ensures that the views are updated to enforce the media format migration.
%% @end
%%------------------------------------------------------------------------------
-spec refresh_views() -> 'ok'.
refresh_views() ->
    _ = kapps_maintenance:refresh(?KZ_FAXES_DB),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Flush the fax local cache
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() -> kz_cache:flush_local(?CACHE_NAME).

-spec account_jobs(kz_term:ne_binary()) -> 'no_return'.
account_jobs(AccountId) ->
    account_jobs(AccountId, <<"pending">>).

-spec account_jobs(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
account_jobs(AccountId, State) ->
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-30s | ~-17s | ~-15s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Job">>, <<"Date">>, <<"State">>, <<"Account">>, <<"Faxbox">>, <<"From">>, <<"To">>]),
    io:format("+================================+===================+=================+==================================+==================================+======================+======================+~n", []),
    ViewOptions = [{'startkey', [AccountId, State]}
                  ,{'endkey', [AccountId, State, kz_json:new()]}
                  ],

    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/list_by_account_state">>, ViewOptions) of
        {'ok', Jobs} ->
            F = fun (JObj) ->
                        io:format(FormatString, [kz_json:get_value([<<"value">>, <<"id">>], JObj)
                                                ,kz_time:format_datetime(
                                                   kz_json:get_value([<<"value">>, <<"modified">>], JObj))
                                                ,kz_json:get_value([<<"value">>, <<"status">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"faxbox_id">>], JObj, <<"(none)">>)
                                                ,kz_json:get_value([<<"value">>, <<"from">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"to">>], JObj)
                                                ])
                end,
            lists:foreach(F, Jobs);
        {'error', _Reason} ->
            io:format("Error getting faxes\n")
    end,
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

-spec faxbox_jobs(kz_term:ne_binary()) -> 'no_return'.
faxbox_jobs(FaxboxId) ->
    faxbox_jobs(FaxboxId, <<"pending">>).

-spec faxbox_jobs(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
faxbox_jobs(FaxboxId, State) ->
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-30s | ~-17s | ~-15s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Job">>, <<"Date">>, <<"State">>, <<"Account">>, <<"Faxbox">>, <<"From">>, <<"To">>]),
    io:format("+================================+===================+=================+==================================+==================================+======================+======================+~n", []),
    ViewOptions = [{'startkey', [FaxboxId, State]}
                  ,{'endkey', [FaxboxId, State, kz_json:new()]}
                  ],

    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/list_by_faxbox_state">>, ViewOptions) of
        {'ok', Jobs} ->
            F = fun(JObj) ->
                        io:format(FormatString, [kz_json:get_value([<<"value">>, <<"id">>], JObj)
                                                ,kz_time:format_datetime(
                                                   kz_json:get_value([<<"value">>, <<"modified">>], JObj))
                                                ,kz_json:get_value([<<"value">>, <<"status">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"faxbox_id">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"from">>], JObj)
                                                ,kz_json:get_value([<<"value">>, <<"to">>], JObj)
                                                ])
                end,
            lists:foreach(F, Jobs);
        {'error', _Reason} ->
            io:format("Error getting faxes~n", [])
    end,
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

-spec pending_jobs() -> 'no_return'.
pending_jobs() ->
    io:format("+----------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+-------+~n", []),
    FormatString = "| ~-32s | ~-17s | ~-32s | ~-32s | ~-20s | ~-20s | ~-5s |~n",
    io:format(FormatString, [<<"Job">>, <<"Date">>, <<"Account">>, <<"Faxbox">>, <<"From">>, <<"To">>, <<"Tries">>]),
    io:format("+==================================+===================+==================================+==================================+======================+======================+=======+~n", []),
    _ = case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/jobs">>) of
            {'ok', Jobs} ->
                [io:format(FormatString, [kz_json:get_value([<<"value">>, <<"id">>], JObj)
                                         ,kz_time:format_datetime(
                                            kz_json:get_value([<<"value">>, <<"modified">>], JObj))
                                         ,kz_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                         ,kz_json:get_value([<<"value">>, <<"faxbox_id">>], JObj, <<"(none)">>)
                                         ,kz_json:get_value([<<"value">>, <<"from">>], JObj)
                                         ,kz_json:get_value([<<"value">>, <<"to">>], JObj)
                                         ,kz_json:get_value([<<"value">>, <<"tries">>], JObj)
                                         ]) || JObj <- Jobs];
            {'error', _Reason} ->
                io:format("Error getting faxes~n", [])
        end,
    io:format("+----------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+-------+~n", []),
    'no_return'.

-spec active_jobs() -> 'no_return'.
active_jobs() ->
    io:format("+----------------------+----------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-20s | ~-32s | ~-17s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Node">>, <<"Job">>, <<"Date">>, <<"Account">>, <<"FaxBox">>, <<"From">>, <<"To">>]),
    io:format("+======================+==================================+===================+==================================+==================================+======================+======================+~n", []),
    _ = case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxes/processing_by_node">>) of
            {'ok', Jobs} ->
                [io:format(FormatString, [kz_json:get_value([<<"value">>, <<"node">>], JObj)
                                         ,kz_json:get_value([<<"value">>, <<"id">>], JObj)
                                         ,kz_time:format_datetime(
                                            kz_json:get_value([<<"value">>, <<"modified">>], JObj))
                                         ,kz_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                         ,kz_json:get_value([<<"value">>, <<"faxbox_id">>], JObj, <<"(none)">>)
                                         ,kz_json:get_value([<<"value">>, <<"from">>], JObj)
                                         ,kz_json:get_value([<<"value">>, <<"to">>], JObj)
                                         ]) || JObj <- Jobs];
            {'error', _Reason} ->
                io:format("Error getting faxes~n", [])
        end,
    io:format("+----------------------+----------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

-spec restart_job(binary()) -> 'no_return'.
restart_job(JobID) ->
    _ = case update_job(JobID, <<"pending">>) of
            'ok' -> 'ok';
            {'error',_}=E ->
                lager:debug("restarting job ~p failed : ~p", [JobID, E])
        end,
    'no_return'.

-spec update_job(binary(), binary()) -> 'ok' | {'error', any()}.
update_job(JobID, State) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, JobID) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            update_job(JobID, State, JObj)
    end.

-spec update_job(binary(), binary(), kz_json:object()) -> 'ok' | {'error', any()}.
update_job(JobID, State, JObj) ->
    case kz_json:get_value(<<"pvt_job_status">>, JObj) of
        State ->
            lager:debug("job ~s already in state ~s", [JobID, State]),
            {'error', 'job_not_already_in_state'};
        _Other ->
            Opts = [{'rev', kz_doc:revision(JObj)}],
            _ = kz_datamgr:save_doc(?KZ_FAXES_DB
                                   ,kz_json:set_values([{<<"pvt_job_status">>, State}
                                                       ,{<<"pvt_modified">>, kz_time:now_s()}
                                                       ]
                                                      ,JObj
                                                      )
                                   ,Opts
                                   ),
            'ok'
    end.

-spec migrate_outbound_faxes() -> 'ok'.
migrate_outbound_faxes() ->
    migrate_outbound_faxes(?DEFAULT_BATCH_SIZE).

-spec migrate_outbound_faxes(kz_term:ne_binary() | integer() | kz_term:proplist()) -> 'ok'.
migrate_outbound_faxes(Number) when is_binary(Number) ->
    migrate_outbound_faxes(kz_term:to_integer(Number));
migrate_outbound_faxes(Number) when is_integer(Number) ->
    io:format("start migrating outbound faxes with batch size ~p~n", [Number]),
    migrate_outbound_faxes([{'limit', Number}]);
migrate_outbound_faxes(Options) ->
    case kz_datamgr:all_docs(?KZ_FAXES_DB, Options) of
        {'error', _E} ->
            io:format("failed to crawl faxes db: ~p~n", [ _E]);
        {'ok', []} ->
            io:format("finished crawling faxes db~n");
        {'ok', Docs} ->
            Last = migrate_outbound_faxes(Docs, 'undefined'),
            migrate_outbound_faxes([{'startkey', next_key(Last)}
                                   ,{'limit', props:get_value('limit', Options)}
                                   ])
    end.

-spec migrate_outbound_faxes(kz_json:objects(), kz_term:api_binary()) -> kz_term:api_binary().
migrate_outbound_faxes([], Acc) -> Acc;
migrate_outbound_faxes([JObj | JObjs], _Acc) ->
    DocId = kz_doc:id(JObj),
    maybe_migrate_outbound_fax(DocId),
    migrate_outbound_faxes(JObjs, DocId).

-spec maybe_migrate_outbound_fax(kz_term:ne_binary()) -> 'ok'.
maybe_migrate_outbound_fax(<<"_design/", _/binary>>) -> 'ok';
maybe_migrate_outbound_fax(DocId) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, DocId) of
        {'ok', Doc} -> maybe_migrate_outbound_fax(kz_doc:type(Doc), Doc);
        {'error', _E} -> io:format("error opening document ~s in faxes db~n", [DocId])
    end.

-spec maybe_migrate_outbound_fax(kz_term:api_binary(), kz_json:object()) -> 'ok'.
maybe_migrate_outbound_fax(<<"fax">>, JObj) ->
    case kz_json:get_value(<<"pvt_job_status">>, JObj) of
        <<"failed">> -> migrate_outbound_fax(JObj);
        <<"completed">> -> migrate_outbound_fax(JObj);
        _ -> 'ok'
    end;
maybe_migrate_outbound_fax(_Type, _JObj) -> 'ok'.

-spec migrate_outbound_fax(kz_json:object()) -> 'ok'.
migrate_outbound_fax(JObj) ->
    FromId = kz_doc:id(JObj),
    {Year, Month, _D} = kz_term:to_date(kz_doc:created(JObj)),
    FromDB = kz_doc:account_db(JObj),
    AccountId = kz_doc:account_id(JObj),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),

    ToDB = kz_util:format_account_modb(AccountMODb, 'encoded'),
    ToId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month),FromId),

    case kazoo_modb:move_doc(FromDB, FromId, ToDB, ToId, ?OVERRIDE_DOCS) of
        {'ok', _} -> io:format("document ~s/~s moved to ~s/~s~n", [FromDB, FromId, ToDB, ToId]);
        {'error', _E} -> io:format("error ~p moving document ~s/~s to ~s/~s~n", [_E, FromDB, FromId, ToDB, ToId])
    end.

-spec next_key(binary()) -> kz_term:ne_binary().
next_key(<<>>) ->
    <<"\ufff0">>;
next_key(Bin) ->
    <<Bin/binary, "\ufff0">>.

-spec load_smtp_attachment(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
load_smtp_attachment(DocId, Filename) ->
    case file:read_file(Filename) of
        {'ok', FileContents} ->
            load_smtp_attachment(DocId, Filename, FileContents);
        Error ->
            io:format("error obtaining file ~s contents for docid ~s : ~p~n", [Filename, DocId, Error])
    end.

-spec load_smtp_attachment(kz_term:ne_binary(), kz_term:ne_binary(), binary()) -> 'ok'.
load_smtp_attachment(DocId, Filename, FileContents) ->
    CT = kz_mime:from_filename(Filename),
    case kz_datamgr:open_cache_doc(?KZ_FAXES_DB, DocId) of
        {'ok', JObj} ->
            case kz_fax_attachment:save_outbound(?KZ_FAXES_DB, JObj, FileContents, CT) of
                {'ok', _Doc} -> io:format("attachment ~s for docid ~s recovered~n", [Filename, DocId]);
                {'error', E} -> io:format("error attaching ~s to docid ~s : ~p~n", [Filename, DocId, E])
            end;
        {'error', E} -> io:format("error opening docid ~s for attaching ~s : ~p~n", [DocId, Filename, E])
    end.
