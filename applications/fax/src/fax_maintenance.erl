%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------

-module(fax_maintenance).
-include("fax.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([migrate/0, migrate/1, migrate/2]).
-export([flush/0]).

-export([restart_job/1 , update_job/2]).
-export([account_jobs/1, account_jobs/2]).
-export([faxbox_jobs/1, faxbox_jobs/2]).
-export([pending_jobs/0, active_jobs/0]).

-define(DEFAULT_MIGRATE_OPTIONS, []).
-define(OVERRIDE_DOCS, ['override_existing_document']).

-spec migrate() -> 'ok'.
migrate() ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_faxes_fold(A, C, Total,?DEFAULT_MIGRATE_OPTIONS) end, 1, Accounts),
    'ok'.

-spec migrate(ne_binaries() | ne_binary()) -> 'ok'.
migrate([]) -> 'ok';
migrate(<<"override_existing_documents">>) ->
    Accounts = whapps_util:get_all_accounts(),
    Total = length(Accounts),
    lists:foldr(fun(A, C) -> migrate_faxes_fold(A, C, Total, ?OVERRIDE_DOCS) end, 1, Accounts),
    'ok';
migrate([Account|Accounts]) ->
    _ = migrate_faxes(Account, ?DEFAULT_MIGRATE_OPTIONS),
    migrate(Accounts);
migrate(Account) ->
    migrate_faxes(Account, ?DEFAULT_MIGRATE_OPTIONS).

-spec migrate(ne_binaries() | ne_binary(), ne_binary() | wh_proplist()) -> 'ok'.
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

%% ====================================================================
%% Internal functions
%% ====================================================================

migrate_faxes_fold(AccountDb, Current, Total, Options) ->
    io:format("migrating faxes in database (~p/~p) '~s'~n", [Current, Total, AccountDb]),
    _ = migrate_faxes(AccountDb, Options),
    Current + 1.

-spec migrate_faxes(atom() | string() | binary(),  wh_proplist()) -> 'ok'.
migrate_faxes(Account, Options) when not is_binary(Account) ->
    migrate_faxes(wh_util:to_binary(Account), Options);
migrate_faxes(Account, Options) ->
    migrate_private_media(Account),
    recover_private_media(Account),
    migrate_faxes_to_modb(Account, Options).

-spec migrate_private_media(ne_binary()) -> 'ok'.
-spec migrate_private_media(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
-spec maybe_migrate_private_media(ne_binary(), wh_json:object()) -> 'ok'.

migrate_private_media(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"private_media">>}],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs3}->
            _ = [maybe_migrate_private_media(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch private media files in db ~s: ~p~n", [AccountDb, E3])
    end.

maybe_migrate_private_media(AccountDb, JObj) ->
    DocId = wh_doc:id(JObj),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'ok', Doc } ->
            MediaType = wh_json:get_value(<<"media_type">>, Doc),
            migrate_private_media(AccountDb, Doc, MediaType);
        {'error', Error} ->
            io:format("document ~s not found in database ~s : ~p~n", [DocId, AccountDb, Error])
    end.

migrate_private_media(AccountDb, Doc, <<"tiff">>) ->
    {'ok', _} = couch_mgr:ensure_saved(AccountDb, wh_doc:set_type(Doc, <<"fax">>)),
    'ok';
migrate_private_media(_AccountDb, _JObj, _MediaType) -> 'ok'.

-spec recover_private_media(ne_binary()) -> 'ok'.
-spec recover_private_media(ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
-spec maybe_recover_private_media(ne_binary(), wh_json:object()) -> 'ok'.

recover_private_media(Account) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"fax">>}],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs3}->
            _ = [maybe_recover_private_media(AccountDb, JObj) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch fax docs in db ~s: ~p~n", [AccountDb, E3])
    end.

maybe_recover_private_media(AccountDb, JObj) ->
    {'ok', Doc } = couch_mgr:open_doc(AccountDb, wh_doc:id(JObj)),
    recover_private_media(AccountDb, Doc, wh_json:get_value(<<"media_type">>, Doc)).

recover_private_media(_AccountDb, _Doc, <<"tiff">>) ->
    'ok';
recover_private_media(AccountDb, Doc, _MediaType) ->
    {'ok', _ } = couch_mgr:ensure_saved(AccountDb, wh_doc:set_type(Doc, <<"private_media">>)),
    'ok'.

-spec migrate_faxes_to_modb(ne_binary(),  wh_proplist()) -> 'ok'.
-spec maybe_migrate_fax_to_modb(ne_binary(), wh_json:object(),  wh_proplist()) -> 'ok'.
-spec migrate_fax_to_modb(ne_binary(), ne_binary(), wh_json:object(),  wh_proplist()) -> 'ok'.

migrate_faxes_to_modb(Account, Options) ->
    AccountDb = case couch_mgr:db_exists(Account) of
                    'true' -> Account;
                    'false' -> wh_util:format_account_id(Account, 'encoded')
                end,
    ViewOptions = [{'key', <<"fax">>}],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', []} -> io:format("no fax docs in db for fax migration ~s~n", [AccountDb]);
        {'ok', JObjs3}->
            _ = [maybe_migrate_fax_to_modb(AccountDb, JObj, Options) || JObj <- JObjs3],
            'ok';
        {'error', _}=E3 ->
            io:format("unable to fetch fax docs in db ~s: ~p~n", [AccountDb, E3])
    end.

maybe_migrate_fax_to_modb(AccountDb, JObj, Options) ->
    DocId = wh_doc:id(JObj),
    case couch_mgr:open_doc(AccountDb, DocId) of
        {'ok', Doc} ->
            case wh_doc:attachments(Doc) of
                'undefined' ->
                    case whapps_config:get_is_true(<<"fax">>, <<"delete_empty_faxes">>, 'false') of
                        'true' ->
                            io:format("deleting no attachments fax doc ~s from ~s~n",[DocId, AccountDb]),
                            couch_mgr:del_doc(AccountDb, Doc);
                        'false' -> 'ok'
                    end;
                _Attachments ->
                    migrate_fax_to_modb(AccountDb, DocId, Doc, Options)
            end;
        {'error', E} ->
            io:format("unable to get document ~s for fax migration : ~p",[DocId, E])
    end.

migrate_fax_to_modb(AccountDb, DocId, JObj, Options) ->
    Timestamp = wh_doc:created(JObj, wh_util:current_tstamp()),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    AccountMODb = kazoo_modb:get_modb(AccountDb, Year, Month),
    FaxMODb = wh_util:format_account_id(AccountMODb, 'encoded'),
    FaxId = <<(wh_util:to_binary(Year))/binary
             ,(wh_util:pad_month(Month))/binary
             ,"-"
             ,DocId/binary
            >>,
    io:format("moving doc ~s/~s to ~s/~s~n",[AccountDb, DocId, AccountMODb, FaxId]),
    kazoo_modb:create(AccountMODb),
    case couch_mgr:move_doc(AccountDb, DocId, FaxMODb, FaxId, Options) of
        {'ok', _JObj} -> io:format("document ~s moved to ~s~n",[DocId, FaxId]);
        {'error', Error} -> io:format("error ~p moving document ~s to ~s~n",[Error, DocId, FaxId])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the fax local cache
%% @end
%%--------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() -> wh_cache:flush_local(?FAX_CACHE).

account_jobs(AccountId) ->
    account_jobs(AccountId, <<"pending">>).

account_jobs(AccountId, State) ->
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-30s | ~-17s | ~-15s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Job">>, <<"Date">>, <<"State">>, <<"Account">>, <<"Faxbox">>, <<"From">>, <<"To">>]),
    io:format("+================================+===================+=================+==================================+==================================+======================+======================+~n", []),
    ViewOptions = [{'startkey', [AccountId, State]}
                   ,{'endkey', [AccountId, State, wh_json:new()]}
                   ],

    _ = case couch_mgr:get_results(?WH_FAXES_DB, <<"faxes/list_by_account_state">>, ViewOptions) of
            {'ok', Jobs} ->
                [io:format(FormatString, [wh_json:get_value([<<"value">>, <<"id">>], JObj)
                                          ,wh_util:format_datetime(
                                             wh_json:get_value([<<"value">>, <<"modified">>], JObj))
                                          ,wh_json:get_value([<<"value">>, <<"status">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"faxbox_id">>], JObj, <<"(none)">>)
                                          ,wh_json:get_value([<<"value">>, <<"from">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"to">>], JObj)
                                         ]) || JObj <- Jobs];
            {'error', _Reason} ->
                io:format("Error getting faxes\n")
        end,
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

faxbox_jobs(FaxboxId) ->
    faxbox_jobs(FaxboxId, <<"pending">>).

faxbox_jobs(FaxboxId, State) ->
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-30s | ~-17s | ~-15s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Job">>, <<"Date">>, <<"State">>, <<"Account">>, <<"Faxbox">>, <<"From">>, <<"To">>]),
    io:format("+================================+===================+=================+==================================+==================================+======================+======================+~n", []),
    ViewOptions = [{'startkey', [FaxboxId, State]}
                   ,{'endkey', [FaxboxId, State, wh_json:new()]}
                   ],

    _ = case couch_mgr:get_results(?WH_FAXES_DB, <<"faxes/list_by_faxbox_state">>, ViewOptions) of
            {'ok', Jobs} ->
                [io:format(FormatString, [wh_json:get_value([<<"value">>, <<"id">>], JObj)
                                          ,wh_util:format_datetime(
                                             wh_json:get_value([<<"value">>, <<"modified">>], JObj))
                                          ,wh_json:get_value([<<"value">>, <<"status">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"faxbox_id">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"from">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"to">>], JObj)
                                         ]) || JObj <- Jobs];
            {'error', _Reason} ->
                io:format("Error getting faxes~n", [])
        end,
    io:format("+--------------------------------+-------------------+-----------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

-spec pending_jobs() -> 'no_return'.
pending_jobs() ->
    io:format("+--------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-30s | ~-17s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Job">>, <<"Date">>, <<"Account">>, <<"Faxbox">>, <<"From">>, <<"To">>]),
    io:format("+================================+===================+==================================+==================================+======================+======================+~n", []),
    _ = case couch_mgr:get_results(?WH_FAXES_DB, <<"faxes/jobs">>) of
            {'ok', Jobs} ->
                [io:format(FormatString, [wh_json:get_value([<<"value">>, <<"id">>], JObj)
                                          ,wh_util:format_datetime(
                                             wh_json:get_value([<<"value">>, <<"modified">>], JObj))
                                          ,wh_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"faxbox_id">>], JObj, <<"(none)">>)
                                          ,wh_json:get_value([<<"value">>, <<"from">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"to">>], JObj)
                                         ]) || JObj <- Jobs];
            {'error', _Reason} ->
                io:format("Error getting faxes~n", [])
        end,
    io:format("+--------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

-spec active_jobs() -> 'no_return'.
active_jobs() ->
    io:format("+--------------------------------+--------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    FormatString = "| ~-30s | ~-30s | ~-17s | ~-32s | ~-32s | ~-20s | ~-20s |~n",
    io:format(FormatString, [<<"Node">>, <<"Job">>, <<"Date">>, <<"Account">>, <<"From">>, <<"To">>]),
    io:format("+================================+================================+===================+==================================+==================================+======================+======================+~n", []),
    _ = case couch_mgr:get_results(?WH_FAXES_DB, <<"faxes/processing_by_node">>) of
            {'ok', Jobs} ->
                [io:format(FormatString, [wh_json:get_value([<<"value">>, <<"node">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"id">>], JObj)
                                          ,wh_util:format_datetime(
                                             wh_json:get_value([<<"value">>, <<"modified">>], JObj))
                                          ,wh_json:get_value([<<"value">>, <<"account_id">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"faxbox_id">>], JObj, <<"(none)">>)
                                          ,wh_json:get_value([<<"value">>, <<"from">>], JObj)
                                          ,wh_json:get_value([<<"value">>, <<"to">>], JObj)
                                         ]) || JObj <- Jobs];
            {'error', _Reason} ->
                io:format("Error getting faxes~n", [])
        end,
    io:format("+--------------------------------+--------------------------------+-------------------+----------------------------------+----------------------------------+----------------------+----------------------+~n", []),
    'no_return'.

-spec restart_job(binary()) -> 'no_return'.
restart_job(JobID) ->
    _ = case update_job(JobID, <<"pending">>) of
            'ok' -> 'ok';
            {'error',_}=E ->
                lager:debug("restarting job ~p failed : ~p", [JobID, E])
        end,
    'no_return'.

-spec update_job(binary(), binary()) -> 'ok' | {'error', _}.
-spec update_job(binary(), binary(), wh_json:object()) -> 'ok' | {'error', _}.
update_job(JobID, State) ->
    case couch_mgr:open_doc(?WH_FAXES_DB, JobID) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            update_job(JobID, State, JObj)
    end.

update_job(JobID, State, JObj) ->
    case wh_json:get_value(<<"pvt_job_status">>, JObj) of
        State ->
            lager:debug("job ~s already in state ~s", [JobID, State]),
            {'error', 'job_not_already_in_state'};
        _Other ->
            Opts = [{'rev', wh_doc:revision(JObj)}],
            couch_mgr:save_doc(?WH_FAXES_DB
                               ,wh_json:set_values([{<<"pvt_job_status">>, State}
                                                    ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                   ]
                                                   ,JObj
                                                  )
                               ,Opts
                              ),
            'ok'
    end.
