%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_yodb).

-include("kazoo_yodb.hrl").

-export([get_results/3]).
-export([open_doc/2, open_doc/3]).
-export([save_doc/2, save_doc/3, save_doc/4]).
-export([move_doc/4, move_doc/5]).
-export([copy_doc/4, copy_doc/5]).
-export([get_yodb/1, get_yodb/2]).
-export([maybe_archive_yodb/1]).
-export([refresh_views/1]).
-export([create/1, maybe_create/1
        ,add_routine/1
        ]).
-export([maybe_delete/2]).
-export([get_range/3, get_range/4]).
-export([get_year_month_sequence/3, get_year_month_sequence/4]).
-export([strip_yodb_options/1]).

-type view_option() :: {'year', kz_time:year()} |
                       {'create_db', boolean()} | 'create_db' |
                       {'allow_old_yodb_creation', boolean()} | 'allow_old_yodb_creation' |
                       {'missing_as_error', boolean()} | 'missing_as_error' |
                       kz_datamgr:view_option().
-type view_options() :: [view_option()].

-define(MAX_RETRIES, 2).
-define(YEAR_INT_BOUNDARY, 9999).

-export_type([view_options/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec get_results(kz_term:ne_binary(), kz_term:ne_binary(), view_options()) ->
          {'ok', kz_json:json_terms()} |
          {'error', atom()}.
get_results(Account, View, ViewOptions) ->
    MaxRetries = props:get_integer_value('max_retries', ViewOptions, ?MAX_RETRIES),
    get_results(Account, View, ViewOptions, 'first_try', MaxRetries).

-spec get_results(kz_term:ne_binary(), kz_term:ne_binary(), view_options(), atom(), non_neg_integer()) ->
          {'ok', kz_json:json_terms()} |
          {'error', atom()}.
get_results(_Account, _View, _ViewOptions, Reason, Retry) when Retry =< 0 ->
    lager:debug("max retries to get view ~s/~s results: ~p", [_Account, _View, Reason]),
    {'error', Reason};
get_results(Account, View, ViewOptions, _Reason, Retry) ->
    AccountYODb = get_yodb(Account, ViewOptions),
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    case kz_datamgr:get_results(EncodedYODb, View, strip_yodb_options(ViewOptions)) of
        {'error', 'not_found'} ->
            get_results_missing_db(Account, View, ViewOptions, Retry);
        {'error', 'timeout'} ->
            get_results(Account, View, ViewOptions, 'timeout', Retry-1);
        Results -> Results
    end.

-spec strip_yodb_options(view_options()) -> kz_datamgr:view_options().
strip_yodb_options(ViewOptions) ->
    [Option || Option <- ViewOptions,
               not is_yodb_option(Option)
    ].

-spec is_yodb_option(view_option()) -> boolean().
is_yodb_option({'year', _}) -> 'true';
is_yodb_option({'create_db', _}) -> 'true';
is_yodb_option('create_db') -> 'true';
is_yodb_option({'allow_old_yodb_creation', _}) -> 'true';
is_yodb_option('allow_old_yodb_creation') -> 'true';
is_yodb_option({'ensure_saved', _}) -> 'true';
is_yodb_option({'max_retries', _}) -> 'true';
is_yodb_option({'missing_as_error', _}) -> 'true';
is_yodb_option('missing_as_error') -> 'true';
is_yodb_option(_) -> 'false'.

-spec get_results_missing_db(kz_term:ne_binary(), kz_term:ne_binary(), view_options(), integer()) ->
          {'ok', kz_json:objects()}.
get_results_missing_db(Account, View, ViewOptions, Retry) ->
    AccountYODb = get_yodb(Account, ViewOptions),
    ShouldCreate = props:get_is_true('create_db', ViewOptions, 'true'),
    MissingAsError = props:get_is_true('missing_as_error', ViewOptions, 'false'),
    lager:info("yodb ~p not found, maybe creating...", [AccountYODb]),
    case ShouldCreate
        andalso maybe_create_current_yodb(AccountYODb, ViewOptions)
    of
        'true' -> get_results(Account, View, ViewOptions, 'not_found', Retry-1);
        'too_old' when MissingAsError ->
            {'error', 'db_not_found'};
        'too_old' ->
            {'ok', []};
        'false' when ShouldCreate ->
            lager:info("yodb ~s creation failed, maybe due to race condition, re-trying get_results", [AccountYODb]),
            get_results(Account, View, ViewOptions, 'not_found', Retry-1);
        'false' when MissingAsError ->
            {'error', 'db_not_found'};
        'false' ->
            lager:info("create_db is false, not creating yodb ~s ...", [AccountYODb]),
            {'ok', []}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec open_doc(kz_term:ne_binary(), kazoo_data:docid()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
open_doc(Account, {_, ?MATCH_YODB_PREFIX(Year, Account)} = DocId) ->
    AccountYODb = get_yodb(Account, kz_term:to_integer(Year)),
    couch_open(AccountYODb, DocId);
open_doc(Account, ?MATCH_YODB_PREFIX(Year, Account) = DocId) ->
    AccountYODb = get_yodb(Account, kz_term:to_integer(Year)),
    couch_open(AccountYODb, DocId);
open_doc(Account, DocId) ->
    AccountYODb = get_yodb(Account),
    couch_open(AccountYODb, DocId).

-spec open_doc(kz_term:ne_binary(), kazoo_data:docid(), integer() | view_options()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
open_doc(Account, DocId, Options)
  when is_list(Options) ->
    AccountYODb = get_yodb(Account, Options),
    couch_open(AccountYODb, DocId, Options);
open_doc(Account, DocId, Timestamp)
  when is_integer(Timestamp)
       andalso Timestamp > ?YEAR_INT_BOUNDARY ->
    AccountYODb = get_yodb(Account, Timestamp),
    couch_open(AccountYODb, DocId);
open_doc(Account, DocId, Year) ->
    AccountYODb = get_yodb(Account, Year),
    couch_open(AccountYODb, DocId).


-spec couch_open(kz_term:ne_binary(), kazoo_data:docid()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
couch_open(AccountYODb, DocId) ->
    couch_open(AccountYODb, DocId, []).

-spec couch_open(kz_term:ne_binary(), kazoo_data:docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
couch_open(AccountYODb, DocId, Options) ->
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    case kz_datamgr:open_doc(EncodedYODb, DocId, Options) of
        {'ok', _}=Ok -> Ok;
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec save_doc(kz_term:ne_binary(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
save_doc(Account, Doc) ->
    save_doc(Account, Doc, []).

-spec save_doc(kz_term:ne_binary(), kz_json:object(), kz_time:now() | kz_time:gregorian_seconds() | kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
save_doc(Account, Doc, Options) when is_list(Options) ->
    AccountYODb = get_yodb(Account),
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    couch_save(AccountYODb, Doc, Options, 'first_try', MaxRetries);
save_doc(Account, Doc, Timestamp) ->
    save_doc(Account, Doc, Timestamp, []).

-spec save_doc(kz_term:ne_binary(), kz_json:object(), kz_time:year() | kz_term:ne_binary() | kz_time:now(), view_options()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
save_doc(Account, Doc, Timestamp, Options) when is_list(Options) ->
    AccountYODb = get_yodb(Account, Timestamp),
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    couch_save(AccountYODb, Doc, Options, 'first_try', MaxRetries).

-spec couch_save(kz_term:ne_binary(), kz_json:object(), kz_term:proplist(), atom(), integer()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
couch_save(AccountYODb, _Doc, _Options, Reason, Retry) when Retry =< 0 ->
    lager:debug("max retries to save doc in ~s: ~p", [AccountYODb, Reason]),
    {'error', Reason};
couch_save(AccountYODb, Doc, Options, _Reason, Retry) ->
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    SaveFun = save_fun(props:get_is_true('ensure_saved', Options, 'false')),
    case SaveFun(EncodedYODb, Doc, strip_yodb_options(Options)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} = NotFound ->
            ShouldCreate = props:get_is_true('create_db', Options, 'true'),
            lager:info("yodb ~p not found, maybe creating...", [AccountYODb]),
            case ShouldCreate
                andalso maybe_create_current_yodb(AccountYODb, Options)
            of
                'true' ->
                    couch_save(AccountYODb, Doc, Options, 'not_found', Retry-1);
                'too_old' ->
                    NotFound;
                'false' when ShouldCreate ->
                    lager:info("yodb ~s creation failed, maybe due to race condition, re-trying save_doc", [AccountYODb]),
                    couch_save(AccountYODb, Doc, Options, 'not_found', Retry-1);
                'false' ->
                    lager:info("create_db is false, not creating yodb ~s ...", [AccountYODb]),
                    NotFound
            end;
        {'error', 'conflict'}=Conflict -> Conflict;
        {'error', 'timeout'} -> couch_save(AccountYODb, Doc, Options, 'timeout', Retry-1);
        Error -> Error
    end.

-spec save_fun(boolean()) -> function().
save_fun('false') -> fun kz_datamgr:save_doc/3;
save_fun('true') -> fun kz_datamgr:ensure_saved/3.

%%------------------------------------------------------------------------------
%% @doc Move a document from source to destination with attachments,
%% optionally applies a transform function on the document.
%%
%% <div class="notice">Caller is responsible to format both source and destination
%% databases!</div>
%% @end
%%------------------------------------------------------------------------------
-spec move_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kazoo_data:docid()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
move_doc(FromDb, FromId, ToDb, ToId) ->
    move_doc(FromDb, FromId, ToDb, ToId, []).

-spec move_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kazoo_data:docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
move_doc(FromDb, FromId, ToDb, ToId, Options) ->
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    move_doc(FromDb, FromId, ToDb, ToId, Options, 'first_try', MaxRetries).

-spec move_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kazoo_data:docid(), kz_term:proplist(), atom(), integer()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
move_doc(_FromDb, _FromId, _ToDb, _ToId, _Options, Reason, Retry) when Retry =< 0 ->
    lager:error("max retries to move doc from ~s/~p to ~s/~p : ~p"
               ,[_FromDb, _FromId, _ToDb, _ToId, Reason]
               ),
    {'error', Reason};
move_doc(FromDb, FromId, ToDb, ToId, Options, _Reason, Retry) ->
    case kz_datamgr:move_doc(FromDb, FromId, ToDb, ToId, strip_yodb_options(Options)) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} ->
            case maybe_create_destination_db(FromDb, FromId, ToDb, Options) of
                'true' -> move_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1);
                'source_not_exists' -> {'error', 'not_found'};
                'too_old' -> {'error', 'not_found'};
                'false' ->
                    lager:info("yodb ~s creation failed, maybe due to race condition, re-trying move_doc", [ToDb]),
                    move_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1)
            end;
        {'error', 'conflict'}=Conflict -> Conflict;
        {'error', 'timeout'} -> move_doc(FromDb, FromId, ToDb, ToId, Options, 'timeout', Retry-1);
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Copy a document from source to destination with attachments,
%% optionally applies a transform function on the document.
%%
%% <div class="notice">Caller is responsible to format both source and destination
%% databases!</div>
%% @end
%%------------------------------------------------------------------------------
-spec copy_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kazoo_data:docid()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
copy_doc(FromDb, FromId, ToDb, ToId) ->
    copy_doc(FromDb, FromId, ToDb, ToId, []).

-spec copy_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kazoo_data:docid(), kz_term:proplist()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
copy_doc(FromDb, FromId, ToDb, ToId, Options) ->
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    copy_doc(FromDb, FromId, ToDb, ToId, Options, 'first_try', MaxRetries).

-spec copy_doc(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kazoo_data:docid(), kz_term:proplist(), atom(), integer()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
copy_doc(_FromDb, _FromId, _ToDb, _ToId, _Options, Reason, Retry) when Retry =< 0 ->
    lager:error("max retries to copy doc from ~s/~p to ~s/~p : ~p"
               ,[_FromDb, _FromId, _ToDb, _ToId, Reason]
               ),
    {'error', Reason};
copy_doc(FromDb, FromId, ToDb, ToId, Options, _Reason, Retry) ->
    case kz_datamgr:copy_doc(FromDb, FromId, ToDb, ToId, strip_yodb_options(Options)) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} ->
            case maybe_create_destination_db(FromDb, FromId, ToDb, Options) of
                'true' -> copy_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1);
                'source_not_exists' -> {'error', 'not_found'};
                'too_old' -> {'error', 'not_found'};
                'false' ->
                    lager:info("yodb ~s creation failed, maybe due to race condition, re-trying copy_doc", [ToDb]),
                    copy_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1)
            end;
        {'error', 'conflict'}=Conflict -> Conflict;
        {'error', 'timeout'} -> copy_doc(FromDb, FromId, ToDb, ToId, Options, 'timeout', Retry-1);
        {'error', _}=Error -> Error
    end.

-spec maybe_create_destination_db(kz_term:ne_binary(), kz_term:docid(), kz_term:ne_binary(), kz_term:proplist()) ->
          'source_not_exists' |
          'too_old'|
          boolean().
maybe_create_destination_db(FromDb, FromId, ToDb, Options) ->
    ShouldCreate = props:get_is_true('create_db', Options, 'true'),
    lager:info("destination yodb ~p not found, maybe creating...", [ToDb]),
    case ShouldCreate
        andalso kz_datamgr:db_exists(FromDb)
        andalso kz_datamgr:open_doc(FromDb, FromId)
    of
        'false' when ShouldCreate ->
            lager:info("source yodb ~s does not exist, not creating destination yodb ~s", [FromDb, ToDb]),
            'source_not_exists';
        'false' ->
            lager:info("create_db is false, not creating yodb ~s ...", [ToDb]),
            'source_not_exists';
        {'ok', _} ->
            maybe_create_current_yodb(ToDb, Options);
        {'error', _} ->
            lager:info("source document ~s/~p does not exist, not creating destination yodb ~s", [FromDb, FromId, ToDb]),
            'source_not_exists'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec get_yodb(kz_term:ne_binary()) -> kz_term:ne_binary().
get_yodb(?MATCH_YODB_SUFFIX_RAW(_,_) = AccountYODb) ->
    AccountYODb;
get_yodb(?MATCH_YODB_SUFFIX_ENCODED(_,_) = AccountYODb) ->
    kzs_util:format_account_yodb(AccountYODb, 'raw');
get_yodb(?MATCH_YODB_SUFFIX_UNENCODED(_,_) = AccountYODb) ->
    kzs_util:format_account_yodb(AccountYODb, 'raw');
get_yodb(Account) ->
    AccountDb = kzs_util:format_account_db(Account),
    {Year, _, _} = erlang:date(),
    get_yodb(AccountDb, Year).

-spec get_yodb(kz_term:ne_binary(), view_options() | kz_time:gregorian_seconds() | kz_time:now()) ->
          kz_term:ne_binary().
get_yodb(Account, ViewOptions) when is_list(ViewOptions) ->
    AccountDb = kzs_util:format_account_db(Account),
    case props:get_value('year', ViewOptions) of
        'undefined' -> get_yodb(Account);
        Year ->
            get_yodb(AccountDb, Year)
    end;
get_yodb(?MATCH_YODB_SUFFIX_ENCODED(_,_) = AccountYODb, _Year) ->
    kzs_util:format_account_yodb(AccountYODb, 'raw');
get_yodb(?MATCH_YODB_SUFFIX_UNENCODED(_,_) = AccountYODb, _Year) ->
    kzs_util:format_account_yodb(AccountYODb, 'raw');
get_yodb(?MATCH_YODB_SUFFIX_RAW(_,_) = AccountYODb, _Year) ->
    AccountYODb;
get_yodb(Account, Year) when is_integer(Year)
                             andalso Year < ?YEAR_INT_BOUNDARY ->
    kzs_util:format_account_yod_id(Account, Year);
get_yodb(Account, Timestamp) ->
    kzs_util:format_account_yod_id(Account, Timestamp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_create_current_yodb(kz_term:ne_binary(), kz_term:proplist()) -> 'too_old' | boolean().
maybe_create_current_yodb(?MATCH_YODB_SUFFIX_RAW(_AccountId, Year) = AccountYODb, Options) ->
    {Y, _M, _} = erlang:date(),
    ShouldCreateOld = props:get_is_true('allow_old_yodb_creation', Options, 'false'),
    case kz_term:to_binary(Y) of
        Year ->
            maybe_create(AccountYODb);
        _Year when ShouldCreateOld ->
            maybe_create(AccountYODb);
        _Year ->
            lager:info("yodb ~p is not for the current year, skip creating", [AccountYODb]),
            'too_old'
    end;
maybe_create_current_yodb(?MATCH_YODB_SUFFIX_ENCODED(_, _) = AccountYODb, Options) ->
    maybe_create_current_yodb(kzs_util:format_account_yodb(AccountYODb, 'raw'), Options);
maybe_create_current_yodb(?MATCH_YODB_SUFFIX_UNENCODED(_, _) = AccountYODb, Options) ->
    maybe_create_current_yodb(kzs_util:format_account_yodb(AccountYODb, 'raw'), Options).

-spec maybe_create(kz_term:ne_binary()) -> boolean().
maybe_create(?MATCH_YODB_SUFFIX_RAW(AccountId, _) = AccountYODb) ->
    maybe_create(AccountYODb, is_account_deleted(AccountId));
maybe_create(?MATCH_YODB_SUFFIX_ENCODED(_, _) = AccountYODb) ->
    maybe_create(kzs_util:format_account_yodb(AccountYODb, 'raw'));
maybe_create(?MATCH_YODB_SUFFIX_UNENCODED(_, _) = AccountYODb) ->
    maybe_create(kzs_util:format_account_yodb(AccountYODb, 'raw')).

-spec maybe_create(kz_term:ne_binary(), boolean()) -> boolean().
maybe_create(?MATCH_YODB_SUFFIX_RAW(_AccountId, _) = _AccountYODb, 'true') ->
    lager:info("account ~s is deleted, not creating yodb ~s", [_AccountId, _AccountYODb]),
    'false';
maybe_create(AccountYODb, 'false') ->
    create(AccountYODb).

-spec create(kz_term:ne_binary()) -> boolean().
create(AccountYODb) ->
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    IsDbExists = kz_datamgr:db_exists_all(EncodedYODb),
    lager:notice("~ncreate/1 (~p) database: ~p~ndb exists: ~p", [AccountYODb, EncodedYODb, IsDbExists]),
    do_create(AccountYODb, IsDbExists).

-spec do_create(kz_term:ne_binary(), boolean()) -> boolean().
do_create(AccountYODb, 'true') ->
    lager:info("yodb '~s' exists, just refreshing views and running routines...", [AccountYODb]),
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    _ = refresh_views(EncodedYODb),
    run_routines(AccountYODb),
    'true';
do_create(AccountYODb, 'false') ->
    lager:debug("creating yodb '~s'", [AccountYODb]),
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    lager:notice("creating encoded yodb '~s'", [EncodedYODb]),
    case kz_datamgr:db_create(EncodedYODb) of
        'true' ->
            _ = refresh_views(EncodedYODb),
            run_routines(AccountYODb),
            'true';
        _Error ->
            lager:notice("~nError: ~p~nYODb: ~p~nEnc_YODB: ~p~n", [_Error, AccountYODb, EncodedYODb]),
            'false'
    end.

-spec is_account_deleted(kz_term:ne_binary()) -> boolean().
is_account_deleted(AccountId) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> kz_doc:is_soft_deleted(JObj);
        {'error', _} -> 'true'
    end.

-spec refresh_views(kz_term:ne_binary()) -> boolean() | {'error', 'invalid_db_name' | 'db_not_found'}.
refresh_views(AccountYODb) ->
    lager:debug("refresh views on yodb ~p", [AccountYODb]),
    EncodedYODb = kzs_util:format_account_yodb(AccountYODb, 'encoded'),
    kz_datamgr:refresh_views(EncodedYODb).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec run_routines(kz_term:ne_binary()) -> 'ok'.
run_routines(AccountYODb) ->
    Routines = kapps_config:get_ne_binaries(?CONFIG_CAT, <<"routines">>, []),
    Runs = [{Routine, kz_process:spawn_monitor(kz_term:to_atom(Routine), 'yodb', [AccountYODb])}
            || Routine <- Routines,
               kz_module:is_exported(Routine, 'yodb', 1)
           ],
    wait_for_runs(Runs, kz_time:start_time()).

-type run() :: {kz_term:ne_binary(), kz_term:pid_ref()}.
-type runs() :: [run()].

-spec wait_for_runs(runs(), kz_time:start_time()) -> 'ok'.
wait_for_runs([], _Start) -> 'ok';
wait_for_runs(Runs, Start) ->
    receive
        {'DOWN', Ref, 'process', Pid, Reason} ->
            handle_finished_run(Runs, Start, {Pid, Ref}, Reason)
    after
        ?MILLISECONDS_IN_MINUTE ->
            lager:info("runs haven't finished yet, moving on: ~p", [Runs])
    end.

-spec handle_finished_run(runs(), kz_time:start_time(), kz_term:pid_ref(), any()) -> 'ok'.
handle_finished_run(Runs, Start, PidRef, Reason) ->
    case lists:keytake(PidRef, 2, Runs) of
        'false' ->
            lager:debug("ignoring unknown pid/ref ~p: ~p", [PidRef, Reason]),
            wait_for_runs(Runs, Start);
        {'value', {Routine, PidRef}, RestRuns} when Reason =:= 'normal' ->
            lager:debug("routine ~s(~p) finished after ~pms", [Routine, element(1, PidRef), kz_time:elapsed_ms(Start)]),
            wait_for_runs(RestRuns, Start);
        {'value', {Routine, PidRef}, RestRuns} ->
            lager:info("routine ~s(~p) crashed after ~pms: ~p", [Routine, element(1, PidRef), kz_time:elapsed_ms(Start), Reason]),
            wait_for_runs(RestRuns, Start)
    end.

-spec add_routine(kz_term:ne_binary() | atom()) -> 'ok'.
add_routine(Module) ->
    Routine = kz_term:to_binary(Module),
    Routines = kapps_config:get(?CONFIG_CAT, <<"routines">>, []),
    case add_migrate_routines(Routines, Routine) of
        Routines -> 'ok';
        NewRoutines ->
            _ = kapps_config:set_default(?CONFIG_CAT, <<"routines">>, NewRoutines),
            'ok'
    end.

-spec add_migrate_routines(kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_term:ne_binaries().
add_migrate_routines(Routines, Module) ->
    lists:usort([Module | migrate_routines(Routines, [])] -- [<<"kz_currency">>]).

-spec migrate_routines(kz_term:ne_binaries(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
migrate_routines([], Acc) -> Acc;
migrate_routines([<<"wh_", Rest/binary>> | Rs], Acc) ->
    migrate_routines(Rs, [<<"kz_", Rest/binary>> | Acc]);
migrate_routines([R | Rs], Acc) ->
    migrate_routines(Rs, [R | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_archive_yodb(kz_term:ne_binary()) -> 'ok'.
maybe_archive_yodb(AccountYODb) ->
    {Year, _, _} = erlang:date(),
    case should_archive(AccountYODb, Year) of
        'true' ->
            lager:info("account yodb ~s needs archiving", [AccountYODb]),
            'ok' = kz_datamgr:db_archive(AccountYODb),
            lager:info("account yodb ~s archived, removing the db", [AccountYODb]),
            Rm = kz_datamgr:db_delete(AccountYODb),
            lager:info("account yodb ~s deleted: ~p", [AccountYODb, Rm]);
        'false' ->
            lager:info("account yodb ~s still current enough to keep", [AccountYODb])
    end.

-spec should_archive(kz_term:ne_binary(), kz_time:year()) -> boolean().
should_archive(AccountYODb, Year) ->
    case kazoo_yodb_util:split_account_yod(AccountYODb) of
        {_AccountId, Year} -> 'false';
        {_AccountId, YodbYear} ->
            (Year - YodbYear) > kapps_config:get_integer(?CONFIG_CAT, <<"active_yodbs">>, 6)
    end.


%%------------------------------------------------------------------------------
%% @doc Delete an yodb if it is no longer associated with its account.
%% (That is: orphaned).
%% AccountYODb must be 'encoded' otherwise kz_datamgr:db_delete/1 will fail.
%% AccountIds should be kapps_util:get_all_accounts('raw').
%% Returns whether AccountYODb has been deleted.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_delete(kz_term:ne_binary(), [kz_term:ne_binary()]) -> boolean().
maybe_delete(AccountYODb, AccountIds) ->
    AccountId = kzs_util:format_account_id(AccountYODb),
    IsOrphaned = not lists:member(AccountId, AccountIds),
    delete_if_orphaned(AccountYODb, IsOrphaned).

-spec delete_if_orphaned(kz_term:ne_binary(), boolean()) -> boolean().
delete_if_orphaned(_AccountYODb, 'false') -> 'false';
delete_if_orphaned(AccountYODb, 'true') ->
    Succeeded = kz_datamgr:db_delete(AccountYODb),
    lager:debug("cleanse orphaned yodb ~p... ~p", [AccountYODb,Succeeded]),
    Succeeded.

-spec get_range(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
          kz_term:ne_binaries().
get_range(AccountId, From, To) ->
    get_range(<<"any">>, AccountId, From, To).

-spec get_range(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
          kz_term:ne_binaries().
get_range(Type, AccountId, From, To) ->
    {{FromYear, FromMonth, _}, _} = calendar:gregorian_seconds_to_datetime(From),
    {{ToYear,   ToMonth,   _}, _} = calendar:gregorian_seconds_to_datetime(To),
    [YODb
     || YODb <- get_year_month_sequence(AccountId
                                       ,{FromYear, FromMonth}
                                       ,{ToYear, ToMonth}
                                       ),
        kz_datamgr:db_exists(YODb, Type)
    ].

-type year_month_tuple() :: {kz_time:year(), kz_time:month()}.

-spec get_year_month_sequence(kz_term:ne_binary(), year_month_tuple(), year_month_tuple()) ->
          kz_term:ne_binaries().
get_year_month_sequence(Account, From, To) ->
    get_year_month_sequence(Account, From, To, []).

-spec get_year_month_sequence(kz_term:ne_binary(), year_month_tuple(), year_month_tuple(), kz_term:proplist()) ->
          kz_term:ne_binaries().
get_year_month_sequence(Account, Tuple, Tuple, Range) ->
    ToYODbId = fun ({Year, _Month}, Acc) -> [get_yodb(Account, Year)|Acc] end,
    lists:foldl(ToYODbId, [], [Tuple|Range]);
get_year_month_sequence(Account, {FromYear,13}, To, Range) ->
    get_year_month_sequence(Account, {FromYear+1,1}, To, Range);
get_year_month_sequence(Account, {FromYear,FromMonth}=From, {ToYear,ToMonth}=To, Range) ->
    'true' = (FromYear * 12 + FromMonth) =< (ToYear * 12 + ToMonth),
    get_year_month_sequence(Account, {FromYear,FromMonth+1}, To, [From|Range]).
