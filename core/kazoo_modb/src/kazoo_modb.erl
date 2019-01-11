%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(kazoo_modb).

-include("kazoo_modb.hrl").

-export([get_results/3]).
-export([open_doc/2, open_doc/3, open_doc/4]).
-export([save_doc/2, save_doc/3, save_doc/4]).
-export([move_doc/4, move_doc/5]).
-export([copy_doc/4, copy_doc/5]).
-export([get_modb/1, get_modb/2, get_modb/3]).
-export([maybe_archive_modb/1]).
-export([refresh_views/1]).
-export([create/1, maybe_create/1
        ,add_routine/1
        ]).
-export([maybe_delete/2]).
-export([get_range/3, get_range/4]).
-export([get_year_month_sequence/3, get_year_month_sequence/4]).
-export([strip_modb_options/1]).

-type view_option() :: {'year', kz_year()} |
                       {'month', kz_month()} |
                       {'create_db', boolean()} |
                       {'allow_old_modb_creation', boolean()} |
                       kz_datamgr:view_option().
-type view_options() :: [view_option()].

-define(MAX_RETRIES, 2).

-export_type([view_options/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec get_results(ne_binary(), ne_binary(), view_options()) ->
                         {'ok', kz_json:objects()} |
                         {'error', atom()}.
-spec get_results(ne_binary(), ne_binary(), view_options(), atom(), non_neg_integer()) ->
                         {'ok', kz_json:objects()} |
                         {'error', atom()}.
get_results(Account, View, ViewOptions) ->
    MaxRetries = props:get_integer_value('max_retries', ViewOptions, ?MAX_RETRIES),
    get_results(Account, View, ViewOptions, 'first_try', MaxRetries).

get_results(_Account, _View, _ViewOptions, Reason, Retry) when Retry =< 0 ->
    lager:debug("max retries to get view ~s/~s results: ~p", [_Account, _View, Reason]),
    {'error', Reason};
get_results(Account, View, ViewOptions, _Reason, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    case kz_datamgr:get_results(EncodedMODb, View, strip_modb_options(ViewOptions)) of
        {'error', 'not_found'} ->
            get_results_missing_db(Account, View, ViewOptions, Retry);
        {'error', 'timeout'} ->
            get_results(Account, View, ViewOptions, 'timeout', Retry-1);
        Results -> Results
    end.

-spec strip_modb_options(view_options()) -> kz_datamgr:view_options().
strip_modb_options(ViewOptions) ->
    [Option || Option <- ViewOptions,
               not is_modb_option(Option)
    ].

-spec is_modb_option(view_option()) -> boolean().
is_modb_option({'year', _}) -> 'true';
is_modb_option({'month', _}) -> 'true';
is_modb_option({'create_db', _}) -> 'true';
is_modb_option({'allow_old_modb_creation', _}) -> 'true';
is_modb_option({'ensure_saved', _}) -> 'true';
is_modb_option({'max_retries', _}) -> 'true';
is_modb_option(_) -> 'false'.

-spec get_results_missing_db(ne_binary(), ne_binary(), view_options(), integer()) ->
                                    {'ok', kz_json:objects()}.
get_results_missing_db(Account, View, ViewOptions, Retry) ->
    AccountMODb = get_modb(Account, ViewOptions),
    ShouldCreate = props:get_is_true('create_db', ViewOptions, 'true'),
    lager:info("modb ~p not found, maybe creating...", [AccountMODb]),
    case ShouldCreate
        andalso maybe_create_current_modb(AccountMODb, ViewOptions)
    of
        'true' -> get_results(Account, View, ViewOptions, 'not_found', Retry-1);
        'too_old' ->
            {'ok', []};
        'false' when ShouldCreate ->
            lager:info("modb ~s creation failed, maybe due to race condition, re-trying get_results", [AccountMODb]),
            get_results(Account, View, ViewOptions, 'not_found', Retry-1);
        'false' ->
            lager:info("create_db is false, not creating modb ~s ...", [AccountMODb]),
            {'ok', []}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec open_doc(ne_binary(), kazoo_data:docid()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
-spec open_doc(ne_binary(), kazoo_data:docid(), integer() | view_options()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
-spec open_doc(ne_binary(), kazoo_data:docid(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
open_doc(Account, {_, ?MATCH_MODB_PREFIX(Year,Month,_)} = DocId) ->
    AccountMODb = get_modb(Account, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    couch_open(AccountMODb, DocId);
open_doc(Account, ?MATCH_MODB_PREFIX(Year,Month,_) = DocId) ->
    AccountMODb = get_modb(Account, kz_term:to_integer(Year), kz_term:to_integer(Month)),
    couch_open(AccountMODb, DocId);
open_doc(Account, DocId) ->
    AccountMODb = get_modb(Account),
    couch_open(AccountMODb, DocId).

open_doc(Account, DocId, Options)
  when is_list(Options) ->
    AccountMODb = get_modb(Account, Options),
    couch_open(AccountMODb, DocId, Options);
open_doc(Account, DocId, Timestamp)
  when is_integer(Timestamp) ->
    AccountMODb = get_modb(Account, Timestamp),
    couch_open(AccountMODb, DocId).

open_doc(Account, DocId, Year, Month) ->
    AccountMODb = get_modb(Account, Year, Month),
    couch_open(AccountMODb, DocId).

-spec couch_open(ne_binary(), kazoo_data:docid()) ->
                        {'ok', kz_json:object()} |
                        {'error', atom()}.
couch_open(AccountMODb, DocId) ->
    couch_open(AccountMODb, DocId, []).

-spec couch_open(ne_binary(), kazoo_data:docid(), kz_proplist()) ->
                        {'ok', kz_json:object()} |
                        {'error', atom()}.
couch_open(AccountMODb, DocId, Options) ->
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    case kz_datamgr:open_doc(EncodedMODb, DocId, Options) of
        {'ok', _}=Ok -> Ok;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec save_doc(ne_binary(), kz_json:object()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
-spec save_doc(ne_binary(), kz_json:object(), kz_now() | gregorian_seconds() | kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
-spec save_doc(ne_binary(), kz_json:object(), kz_year() | ne_binary() | kz_now(), kz_month() | ne_binary() | kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
save_doc(Account, Doc) ->
    save_doc(Account, Doc, []).

save_doc(Account, Doc, Options) when is_list(Options) ->
    AccountMODb = get_modb(Account),
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    couch_save(AccountMODb, Doc, Options, 'first_try', MaxRetries);
save_doc(Account, Doc, Timestamp) ->
    save_doc(Account, Doc, Timestamp, []).

save_doc(Account, Doc, Timestamp, Options) when is_list(Options) ->
    AccountMODb = get_modb(Account, Timestamp),
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    couch_save(AccountMODb, Doc, Options, 'first_try', MaxRetries);
save_doc(Account, Doc, Year, Month) ->
    save_doc(Account, Doc, Year, Month, []).

save_doc(Account, Doc, Year, Month, Options) ->
    AccountMODb = get_modb(Account, Year, Month),
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    couch_save(AccountMODb, Doc, Options, 'first_try', MaxRetries).

-spec couch_save(ne_binary(), kz_json:object(), kz_proplist(), atom(), integer()) ->
                        {'ok', kz_json:object()} |
                        {'error', atom()}.
couch_save(AccountMODb, _Doc, _Options, Reason, Retry) when Retry =< 0 ->
    lager:debug("max retries to save doc in ~s: ~p", [AccountMODb, Reason]),
    {'error', Reason};
couch_save(AccountMODb, Doc, Options, _Reason, Retry) ->
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    SaveFun = save_fun(props:get_is_true('ensure_saved', Options, 'false')),
    case SaveFun(EncodedMODb, Doc, strip_modb_options(Options)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} = NotFound ->
            ShouldCreate = props:get_is_true('create_db', Options, 'true'),
            lager:info("modb ~p not found, maybe creating...", [AccountMODb]),
            case ShouldCreate
                andalso maybe_create_current_modb(AccountMODb, Options)
            of
                'true' ->
                    couch_save(AccountMODb, Doc, Options, 'not_found', Retry-1);
                'too_old' ->
                    NotFound;
                'false' when ShouldCreate ->
                    lager:info("modb ~s creation failed, maybe due to race condition, re-trying save_doc", [AccountMODb]),
                    couch_save(AccountMODb, Doc, Options, 'not_found', Retry-1);
                'false' ->
                    lager:info("create_db is false, not creating modb ~s ...", [AccountMODb]),
                    NotFound
            end;
        {'error', 'conflict'}=Conflict -> Conflict;
        {'error', 'timeout'} -> couch_save(AccountMODb, Doc, Options, 'timeout', Retry-1);
        Error -> Error
    end.

-spec save_fun(boolean()) -> function().
save_fun('false') -> fun kz_datamgr:save_doc/3;
save_fun('true') -> fun kz_datamgr:ensure_saved/3.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Move a document from source to destination with attachments,
%% optionally applies a transform function on the document
%% Note: Caller is responsible to format both source and destination
%% databases!
%% @end
%%--------------------------------------------------------------------
-spec move_doc(ne_binary(), kazoo_data:docid(), ne_binary(), kazoo_data:docid()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
move_doc(FromDb, FromId, ToDb, ToId) ->
    move_doc(FromDb, FromId, ToDb, ToId, []).

-spec move_doc(ne_binary(), kazoo_data:docid(), ne_binary(), kazoo_data:docid(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
move_doc(FromDb, FromId, ToDb, ToId, Options) ->
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    move_doc(FromDb, FromId, ToDb, ToId, Options, 'first_try', MaxRetries).

-spec move_doc(ne_binary(), kazoo_data:docid(), ne_binary(), kazoo_data:docid(), kz_proplist(), atom(), integer()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
move_doc(_FromDb, _FromId, _ToDb, _ToId, _Options, Reason, Retry) when Retry =< 0 ->
    lager:error("max retries to move doc from ~s/~p to ~s/~p : ~p"
               ,[_FromDb, _FromId, _ToDb, _ToId, Reason]
               ),
    {'error', Reason};
move_doc(FromDb, FromId, ToDb, ToId, Options, _Reason, Retry) ->
    case kz_datamgr:move_doc(FromDb, FromId, ToDb, ToId, strip_modb_options(Options)) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} ->
            case maybe_create_destination_db(FromDb, FromId, ToDb, Options) of
                'true' -> move_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1);
                'source_not_exists' -> {'error', 'not_found'};
                'too_old' -> {'error', 'not_found'};
                'false' ->
                    lager:info("modb ~s creation failed, maybe due to race condition, re-trying move_doc", [ToDb]),
                    move_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1)
            end;
        {'error', 'conflict'}=Conflict -> Conflict;
        {'error', 'timeout'} -> move_doc(FromDb, FromId, ToDb, ToId, Options, 'timeout', Retry-1);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Copy a document from source to destination with attachments,
%% optionally applies a transform function on the document
%% Note: Caller is responsible to format both source and destination
%% databases!
%% @end
%%--------------------------------------------------------------------
-spec copy_doc(ne_binary(), kazoo_data:docid(), ne_binary(), kazoo_data:docid()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
copy_doc(FromDb, FromId, ToDb, ToId) ->
    copy_doc(FromDb, FromId, ToDb, ToId, []).

-spec copy_doc(ne_binary(), kazoo_data:docid(), ne_binary(), kazoo_data:docid(), kz_proplist()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
copy_doc(FromDb, FromId, ToDb, ToId, Options) ->
    MaxRetries = props:get_integer_value('max_retries', Options, ?MAX_RETRIES),
    copy_doc(FromDb, FromId, ToDb, ToId, Options, 'first_try', MaxRetries).

-spec copy_doc(ne_binary(), kazoo_data:docid(), ne_binary(), kazoo_data:docid(), kz_proplist(), atom(), integer()) ->
                      {'ok', kz_json:object()} |
                      {'error', atom()}.
copy_doc(_FromDb, _FromId, _ToDb, _ToId, _Options, Reason, Retry) when Retry =< 0 ->
    lager:error("max retries to copy doc from ~s/~p to ~s/~p : ~p"
               ,[_FromDb, _FromId, _ToDb, _ToId, Reason]
               ),
    {'error', Reason};
copy_doc(FromDb, FromId, ToDb, ToId, Options, _Reason, Retry) ->
    case kz_datamgr:copy_doc(FromDb, FromId, ToDb, ToId, strip_modb_options(Options)) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} ->
            case maybe_create_destination_db(FromDb, FromId, ToDb, Options) of
                'true' -> copy_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1);
                'source_not_exists' -> {'error', 'not_found'};
                'too_old' -> {'error', 'not_found'};
                'false' ->
                    lager:info("modb ~s creation failed, maybe due to race condition, re-trying copy_doc", [ToDb]),
                    copy_doc(FromDb, FromId, ToDb, ToId, Options, 'not_found', Retry-1)
            end;
        {'error', 'conflict'}=Conflict -> Conflict;
        {'error', 'timeout'} -> copy_doc(FromDb, FromId, ToDb, ToId, Options, 'timeout', Retry-1);
        {'error', _}=Error -> Error
    end.

-spec maybe_create_destination_db(ne_binary(), kazoo_data:docid(), ne_binary(), kz_proplist()) ->
                                         'source_not_exists' |
                                         'too_old'|
                                         boolean().
maybe_create_destination_db(FromDb, FromId, ToDb, Options) ->
    ShouldCreate = props:get_is_true('create_db', Options, 'true'),
    lager:info("destination modb ~p not found, maybe creating...", [ToDb]),
    case ShouldCreate
        andalso kz_datamgr:db_exists(FromDb)
        andalso kz_datamgr:open_doc(FromDb, FromId)
    of
        'false' when ShouldCreate ->
            lager:info("source modb ~s does not exist, not creating destination modb ~s", [FromDb, ToDb]),
            'source_not_exists';
        'false' ->
            lager:info("create_db is false, not creating modb ~s ...", [ToDb]),
            'source_not_exists';
        {'ok', _} ->
            maybe_create_current_modb(ToDb, Options);
        {'error', _} ->
            lager:info("source document ~s/~p does not exist, not creating destination modb ~s", [FromDb, FromId, ToDb]),
            'source_not_exists'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_modb(ne_binary()) -> ne_binary().
-spec get_modb(ne_binary(), view_options() | gregorian_seconds() | kz_now()) ->
                      ne_binary().
-spec get_modb(ne_binary(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                      ne_binary().
get_modb(?MATCH_MODB_SUFFIX_RAW(_,_,_) = AccountMODb) ->
    AccountMODb;
get_modb(?MATCH_MODB_SUFFIX_ENCODED(_,_,_) = AccountMODb) ->
    kz_util:format_account_modb(AccountMODb, 'raw');
get_modb(?MATCH_MODB_SUFFIX_UNENCODED(_,_,_) = AccountMODb) ->
    kz_util:format_account_modb(AccountMODb, 'raw');
get_modb(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    {Year, Month, _} = erlang:date(),
    get_modb(AccountDb, Year, Month).

get_modb(Account, ViewOptions) when is_list(ViewOptions) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case {props:get_value('month', ViewOptions)
         ,props:get_value('year', ViewOptions)
         }
    of
        {'undefined', 'undefined'} -> get_modb(Account);
        {'undefined', _Year} -> get_modb(Account);
        {Month, 'undefined'} ->
            {Year, _, _} = erlang:date(),
            get_modb(AccountDb, Year, Month);
        {Month, Year} ->
            get_modb(AccountDb, Year, Month)
    end;
get_modb(Account, Timestamp) ->
    kz_util:format_account_mod_id(Account, Timestamp).

get_modb(?MATCH_MODB_SUFFIX_RAW(_,_,_) = AccountMODb, _Year, _Month) ->
    AccountMODb;
get_modb(?MATCH_MODB_SUFFIX_ENCODED(_,_,_) = AccountMODb, _Year, _Month) ->
    kz_util:format_account_modb(AccountMODb, 'raw');
get_modb(?MATCH_MODB_SUFFIX_UNENCODED(_,_,_) = AccountMODb, _Year, _Month) ->
    kz_util:format_account_modb(AccountMODb, 'raw');
get_modb(Account, Year, Month) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    kz_util:format_account_mod_id(AccountDb, Year, Month).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_create_current_modb(ne_binary(), kz_proplist()) -> 'too_old' | boolean().
maybe_create_current_modb(?MATCH_MODB_SUFFIX_RAW(_AccountId, Year, Month) = AccountMODb, Options) ->
    {Y, M, _} = erlang:date(),
    ShouldCreateOld = props:get_is_true('allow_old_modb_creation', Options, 'false'),
    case {kz_term:to_binary(Y), kz_date:pad_month(M)} of
        {Year, Month} ->
            maybe_create(AccountMODb);
        {_Year, _Month} when ShouldCreateOld ->
            maybe_create(AccountMODb);
        {_Year, _Month} ->
            lager:info("modb ~p is not for the current month, skip creating", [AccountMODb]),
            'too_old'
    end;
maybe_create_current_modb(?MATCH_MODB_SUFFIX_ENCODED(_, _, _) = AccountMODb, Options) ->
    maybe_create_current_modb(kz_util:format_account_modb(AccountMODb, 'raw'), Options);
maybe_create_current_modb(?MATCH_MODB_SUFFIX_UNENCODED(_, _, _) = AccountMODb, Options) ->
    maybe_create_current_modb(kz_util:format_account_modb(AccountMODb, 'raw'), Options).

-spec maybe_create(ne_binary()) -> boolean().
maybe_create(?MATCH_MODB_SUFFIX_RAW(AccountId, _, _) = AccountMODb) ->
    maybe_create(AccountMODb, is_account_deleted(AccountId));
maybe_create(?MATCH_MODB_SUFFIX_ENCODED(_, _, _) = AccountMODb) ->
    maybe_create(kz_util:format_account_modb(AccountMODb, 'raw'));
maybe_create(?MATCH_MODB_SUFFIX_UNENCODED(_, _, _) = AccountMODb) ->
    maybe_create(kz_util:format_account_modb(AccountMODb, 'raw')).

-spec maybe_create(ne_binary(), boolean()) -> boolean().
maybe_create(?MATCH_MODB_SUFFIX_RAW(_AccountId, _, _) = _AccountMODb, 'true') ->
    lager:info("account ~s is deleted, not creating modb ~s", [_AccountId, _AccountMODb]),
    'false';
maybe_create(AccountMODb, 'false') ->
    create(AccountMODb).

-spec create(ne_binary()) -> boolean().
create(AccountMODb) ->
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    IsDbExists = kz_datamgr:db_exists_all(EncodedMODb),
    do_create(AccountMODb, IsDbExists).

-spec do_create(ne_binary(), boolean()) -> boolean().
do_create(AccountMODb, 'true') ->
    lager:info("modb ~p is exists, just refreshing views...", [AccountMODb]),
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    refresh_views(EncodedMODb),
    run_routines(AccountMODb),
    'true';
do_create(AccountMODb, 'false') ->
    lager:debug("creating modb ~p", [AccountMODb]),
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    case kz_datamgr:db_create(EncodedMODb) of
        'true' ->
            refresh_views(EncodedMODb),
            run_routines(AccountMODb),
            'true';
        _ -> 'false'
    end.

-spec is_account_deleted(ne_binary()) -> boolean().
is_account_deleted(AccountId) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> kz_doc:is_soft_deleted(JObj);
        {'error', _} -> 'true'
    end.

-spec refresh_views(ne_binary()) -> 'ok'.
refresh_views(AccountMODb) ->
    lager:debug("refresh views on modb ~p", [AccountMODb]),
    EncodedMODb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    Views = get_modb_views(),
    _ = kapps_util:update_views(EncodedMODb, Views, 'true'),
    'ok'.

-spec get_modb_views() -> kz_proplist().
get_modb_views() ->
    case get('account_modb_views') of
        'undefined' ->
            Views = fetch_modb_views(),
            put('account_modb_views', Views),
            Views;
        Views -> Views
    end.

-spec fetch_modb_views() -> [{ne_binary(), kz_json:object()}].
fetch_modb_views() ->
    kapps_util:get_views_json(?MODULE, "views").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec run_routines(ne_binary()) -> 'ok'.
run_routines(AccountMODb) ->
    Routines = kapps_config:get_ne_binaries(?CONFIG_CAT, <<"routines">>, []),
    _ = [run_routine(AccountMODb, Routine) || Routine <- Routines],
    'ok'.

-spec run_routine(ne_binary(), ne_binary()) -> any().
run_routine(AccountMODb, Routine) ->
    Module = kz_term:to_atom(Routine),
    _ = Module:modb(AccountMODb).

-spec add_routine(ne_binary() | atom()) -> 'ok'.
add_routine(Module) ->
    Routine = kz_term:to_binary(Module),
    Routines = kapps_config:get(?CONFIG_CAT, <<"routines">>, []),
    case add_migrate_routines(Routines, Routine) of
        Routines -> 'ok';
        NewRoutines ->
            kapps_config:set_default(?CONFIG_CAT, <<"routines">>, NewRoutines),
            'ok'
    end.

-spec add_migrate_routines(ne_binaries(), ne_binary()) -> ne_binaries().
add_migrate_routines(Routines, Module) ->
    lists:usort([Module | migrate_routines(Routines, [])] ++ [<<"wht_util">>]).

-spec migrate_routines(ne_binaries(), ne_binaries()) -> ne_binaries().
migrate_routines([], Acc) -> Acc;
migrate_routines([<<"wh_", Rest/binary>> | Rs], Acc) ->
    migrate_routines(Rs, [<<"kz_", Rest/binary>> | Acc]);
migrate_routines([R | Rs], Acc) ->
    migrate_routines(Rs, [R | Acc]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_archive_modb(ne_binary()) -> 'ok'.
maybe_archive_modb(AccountMODb) ->
    {Year, Month, _} = erlang:date(),
    case should_archive(AccountMODb, Year, Month) of
        'true' ->
            lager:info("account modb ~s needs archiving", [AccountMODb]),
            'ok' = kz_datamgr:db_archive(AccountMODb),
            lager:info("account modb ~s archived, removing the db", [AccountMODb]),
            Rm = kz_datamgr:db_delete(AccountMODb),
            lager:info("account modb ~s deleted: ~p", [AccountMODb, Rm]);
        'false' ->
            lager:info("account modb ~s still current enough to keep", [AccountMODb])
    end.

-spec should_archive(ne_binary(), kz_year(), kz_month()) -> boolean().
should_archive(AccountMODb, Year, Month) ->
    case kazoo_modb_util:split_account_mod(AccountMODb) of
        {_AccountId, Year, Month} -> 'false';
        {_AccountId, ModbYear, ModbMonth} ->
            Months = (Year * 12) + Month,
            ModbMonths = (ModbYear * 12) + ModbMonth,
            (Months - ModbMonths) > kapps_config:get_integer(?CONFIG_CAT, <<"active_modbs">>, 6)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Delete an modb if it is no longer associated with its account.
%% (That is: orphaned).
%% AccountMODb must be 'encoded' otherwise kz_datamgr:db_delete/1 will fail.
%% AccountIds should be kapps_util:get_all_accounts('raw').
%% Returns whether AccountMODb has been deleted.
%% @end
%%--------------------------------------------------------------------
-spec maybe_delete(ne_binary(), [ne_binary()]) -> boolean().
maybe_delete(AccountMODb, AccountIds) ->
    AccountId = kz_util:format_account_id(AccountMODb, 'raw'),
    IsOrphaned = not lists:member(AccountId, AccountIds),
    delete_if_orphaned(AccountMODb, IsOrphaned).

-spec delete_if_orphaned(ne_binary(), boolean()) -> boolean().
delete_if_orphaned(_AccountMODb, 'false') -> 'false';
delete_if_orphaned(AccountMODb, 'true') ->
    Succeeded = kz_datamgr:db_delete(AccountMODb),
    lager:debug("cleanse orphaned modb ~p... ~p", [AccountMODb,Succeeded]),
    Succeeded.


%% @public
-spec get_range(ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                       ne_binaries().
-spec get_range(ne_binary(), ne_binary(), gregorian_seconds(), gregorian_seconds()) ->
                       ne_binaries().
get_range(AccountId, From, To) ->
    get_range(<<"any">>, AccountId, From, To).

get_range(Type, AccountId, From, To) ->
    {{FromYear, FromMonth, _}, _} = calendar:gregorian_seconds_to_datetime(From),
    {{ToYear,   ToMonth,   _}, _} = calendar:gregorian_seconds_to_datetime(To),
    [MODb
     || MODb <- get_year_month_sequence(AccountId
                                       ,{FromYear, FromMonth}
                                       ,{ToYear, ToMonth}
                                       ),
        kz_datamgr:db_exists(MODb, Type)
    ].

-type year_month_tuple() :: {kz_year(), kz_month()}.

%% @public
-spec get_year_month_sequence(ne_binary(), year_month_tuple(), year_month_tuple()) ->
                                     ne_binaries().
get_year_month_sequence(Account, From, To) ->
    get_year_month_sequence(Account, From, To, []).

%% @public
-spec get_year_month_sequence(ne_binary(), year_month_tuple(), year_month_tuple(), kz_proplist()) ->
                                     ne_binaries().
get_year_month_sequence(Account, Tuple, Tuple, Range) ->
    ToMODbId = fun ({Year,Month}, Acc) -> [get_modb(Account, Year, Month)|Acc] end,
    lists:foldl(ToMODbId, [], [Tuple|Range]);
get_year_month_sequence(Account, {FromYear,13}, To, Range) ->
    get_year_month_sequence(Account, {FromYear+1,1}, To, Range);
get_year_month_sequence(Account, {FromYear,FromMonth}=From, {ToYear,ToMonth}=To, Range) ->
    'true' = (FromYear * 12 + FromMonth) =< (ToYear * 12 + ToMonth),
    get_year_month_sequence(Account, {FromYear,FromMonth+1}, To, [From|Range]).
