%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_migrate_account).

-export([start_worker/2
        ]).

-export([manual_account_migrate/1, manual_vmbox_migrate/2
        ]).

-include("kz_voicemail.hrl").

-define(DEFAULT_VM_EXTENSION
       ,kapps_config:get_ne_binary(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"extension">>], <<"mp3">>)).

-define(MAX_BULK_INSERT
       ,kapps_config:get_integer(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_bulk_insert">>], kz_datamgr:max_bulk_insert())).

-define(LEGACY_MSG_LISTING, <<"vmboxes/legacy_msg_by_timestamp">>).

-define(TOTAL_MESSAGES, 'total_messages').
-define(TOTAL_SUCCEEDED, 'total_succeeded').
-define(TOTAL_FAILED, 'total_failed').
-define(FAILED, 'moved_failed').
-define(FAILED_MODB, 'no_modb').
-define(SUCCEEDED, 'succeeded').

-type migrate_stats() :: non_neg_integer() |
                         kz_term:ne_binary() |
                         kz_term:ne_binaries() |
                         {kz_term:ne_binary(), atom()} |
                         [{kz_term:ne_binary(), atom()}] |
                         'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc Start a migration cycle
%% @end
%%--------------------------------------------------------------------
-spec start_worker(next_account(), pid()) -> 'ok'.
start_worker({AccountId, FirstOfMonth, LastOfMonth}, Server) ->
    ViewOpts = props:filter_empty(
                 [{'limit', ?MAX_BULK_INSERT}
                 ,{'startkey', LastOfMonth}
                 ,{'endkey', FirstOfMonth}
                 ,'descending'
                 ]),
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?LEGACY_MSG_LISTING, ViewOpts) of
        {'ok', []} ->
            ?SUP_LOG_WARNING("  [~s] no legacy voicemail messages left", [log_account_id(AccountId)]),
            kvm_migrate_crawler:account_is_done(Server, AccountId, FirstOfMonth, LastOfMonth);
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults),
            migration_result(Server, AccountId, FirstOfMonth, LastOfMonth);
        {'error', 'not_found'} ->
            Views = kapps_maintenance:get_all_account_views(),
            _ = kapps_util:update_views(Db, Views, 'true'),
            start_worker({AccountId, FirstOfMonth, LastOfMonth}, Server);
        {'error', R} ->
            ?SUP_LOG_ERROR("  [~s] failed to fetch legacy voicemail messages: ~p", [log_account_id(AccountId), R]),
            kvm_migrate_crawler:account_maybe_failed(Server, AccountId, FirstOfMonth, LastOfMonth, R)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Manual migration for an Account or a list of account's mailboxes
%% @end
%%--------------------------------------------------------------------
-spec manual_account_migrate(kz_term:ne_binary()) -> 'ok'.
manual_account_migrate(AccountId) ->
    ?SUP_LOG_WARNING(":: beginning migrating voicemails for account ~s", [log_account_id(AccountId)]),
    manual_migrate_loop(AccountId, 1).

manual_migrate_loop(AccountId, LoopCount) ->
    ?SUP_LOG_WARNING("  [~s] start migration cycle #~b", [log_account_id(AccountId), LoopCount]),
    ViewOpts = [{'limit', ?MAX_BULK_INSERT}
               ,'descending'
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?LEGACY_MSG_LISTING, ViewOpts) of
        {'ok', []} ->
            ?SUP_LOG_WARNING("  [~s] no legacy voicemail messages left", [log_account_id(AccountId)]),
            print_summary(AccountId),
            ?SUP_LOG_WARNING(":: voicemail migration process for account ~s is done", [log_account_id(AccountId)]);
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults),
            timer:sleep(?TIME_BETWEEN_ACCOUNT_CRAWLS),
            case is_latest_modb(AccountId) of
                'true' ->
                    ?SUP_LOG_WARNING("  [~s] reached to the latest available modb", [log_account_id(AccountId)]),
                    ?SUP_LOG_WARNING(":: voicemail migration process for account ~s is done", [log_account_id(AccountId)]);
                'false' ->
                    manual_migrate_loop(AccountId, LoopCount + 1)
            end;
        {'error', 'not_found'} ->
            Views = kapps_maintenance:get_all_account_views(),
            _ = kapps_util:update_views(Db, Views, 'true'),
            manual_migrate_loop(AccountId, LoopCount);
        {'error', _R} ->
            ?SUP_LOG_ERROR("  [~s] failed to fetch legacy voicemail message: ~p", [log_account_id(AccountId), _R]),
            print_summary(AccountId),
            ?SUP_LOG_WARNING(":: voicemail migration process for account ~s is done", [log_account_id(AccountId)])
    end.

-spec manual_vmbox_migrate(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> 'ok'.
manual_vmbox_migrate(AccountId, ?NE_BINARY = BoxId) ->
    manual_vmbox_migrate(AccountId, [BoxId]);
manual_vmbox_migrate(AccountId, BoxIds) ->
    ?SUP_LOG_WARNING(":: beginning migrating voicemails for ~b mailbox(es) in account ~s", [length(BoxIds), AccountId]),
    case get_messages_from_vmboxes(AccountId, BoxIds) of
        {'ok', []} ->
            ?SUP_LOG_WARNING("  [~s] no legacy voicemail messages left", [log_account_id(AccountId)]);
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults);
        {'error', _R} ->
            ?SUP_LOG_ERROR("  [~s] failed to fetch legacy voicemail message: ~p", [log_account_id(AccountId), _R])
    end,
    print_summary(AccountId),
    ?SUP_LOG_WARNING(":: voicemail migration process for account ~s is done", [log_account_id(AccountId)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Process messages and do migrate
%% @end
%%--------------------------------------------------------------------
-spec migrate_messages(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
migrate_messages(AccountId, ViewResults) ->
    MsgCount = length(ViewResults),
    _ = update_process_key(?TOTAL_MESSAGES, MsgCount),

    ?SUP_LOG_WARNING("  [~s] processing ~b voicemail messages", [log_account_id(AccountId), MsgCount]),
    MsgsMap = process_messages(AccountId, ViewResults),
    maybe_migrate(AccountId, ViewResults, MsgsMap, maps:keys(MsgsMap)).

%%--------------------------------------------------------------------
%% @private
%% @doc Check Db existence and process with migration
%% @end
%%--------------------------------------------------------------------
-spec maybe_migrate(kz_term:ne_binary(), kz_json:objects(), map(), kz_term:ne_binaries() | non_neg_integer()) -> 'ok'.
maybe_migrate(AccountId, ViewResults, MsgsMap, Dbs) when is_list(Dbs) ->
    NewMsgsMap = check_dbs_existence(Dbs, MsgsMap),
    maybe_migrate(AccountId, ViewResults, NewMsgsMap, maps:size(NewMsgsMap));
maybe_migrate(_AccountId, _ViewResults, _MsgsMap, 0) ->
    lager:debug("  [~s] none of modbs for migrating messages in this cycle are exists", [log_account_id(_AccountId)]);
maybe_migrate(AccountId, ViewResults, MsgsMap, _DbCount) ->
    maps:fold(fun bulk_save_modb/3, [], MsgsMap),
    update_mailboxes(AccountId, ViewResults).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bulk_save_modb(kz_term:ne_binary(), kz_json:objects(), list()) -> 'ok'.
bulk_save_modb(Db, Js, _Acc) ->
    case kz_datamgr:save_docs(Db, Js) of
        {'ok', Saved} ->
            _ = normalize_bulk_result(Db, Saved, #{<<"succeeded">> => [], <<"failed">> => []}),
            'ok';
        {'error', R} ->
            update_stats(?FAILED, Js, R),
            ?SUP_LOG_ERROR("    [~s] failed to migrate voicemail messages to db ~s: ~p"
                          ,[kz_util:format_account_id(Db), Db, R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_mailboxes(kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
update_mailboxes(AccountId, ViewResults) ->
    BoxIds = lists:usort([kz_doc:id(B) || B <- ViewResults]),

    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:open_cache_docs(Db, BoxIds) of
        {'ok', BoxJObjs} ->
            NewBoxJObjs = update_mailbox_jobjs(BoxJObjs),
            case kz_datamgr:save_docs(Db, NewBoxJObjs) of
                {'ok', _} -> 'ok';
                {'error', R} ->
                    ?SUP_LOG_ERROR("  [~s] failed to save new message array into mailboxes", [log_account_id(AccountId)]),
                    failed_to_update_mailbox(ViewResults, R)
            end;
        {'error', R} ->
            ?SUP_LOG_ERROR("  [~s] failed to open mailboxes for update", [log_account_id(AccountId)]),
            failed_to_update_mailbox(ViewResults, R)
    end.

failed_to_update_mailbox(ViewResults, Reason) ->
    %% nuke process stats to start process this account later
    _ = erlang:erase(?SUCCEEDED),
    _ = erlang:erase('no_timestamp'),
    _ = update_process_key(?TOTAL_SUCCEEDED, 0),
    Failed = [{kz_json:get_value([<<"value">>, <<"metadata">>, <<"media_id">>], M), Reason}
              || M <- ViewResults
             ],
    _ = erlang:put(?FAILED, Failed),
    _ = erlang:put(?TOTAL_FAILED, length(Failed)),
    'ok'.

-spec update_mailbox_jobjs(kz_json:objects()) -> kz_json:objects().
update_mailbox_jobjs(BoxJObjs) ->
    MODbFailed = maps:from_list(get_stats(?FAILED_MODB)),
    Failed = maps:from_list(get_stats(?FAILED)),
    NoTimestamp = maps:from_list(get_stats('no_timestamp')),
    [update_message_array(kz_json:get_value(<<"doc">>, J), MODbFailed, Failed, NoTimestamp)
     || J <- BoxJObjs
    ].

-spec update_message_array(kz_json:object(), map(), map(), map()) -> kz_json:object().
update_message_array(BoxJObj, MODbFailed, Failed, NoTimestamp) ->
    %% check if messages are failed or not, if not remove them from message array
    Fun = fun(Msg, Acc) ->
                  Timestamp = kz_json:get_integer_value(<<"timestamp">>, Msg),
                  Id = kz_json:get_ne_binary_value(<<"media_id">>, Msg),
                  case update_vmbox_message(Msg, MODbFailed, Failed, NoTimestamp, Id, Timestamp) of
                      'undefined' -> Acc;
                      M -> [M|Acc]
                  end
          end,
    NewMessages = lists:foldl(Fun, [], kz_json:get_value(<<"messages">>, BoxJObj, [])),
    kz_json:set_value(?VM_KEY_MESSAGES, NewMessages, BoxJObj).

-spec update_vmbox_message(kz_json:object(), map(), map(), map(), kz_term:binary(), kz_time:api_seconds()) -> kz_term:api_object().
update_vmbox_message(Message, _, _, _, 'undefined', _) ->
    %% no media_id = no migration
    kz_json:set_value(<<"migration_error">>, <<"no_media_id">>, Message);
update_vmbox_message(Message, MODbFailed, Failed, NoTimestamp, Id, 'undefined') ->
    %% if we don't remove the message here migration would stuck in loop.
    %% messages without timestamp should be migrated anyway
    %% either by their private media modified/created time or kz_time:now_s()
    case maps:get(Id, NoTimestamp, 'undefined') of
        'undefined' ->
            kz_json:set_value(<<"migration_error">>, <<"no_timestamp">>, Message);
        Timestamp ->
            update_vmbox_message(Message, MODbFailed, Failed, NoTimestamp, Id, Timestamp)
    end;
update_vmbox_message(Message, MODbFailed, Failed, _, Id, Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    MsgId = kazoo_modb_util:modb_id(Year, Month, Id),

    case {maps:get(MsgId, MODbFailed, 'undefined'), maps:get(MsgId, Failed, 'undefined')} of
        {'undefined', 'undefined'} ->
            'undefined';
        {MODBError, _} when MODBError =/= 'undefined' ->
            kz_json:set_value(<<"migration_error">>, kz_term:to_binary(MODBError), Message);
        {_, Error} ->
            kz_json:set_value(<<"migration_error">>, kz_term:to_binary(Error), Message)
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc Get messages from mailbox arrays and generate lagecy_msg listing view
%% fake message_doc result for manual migration
%% @end
%%--------------------------------------------------------------------
-spec get_messages_from_vmboxes(kz_term:ne_binary(), kz_term:ne_binaries()) -> db_ret().
get_messages_from_vmboxes(AccountId, ExpectedBoxIds) ->
    case kz_datamgr:open_cache_docs(kz_util:format_account_db(AccountId), ExpectedBoxIds) of
        {'ok', JObjs} -> {'ok', normalize_mailbox_results(JObjs)};
        {'error', _E} = Error ->
            ?SUP_LOG_ERROR("  [~s] failed to open mailbox(es)", [log_account_id(AccountId)]),
            Error
    end.

-spec normalize_mailbox_results(kz_json:objects()) -> kz_json:objects().
normalize_mailbox_results(JObjs) ->
    [create_legacy_view_result(BoxJObj, Message)
     || JObj <- JObjs,
        BoxJObj <- [kz_json:get_value(<<"doc">>, JObj)],
        Message <- kz_json:get_list_value(?VM_KEY_MESSAGES, BoxJObj, [])
    ].

-spec create_legacy_view_result(kz_json:object(), kz_json:object()) -> kz_json:object().
create_legacy_view_result(BoxJObj, Message) ->
    BoxId = kz_doc:id(BoxJObj),
    OwnerId = kz_json:get_value(<<"owner_id">>, BoxJObj),
    Mailbox = kz_json:get_value(<<"mailbox">>, BoxJObj),
    Timezone = kz_json:get_value(<<"timezone">>, BoxJObj),

    BoxTimestamp = kz_doc:modified(BoxJObj, kz_doc:created(BoxJObj, kz_time:now_s())),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, Message),
    Value = kz_json:from_list(
              [{<<"source_id">>, BoxId}
              ,{<<"owner_id">>, OwnerId}
              ,{<<"timezone">>, Timezone}
              ,{<<"box_timestamp">>, BoxTimestamp}
              ,{<<"mailbox">>, Mailbox}
              ,{<<"metadata">>, kz_json:set_value(<<"timestamp">>, Timestamp, Message)}
              ]
             ),
    kz_json:from_list(
      [{<<"id">>, BoxId}
      ,{<<"key">>, Timestamp}
      ,{<<"value">>, Value}
      ,{<<"_id">>, BoxId}
      ]
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc Check Db existence and remove messages that non exists dbs
%% @end
%%--------------------------------------------------------------------
-spec check_dbs_existence(kz_term:ne_binaries(), map()) -> map().
check_dbs_existence([], MsgsMap) -> MsgsMap;
check_dbs_existence([Db | Dbs], MsgsMap) ->
    case kz_datamgr:db_exists(Db) of
        'true' ->
            check_dbs_existence(Dbs, MsgsMap);
        'false' ->
            lager:warning("modb ~s is not exists", [Db]),
            update_stats(?FAILED_MODB, maps:get(Db, MsgsMap), <<"modb_not_exists">>),
            check_dbs_existence(Dbs, maps:remove(Db, MsgsMap))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize bulk save results and update stats accordingly
%% @end
%%--------------------------------------------------------------------
-spec normalize_bulk_result(kz_term:ne_binary(), kz_json:objects(), map()) ->
                                   {non_neg_integer(), non_neg_integer()}.
normalize_bulk_result(_Db, [], #{<<"succeeded">> := Succeeded, <<"failed">> := Failed}) ->
    update_stats(?FAILED, Failed),
    update_stats(?SUCCEEDED, Succeeded),
    {length(Succeeded), length(Failed)};
normalize_bulk_result(Db, [S | Saved], #{<<"succeeded">> := Succeeded, <<"failed">> := Failed}=Map) ->
    Id = kz_json:get_first_defined([<<"key">>, <<"id">>, <<"_id">>], S),
    case kz_json:get_value(<<"error">>, S) of
        'undefined' ->
            %% successful
            normalize_bulk_result(Db, Saved, Map#{<<"succeeded">> => [Id|Succeeded]});
        <<"conflict">> ->
            %% successful
            normalize_bulk_result(Db, Saved, Map#{<<"succeeded">> => [Id|Succeeded]});
        Reason ->
            lager:debug("  [~s] failed to save voice mail message ~s in db ~s: ~p", [log_account_id(Db), Id, Db, Reason]),
            normalize_bulk_result(Db, Saved, Map#{<<"failed">> => [{Id, Reason}|Failed]})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc process legacy message view result, generate a new message doc
%% for them and map them to proper modb.
%%
%% Note: We are moving metadata only, attachment still remains in AccountDb,
%% This is so much faster than moving with attachments which probably
%% takes a couple of days for huge systems. We are creating message docs
%% from scratch based on message metadata from each mailbox, and we use
%% kazoo_data bulk operation for faster db writes.
%% @end
%%--------------------------------------------------------------------
-spec process_messages(kz_term:ne_binary(), kz_json:objects()) -> map().
process_messages(AccountId, JObjs) ->
    DefaultExt = ?DEFAULT_VM_EXTENSION,
    {Map, NeedTimestamp} = lists:foldl(fun(J, Acc) -> check_create_and_map(AccountId, J, DefaultExt, Acc) end, {#{}, []}, JObjs),
    TimestampUpdated = maybe_get_timestamp(AccountId, NeedTimestamp),
    lists:foldl(fun(J, Acc) -> create_and_map(AccountId, J, DefaultExt, Acc) end, Map, TimestampUpdated).

-spec check_create_and_map(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), {map(), kz_json:objects()}) -> {map(), kz_json:objects()}.
check_create_and_map(AccountId, JObj, DefaultExt, {Map, NoTimestamp}) ->
    Value = kz_json:get_value(<<"value">>, JObj),

    case kz_json:get_ne_binary_value([<<"metadata">>, <<"media_id">>], Value) =/= 'undefined'
        andalso kz_json:get_integer_value([<<"metadata">>, <<"timestamp">>], Value)
    of
        'false' -> {Map, NoTimestamp};
        'undefined' -> {Map, [Value|NoTimestamp]};
        _ -> {create_and_map(AccountId, Value, DefaultExt, Map), NoTimestamp}
    end.

-spec create_and_map(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), map()) -> map().
create_and_map(AccountId, JObj, DefaultExt, Map) ->
    Doc = create_message(AccountId, JObj, DefaultExt),
    maps:update_with(kz_doc:account_db(Doc), fun(List) -> [Doc|List] end, [Doc], Map).

-spec create_message(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
create_message(AccountId, FakeVMBoxMsgJObj, DefaultExt) ->
    AccountDb = kvm_util:get_db(AccountId),
    BoxJObj0 = kz_doc:set_account_id(FakeVMBoxMsgJObj, AccountId),
    BoxJObj = kz_doc:set_account_db(BoxJObj0, AccountDb),

    BoxNum = kzd_voicemail_box:mailbox_number(BoxJObj),
    TimeZone = kzd_voicemail_box:timezone(BoxJObj),

    Metadata = kzd_box_message:metadata(BoxJObj),
    Timestamp = kz_json:get_integer_value(<<"timestamp">>, Metadata),

    %% setting a db_link as attachment
    AttName = <<(kz_binary:rand_hex(16))/binary, ".", DefaultExt/binary>>,
    AttHandlerProps = [{<<"att_dbname">>, kz_util:format_account_db(AccountId)}
                      ,{<<"att_docid">>, kzd_box_message:media_id(Metadata)}
                      ],
    AttHandler = kz_json:from_list([{<<"kz_att_link">>, kz_json:from_list(AttHandlerProps)}]),
    AttProps = [{<<"content_type">>, kz_mime:from_extension(DefaultExt)}
               ,{<<"length">>, 0}
               ,{<<"stub">>, 'false'}
               ,{<<"handler">>, AttHandler}
               ],
    Att = kz_json:from_list([{AttName, kz_json:from_list(AttProps)}]),

    Props = props:filter_undefined(
              [{<<"Box-Id">>, kz_json:get_value(<<"source_id">>, BoxJObj)}
              ,{<<"Media-ID">>, kzd_box_message:media_id(Metadata)}
              ,{<<"Box-Num">>, BoxNum}
              ,{<<"Timezone">>, TimeZone}
              ,{<<"Message-Timestamp">>, Timestamp}
               %% @lazedo comment:
               %%   I'm debating if we should move all existing vm messages to current modb
               %%   that would be great for bulk create of modb docs
               %%   since they would all go into same db
               %%   if that is the option, then change Timestamp below
               %%   with kz_time:now_s()
               %%
               %% CAUTION: setting this to now_s breaks crossbar qs_filter (created_from, created_to)
              ,{<<"Document-Timestamp">>, Timestamp}
              ,{<<"Attachment-Name">>, AttName}
              ]),

    Msg = kzd_box_message:new(AccountId, Props),
    UpdateProps =
        [{<<"metadata">>, kzd_box_message:set_media_id(kz_doc:id(Msg), Metadata)}
        ,{<<"pvt_moved_to_modb">>, <<"true">>}
        ,{<<"pvt_previous_id">>, kzd_box_message:media_id(Metadata)}
        ,{<<"pvt_attachments">>, Att}
        ],
    kz_json:set_values(UpdateProps, Msg).

%% @doc try to get private_media to read its create/modified timestamp to set as
%% voice mail message timestamp.
%% @end
-spec maybe_get_timestamp(kz_term:ne_binary(), kz_json:objects()) -> kz_json:objects().
maybe_get_timestamp(_, []) -> [];
maybe_get_timestamp(AccountId, JObjs) when is_binary(AccountId) ->
    WithIds = maps:from_list([{Id, J}
                              || J <- JObjs,
                                 Id <- [kz_json:get_ne_binary_value([<<"metadata">>, <<"media_id">>], J)],
                                 Id =/= 'undefined'
                             ]
                            ),
    case kz_datamgr:open_docs(kvm_util:get_db(AccountId), maps:keys(WithIds)) of
        {'ok', PrivateMedias} ->
            get_timestamp(WithIds, PrivateMedias);
        {'error', _Reason} ->
            ?SUP_LOG_ERROR("  [~s] failed to open  private media to find timestamp, setting current time as timestamp: ~p", [log_account_id(AccountId), _Reason]),
            Fun = fun(_, JObj) ->
                          kz_json:set_value([<<"metadata">>, <<"timestamp">>]
                                           ,kz_json:get_integer_value(<<"box_timestamp">>, JObj, kz_time:now_s())
                                           ,JObj
                                           )
                  end,
            maps:to_list(maps:map(Fun, WithIds))
    end.

-spec get_timestamp(map(), kz_json:objects()) -> kz_json:objects().
get_timestamp(WithIds, PrivateMedias) ->
    Fun = fun(PrivateMedia, {TimeMap, Messages}) ->
                  Id = kz_doc:id(PrivateMedia),
                  Doc = kz_json:get_value(<<"doc">>, PrivateMedia, kz_json:new()),
                  Message = maps:get(Id, WithIds),

                  BoxTimestamp = kz_json:get_integer_value(<<"box_timestamp">>, Message, kz_time:now_s()),
                  Timestamp = kz_doc:modified(Doc, kz_doc:created(Doc, BoxTimestamp)),

                  {[{Id, Timestamp}|TimeMap], [kz_json:set_value([<<"metadata">>, <<"timestamp">>], Timestamp, Message)|Messages]}
          end,
    {NoTimeMap, Msgs} = lists:foldl(Fun, {[], []}, PrivateMedias),
    update_stats('no_timestamp', NoTimeMap),
    Msgs.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_stats(atom()) -> migrate_stats().
get_stats(Key) ->
    case erlang:get(Key) of
        'undefined' ->
            return_default_value(Key);
        Value -> Value
    end.

-spec return_default_value(atom()) -> migrate_stats().
return_default_value(?TOTAL_MESSAGES) -> 0;
return_default_value(?TOTAL_FAILED) -> 0;
return_default_value(?TOTAL_SUCCEEDED) -> 0;
return_default_value(_) -> [].

-spec update_stats(atom(), migrate_stats()) -> 'ok'.
-spec update_stats(atom(), kz_json:objects(), any()) -> 'ok'.
update_stats(Key, Values) ->
    _ = update_process_total_key(Key, length(Values)),
    _ = update_process_key(Key, Values),
    'ok'.

update_stats(Key, Msgs, Reason) ->
    NewStats = [{kz_doc:id(M), Reason}
                || M <- Msgs
               ],
    _ = update_process_total_key(Key, length(NewStats)),
    _ = update_process_key(Key, NewStats),
    'ok'.

-spec update_process_key(atom(), migrate_stats()) -> migrate_stats().
update_process_key(Key, Value) ->
    case erlang:get(Key) of
        'undefined' ->
            erlang:put(Key, Value);
        OldVal ->
            case is_list(Value) of
                'true' ->
                    erlang:put(Key, lists:flatten(OldVal, Value));
                'false' when is_integer(OldVal) ->
                    erlang:put(Key, OldVal + Value);
                'false' when is_binary(OldVal) ->
                    erlang:put(Key, [Value, OldVal]);
                'false' ->
                    erlang:put(Key, [Value | OldVal])
            end
    end.

-spec update_process_total_key(atom(), non_neg_integer()) -> migrate_stats().
update_process_total_key(?SUCCEEDED, Count) ->
    update_process_key(?TOTAL_SUCCEEDED, Count);
update_process_total_key(?FAILED, Count) ->
    update_process_key(?TOTAL_FAILED, Count);
update_process_total_key(?FAILED_MODB, Count) ->
    update_process_key(?TOTAL_FAILED, Count);
update_process_total_key(_, _) ->
    'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec migration_result(pid(), kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) -> 'ok'.
migration_result(Server, AccountId, FirstOfMonth, LastOfMonth) ->
    TotalMsgs = get_stats(?TOTAL_MESSAGES),
    TotalSucceeded = get_stats(?TOTAL_SUCCEEDED),
    TotalFailed = get_stats(?TOTAL_FAILED),
    MODbFailed = length(get_stats(?FAILED_MODB)),

    Props = [{<<"total_processed">>, TotalMsgs}
            ,{<<"total_succeeded">>, TotalSucceeded}
            ,{<<"total_failed">>, TotalFailed}
            ],
    case TotalMsgs == MODbFailed of
        'true' ->
            kvm_migrate_crawler:update_stats(Server, AccountId, Props),
            kvm_migrate_crawler:account_is_done(Server, AccountId, FirstOfMonth, LastOfMonth),
            ?SUP_LOG_WARNING("  [~s] reached to the latest available modb", [log_account_id(AccountId)]);
        'false' ->
            kvm_migrate_crawler:update_stats(Server, AccountId, Props),
            ?SUP_LOG_WARNING("  [~s] finished a migrate cycle: [processed: ~b] [succeeded: ~b] [save_failed: ~b] [no_modb: ~b]"
                            ,[log_account_id(AccountId), TotalMsgs, TotalSucceeded, TotalFailed, MODbFailed]
                            )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_latest_modb(kz_term:ne_binary()) -> boolean().
is_latest_modb(AccountId) ->
    print_summary(AccountId, 'true').

-spec print_summary(kz_term:ne_binary()) -> 'ok'.
-spec print_summary(kz_term:ne_binary(), boolean()) -> 'ok' | boolean().
print_summary(AccountId) ->
    print_summary(AccountId, 'false').

print_summary(_AccountId, ShouldCheckMODB) ->
    TotalMsgs = get_stats(?TOTAL_MESSAGES),
    MODbFailed = length(get_stats(?FAILED_MODB)),
    _TotalSucceeded = get_stats(?TOTAL_SUCCEEDED),
    _TotalFailed = get_stats(?TOTAL_FAILED),

    ?SUP_LOG_WARNING("  [~s] finished a migrate cycle: [processed: ~b] [succeeded: ~b] [save_failed: ~b] [no_modb: ~b]"
                    ,[log_account_id(_AccountId), TotalMsgs, _TotalSucceeded, _TotalFailed, MODbFailed]
                    ),

    case ShouldCheckMODB of
        'false' -> 'ok';
        'true' ->
            TotalMsgs == MODbFailed
    end.

-spec log_account_id(kz_term:ne_binary()) -> kz_term:ne_binary().
log_account_id(Account) ->
    kz_util:format_account_id(Account).
