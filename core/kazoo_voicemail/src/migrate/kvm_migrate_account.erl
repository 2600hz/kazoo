%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kvm_migrate_account).

-export([start_worker/2
        ]).

-export([manual_account_migrate/1, manual_vmbox_migrate/2
        ]).

-include("kz_voicemail.hrl").

-define(DEFAULT_VM_EXTENSION
       ,kapps_config:get_ne_binary(?VM_CONFIG_CAT, [?KEY_VOICEMAIL, <<"extension">>], <<"mp3">>)
       ).

-define(MAX_BULK_INSERT
       ,kapps_config:get_integer(?VM_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_bulk_insert">>], kz_datamgr:max_bulk_insert())
       ).

-define(LEGACY_MSG_LISTING, <<"vmboxes/legacy_msg_by_timestamp">>).

-record(total_stats, {total_processed = 0 :: non_neg_integer()
                     ,total_succeeded = 0 :: non_neg_integer()
                     ,total_no_modb = 0 :: non_neg_integer()
                     ,total_no_ids = 0 :: non_neg_integer()
                     ,total_failed = 0 :: non_neg_integer()
                     }).

-record(ctx, {mode :: kz_term:ne_binary()
             ,account_id :: kz_term:ne_binary()
             ,account_db :: kz_term:ne_binary()
             ,startkey = 'undefined' :: kz_term:api_seconds()
             ,endkey = 'undefined' :: kz_term:api_seconds()
             ,server = 'undefined' :: kz_term:api_pid()
             ,manual_vmboxes = 'undefined' :: kz_term:api_ne_binaries()
             ,last_error = 'undefined' :: kz_term:api_ne_binary()
             ,retries = 0 :: non_neg_integer()
             ,defualt_extension = ?DEFAULT_VM_EXTENSION :: kz_term:ne_binary()
             ,total_msgs = 0 :: non_neg_integer()
             ,no_ids = 0 :: non_neg_integer()
             ,succeeded = [] :: kz_term:ne_binaries()
             ,no_modb = [] :: kz_term:ne_binaries()
             ,no_timestamp = [] :: kz_term:proplist()
             ,failed = [] :: kz_term:proplist()
             ,total_stats = #total_stats{} :: #total_stats{}
             }).
-type ctx() :: #ctx{}.

%%------------------------------------------------------------------------------
%% @doc Start worker for a migration cycle
%% @end
%%------------------------------------------------------------------------------
-spec start_worker(next_account(), pid()) -> 'ok'.
start_worker({AccountId, FirstOfMonth, LastOfMonth}, Server) ->
    migration_loop(#ctx{mode = <<"worker">>
                       ,account_id = kz_util:format_account_id(AccountId)
                       ,account_db = kz_util:format_account_db(AccountId)
                       ,startkey = FirstOfMonth
                       ,endkey = LastOfMonth
                       ,server = Server
                       }
                  ).

%%------------------------------------------------------------------------------
%% @doc Start a manual migration for all mailboxes in an Account
%% @end
%%------------------------------------------------------------------------------
-spec manual_account_migrate(kz_term:ne_binary()) -> kz_term:proplist().
manual_account_migrate(Account) ->
    AccountId = kz_util:format_account_id(Account),
    ?SUP_LOG_INFO(":: beginning migrating voicemails for account ~s", [AccountId]),
    migration_loop(#ctx{mode = <<"account">>
                       ,account_id = AccountId
                       ,account_db = kz_util:format_account_db(AccountId)
                       }
                  ).

%%------------------------------------------------------------------------------
%% @doc Start a manual migration for specified mailboxes in an Account
%% @end
%%------------------------------------------------------------------------------
-spec manual_vmbox_migrate(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> kz_term:proplist().
manual_vmbox_migrate(Account, ?NE_BINARY = BoxId) ->
    manual_vmbox_migrate(Account, [BoxId]);
manual_vmbox_migrate(Account, BoxIds) ->
    AccountId = kz_util:format_account_id(Account),
    ?SUP_LOG_INFO(":: beginning migrating voicemails for ~b mailbox(es) in account ~s", [length(BoxIds), AccountId]),
    migration_loop(#ctx{mode = <<"vmboxes">>
                       ,account_id = AccountId
                       ,account_db = kz_util:format_account_db(AccountId)
                       ,manual_vmboxes = BoxIds
                       }
                  ).

%% @doc Main migration loop
%% @end
-spec migration_loop(ctx()) -> 'ok' | kz_term:proplist().
migration_loop(#ctx{account_id = _AccountId
                   ,retries = Retries
                   ,last_error = LastError
                   }=Ctx
              ) when Retries > 3 ->
    Reason = case LastError of
                 'undefined' -> <<"maximum retries">>;
                 _ -> <<"maximum retries, last error was ", LastError/binary>>
             end,
    ?SUP_LOG_WARNING("  [~s] retries exhausted: last error: ~s", [_AccountId, Reason]),
    account_is_failed(Ctx, Reason);
migration_loop(Ctx) ->
    handle_result(Ctx, get_messages(Ctx)).

%% @doc Get legacy messages from DB
%% @end
-spec get_messages(ctx()) -> db_ret().
get_messages(#ctx{mode = <<"worker">>
                 ,account_db = AccountDb
                 ,startkey = StartKey
                 ,endkey = EndKey
                 }) ->
    ViewOpts = props:filter_empty(
                 [{'limit', ?MAX_BULK_INSERT}
                 ,{'startkey', EndKey}
                 ,{'endkey', StartKey}
                 ,'descending'
                 ]),
    kz_datamgr:get_results(AccountDb, ?LEGACY_MSG_LISTING, ViewOpts);
get_messages(#ctx{mode = <<"account">>, account_db = AccountDb}) ->
    ViewOpts = [{'limit', ?MAX_BULK_INSERT}
               ,'descending'
               ],
    kz_datamgr:get_results(AccountDb, ?LEGACY_MSG_LISTING, ViewOpts);
get_messages(#ctx{mode = <<"vmboxes">>, account_db = AccountDb, manual_vmboxes = BoxIds}) ->
    get_messages_from_vmboxes(AccountDb, BoxIds).

%% @doc Do action on get_messages result. If we have something to process, reset context first.
%% @end
-spec handle_result(ctx(), db_ret()) -> 'ok' | kz_term:proplist().
handle_result(#ctx{account_id = _AccountId}=Ctx, {'ok', []}) ->
    ?SUP_LOG_INFO("  [~s] no legacy voicemail messages left", [_AccountId]),
    account_is_done(Ctx);
handle_result(#ctx{account_id = _AccountId}=Ctx, {'ok', ViewResults}) ->
    MsgCount = length(ViewResults),

    ?SUP_LOG_INFO("  [~s] processing ~b voicemail messages", [_AccountId, MsgCount]),

    NewCtx = reset_context_msgs(Ctx#ctx{total_msgs = MsgCount
                                       ,last_error = 'undefined'
                                       }
                               ),
    maybe_next_cycle(migrate_messages(NewCtx, ViewResults));
handle_result(#ctx{account_id = _AccountId, account_db = AccountDb, retries = Retries}=Ctx, {'error', 'not_found'}) when Retries > 3 ->
    ?SUP_LOG_WARNING("  [~s] refreshing view", [_AccountId]),
    _ = kapps_maintenance:refresh(AccountDb),

    timer:sleep(?TIME_BETWEEN_ACCOUNT_CRAWLS),
    migration_loop(Ctx#ctx{retries = Retries + 1
                          ,last_error = <<"missing view">>
                          }
                  );
handle_result(#ctx{account_id = _AccountId, retries = Retries}=Ctx, {'error', Reason}) ->
    ?SUP_LOG_ERROR("  [~s] failed to fetch legacy voicemail messages: ~p", [_AccountId, Reason]),
    timer:sleep(?TIME_BETWEEN_ACCOUNT_CRAWLS),
    migration_loop(Ctx#ctx{retries = Retries + 1
                          ,last_error = kz_term:to_binary(io_lib:format("get legacy messages failed: ~p", [Reason]))
                          }
                  ).

%% @doc Check first to see we're hit the oldest MODB, if not and we're in manual account mode
%% go to next loop, otherwise report summary and go down.
%% @end
-spec maybe_next_cycle(ctx()) -> 'ok' | kz_term:proplist().
maybe_next_cycle(#ctx{last_error = LastError, retries = Retries}=Ctx) when is_binary(LastError) ->
    timer:sleep(?TIME_BETWEEN_ACCOUNT_CRAWLS),
    migration_loop(Ctx#ctx{retries = Retries + 1});
maybe_next_cycle(#ctx{account_id = _AccountId
                     ,total_msgs = TotalMsgs, no_modb = NoModb
                     }=Ctx) when TotalMsgs == length(NoModb) ->
    ?SUP_LOG_INFO("  [~s] reached to the last/oldest available modb", [_AccountId]),
    hit_last_modb(Ctx);
maybe_next_cycle(#ctx{mode = <<"worker">>, account_id = AccountId
                     ,server = Server, total_stats = TotalStats}) ->
    kvm_migrate_crawler:update_stats(Server, AccountId, total_stats_to_prop(TotalStats));
maybe_next_cycle(#ctx{mode = <<"account">>}=Ctx) ->
    timer:sleep(?TIME_BETWEEN_ACCOUNT_CRAWLS),
    migration_loop(Ctx);
maybe_next_cycle(#ctx{mode = <<"vmboxes">>}=Ctx) ->
    account_is_done(Ctx).

%% @doc Last cycle was hit MODB
%% @end
-spec hit_last_modb(ctx()) -> 'ok' | kz_term:proplist().
hit_last_modb(#ctx{mode = <<"worker">>, account_id = AccountId
                  ,total_stats = TotalStats, server = Server
                  ,startkey = StartKey, endkey = EndKey
                  }) ->
    kvm_migrate_crawler:update_stats(Server, AccountId, total_stats_to_prop(TotalStats)),
    kvm_migrate_crawler:account_is_done(Server, AccountId, StartKey, EndKey);
hit_last_modb(Ctx) ->
    account_is_done(Ctx).

%% @doc Migration finished, going down
%% @end
-spec account_is_done(ctx()) -> 'ok' | kz_term:proplist().
account_is_done(#ctx{mode = <<"worker">>, account_id = AccountId
                    ,startkey = StartKey, endkey = EndKey
                    ,server = Server}) ->
    kvm_migrate_crawler:account_is_done(Server, AccountId, StartKey, EndKey);
account_is_done(#ctx{account_id = _AccountId, total_stats = TotalStats}) ->
    ?SUP_LOG_INFO(":: voicemail migration process for account ~s has been finished", [_AccountId]),
    total_stats_to_prop(TotalStats).

%% @doc If this manual migration, check tries and maybe retry again.
%% @end
-spec account_is_failed(ctx(), kz_term:ne_binary()) -> 'ok' | kz_term:proplist().
account_is_failed(#ctx{mode = <<"worker">>, account_id = AccountId
                      ,startkey = StartKey, endkey = EndKey, server = Server
                      }, Reason) ->
    kvm_migrate_crawler:account_maybe_failed(Server, AccountId, StartKey, EndKey, Reason);
account_is_failed(Ctx, _) ->
    account_is_done(Ctx).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process messages and do migrate
%% @end
%%------------------------------------------------------------------------------
-spec migrate_messages(ctx(), kz_json:objects()) -> ctx().
migrate_messages(#ctx{account_id = _AccountId}=Ctx, ViewResults) ->
    {NewCtx, BoxIds, MsgMap} = process_messages(Ctx, ViewResults),
    maybe_migrate(NewCtx, BoxIds, MsgMap, maps:keys(MsgMap)).

%%------------------------------------------------------------------------------
%% @doc Check Db existence and process with migration
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate(ctx(), kz_term:ne_binaries(), map(), kz_term:ne_binaries() | non_neg_integer()) -> ctx().
maybe_migrate(Ctx0, BoxIds, MsgMap, Dbs) when is_list(Dbs) ->
    {Ctx1, NewMsgMap} = check_dbs_existence(Ctx0, Dbs, MsgMap),
    maybe_migrate(Ctx1, BoxIds, NewMsgMap, maps:size(NewMsgMap));
maybe_migrate(#ctx{account_id = _AccountId}=Ctx, _BoxIds, _MsgsMap, 0) ->
    ?SUP_LOG_DEBUG("  [~s] none of the modbs are exists, considered it the last modb", [_AccountId]),
    Ctx;
maybe_migrate(Ctx, BoxIds, MsgMap, _DbCount) ->
    update_mailboxes(maps:fold(fun bulk_save_modb/3, Ctx, MsgMap), BoxIds).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bulk_save_modb(kz_term:ne_binary(), kz_json:objects(), ctx()) -> ctx().
bulk_save_modb(Db, JObjs, #ctx{account_id = _AccountId}=Ctx) ->
    case kz_datamgr:save_docs(Db, JObjs) of
        {'ok', Saved} ->
            normalize_bulk_result(Ctx, Db, Saved);
        {'error', Reason} ->
            ?SUP_LOG_ERROR("  [~s] failed to migrate voicemail messages to db ~s: ~p", [_AccountId, Db, Reason]),
            ctx_field_update(Ctx, 'failed', [{kz_doc:id(JObj), Reason} || JObj <- JObjs])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_mailboxes(ctx(), kz_term:ne_binaries()) -> ctx().
update_mailboxes(#ctx{account_id = _AccountId
                     ,total_msgs = TotalMsgs
                     ,failed = [{_Id, Reason}|_]=Failed
                     ,no_modb = NoModb
                     ,no_ids = NoIds
                     }=Ctx, _) when TotalMsgs == length(Failed) + length(NoModb) + NoIds ->
    ?SUP_LOG_ERROR("  [~s] bulk save failed for all docs: ~p", [_AccountId, Reason]),
    update_total_stats(Ctx#ctx{last_error = kz_term:to_binary(io_lib:format("bulk save failed: ~p", [Reason]))});
update_mailboxes(#ctx{account_id = _AccountId
                     ,account_db = AccountDb
                     }=Ctx, BoxIds) ->
    case kz_datamgr:open_cache_docs(AccountDb, BoxIds) of
        {'ok', BoxJObjs} ->
            NewBoxJObjs = update_mailbox_jobjs(Ctx, BoxJObjs),
            case kz_datamgr:save_docs(AccountDb, NewBoxJObjs) of
                {'ok', _} -> update_total_stats(Ctx);
                {'error', _Reason} ->
                    ?SUP_LOG_ERROR("  [~s] failed to save new message array into mailboxes: ~p", [_AccountId, _Reason]),
                    reset_context_msgs(Ctx#ctx{last_error = kz_term:to_binary(io_lib:format("update vmbox failed: ~p", [_Reason]))})
            end;
        {'error', _Reason} ->
            ?SUP_LOG_ERROR("  [~s] failed to open mailboxes for update: ~p", [_AccountId, _Reason]),
            reset_context_msgs(Ctx#ctx{last_error = kz_term:to_binary(io_lib:format("open vmbox failed: ~p", [_Reason]))})
    end.

-spec update_mailbox_jobjs(ctx(), kz_json:objects()) -> kz_json:objects().
update_mailbox_jobjs(#ctx{no_modb = NoModb, failed = Failed, no_timestamp = NoTime}, BoxJObjs) ->
    NoModbSet = sets:from_list(NoModb),
    FailedMap = maps:from_list(Failed),
    NoTimeMap = maps:from_list(NoTime),
    [update_message_array(kz_json:get_value(<<"doc">>, J), NoModbSet, FailedMap, NoTimeMap)
     || J <- BoxJObjs
    ].

-spec update_message_array(kz_json:object(), sets:set(), map(), map()) -> kz_json:object().
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

-spec update_vmbox_message(kz_json:object(), sets:set(), map(), map(), kz_term:api_ne_binary(), kz_time:api_seconds()) -> kz_term:api_object().
update_vmbox_message(Message, _, _, _, 'undefined', _) ->
    %% no media_id = no migration
    kz_json:set_value(<<"migration_error">>, <<"no_media_id">>, Message);
update_vmbox_message(Message, MODbFailed, Failed, NoTimestamp, Id, 'undefined') ->
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

    case {sets:is_element(MsgId, MODbFailed), maps:get(MsgId, Failed, 'undefined')} of
        {'true', _} ->
            kz_json:set_value(<<"migration_error">>, <<"no_modb">>, Message);
        {'false', 'undefined'} ->
            'undefined';
        {_, Error} ->
            kz_json:set_value(<<"migration_error">>, kz_term:to_binary(Error), Message)
    end.

%%------------------------------------------------------------------------------
%% @doc Get messages from mailbox arrays and generate legacy_msg listing view
%% fake message_doc result for manual migration
%% @end
%%------------------------------------------------------------------------------
-spec get_messages_from_vmboxes(kz_term:ne_binary(), kz_term:ne_binaries()) -> db_ret().
get_messages_from_vmboxes(AccountDb, ExpectedBoxIds) ->
    case kz_datamgr:open_cache_docs(AccountDb, ExpectedBoxIds) of
        {'ok', JObjs} -> {'ok', normalize_mailbox_results(JObjs)};
        {'error', _E} = Error ->
            ?SUP_LOG_ERROR("  [~s] failed to open mailbox(es)", [kz_util:format_account_id(AccountDb)]),
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

%%------------------------------------------------------------------------------
%% @doc Check Db existence and remove messages that non exists dbs
%% @end
%%------------------------------------------------------------------------------
-spec check_dbs_existence(ctx(), kz_term:ne_binaries(), map()) -> {ctx(), map()}.
check_dbs_existence(Ctx, [], MsgMap) -> {Ctx, MsgMap};
check_dbs_existence(Ctx, [Db | Dbs], MsgMap) ->
    case kz_datamgr:db_exists(Db) of
        'true' ->
            check_dbs_existence(Ctx, Dbs, MsgMap);
        'false' ->
            lager:warning("modb ~s is not exists", [Db]),
            check_dbs_existence(ctx_field_update(Ctx, 'no_modb', [kz_doc:id(Msg) || Msg <- maps:get(Db, MsgMap)])
                               ,Dbs
                               ,maps:remove(Db, MsgMap)
                               )
    end.

%%------------------------------------------------------------------------------
%% @doc Normalize bulk save results and update stats accordingly
%% @end
%%------------------------------------------------------------------------------
-spec normalize_bulk_result(ctx(), kz_term:ne_binary(), kz_json:objects()) -> ctx().
normalize_bulk_result(Ctx, _Db, []) ->
    Ctx;
normalize_bulk_result(#ctx{account_id = _AccountId}=Ctx, Db, [S | Saved]) ->
    Id = kz_doc:id(S),
    case kz_json:get_value(<<"error">>, S) of
        'undefined' ->
            %% successful
            normalize_bulk_result(ctx_field_update(Ctx, 'succeeded', [Id]), Db, Saved);
        <<"conflict">> ->
            %% successful
            normalize_bulk_result(ctx_field_update(Ctx, 'succeeded', [Id]), Db, Saved);
        Reason ->
            lager:debug("  [~s] failed to save voice mail message ~s in db ~s: ~p", [_AccountId, Id, Db, Reason]),
            normalize_bulk_result(ctx_field_update(Ctx, 'failed', [{Id, Reason}]), Db, Saved)
    end.

%%------------------------------------------------------------------------------
%% @doc process legacy message view result, generate a new message doc
%% for them and map them to proper modb.
%%
%% <div class="notice"> We are moving metadata only, attachment still remains in AccountDb,
%% This is so much faster than moving with attachments which probably
%% takes a couple of days for huge systems. We are creating message docs
%% from scratch based on message metadata from each mailbox, and we use
%% kazoo_data bulk operation for faster db writes.</div>
%% @end
%%------------------------------------------------------------------------------
-spec process_messages(ctx(), kz_json:objects()) -> {ctx(), kz_term:ne_binaries(), map()}.
process_messages(Ctx0, JObjs) ->
    {Ctx1, BoxSet, MsgMap0, NeedTimestamp} = lists:foldl(fun check_create_and_map/2, {Ctx0, sets:new(), #{}, []}, JObjs),
    {Ctx2, TimestampUpdated} = maybe_get_timestamp(Ctx1, NeedTimestamp),
    {Ctx3, MsgMap1} = lists:foldl(fun(J, Acc) -> create_and_map(J, Acc) end, {Ctx2, MsgMap0}, TimestampUpdated),

    {Ctx3, sets:to_list(BoxSet), MsgMap1}.

-spec check_create_and_map(kz_json:object(), {ctx(), sets:set(), map(), kz_json:objects()}) ->
                                  {ctx(), sets:set(), map(), kz_json:objects()}.
check_create_and_map(JObj, {Ctx, BoxSet, MsgMap, NoTimestamp}) ->
    Value = kz_json:get_value(<<"value">>, JObj),
    MediaId = kz_json:get_ne_binary_value([<<"metadata">>, <<"media_id">>], Value),

    case MediaId =/= 'undefined'
        andalso kz_json:get_integer_value([<<"metadata">>, <<"timestamp">>], Value)
    of
        'false' ->
            {ctx_field_update(Ctx, 'no_ids', 1), BoxSet, MsgMap, NoTimestamp};
        'undefined' ->
            {Ctx, sets:add_element(kz_doc:id(JObj), BoxSet), MsgMap, [Value|NoTimestamp]};
        _ ->
            {Ctx1, NewMsgMap} = create_and_map(Value, {Ctx, MsgMap}),
            {Ctx1, sets:add_element(kz_doc:id(JObj), BoxSet), NewMsgMap, NoTimestamp}
    end.

-spec create_and_map(kz_json:object(), {ctx(), map()}) -> {ctx(), map()}.
create_and_map(JObj, {Ctx, MsgMap}) ->
    Doc = create_message(Ctx, JObj),
    {Ctx
    ,maps:update_with(kz_doc:account_db(Doc), fun(List) -> [Doc|List] end, [Doc], MsgMap)
    }.

-spec create_message(ctx(), kz_json:object()) -> kz_json:object().
create_message(#ctx{account_id = AccountId
                   ,account_db = AccountDb
                   ,defualt_extension = DefaultExt
                   }, FakeVMBoxMsgJObj) ->
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
-spec maybe_get_timestamp(ctx(), kz_json:objects()) -> {ctx(), kz_json:objects()}.
maybe_get_timestamp(Ctx, []) -> {Ctx, []};
maybe_get_timestamp(#ctx{account_id = _AccountId
                        ,account_db = AccountDb
                        }=Ctx, JObjs) ->
    IdsMap = maps:from_list([{Id, J}
                             || J <- JObjs,
                                Id <- [kz_json:get_ne_binary_value([<<"metadata">>, <<"media_id">>], J)],
                                Id =/= 'undefined'
                            ]
                           ),
    case kz_datamgr:open_docs(AccountDb, maps:keys(IdsMap)) of
        {'ok', PrivateMedias} ->
            get_timestamp(Ctx, IdsMap, PrivateMedias);
        {'error', _Reason} ->
            ?SUP_LOG_ERROR("  [~s] failed to open private media to find timestamp, setting vmbox/current_time as timestamp: ~p", [_AccountId, _Reason]),
            {NoTime, Msgs} = maps:fold(fun all_no_timestamp/3, {[], []}, IdsMap),
            {ctx_field_update(Ctx, 'no_timestamp', NoTime), Msgs}
    end.

-spec all_no_timestamp(kz_term:ne_binary(), kz_json:object(), {kz_term:proplist(), kz_json:objects()}) -> {kz_term:proplist(), kz_json:objects()}.
all_no_timestamp(Id, JObj, {NoTime, JAcc}) ->
    Timestamp = kz_json:get_integer_value(<<"box_timestamp">>, JObj, kz_time:now_s()),
    {[{Id, Timestamp} | NoTime] ,[kz_json:set_value([<<"metadata">>, <<"timestamp">>], Timestamp, JObj) | JAcc]}.

-spec get_timestamp(ctx(), map(), kz_json:objects()) -> {ctx(), kz_json:objects()}.
get_timestamp(Ctx, IdsMap, PrivateMedias) ->
    Fun = fun(PrivateMedia, {TimeMap, Messages}) ->
                  Id = kz_doc:id(PrivateMedia, kz_json:get_value(<<"key">>, PrivateMedia)),
                  Doc = kz_json:get_value(<<"doc">>, PrivateMedia, kz_json:new()),
                  Message = maps:get(Id, IdsMap),

                  BoxTimestamp = kz_json:get_integer_value(<<"box_timestamp">>, Message, kz_time:now_s()),
                  Timestamp = kz_doc:modified(Doc, kz_doc:created(Doc, BoxTimestamp)),

                  {[{Id, Timestamp}|TimeMap], [kz_json:set_value([<<"metadata">>, <<"timestamp">>], Timestamp, Message)|Messages]}
          end,
    {NoTime, Msgs} = lists:foldl(Fun, {[], []}, PrivateMedias),
    {ctx_field_update(Ctx, 'no_timestamp', NoTime), Msgs}.


-type ctx_fields() :: 'no_ids' | 'no_modb' | 'no_timestamp' | 'failed' | 'succeeded'.
-type ctx_filed_values() :: non_neg_integer() | kz_term:ne_binaries() | kz_term:proplist().

-spec ctx_field_update(ctx(), ctx_fields(), ctx_filed_values()) -> ctx().
ctx_field_update(#ctx{no_ids = Total}=Ctx, 'no_ids', Value) ->
    Ctx#ctx{no_ids = Total + Value};
ctx_field_update(#ctx{no_modb = NoModb}=Ctx, 'no_modb', Value) ->
    Ctx#ctx{no_modb = Value ++ NoModb};
ctx_field_update(#ctx{no_timestamp = NoTime}=Ctx, 'no_timestamp', Value) ->
    Ctx#ctx{no_timestamp = Value ++ NoTime};
ctx_field_update(#ctx{failed = Failed}=Ctx, 'failed', Value) ->
    Ctx#ctx{failed = Value ++ Failed};
ctx_field_update(#ctx{succeeded = Succeeded}=Ctx, 'succeeded', Ids) ->
    Ctx#ctx{succeeded = Ids ++ Succeeded}.

-spec reset_context_msgs(ctx()) -> ctx().
reset_context_msgs(Ctx) ->
    Ctx#ctx{succeeded = [], no_modb = []
           ,no_timestamp = [], no_ids = 0
           ,failed = []
           }.

-spec update_total_stats(ctx()) -> ctx().
update_total_stats(#ctx{total_msgs = TotalMsgs, no_ids = CurrNoIds
                       ,succeeded = Succeeded, no_modb = NoModb
                       ,no_timestamp = NoTime, failed = Failed
                       ,account_id = _AccountId
                       ,total_stats = #total_stats{total_processed = TotalProcessed, total_succeeded = TotalSucceeded
                                                  ,total_failed = TotalFailed, total_no_modb = TotalNoModb
                                                  ,total_no_ids = TotalNoIds
                                                  }=TotalStats
                       }=Ctx) ->
    CurrSucceeded = length(Succeeded),
    CurrFailed = length(Failed),
    CurrNoModb = length(NoModb),
    CurrNoTime = length(NoTime),

    ?SUP_LOG_INFO("  [~s] finished a cycle, processed: ~b succeeded: ~b save_failed: ~b no_modb: ~b fix_timestamp: ~b no_ids: ~b"
                 ,[_AccountId, TotalMsgs, CurrSucceeded, CurrFailed, CurrNoModb, CurrNoTime, CurrNoIds]
                 ),

    Ctx#ctx{total_stats = TotalStats#total_stats{total_processed = TotalMsgs + TotalProcessed
                                                ,total_succeeded = CurrSucceeded + TotalSucceeded
                                                ,total_failed = CurrFailed + TotalFailed
                                                ,total_no_modb = CurrNoModb + TotalNoModb
                                                ,total_no_ids = CurrNoIds + TotalNoIds
                                                }
           }.

-spec total_stats_to_prop(#total_stats{}) -> kz_term:proplist().
total_stats_to_prop(#total_stats{total_processed = Processed, total_succeeded = Succeeded
                                ,total_failed = Failed, total_no_modb = TotalNoModb
                                ,total_no_ids = NoIds}) ->
    [{<<"total_processed">>, Processed}
    ,{<<"total_succeeded">>, Succeeded}
    ,{<<"total_failed">>, Failed + TotalNoModb}
    ,{<<"total_no_ids">>, NoIds}
    ].
