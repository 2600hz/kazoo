%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_migrate_account).

-export([start_worker/2
        ]).

-export([manual_migrate/1, manual_migrate/2
        ]).

-include("kz_voicemail.hrl").

-define(DEFAULT_VM_EXTENSION,
        kapps_config:get(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"extension">>], <<"mp3">>)).

-define(MAX_BULK_INSERT,
        kapps_config:get(?CF_CONFIG_CAT, [?KEY_VOICEMAIL, <<"migrate_max_bulk_insert">>], 2000)).

-define(LEGACY_MSG_LISTING, <<"vmboxes/legacy_msg_by_timestamp">>).

-define(TOTAL_MESSAGES, 'total_messages').
-define(TOTAL_SUCCEEDED, 'total_succeeded').
-define(TOTAL_FAILED, 'total_failed').
-define(FAILED, 'moved_failed').
-define(FAILED_MODB, 'no_modb').
-define(SUCCEEDED, 'succeeded').

-define(DEBUG(Format, Args),
        begin
            lager:debug(Format, Args),
            io:format(Format ++ "\n", Args)
        end
       ).

-define(WARNING(Format, Args),
        begin
            lager:warning(Format, Args),
            io:format(Format ++ "\n", Args)
        end
       ).

-define(ERROR(Format, Args),
        begin
            lager:error(Format, Args),
            io:format(Format ++ "\n", Args)
        end
       ).

-type migrate_stats() :: non_neg_integer() |
                         ne_binary() |
                         ne_binaries() |
                         {ne_binary(), atom()} |
                         [{ne_binary(), atom()}] |
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
            ?WARNING("    [~s] no legacy voicemail messages left", [AccountId]),
            kvm_migrate_crawler:account_is_done(Server, AccountId, FirstOfMonth, LastOfMonth);
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults),
            migration_result(Server, AccountId, FirstOfMonth, LastOfMonth);
        {'error', 'not_found'} ->
            Views = kapps_maintenance:get_all_account_views(),
            _ = kapps_util:update_views(Db, Views, 'true'),
            start_worker({AccountId, FirstOfMonth, LastOfMonth}, Server);
        {'error', R} ->
            ?ERROR("    [~s] failed to fetch legacy voicemail message: ~p", [AccountId, R]),
            kvm_migrate_crawler:account_maybe_failed(Server, AccountId, FirstOfMonth, LastOfMonth, R)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Manual migration for an Account or a list of account's mailboxes
%% @end
%%--------------------------------------------------------------------
-spec manual_migrate(ne_binary()) -> 'ok'.
-spec manual_migrate(ne_binary(), ne_binary() | ne_binaries()) -> 'ok'.
manual_migrate(AccountId) ->
    ?WARNING("######## Beginnig migration for account ~s~n~n", [AccountId]),
    manual_migrate_loop(AccountId, 1).

manual_migrate_loop(AccountId, LoopCount) ->
    ?WARNING("    [~s] migration cycle #~b", [AccountId, LoopCount]),
    ViewOpts = [{'limit', ?MAX_BULK_INSERT}
               ,'descending'
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:get_results(Db, ?LEGACY_MSG_LISTING, ViewOpts) of
        {'ok', []} ->
            ?WARNING("    [~s] no legacy voicemail messages left", [AccountId]),
            print_summary(AccountId),
            ?WARNING("~n~n######## Account ~s migration is done ########", [AccountId]);
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults),
            timer:sleep(?TIME_BETWEEN_ACCOUNT_CRAWLS),
            case is_latest_modb(AccountId) of
                'true' ->
                    ?WARNING("    [~s] reached to the latest avialable modb", [AccountId]),
                    ?WARNING("~n~n######## Account ~s migration is done ########", [AccountId]);
                'false' ->
                    manual_migrate_loop(AccountId, LoopCount + 1)
            end;
        {'error', 'not_found'} ->
            Views = kapps_maintenance:get_all_account_views(),
            _ = kapps_util:update_views(Db, Views, 'true'),
            manual_migrate_loop(AccountId, LoopCount);
        {'error', R} ->
            ?ERROR("    [~s] failed to fetch legacy voicemail message: ~p", [AccountId, R]),
            print_summary(AccountId),
            ?WARNING("~n~n######## Account ~s migration is done ########", [AccountId])
    end.

manual_migrate(AccountId, ?NE_BINARY = BoxId) ->
    manual_migrate(AccountId, [BoxId]);
manual_migrate(AccountId, BoxIds) ->
    ?WARNING("######## Beginnig migration for ~b mailbox(es) in account ~s~n~n", [length(BoxIds), AccountId]),
    case get_messages_from_vmboxes(AccountId, BoxIds) of
        {'ok', []} ->
            ?WARNING("    [~s] no legacy voicemail messages left", [AccountId]);
        {'ok', ViewResults} ->
            migrate_messages(AccountId, ViewResults);
        {'error', R} ->
            ?ERROR("    [~s] failed to fetch legacy voicemail message: ~p", [AccountId, R])
    end,
    print_summary(AccountId),
    ?WARNING("~n~n######## Account ~s migration is done ########", [AccountId]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Process messages and do migrate
%% @end
%%--------------------------------------------------------------------
-spec migrate_messages(ne_binary(), kz_json:objects()) -> 'ok'.
migrate_messages(AccountId, ViewResults) ->
    MsgCount = length(ViewResults),
    _ = update_process_key(?TOTAL_MESSAGES, MsgCount),

    ?WARNING("    [~s] processing ~b voicemail messages", [AccountId, MsgCount]),
    MsgsDict = process_messages(AccountId, ViewResults),
    maybe_migrate(AccountId, ViewResults, MsgsDict, dict:fetch_keys(MsgsDict)).

%%--------------------------------------------------------------------
%% @private
%% @doc Check Db existence and process with migration
%% @end
%%--------------------------------------------------------------------
-spec maybe_migrate(ne_binary(), kz_json:objects(), kz_json:objects(), ne_binaries() | non_neg_integer()) -> 'ok'.
maybe_migrate(AccountId, ViewResults, MsgsDict, Dbs) when is_list(Dbs) ->
    NewMsgsDict = check_dbs_existence(Dbs, MsgsDict),
    maybe_migrate(AccountId, ViewResults, NewMsgsDict, dict:size(NewMsgsDict));
maybe_migrate(_AccountId, _ViewResults, _MsgsDict, 0) ->
    lager:debug("    [~s] none of modbs for proccessed messages is exists", [_AccountId]);
maybe_migrate(AccountId, ViewResults, MsgsDict, _DbCount) ->
    do_migrate(MsgsDict),
    update_mailboxes(AccountId, ViewResults).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec do_migrate(dict:dict()) -> 'ok'.
do_migrate(MsgsDict) ->
    dict:fold(fun bulk_save_modb/3, [], MsgsDict).

-spec bulk_save_modb(ne_binary(), kz_json:objects(), list()) -> 'ok'.
bulk_save_modb(Db, Js, _Acc) ->
    case kz_datamgr:save_docs(Db, Js) of
        {'ok', Saved} ->
            _ = normalize_bulk_result(Db, Saved),
            'ok';
        {'error', R} ->
            update_stats(?FAILED, Js, R),
            ?ERROR("    [~s] failed to migrate voicemail messages to db ~s: ~p"
                  ,[kz_util:format_account_id(Db), Db, R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_mailboxes(kz_json:object(), kz_json:objects()) -> 'ok'.
update_mailboxes(AccountId, ViewResults) ->
    MODbFailed = dict:from_list(get_stats(?FAILED_MODB)),
    Failed = dict:from_list(get_stats(?FAILED)),
    BoxIds = lists:usort([kz_doc:id(B) || B <- ViewResults]),

    ViewOpts = [{'keys', BoxIds}
               ,'include_docs'
               ],
    Db = kvm_util:get_db(AccountId),
    case kz_datamgr:all_docs(Db, ViewOpts) of
        {'ok', BoxJObjs} ->
            NewBoxJObjs = update_mailbox_jobjs(BoxJObjs, MODbFailed, Failed),
            case kz_datamgr:save_docs(Db, NewBoxJObjs) of
                {'ok', _} -> 'ok';
                {'error', R} ->
                    ?ERROR("    [~s] failed to save new message array into mailboxes", [AccountId]),
                    failed_to_update_mailbox(ViewResults, R)
            end;
        {'error', R} ->
            ?ERROR("    [~s] failed to open mailboxes for update", [AccountId]),
            failed_to_update_mailbox(ViewResults, R)
    end.

failed_to_update_mailbox(ViewResults, Reason) ->
    %% nuke process stats to start process this account later
    _ = erlang:erase(?SUCCEEDED),
    _ = update_process_key(?TOTAL_SUCCEEDED, 0),
    Failed = [{kz_json:get_value([<<"value">>, <<"metadata">>, <<"media_id">>], M), Reason}
              || M <- ViewResults
             ],
    _ = erlang:put(?FAILED, Failed),
    _ = erlang:put(?TOTAL_FAILED, length(Failed)),
    'ok'.

update_mailbox_jobjs(BoxJObjs, MODbFailed, Failed) ->
    [update_message_array(kz_json:get_value(<<"doc">>, J), MODbFailed, Failed)
     || J <- BoxJObjs
    ].

-spec update_message_array(kz_json:object(), dict:dict(), dict:dict()) -> kz_json:object().
update_message_array(BoxJObj, MODbFailed, Failed) ->
    Messages = kz_json:get_value(<<"messages">>, BoxJObj),
    %% check if messages are failed or not, if not remove them from message array
    Fun = fun(Msg, Acc) ->
              Timestamp = kz_json:get_value(<<"timestamp">>, Msg),
              {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
              MsgId = ?MODB_MSG_ID(Year, Month, kz_json:get_value(<<"media_id">>, Msg)),
              M = dict:is_key(MsgId, MODbFailed),
              F = dict:is_key(MsgId, Failed),

              case {M, F} of
                  {'true', _} ->
                      Error = dict:fetch(MsgId, MODbFailed),
                      [kz_json:set_value(<<"migration_error">>, kz_util:to_binary(Error), Msg) | Acc];
                  {_, 'true'} ->
                      Error = dict:fetch(MsgId, Failed),
                      [kz_json:set_value(<<"migration_error">>, kz_util:to_binary(Error), Msg) | Acc];
                  _ -> Acc
              end
          end,
    NewMessages = lists:foldl(Fun, [], Messages),
    kz_json:set_value(?VM_KEY_MESSAGES, NewMessages, BoxJObj).

%%--------------------------------------------------------------------
%% @private
%% @doc Get messages from mailbox arrays and generate lagecy_msg lisiting view
%% fake message_doc result for manual migration
%% @end
%%--------------------------------------------------------------------
-spec get_messages_from_vmboxes(ne_binary(), ne_binaries()) -> db_ret().
get_messages_from_vmboxes(AccountId, ExpectedBoxIds) ->
    Db = kz_util:format_account_db(AccountId),
    ViewOpts = props:filter_empty(
                 [{'keys', ExpectedBoxIds}
                 ,'include_docs'
                 ]),
    case kz_datamgr:all_docs(Db, ViewOpts) of
        {'ok', JObjs} ->
            {'ok', lists:flatten(normalize_mailbox_results(JObjs))};
        {'error', _E} = Error ->
            ?ERROR("    [~s] failed to open mailbox(es)", [AccountId]),
            Error
    end.

-spec normalize_mailbox_results(kz_json:objects()) -> kz_json:objects().
normalize_mailbox_results(JObjs) ->
    [generate_lagecy_view_result(kz_json:get_value(<<"doc">>, J))
     || J <- JObjs,
        has_messages(J)
    ].

-spec generate_lagecy_view_result(kz_json:object()) -> kz_json:objects().
generate_lagecy_view_result(BoxJObj) ->
    BoxId = kz_doc:id(BoxJObj),
    OwnerId = kz_json:get_value(<<"owner_id">>, BoxJObj),
    Mailbox = kz_json:get_value(<<"mailbox">>, BoxJObj),
    Timezone = kz_json:get_value(<<"timezone">>, BoxJObj),
    Messages = kz_json:get_value(?VM_KEY_MESSAGES, BoxJObj, []),
    lists:foldl(fun(M, Acc) ->
                    Value = kz_json:from_list(
                              props:filter_empty(
                                   [{<<"source_id">>, BoxId}
                                   ,{<<"owner_id">>, OwnerId}
                                   ,{<<"timezone">>, Timezone}
                                   ,{<<"mailbox">>, Mailbox}
                                   ,{<<"metadata">>, M}
                                   ])),
                    [kz_json:from_list(
                       props:filter_empty(
                         [{<<"id">>, BoxId}
                         ,{<<"key">>, kz_json:get_value(<<"timestamp">>, M)}
                         ,{<<"value">>, Value}
                         ,{<<"_id">>, BoxId}
                         ]))
                     | Acc
                    ]
                end
               ,[]
               ,Messages
               ).

-spec has_messages(kz_json:object()) -> boolean().
has_messages(JObj) ->
    length(kz_json:get_value([<<"doc">>, ?VM_KEY_MESSAGES], JObj, [])) > 0.

%%--------------------------------------------------------------------
%% @private
%% @doc Check Db existence and remove messages that non exists dbs
%% @end
%%--------------------------------------------------------------------
-spec check_dbs_existence(ne_binaries(), dict:dict()) -> dict:dict().
check_dbs_existence([], MsgsDict) -> MsgsDict;
check_dbs_existence([Db | Dbs], MsgsDict) ->
    case kz_datamgr:db_exists(Db) of
        'true' ->
            check_dbs_existence(Dbs, MsgsDict);
        'false' ->
            lager:warning("modb ~s is not exists", [Db]),
            update_stats(?FAILED_MODB, dict:fetch(Db, MsgsDict), <<"modb_not_exists">>),
            dict:erase(Db, MsgsDict)
    end.

%--------------------------------------------------------------------
%% @private
%% @doc Normalize bulk save results and update stats accordingly
%% @end
%%--------------------------------------------------------------------
-spec normalize_bulk_result(ne_binary(), kz_json:objects()) ->
                                    {non_neg_integer(), non_neg_integer()}.
-spec normalize_bulk_result(ne_binary(), kz_json:objects(), dict:dict()) ->
                                    {non_neg_integer(), non_neg_integer()}.
normalize_bulk_result(Db, Saved) ->
    DefaultDict = dict:from_list([{<<"succeeded">>, []}
                                 ,{<<"failed">>, []}
                                 ]),
    normalize_bulk_result(Db, Saved, DefaultDict).

normalize_bulk_result(_Db, [], Dict) ->
    Succeeded = dict:fetch(<<"succeeded">>, Dict),
    Failed = dict:fetch(<<"failed">>, Dict),
    update_stats(?FAILED, Failed),
    update_stats(?SUCCEEDED, Succeeded),
    {length(Succeeded), length(Failed)};
normalize_bulk_result(Db, [S | Saved], Dict) ->
    Id = kz_json:get_first_defined([<<"key">>, <<"id">>, <<"_id">>], S),
    case kz_json:get_value(<<"error">>, S) of
        'undefined' ->
            %% successful
            normalize_bulk_result(Db, Saved, dict:append(<<"succeeded">>, Id, Dict));
        <<"conflict">> ->
            %% successful
            normalize_bulk_result(Db, Saved, dict:append(<<"succeeded">>, Id, Dict));
        Reason ->
            lager:debug("    [~s] failed to save voicemail message ~s in db ~s: ~p"
                       ,[kz_util:format_account_id(Db), Id, Db, Reason]),
            normalize_bulk_result(Db, Saved, dict:append(<<"failed">>, {Id, Reason}, Dict))
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
-spec process_messages(ne_binary(), kz_json:objects()) -> dict:dict().
process_messages(AccountId, JObjs) ->
    DefaultExt = ?DEFAULT_VM_EXTENSION,
    Fun = fun(J, Acc) ->
              Doc = create_message(AccountId, J, DefaultExt),
              dict:append(kz_doc:account_db(Doc), Doc, Acc)
          end,
    lists:foldl(Fun, dict:new(), JObjs).

-spec create_message(ne_binary(), kz_json:object(), ne_binary()) -> kz_json:object().
create_message(AccountId, FakeBoxJObj, DefaultExt) ->
    AccountDb = kvm_util:get_db(AccountId),
    BoxJObj = kz_doc:set_account_db(kz_json:get_value(<<"value">>, FakeBoxJObj), AccountDb),

    BoxNum = kzd_voicemail_box:mailbox_number(BoxJObj),
    TimeZone = kzd_voicemail_box:timezone(BoxJObj),

    Metadata = kzd_box_message:metadata(BoxJObj),
    Timestamp = kz_json:get_value(<<"timestamp">>, Metadata),

    %% setting a db_link as attachment
    AttName = <<(kz_util:rand_hex_binary(16))/binary, ".", DefaultExt/binary>>,
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
              [{<<"Box-Id">>, kz_doc:id(BoxJObj)}
              ,{<<"Media-ID">>, kzd_box_message:media_id(Metadata)}
              ,{<<"Box-Num">>, BoxNum}
              ,{<<"Timezone">>, TimeZone}
              ,{<<"Message-Timestamp">>, Timestamp}
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
    update_process_key(?TOTAL_FAILED, Count).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec migration_result(pid(), ne_binary(), gregorian_seconds(), gregorian_seconds()) -> 'ok'.
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
            ?WARNING("    [~s] reached to the latest avialable modb", [AccountId]);
        'false' ->
            kvm_migrate_crawler:update_stats(Server, AccountId, Props),
            ?WARNING("    [~s] finished a migrate cycle: [proccessed: ~b] [succeeded: ~b] [save_failed: ~b] [no_modb: ~b]"
                    ,[AccountId, TotalMsgs, TotalSucceeded, TotalFailed, MODbFailed])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_latest_modb(ne_binary()) -> boolean().
is_latest_modb(AccountId) ->
    print_summary(AccountId, 'true').

-spec print_summary(ne_binary()) -> 'ok'.
-spec print_summary(ne_binary(), boolean()) -> 'ok' | boolean().
print_summary(AccountId) ->
    print_summary(AccountId, 'false').

print_summary(AccountId, ShouldCheckMODB) ->
    TotalMsgs = get_stats(?TOTAL_MESSAGES),
    TotalSucceeded = get_stats(?TOTAL_SUCCEEDED),
    TotalFailed = get_stats(?TOTAL_FAILED),
    MODbFailed = length(get_stats(?FAILED_MODB)),

    ?WARNING("    [~s] finished a migrate cycle: [proccessed: ~b] [succeeded: ~b] [save_failed: ~b] [no_modb: ~b]"
            ,[AccountId, TotalMsgs, TotalSucceeded, TotalFailed, MODbFailed]),

    case ShouldCheckMODB of
        'false' -> 'ok';
        'true' ->
            TotalMsgs == MODbFailed
    end.
