%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Voice mail message
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kz_vm_message).

-export([new_message/5
         ,message_doc/2
         ,message/2, messages/2
         ,count/2, count_per_folder/2, count_modb_messages/3
         ,count_by_owner/2, vmbox_summary/1
         ,set_folder/3, update_folder/3

         ,media_url/2

         ,load_vmbox/2
         ,update_message_doc/2, update_message_doc/3

         ,get_db/1, get_db/2
         ,get_range_view/2
        ]).

-include("kz_voicemail.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CF_CONFIG_CAT, <<"callflow">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_RETENTION_DURATION, <<"message_retention_duration">>).

-define(MODB_LISTING_BY_MAILBOX, <<"vm_messages/listing_by_mailbox">>).
-define(MODB_COUNT_VIEW, <<"vm_messages/count_per_folder">>).
-define(BOX_MESSAGES_CB_LIST, <<"vmboxes/crossbar_listing">>).
-define(COUNT_BY_VMBOX, <<"vm_messages/count_by_vmbox">>).

-define(RETENTION_DURATION
        ,whapps_config:get(?CF_CONFIG_CAT
                           ,[?KEY_VOICEMAIL, ?KEY_RETENTION_DURATION]
                           ,93 %% 93 days(3 months)
                          )
       ).

-type db_ret() :: {'ok', wh_json:object() | wh_json:objects()} | {'error', any()}.

-spec get_db(ne_binary()) -> ne_binary().
get_db(AccountId) ->
    wh_util:format_account_id(AccountId, 'encoded').

-spec get_db(ne_binary(), kazoo_data:docid() | wh_json:object()) -> ne_binary().
get_db(AccountId, {_, ?MATCH_MODB_PREFIX(Year, Month, _)}) ->
    get_db(AccountId, Year, Month);
get_db(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)) ->
    get_db(AccountId, Year, Month);
get_db(AccountId, ?JSON_WRAPPER(_)=Doc) ->
    get_db(AccountId, wh_doc:id(Doc));
get_db(AccountId, _DocId) ->
    get_db(AccountId).

-spec get_db(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
get_db(AccountId, Year, Month) ->
    kazoo_modb:get_modb(AccountId, wh_util:to_integer(Year), wh_util:to_integer(Month)).

%%--------------------------------------------------------------------
%% @public
%% @doc recieve a new voicemail message and store it
%% expected options:
%%  [{<<"Attachment-Name">>, AttachmentName}
%%    ,{<<"Box-Id">>, BoxId}
%%    ,{<<"OwnerId">>, OwnerId}
%%    ,{<<"Length">>, Length}
%%    ,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
%%    ,{<<"After-Notify-Action">>, Action}
%%  ]
%% @end
%%--------------------------------------------------------------------
-spec new_message(ne_binary(), ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) -> any().
new_message(AttachmentName, BoxNum, Timezone, Call, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    Length = props:get_value(<<"Length">>, Props),

    lager:debug("saving new ~bms voicemail media and metadata", [Length]),

    {MediaId, MediaUrl} = create_message_doc(AttachmentName, BoxNum, Call, Timezone, Props),

    Msg = io_lib:format("failed to store voicemail media ~s in voicemail box ~s of account ~s"
                        ,[MediaId, BoxId, whapps_call:account_id(Call)]
                       ),
    Funs = [{fun whapps_call:kvs_store/3, 'mailbox_id', BoxId}
            ,{fun whapps_call:kvs_store/3, 'attachment_name', AttachmentName}
            ,{fun whapps_call:kvs_store/3, 'media_id', MediaId}
            ,{fun whapps_call:kvs_store/3, 'media_length', Length}
           ],

    lager:debug("storing voicemail media recording ~s in doc ~s", [AttachmentName, MediaId]),
    case store_recording(AttachmentName, MediaUrl, whapps_call:exec(Funs, Call)) of
        'ok' ->
            _ = notify_and_save(Call, MediaId, Length, Props),
            'ok';
        {'error', Call1} ->
            lager:critical(Msg),
            {'error', Call1, Msg}
    end.

-spec create_message_doc(ne_binary(), ne_binary(), whapps_call:call(), ne_binary(), wh_proplist()) ->
                                {ne_binary(), ne_binary()}.
create_message_doc(AttachmentName, BoxNum, Call, Timezone, Props) ->
    {Year, Month, _} = erlang:date(),
    Db = kazoo_modb:get_modb(whapps_call:account_id(Call), Year, Month),

    MediaId = <<(wh_util:to_binary(Year))/binary
                ,(wh_util:pad_month(Month))/binary
                ,"-"
                ,(wh_util:rand_hex_binary(16))/binary
              >>,
    Doc = kzd_voice_message:new(Db, MediaId, AttachmentName, BoxNum, Timezone, Props),
    {'ok', JObj} = kz_datamgr:save_doc(Db, Doc),
    MediaUrl = wh_media_url:store(JObj, AttachmentName),

    {MediaId, MediaUrl}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary(), whapps_call:call()) ->
                                 'ok' |
                                 {'error', whapps_call:call()}.
store_recording(AttachmentName, Url, Call) ->
    lager:debug("storing recording ~s at ~s", [AttachmentName, Url]),
    case whapps_call_command:store_file(<<"/tmp/", AttachmentName/binary>>, Url, Call) of
        'ok' -> 'ok';
        {'error', _} -> {'error', Call}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notify_and_save(whapps_call:call(), ne_binary(), integer(), wh_proplist()) -> 'ok' | db_ret().
notify_and_save(Call, MediaId, Length, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    NotifyAction = props:get_atom_value(<<"After-Notify-Action">>, Props),

    case publish_voicemail_saved_notify(MediaId, BoxId, Call, Length, Props) of
        {'ok', JObjs} ->
            JObj = get_completed_msg(JObjs),
            maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId);
        {'timeout', JObjs} ->
            case get_completed_msg(JObjs) of
                ?EMPTY_JSON_OBJECT ->
                    lager:info("timed out waiting for voicemail new notification resp"),
                    save_meta(Length, NotifyAction, Call, MediaId, BoxId);
                JObj ->
                    maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId)
            end;
        {'error', _E} ->
            lager:debug("voicemail new notification error: ~p", [_E]),
            save_meta(Length, NotifyAction, Call, MediaId, BoxId)
    end.

-spec get_completed_msg(wh_json:objects()) -> wh_json:object().
get_completed_msg(JObjs) ->
    get_completed_msg(JObjs, wh_json:new()).

-spec get_completed_msg(wh_json:objects(), wh_json:object()) -> wh_json:object().
get_completed_msg([], Acc) -> Acc;
get_completed_msg([JObj|JObjs], Acc) ->
    case wh_json:get_value(<<"Status">>, JObj) of
        <<"completed">> -> get_completed_msg([], JObj);
        _ -> get_completed_msg(JObjs, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_save_meta(pos_integer(), atom(), whapps_call:call(), ne_binary(), wh_json:object(), ne_binary()) -> 'ok' | db_ret().
maybe_save_meta(Length, 'nothing', Call, MediaId, _UpdateJObj, BoxId) ->
    save_meta(Length, 'nothing', Call, MediaId, BoxId);

maybe_save_meta(Length, Action, Call, MediaId, UpdateJObj, BoxId) ->
    case wh_json:get_value(<<"Status">>, UpdateJObj) of
        <<"completed">> ->
            save_meta(Length, Action, Call, MediaId, BoxId);
        <<"failed">> ->
            lager:debug("attachment failed to send out via notification: ~s", [wh_json:get_value(<<"Failure-Message">>, UpdateJObj)]),
            save_meta(Length, Action, Call, MediaId, BoxId)
    end.

-spec save_meta(pos_integer(), atom(), whapps_call:call(), ne_binary(), ne_binary()) -> 'ok' | db_ret().
save_meta(Length, Action, Call, MediaId, BoxId) ->
    AccountId = whapps_call:account_id(Call),
    CIDNumber = get_caller_id_number(Call),
    CIDName = get_caller_id_name(Call),
    Timestamp = new_timestamp(),

    Metadata = kzd_voice_message:create_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, Timestamp),

    case Action of
        'delete' ->
            lager:debug("attachment was sent out via notification, deleteing media file"),
            UpdatedMetadata = apply_folder(?VM_FOLDER_DELETED, Metadata),
            {'ok', _} = save_metadata(UpdatedMetadata, AccountId, MediaId);
        'save' ->
            lager:debug("attachment was sent out via notification, saving media file"),
            UpdatedMetadata = apply_folder(?VM_FOLDER_SAVED, Metadata),
            {'ok', _} = save_metadata(UpdatedMetadata, AccountId, MediaId);
        'nothing' ->
            {'ok', _} = save_metadata(Metadata, AccountId, MediaId),
            lager:debug("stored voicemail metadata for ~s", [MediaId]),
            publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp)
    end.

-spec save_metadata(wh_json:object(), ne_binary(), ne_binary()) -> db_ret().
save_metadata(NewMessage, AccountId, MessageId) ->
    Fun = [fun(JObj) ->
              kzd_voice_message:set_metadata(NewMessage, JObj)
           end
          ],

    case update_message_doc(AccountId, MessageId, Fun) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            lager:info("saving resulted in a conflict, trying again"),
            save_metadata(NewMessage, AccountId, MessageId);
        {'error', R}=E ->
            lager:info("error while storing voicemail metadata: ~p", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch whole message doc from db. It will take care if the message
%% message is stil in accountdb, it will merge metadata from vmbox
%% @end
%%--------------------------------------------------------------------
-spec message_doc(ne_binary(), kazoo_data:docid()) -> db_ret().
message_doc(AccountId, {_, ?MATCH_MODB_PREFIX(Year, Month, _)}=DocId) ->
    open_modb_doc(AccountId, DocId, Year, Month);
message_doc(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)=DocId) ->
    open_modb_doc(AccountId, DocId, Year, Month);
message_doc(AccountId, MediaId) ->
    case open_accountdb_doc(AccountId, MediaId) of
        {'ok', MediaJObj} ->
            SourceId = kzd_voice_message:source_id(MediaJObj),
            case fetch_vmbox_messages(AccountId, SourceId) of
                {'ok', VMBoxMsgs} ->
                    merge_metadata(MediaId, MediaJObj, VMBoxMsgs);
                {'error', _}=E ->
                    E
            end;
        {'error', _}=E ->
            lager:warning("failed to load message doc ~s", [MediaId]),
            E
    end.

-spec merge_metadata(ne_binary(), wh_json:object(), wh_json:objects()) -> db_ret().
merge_metadata(MediaId, MediaJObj, VMBoxMsgs) ->
    case kzd_voice_message:filter_vmbox_messages(MediaId, VMBoxMsgs) of
        {'error', _}=E -> E;
        {Metadata, _} -> {'ok', kzd_voice_message:set_metadata(Metadata, MediaJObj)}
    end.

-spec load_vmbox(ne_binary(), ne_binary()) -> db_ret().
load_vmbox(AccountId, BoxId) ->
    case open_accountdb_doc(AccountId, BoxId) of
        {'ok', JObj} ->
            VmMessages = wh_json:get_value(?VM_KEY_MESSAGES, JObj, []),
            AllMsg = fetch_modb_messages(AccountId, BoxId, VmMessages),
            {'ok', wh_json:set_value(?VM_KEY_MESSAGES, AllMsg, JObj)};
        {'error', _R}=E ->
            lager:debug("failed to open vmbox ~s: ~p", [BoxId, _R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Get vmbox summary view results from accountdb and merge it with
%% MODB vmbox counts
%% @end
%%--------------------------------------------------------------------
-spec vmbox_summary(ne_binary()) -> db_ret().
vmbox_summary(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:get_results(AccountDb, ?BOX_MESSAGES_CB_LIST, []) of
        {'ok', JObjs} ->
            Res = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
            MODBRes = modb_count_summary(AccountId),
            {'ok', merge_summary_results(Res, MODBRes)};
        {'error', _R}=E ->
            lager:debug("error fetching vmbox_summary for account ~s: ~p", [AccountId, _R]),
            E
    end.

-spec modb_count_summary(ne_binary()) -> wh_json:objects().
modb_count_summary(AccountId) ->
    Opts = ['reduce', 'group'],
    ViewOptsList = get_range_view(AccountId, Opts),
    [Res || Res <- results_from_modbs(AccountId, ?COUNT_BY_VMBOX, ViewOptsList, []), Res =/= []].

-spec merge_summary_results(wh_json:objects(), wh_json:objects()) -> wh_json:objects().
merge_summary_results(BoxSummary, MODBSummary) ->
    lists:foldl(fun(JObj, Acc) ->
                    BoxId = wh_json:get_value(<<"id">>, JObj),
                    case wh_json:find_value(<<"key">>, BoxId, MODBSummary) of
                        'undefined' ->
                            [JObj | Acc];
                        J ->
                            BCount = wh_json:get_integer_value(?VM_KEY_MESSAGES, JObj, 0),
                            MCount = wh_json:get_integer_value(<<"value">>, J, 0),
                            [wh_json:set_value(?VM_KEY_MESSAGES, BCount + MCount, JObj) | Acc]
                    end
                end
                , [], BoxSummary).

%%--------------------------------------------------------------------
%% @public
%% @doc fetch all messages for vmbox
%% @end
%%--------------------------------------------------------------------
-spec messages(ne_binary(), ne_binary()) -> wh_json:objects().
messages(AccountId, BoxId) ->
    % first get messages metadata from vmbox for backward compatibility
    case fetch_vmbox_messages(AccountId, BoxId) of
        {'ok', Msgs} -> fetch_modb_messages(AccountId, BoxId, Msgs);
        _ -> fetch_modb_messages(AccountId, BoxId, [])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message metadata from db, will take care of previous
%% message metadata in vmbox.
%% @end
%%--------------------------------------------------------------------
-spec message(ne_binary(), ne_binary()) -> db_ret().
message(AccountId, MessageId) ->
    case message_doc(AccountId, MessageId) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Msg} -> {'ok', kzd_voice_message:metadata(Msg)};
        {'error', _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Folder operations
%% @end
%%--------------------------------------------------------------------
-spec set_folder(ne_binary(), wh_json:object(), ne_binary()) -> any().
set_folder(Folder, Message, AccountId) ->
    MessageId = kzd_voice_message:media_id(Message),
    FromFolder = kzd_voice_message:folder(Message, ?VM_FOLDER_NEW),
    lager:info("setting folder for message ~s to ~s", [MessageId, Folder]),
    maybe_set_folder(FromFolder, Folder, MessageId, AccountId).

-spec maybe_set_folder(ne_binary(), ne_binary(), wh_json:object(), ne_binary()) -> any().
maybe_set_folder(FromFolder, FromFolder, ?MATCH_MODB_PREFIX(_, _, _), _) -> 'ok';
maybe_set_folder(FromFolder, FromFolder, MessageId, AccountId) ->
    lager:info("folder is same, but doc is in accountdb, move it to modb"),
    update_message_doc(AccountId, MessageId);
maybe_set_folder(_FromFolder, ToFolder, MessageId, AccountId) ->
    update_folder(ToFolder, MessageId, AccountId).

-spec update_folder(ne_binary(), ne_binary(), ne_binary()) -> db_ret() | {'error', 'attachment_undefined'}.
update_folder(_, 'undefined', _) ->
    {'error', 'attachment_undefined'};
update_folder(Folder, MessageId, AccountId) ->
    Fun = [fun(JObj) ->
               apply_folder(Folder, JObj)
           end
          ],

    case update_message_doc(AccountId, MessageId, Fun) of
        {'ok', J} -> {'ok', kzd_voice_message:metadata(J)};
        {'error', 'conflict'} ->
            lager:info("updating folder resulted in a conflict, trying again"),
            update_folder(Folder, MessageId, AccountId);
        {'error', R}=E ->
            lager:info("error while updating folder ~s ~p", [Folder, R]),
            E
    end.

-spec apply_folder(ne_binary(), wh_json:object()) -> wh_json:object().
apply_folder(?VM_FOLDER_DELETED, Doc) ->
    Metadata = kzd_voice_message:set_folder_deleted(kzd_voice_message:metadata(Doc)),
    wh_json:set_value(<<"pvt_deleted">>, <<"true">>, kzd_voice_message:set_metadata(Metadata, Doc));
apply_folder(Folder, Doc) ->
    Metadata = kzd_voice_message:set_folder(Folder, kzd_voice_message:metadata(Doc)),
    kzd_voice_message:set_metadata(Metadata, Doc).

%%--------------------------------------------------------------------
%% @private
%% @doc Update message docs and do migration if necessary
%% @end
%%--------------------------------------------------------------------
-type update_funs() :: [fun((wh_json:object()) -> wh_json:object())].

-spec update_message_doc(ne_binary(), ne_binary()) -> db_ret().
update_message_doc(AccountId, DocId) ->
    update_message_doc(AccountId, DocId, []).

-spec update_message_doc(ne_binary(), ne_binary(), update_funs()) -> db_ret().
update_message_doc(AccountId, DocId, Funs) ->
    case message_doc(AccountId, DocId) of
        {'ok', JObj} -> do_update_message_doc(AccountId, DocId, JObj, Funs);
        {'error', _}=E ->
            E
    end.

-spec do_update_message_doc(ne_binary(), ne_binary(), wh_json:object(), update_funs()) -> db_ret().
do_update_message_doc(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _), JObj, Funs) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    kazoo_modb:save_doc(AccountId, NewJObj, Year, Month);
do_update_message_doc(AccountId, DocId, JObj, Funs) ->
    move_to_modb(AccountId, DocId, JObj, Funs).

%%--------------------------------------------------------------------
%% @public
%% @doc Migration methods
%% @end
%%--------------------------------------------------------------------
-spec move_to_modb(ne_binary(), ne_binary(), wh_json:object(), update_funs()) -> db_ret().
move_to_modb(AccountId, DocId, JObj, Funs) ->
    Created = wh_doc:created(JObj),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Created),

    FromDb = get_db(AccountId),
    FromId = wh_doc:id(JObj),
    ToDb = kazoo_modb:get_modb(AccountId, Year, Month),
    ToId = <<(wh_util:to_binary(Year))/binary
              ,(wh_util:pad_month(Month))/binary
              ,"-"
              ,(wh_util:rand_hex_binary(16))/binary
           >>,
    % io:format("FromDb ~p FromId ~p ToDb ~p ToId ~p JObj ~p~n~n", [FromDb, FromId, ToDb, ToId, JObj]),
    TransformFuns = [fun(DestDoc) -> kzd_voice_message:set_metadata(kzd_voice_message:metadata(JObj), DestDoc) end
                     ,fun(DestDoc) -> update_media_id(ToId, DestDoc) end
                     ,fun(DestDoc) -> wh_json:set_value(<<"pvt_type">>, kzd_voice_message:type(), DestDoc) end
                     | Funs
                    ],
    Options = [{'transform', fun(_, B) -> lists:foldl(fun(F, J) -> F(J) end, B, TransformFuns) end}],
    case kz_datamgr:move_doc(FromDb, FromId, ToDb, ToId, Options) of
        {'ok', _}=M ->
            BoxId = kzd_voice_message:source_id(JObj),
            _ = update_mailbox(AccountId, BoxId, FromId),
            _ = kz_datamgr:del_doc(get_db(AccountId), FromId),
            M;
        {'error', _}=E ->
            lager:warning("failed to move voice message ~s to modb", [DocId]),
            E
    end.

-spec update_mailbox(ne_binary(), ne_binary(), ne_binary()) -> db_ret().
update_mailbox(AccountId, BoxId, OldId) ->
    case open_accountdb_doc(AccountId, BoxId) of
        {'ok', VMBox} ->
            Messages = wh_json:get_value(?VM_KEY_MESSAGES, VMBox, []),
            {_, NewMessages} = kzd_voice_message:filter_vmbox_messages(OldId, Messages),
            NewBoxJObj = wh_json:set_value(?VM_KEY_MESSAGES, NewMessages, VMBox),
            case kz_datamgr:save_doc(get_db(AccountId), NewBoxJObj) of
                {'ok', _}=OK -> OK;
                {'error', 'conflict'} -> update_mailbox(AccountId, BoxId, OldId);
                {'error', _R}=E ->
                    lager:debug("could not update mailbox ~s: ~s", [BoxId, _R]),
                    E
            end;
        {'error', _R}=E ->
            lager:warning("failed to open mailbox for update ~s: ~s", [BoxId, _R]),
            E
    end.

-spec update_media_id(ne_binary(), wh_json:object()) -> wh_json:object().
update_media_id(MediaId, JObj) ->
    Metadata = kzd_voice_message:set_media_id(MediaId, kzd_voice_message:metadata(JObj)),
    kzd_voice_message:set_metadata(Metadata, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc Count non-deleted messages
%% @end
%%--------------------------------------------------------------------
-spec count(ne_binary(), ne_binary()) -> {non_neg_integer(), non_neg_integer()}.
count(AccountId, BoxId) ->
    {New, Saved} = count_per_folder(AccountId, BoxId),
    New + Saved.

count_by_owner(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, OwnerId) ->
    AccountId = wh_util:format_account_id(AccountDb),
    count_by_owner(AccountId, OwnerId);
count_by_owner(AccountId, OwnerId) ->
    ViewOpts = [{'key', [OwnerId, <<"vmbox">>]}],

    case kz_datamgr:get_results(get_db(AccountId), <<"cf_attributes/owned">>, ViewOpts) of
        {'ok', [Owned|_]} ->
            VMBoxId = wh_json:get_value(<<"value">>, Owned),
            count_per_folder(AccountId, VMBoxId);
        {'error', _R} ->
            lager:info("unable to lookup vm counts by owner: ~p", [_R]),
            {0, 0}
    end.

-spec count_per_folder(ne_binary(), ne_binary()) -> {non_neg_integer(), non_neg_integer()}.
count_per_folder(AccountId, BoxId) ->
    % first count messages from vmbox for backward compatibility
    case fetch_vmbox_messages(AccountId, BoxId) of
        {'ok', Msgs} ->
            New = kzd_voice_message:count_messages(Msgs, [?VM_FOLDER_NEW]),
            Saved = kzd_voice_message:count_messages(Msgs, [?VM_FOLDER_SAVED]),
            count_modb_messages(AccountId, BoxId, {New, Saved});
        _ -> count_modb_messages(AccountId, BoxId, {0, 0})
    end.

-spec count_modb_messages(ne_binary(), ne_binary(), non_neg_integer()) -> non_neg_integer().
count_modb_messages(AccountId, BoxId, {ANew, ASaved}=AccountDbCounts) ->
    Opts = ['reduce'
            ,'group'
            ,{'group_level', 2}
            ,{'startkey', [BoxId]}
            ,{'endkey', [BoxId, wh_json:new()]}
           ],
    ViewOptions = get_range_view(AccountId, Opts),

    case results_from_modbs(AccountId, ?MODB_COUNT_VIEW, ViewOptions, []) of
        [] ->
            AccountDbCounts;
        Results ->
            {MNew, MSaved} = kzd_voice_message:count_messages(Results),
            {ANew + MNew, ASaved + MSaved}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_url(ne_binary(), ne_binary() | wh_json:object()) -> binary().
media_url(AccountId, ?JSON_WRAPPER(_)=Message) ->
    media_url(AccountId, kzd_voice_message:media_id(Message));
media_url(AccountId, MessageId) ->
    case message_doc(AccountId, MessageId) of
        {'ok', Message} ->
            wh_media_url:playback(Message, Message);
        {'error', _} -> <<>>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Abstract database operations
%% @end
%%--------------------------------------------------------------------
-spec open_modb_doc(ne_binary(), kazoo_data:docid(), ne_binary(), ne_binary()) -> wh_json:object().
open_modb_doc(AccountId, DocId, Year, Month) ->
    case kazoo_modb:open_doc(AccountId, DocId, Year, Month) of
        {'ok', _}=OK -> OK;
        {'error', _}=E -> E
    end.

-spec open_accountdb_doc(ne_binary(), kazoo_data:docid()) -> wh_json:object().
open_accountdb_doc(AccountId, DocId) ->
    case kz_datamgr:open_doc(get_db(AccountId), DocId) of
        {'ok', _}=OK -> OK;
        {'error', _}=E -> E
    end.

fetch_vmbox_messages(AccountId, BoxId) ->
    case open_accountdb_doc(AccountId, BoxId) of
        {'ok', BoxJObj} -> {'ok', wh_json:get_value(?VM_KEY_MESSAGES, BoxJObj, [])};
        {'error', _}=E ->
            lager:debug("error fetching voicemail messages for ~s from accountid ~s", [BoxId, AccountId]),
            E
    end.

fetch_modb_messages(AccountId, DocId, VMBoxMsg) ->
    ViewOpts = [{'key', DocId}
                ,'include_docs'
               ],
    ViewOptsList = get_range_view(AccountId, ViewOpts),

    ModbResults = [kzd_voice_message:metadata(wh_json:get_value(<<"doc">>, Msg))
                   || Msg <- results_from_modbs(AccountId, ?MODB_LISTING_BY_MAILBOX, ViewOptsList, [])
                      ,Msg =/= []
                  ],
    VMBoxMsg ++ ModbResults.

results_from_modbs(_, _, [], ViewResults) ->
    ViewResults;
results_from_modbs(AccountId, View, [ViewOpts|ViewOptsList], Acc) ->
    case kazoo_modb:get_results(AccountId, View, ViewOpts) of
        {'ok', []} -> results_from_modbs(AccountId, View, ViewOptsList, Acc);
        {'ok', Msgs} -> results_from_modbs(AccountId, View, ViewOptsList, Msgs ++ Acc);
        {'error', _}=_E ->
            lager:debug("error when fetching voicemail message for ~s from modb ~s"
                        ,[props:get_value('key', ViewOpts), props:get_value('modb', ViewOpts)]
                       ),
            results_from_modbs(AccountId, View, ViewOptsList, Acc)
    end.

-spec get_range_view(ne_binary(), wh_proplist()) -> wh_proplists().
get_range_view(AccountId, ViewOpts) ->
    To = wh_util:current_tstamp(),
    MaxRange = ?SECONDS_IN_DAY * ?RETENTION_DURATION + ?SECONDS_IN_HOUR,
    From = To - MaxRange,

    [ begin
          {AccountId, Year, Month} = kazoo_modb_util:split_account_mod(MODB),
          [{'year', Year}
           ,{'month', Month}
           ,{'modb', MODB}
           | ViewOpts
          ]
      end || MODB <- kazoo_modb:get_range(AccountId, From, To)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_voicemail_saved_notify(ne_binary(), ne_binary(), whapps_call:call(), pos_integer(), wh_proplist()) ->
                                    {'ok', wh_json:object()} |
                                    {'timeout', wh_json:object()} |
                                    {'error', any()}.
publish_voicemail_saved_notify(MediaId, BoxId, Call, Length, Props) ->
    MaybeTranscribe = props:get_value(<<"Transcribe-Voicemail">>, Props),
    Transcription = maybe_transcribe(Call, MediaId, MaybeTranscribe),

    NotifyProp = [{<<"From-User">>, whapps_call:from_user(Call)}
                  ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
                  ,{<<"To-User">>, whapps_call:to_user(Call)}
                  ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
                  ,{<<"Account-DB">>, whapps_call:account_db(Call)}
                  ,{<<"Account-ID">>, whapps_call:account_id(Call)}
                  ,{<<"Voicemail-Box">>, BoxId}
                  ,{<<"Voicemail-Name">>, MediaId}
                  ,{<<"Caller-ID-Number">>, get_caller_id_number(Call)}
                  ,{<<"Caller-ID-Name">>, get_caller_id_name(Call)}
                  ,{<<"Voicemail-Timestamp">>, new_timestamp()}
                  ,{<<"Voicemail-Length">>, Length}
                  ,{<<"Voicemail-Transcription">>, Transcription}
                  ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                  | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                 ],

    lager:debug("notifying of voicemail saved"),
    wh_amqp_worker:call_collect(NotifyProp
                                ,fun wapi_notifications:publish_voicemail/1
                                ,fun collecting/1
                                ,30 * ?MILLISECONDS_IN_SECOND
                               ).


-spec publish_voicemail_saved(pos_integer(), ne_binary(), whapps_call:call(), ne_binary(), gregorian_seconds()) -> 'ok'.
publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp) ->
    Prop = [{<<"From-User">>, whapps_call:from_user(Call)}
            ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
            ,{<<"To-User">>, whapps_call:to_user(Call)}
            ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
            ,{<<"Account-DB">>, whapps_call:account_db(Call)}
            ,{<<"Account-ID">>, whapps_call:account_id(Call)}
            ,{<<"Voicemail-Box">>, BoxId}
            ,{<<"Voicemail-Name">>, MediaId}
            ,{<<"Caller-ID-Number">>, get_caller_id_number(Call)}
            ,{<<"Caller-ID-Name">>, get_caller_id_name(Call)}
            ,{<<"Voicemail-Timestamp">>, Timestamp}
            ,{<<"Voicemail-Length">>, Length}
            ,{<<"Call-ID">>, whapps_call:call_id(Call)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_notifications:publish_voicemail_saved(Prop),
    lager:debug("published voicemail_saved for ~s", [BoxId]).

-spec collecting(wh_json:objects()) -> boolean().
collecting([JObj|_]) ->
    case wapi_notifications:notify_update_v(JObj)
        andalso wh_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_transcribe(ne_binary(), ne_binary(), boolean()) ->
                              api_object().
maybe_transcribe(AccountId, MediaId, 'true') ->
    Db = get_db(AccountId, MediaId),
    {'ok', MediaDoc} = kz_datamgr:open_doc(Db, MediaId),
    case wh_doc:attachment_names(MediaDoc) of
        [] ->
            lager:warning("no audio attachments on media doc ~s: ~p", [MediaId, MediaDoc]),
            'undefined';
        [AttachmentId|_] ->
            case kz_datamgr:fetch_attachment(Db, MediaId, AttachmentId) of
                {'ok', Bin} ->
                    lager:info("transcribing first attachment ~s", [AttachmentId]),
                    maybe_transcribe(Db, MediaDoc, Bin, wh_doc:attachment_content_type(MediaDoc, AttachmentId));
                {'error', _E} ->
                    lager:info("error fetching vm: ~p", [_E]),
                    'undefined'
            end
    end;
maybe_transcribe(_, _, 'false') -> 'undefined'.

-spec maybe_transcribe(ne_binary(), wh_json:object(), binary(), api_binary()) ->
                              api_object().
maybe_transcribe(_, _, _, 'undefined') -> 'undefined';
maybe_transcribe(_, _, <<>>, _) -> 'undefined';
maybe_transcribe(Db, MediaDoc, Bin, ContentType) ->
    case whapps_speech:asr_freeform(Bin, ContentType) of
        {'ok', Resp} ->
            lager:info("transcription resp: ~p", [Resp]),
            MediaDoc1 = wh_json:set_value(<<"transcription">>, Resp, MediaDoc),
            _ = kz_datamgr:ensure_saved(Db, MediaDoc1),
            is_valid_transcription(wh_json:get_value(<<"result">>, Resp)
                                   ,wh_json:get_value(<<"text">>, Resp)
                                   ,Resp
                                  );
        {'error', _E} ->
            lager:info("error transcribing: ~p", [_E]),
            'undefined'
    end.

-spec is_valid_transcription(api_binary(), binary(), wh_json:object()) ->
                                    api_object().
is_valid_transcription(<<"success">>, ?NE_BINARY, Resp) -> Resp;
is_valid_transcription(_Res, _Txt, _) ->
    lager:info("not valid transcription: ~s: '~s'", [_Res, _Txt]),
    'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the Universal Coordinated Time (UTC) reported by the
%% underlying operating system (local time is used if universal
%% time is not available) as number of gregorian seconds starting
%% with year 0.
%% @end
%%--------------------------------------------------------------------
-spec new_timestamp() -> gregorian_seconds().
new_timestamp() -> wh_util:current_tstamp().

-spec get_caller_id_name(whapps_call:call()) -> ne_binary().
get_caller_id_name(Call) ->
    CallerIdName = whapps_call:caller_id_name(Call),
    case whapps_call:kvs_fetch('prepend_cid_name', Call) of
        'undefined' -> CallerIdName;
        Prepend -> Pre = <<(wh_util:to_binary(Prepend))/binary, CallerIdName/binary>>,
                   wh_util:truncate_right_binary(Pre,
                           kzd_schema_caller_id:external_name_max_length())
    end.

-spec get_caller_id_number(whapps_call:call()) -> ne_binary().
get_caller_id_number(Call) ->
    CallerIdNumber = whapps_call:caller_id_number(Call),
    case whapps_call:kvs_fetch('prepend_cid_number', Call) of
        'undefined' -> CallerIdNumber;
        Prepend -> Pre = <<(wh_util:to_binary(Prepend))/binary, CallerIdNumber/binary>>,
                   wh_util:truncate_right_binary(Pre,
                           kzd_schema_caller_id:external_name_max_length())
    end.
