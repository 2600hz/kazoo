%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Mailbox messages operation
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_message).

-export([new/2
        ,fetch/2, fetch/3, message/2, message/3
        ,set_folder/3, change_folder/3, change_folder/4

        ,update/3, update/4
        ,move_to_vmbox/4, copy_to_vmboxes/4, copy_to_vmboxes/5

        ,media_url/2
        ]).

-include("kz_voicemail.hrl").

-export_type([vm_folder/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc recieve and store a new voicemail message
%% expected options:
%% [{<<"Attachment-Name">>, AttachmentName}
%% ,{<<"Box-Id">>, BoxId}
%% ,{<<"OwnerId">>, OwnerId}
%% ,{<<"Length">>, Length}
%% ,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
%% ,{<<"After-Notify-Action">>, Action}
%% ,{<<"Attachment-Name">>, AttachmentName}
%% ,{<<"Box-Num">>, BoxNum}
%% ,{<<"Timezone">>, Timezone}
%% ]
%% @end
%%--------------------------------------------------------------------
-spec new(kapps_call:call(), kz_proplist()) -> any().
new(Call, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    Length = props:get_value(<<"Length">>, Props),
    AttachmentName = props:get_value(<<"Attachment-Name">>, Props),

    lager:debug("saving new ~bms voicemail media and metadata", [Length]),

    {MessageId, MediaUrl} = create_message_doc(kapps_call:account_id(Call), Props),

    Msg = io_lib:format("failed to store voicemail media ~s in voicemail box ~s of account ~s"
                       ,[MessageId, BoxId, kapps_call:account_id(Call)]
                       ),
    Funs = [{fun kapps_call:kvs_store/3, 'mailbox_id', BoxId}
           ,{fun kapps_call:kvs_store/3, 'attachment_name', AttachmentName}
           ,{fun kapps_call:kvs_store/3, 'media_id', MessageId}
           ,{fun kapps_call:kvs_store/3, 'media_length', Length}
           ],

    lager:debug("storing voicemail media recording ~s in doc ~s", [AttachmentName, MessageId]),
    case store_recording(AttachmentName, MediaUrl, kapps_call:exec(Funs, Call)) of
        'ok' ->
            notify_and_save_meta(Call, MessageId, Length, Props);
        {'error', Call1} ->
            lager:error(Msg),
            {'error', Call1, Msg}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), ne_binary()) -> db_ret().
-spec fetch(ne_binary(), ne_binary(), api_ne_binary()) -> db_ret().
fetch(AccountId, MessageId) ->
    fetch(AccountId, MessageId, 'undefined').

fetch(AccountId, MessageId, BoxId) ->
    case kvm_util:open_modb_doc(AccountId, MessageId, kzd_box_message:type()) of
        {'ok', JObj} = OK ->
            case kvm_util:check_msg_belonging(BoxId, JObj) of
                'true' -> OK;
                'false' -> {'error', 'not_found'}
            end;
        {'error', _E} = Error ->
            lager:debug("failed to open message ~s:~p", [MessageId, _E]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message metadata
%% @end
%%--------------------------------------------------------------------
-spec message(ne_binary(), ne_binary()) -> db_ret().
-spec message(ne_binary(), ne_binary(), api_ne_binary()) -> db_ret().
message(AccountId, MessageId) ->
    message(AccountId, MessageId, 'undefined').

message(AccountId, MessageId, BoxId) ->
    case fetch(AccountId, MessageId, BoxId) of
        {'ok', JObj} ->
            kzd_box_message:metadata(JObj);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Set a message folder, returning the new updated message on success
%% or the old message on failed update
%% @end
%%--------------------------------------------------------------------
-spec set_folder(ne_binary(), kz_json:object(), ne_binary()) -> db_ret().
set_folder(Folder, Message, AccountId) ->
    MessageId = kzd_box_message:media_id(Message),
    FromFolder = kzd_box_message:folder(Message, ?VM_FOLDER_NEW),
    lager:info("setting folder for message ~s to ~s", [MessageId, Folder]),
    case maybe_set_folder(FromFolder, Folder, MessageId, AccountId, Message) of
        {'ok', _} = OK -> OK;
        {'error', _} -> {'error', Message}
    end.

-spec maybe_set_folder(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) -> db_ret().
maybe_set_folder(_, ?VM_FOLDER_DELETED = ToFolder, MessageId, AccountId, _Msg) ->
    %% ensuring that message is really deleted
    change_folder(ToFolder, MessageId, AccountId);
maybe_set_folder(FromFolder, FromFolder, _MessageId, _AccountId, Msg) ->
    {'ok', Msg};
maybe_set_folder(_FromFolder, ToFolder, MessageId, AccountId, _Msg) ->
    change_folder(ToFolder, MessageId, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc Change message folder
%%    Note: if folder is {?VM_FOLDER_DELETED, 'true'}, it would move to
%%      deleted folder and marked as soft-deleted, otherwise it just move to deleted
%%      folder(for recovering later by user)
%% @end
%%--------------------------------------------------------------------
-spec change_folder(vm_folder(), ne_binary(), ne_binary()) -> db_ret().
-spec change_folder(vm_folder(), api_ne_binary(), ne_binary(), api_binary()) -> db_ret().
change_folder(Folder, MessageId, AccountId) ->
    change_folder(Folder, MessageId, AccountId, 'undefined').

change_folder(_, 'undefined', _, _) ->
    {'error', 'attachment_undefined'};
change_folder(Folder, MessageId, AccountId, BoxId) ->
    Fun = [fun(J) -> kzd_box_message:apply_folder(Folder, J) end
          ],
    case update(AccountId, BoxId, MessageId, Fun) of
        {'ok', JObj} ->
            {'ok', kzd_box_message:metadata(JObj)};
        {'error', _R} = Error ->
            lager:debug("failed to update message ~s folder to ~s: ~p", [Folder, _R]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Update a single message doc
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), api_ne_binary(), ne_binary()) -> db_ret().
-spec update(ne_binary(), api_ne_binary(), ne_binary(), update_funs()) -> db_ret().
update(AccountId, BoxId, MsgId) ->
    update(AccountId, BoxId, MsgId, []).

update(AccountId, BoxId, MsgId, Funs) ->
    apply_funs_and_save(AccountId, Funs, fetch(AccountId, MsgId, BoxId)).

%%--------------------------------------------------------------------
%% @public
%% @doc Move a message to another vmbox
%% @end
%%--------------------------------------------------------------------
-spec move_to_vmbox(ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                           kz_json:object().
move_to_vmbox(AccountId, MsgId, OldBoxId, NewBoxId) ->
    AccountDb = kvm_util:get_db(AccountId),
    {'ok', NBoxJ} = kz_datamgr:open_cache_doc(AccountDb, NewBoxId),
    Funs = ?CHANGE_VMBOX_FUNS(AccountId, NewBoxId, NBoxJ, OldBoxId),
    update(AccountId, OldBoxId, MsgId, Funs).

%%--------------------------------------------------------------------
%% @public
%% @doc copy a message to other vmbox(es)
%% @end
%%--------------------------------------------------------------------
-spec copy_to_vmboxes(ne_binary(), ne_binary(), ne_binary(), ne_binary() | ne_binaries()) ->
                             kz_json:object().
copy_to_vmboxes(AccountId, Id, OldBoxId, ?NE_BINARY = NewBoxId) ->
    copy_to_vmboxes(AccountId, Id, OldBoxId, [NewBoxId]);
copy_to_vmboxes(AccountId, Id, OldBoxId, NewBoxIds) ->
    case fetch(AccountId, Id, OldBoxId) of
        {'error', Error} ->
            Failed = kz_json:from_list([{Id, kz_util:to_binary(Error)}]),
            kz_json:from_list([{<<"failed">>, [Failed]}]);
        {'ok', JObj} ->
            Results = copy_to_vmboxes(AccountId, JObj, OldBoxId, NewBoxIds, dict:new()),
            kz_json:from_list(dict:to_list(Results))
    end.

-spec copy_to_vmboxes(ne_binary(), kz_json:object(), ne_binary(), ne_binaries(), dict:dict()) ->
                             kz_json:object().
copy_to_vmboxes(_, _, _, [], ResDict) -> ResDict;
copy_to_vmboxes(AccountId, JObj, OldBoxId, [NBId | NBIds], Copied) ->
    AccountDb = kvm_util:get_db(AccountId),
    {'ok', NBoxJ} = kz_datamgr:open_cache_doc(AccountDb, NBId),

    Funs = ?CHANGE_VMBOX_FUNS(AccountId, NBId, NBoxJ, OldBoxId),
    Id = kz_doc:id(JObj),
    NewCopied = case do_copy(AccountId, JObj, Funs) of
                    {'ok', JObj} ->
                        NewId = kz_doc:id(JObj),
                        dict:append(<<"succeeded">>, NewId, Copied);
                    {'error', R} ->
                        Failed = kz_json:from_list([{Id, kz_util:to_binary(R)}]),
                        dict:append(<<"failed">>, Failed, Copied)
                end,
    copy_to_vmboxes(AccountId, JObj, OldBoxId, NBIds, NewCopied).

-spec do_copy(ne_binary(), kz_json:object(), update_funs()) -> db_ret().
do_copy(AccountId, JObj, Funs) ->
    ?MATCH_MODB_PREFIX(Year, Month, _) = kz_doc:id(JObj),

    FromDb = kazoo_modb:get_modb(AccountId, Year, Month),
    FromId = kz_doc:id(JObj),
    ToDb = kazoo_modb:get_modb(AccountId),
    ToId = <<(kz_util:to_binary(Year))/binary
             ,(kz_util:pad_month(Month))/binary
             ,"-"
             ,(kz_util:rand_hex_binary(16))/binary
           >>,

    TransformFuns = [fun(DestDoc) -> kzd_box_message:update_media_id(ToId, DestDoc) end
                     | Funs
                    ],
    Options = [{'transform', fun(_, B) -> lists:foldl(fun(F, J) -> F(J) end, B, TransformFuns) end}],
    try_copy(FromDb, FromId, ToDb, ToId, Options, 3).

-spec try_copy(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist(), non_neg_integer()) -> db_ret().
try_copy(FromDb, FromId, ToDb, ToId, Options, Tries) ->
    case kz_datamgr:copy_doc(FromDb, FromId, ToDb, ToId, Options) of
        {'ok', _} = OK -> OK;
        {'error', 'not_found'} when Tries > 0 ->
            _ = maybe_create_modb(ToDb),
            try_copy(FromDb, FromId, ToDb, ToId, Options, Tries - 1);
        {'error', _}=Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_url(ne_binary(), ne_binary() | kz_json:object()) -> binary().
media_url(AccountId, ?NE_BINARY = MessageId) ->
    case fetch(AccountId, MessageId) of
        {'ok', Message} ->
            case kz_media_url:playback(Message, Message) of
                {'error', _} -> <<>>;
                Url -> Url
            end;
        {'error', _} -> <<>>
    end;
media_url(AccountId, Message) ->
    media_url(AccountId, kzd_box_message:media_id(Message)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec apply_funs_and_save(ne_binary(), update_funs(), db_ret() | kz_json:object()) -> db_ret().
apply_funs_and_save(_AccountId, _Funs, {'error', _} = Error) ->
    Error;
apply_funs_and_save(AccountId, Funs, {'ok', JObj}) ->
    apply_funs_and_save(AccountId, Funs, JObj);
apply_funs_and_save(AccountId, Funs, JObj) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    Db = kvm_util:get_db(AccountId, JObj),
    case kazoo_modb:save_doc(Db, NewJObj) of
        {'ok', _} = OK -> OK;
        {'error', _R} = Error ->
            lager:debug("failed to update voicemail message ~s: ~p", [kz_doc:id(NewJObj), _R]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_create_modb(ne_binary()) -> boolean().
maybe_create_modb(MODb) ->
    case kz_datamgr:db_exists(MODb) of
        'true' -> 'false';
        'false' ->
            kazoo_modb:create(MODb),
            'true'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_message_doc(ne_binary(), kz_proplist()) -> {ne_binary(), ne_binary()}.
create_message_doc(AccountId, Props) ->
    MsgJObj = kzd_box_message:new(AccountId, Props),
    {'ok', SavedJObj} = kz_datamgr:save_doc(kz_doc:account_db(MsgJObj), MsgJObj),

    MediaUrl = kz_media_url:store(SavedJObj
                                 ,props:get_value(<<"Attachment-Name">>, Props)
                                 ),

    {kz_doc:id(MsgJObj), MediaUrl}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary(), kapps_call:call()) ->
                             'ok' |
                             {'error', kapps_call:call()}.
store_recording(AttachmentName, Url, Call) ->
    lager:debug("storing recording ~s at ~s", [AttachmentName, Url]),
    case kapps_call_command:store_file(<<"/tmp/", AttachmentName/binary>>, Url, Call) of
        'ok' -> 'ok';
        {'error', _} -> {'error', Call}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notify_and_save_meta(kapps_call:call(), ne_binary(), integer(), kz_proplist()) -> 'ok'.
notify_and_save_meta(Call, MediaId, Length, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    NotifyAction = props:get_atom_value(<<"After-Notify-Action">>, Props),

    case kvm_util:publish_saved_notify(MediaId, BoxId, Call, Length, Props) of
        {'ok', JObjs} ->
            JObj = kvm_util:get_notify_completed_message(JObjs),
            maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId);
        {'timeout', JObjs} ->
            JObj = kvm_util:get_notify_completed_message(JObjs),
            maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId);
        {'error', _E} ->
            lager:debug("voicemail new notification error: ~p", [_E]),
            save_meta(Length, NotifyAction, Call, MediaId, BoxId)
    end.

-spec maybe_save_meta(pos_integer(), atom(), kapps_call:call(), ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
maybe_save_meta(Length, 'nothing', Call, MediaId, _UpdateJObj, BoxId) ->
    save_meta(Length, 'nothing', Call, MediaId, BoxId);

maybe_save_meta(Length, Action, Call, MediaId, UpdateJObj, BoxId) ->
    case kz_json:get_value(<<"Status">>, UpdateJObj) of
        <<"completed">> ->
            save_meta(Length, Action, Call, MediaId, BoxId);
        <<"failed">> ->
            lager:debug("attachment failed to send out via notification: ~s", [kz_json:get_value(<<"Failure-Message">>, UpdateJObj)]),
            save_meta(Length, Action, Call, MediaId, BoxId);
        _ ->
            lager:info("timed out waiting for voicemail new notification resp"),
            save_meta(Length, Action, Call, MediaId, BoxId)
    end.

-spec save_meta(pos_integer(), atom(), kapps_call:call(), ne_binary(), ne_binary()) -> 'ok'.
save_meta(Length, Action, Call, MediaId, BoxId) ->
    AccountId = kapps_call:account_id(Call),
    CIDNumber = kvm_util:get_caller_id_number(Call),
    CIDName = kvm_util:get_caller_id_name(Call),
    Timestamp = kz_util:current_tstamp(),

    Metadata = kzd_box_message:build_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, Timestamp),

    case Action of
        'delete' ->
            lager:debug("attachment was sent out via notification, deleteing media file"),
            Fun = [fun(JObj) ->
                           kzd_box_message:apply_folder(?VM_FOLDER_DELETED, JObj)
                   end
                  ],
            save_metadata(Metadata, AccountId, MediaId, Fun);
        'save' ->
            lager:debug("attachment was sent out via notification, saving media file"),
            Fun = [fun(JObj) ->
                           kzd_box_message:apply_folder(?VM_FOLDER_SAVED, JObj)
                   end
                  ],
            save_metadata(Metadata, AccountId, MediaId, Fun);
        'nothing' ->
            save_metadata(Metadata, AccountId, MediaId, []),
            lager:debug("stored voicemail metadata for ~s", [MediaId]),
            kvm_util:publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp)
    end.

-spec save_metadata(kz_json:object(), ne_binary(), ne_binary(), update_funs()) -> 'ok'.
save_metadata(NewMessage, AccountId, MessageId, Funs) ->
    UpdateFuns = [fun(JObj) ->
                          kzd_box_message:set_metadata(NewMessage, JObj)
                  end
                  | Funs
                 ],
    case update(AccountId, 'undefined', MessageId, UpdateFuns) of
        {'ok', _} -> 'ok';
        {'error', _R} ->
            lager:info("error while storing voicemail metadata: ~p", [_R])
    end.
