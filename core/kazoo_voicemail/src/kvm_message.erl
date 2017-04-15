%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Mailbox messages operation
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kvm_message).

-export([new/2, forward_message/4
        ,fetch/2, fetch/3, message/2, message/3
        ,set_folder/3, change_folder/3, change_folder/4

        ,update/3, update/4
        ,move_to_vmbox/4, do_move/5
        ,copy_to_vmboxes/4, copy_to_vmboxes/5

        ,media_url/2
        ]).

-include("kz_voicemail.hrl").

-export_type([vm_folder/0]).

-type new_msg_ret() :: {'ok', kapps_call:call()} | {'error', kapps_call:call(), any()}.

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
-spec new(kapps_call:call(), kz_proplist()) -> new_msg_ret().
new(Call, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    %% FIXME: dis guy is file size not audio duration
    Length = props:get_value(<<"Length">>, Props),
    AttachmentName = props:get_value(<<"Attachment-Name">>, Props),

    lager:debug("saving new ~bms voicemail media and metadata", [Length]),

    case create_new_message_doc(Call, Props) of
        {'error', _} ->
            Msg = io_lib:format("failed to create and save voicemail document for voicemail box ~s of account ~s"
                               ,[BoxId, kapps_call:account_id(Call)]
                               ),
            {'error', Call, Msg};
        {MessageId, MediaUrl} ->
            Msg = io_lib:format("failed to store voicemail media ~s in voicemail box ~s of account ~s"
                               ,[MessageId, BoxId, kapps_call:account_id(Call)]
                               ),
            Funs = [{fun kapps_call:kvs_store/3, 'mailbox_id', BoxId}
                   ,{fun kapps_call:kvs_store/3, 'attachment_name', AttachmentName}
                   ,{fun kapps_call:kvs_store/3, 'media_id', MessageId}
                   ,{fun kapps_call:kvs_store/3, 'media_length', Length}
                   ],

            lager:debug("storing voicemail media recording ~s in doc ~s", [AttachmentName, MessageId]),
            case store_recording(AttachmentName, MediaUrl, kapps_call:exec(Funs, Call), MessageId) of
                'ok' ->
                    notify_and_update_meta(Call, MessageId, Length, Props);
                {'error', Call1} ->
                    lager:error(Msg),
                    {'error', Call1, Msg}
            end
    end.

-spec forward_message(kapps_call:call(), kz_json:object(), ne_binary(), kz_proplist()) -> new_msg_ret().
forward_message(Call, Metadata, SrcBoxId, Props) ->
    case props:get_value(<<"Attachment-Name">>, Props) of
        'undefined' ->
            %% user chose to forward without prepending
            forward_to_vmbox(Call, Metadata, SrcBoxId, Props);
        _AttachmentName ->
            %% user chose to forward and prepend a messge
            new_forward_message(Call, Metadata, SrcBoxId, Props)
    end.

-spec new_forward_message(kapps_call:call(), kz_json:object(), ne_binary(), kz_proplist()) -> new_msg_ret().
new_forward_message(Call, Metadata, SrcBoxId, Props) ->
    DestBoxId = props:get_value(<<"Box-Id">>, Props),
    Length = props:get_value(<<"Length">>, Props),
    AttachmentName = props:get_value(<<"Attachment-Name">>, Props),

    lager:debug("saving new ~bms forward voicemail media and metadata", [Length]),

    case create_forward_message_doc(Call, Metadata, SrcBoxId, Props) of
        {'error', _} ->
            Msg = io_lib:format("failed to create and save voicemail document for forwarded message ~s to voicemail box ~s of account ~s"
                               ,[kzd_box_message:media_id(Metadata), DestBoxId, kapps_call:account_id(Call)]
                               ),
            {'error', Call, Msg};
        {ForwardId, MediaUrl} ->
            Msg = io_lib:format("failed to store forward voicemail media ~s in voicemail box ~s of account ~s"
                               ,[ForwardId, DestBoxId, kapps_call:account_id(Call)]
                               ),
            Funs = [{fun kapps_call:kvs_store/3, 'dest_mailbox_id', DestBoxId}
                   ,{fun kapps_call:kvs_store/3, 'attachment_name', AttachmentName}
                   ,{fun kapps_call:kvs_store/3, 'media_id', ForwardId}
                   ,{fun kapps_call:kvs_store/3, 'media_length', Length}
                   ],

            lager:debug("storing forward voicemail media recording ~s in doc ~s", [AttachmentName, ForwardId]),
            case store_recording(AttachmentName, MediaUrl, kapps_call:exec(Funs, Call), ForwardId) of
                'ok' ->
                    prepend_and_notify(Call, ForwardId, Metadata, SrcBoxId, Props);
                {'error', Call1} ->
                    lager:error(Msg),
                    {'error', Call1, Msg}
            end
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
        {'ok', JObj} ->
            case kvm_util:check_msg_belonging(BoxId, JObj) of
                'false' -> {'error', 'not_found'};
                'true' ->
                    {'ok', kvm_util:maybe_set_deleted_by_retention(JObj)}
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
            {'ok', kzd_box_message:metadata(JObj)};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Set a message folder, returning the new updated message on success
%% or the old message on failed update
%%
%% Note: for use only by cf_voicemail
%% @end
%%--------------------------------------------------------------------
-spec set_folder(ne_binary(), kz_json:object(), ne_binary()) -> db_ret().
set_folder(Folder, Message, AccountId) ->
    MessageId = kzd_box_message:media_id(Message),
    FromFolder = kzd_box_message:folder(Message, ?VM_FOLDER_NEW),
    lager:info("setting folder for message ~s to ~p", [MessageId, Folder]),
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
-spec change_folder(vm_folder(), message(), ne_binary()) -> db_ret().
-spec change_folder(vm_folder(), message(), ne_binary(), api_binary()) -> db_ret().
change_folder(Folder, Message, AccountId) ->
    change_folder(Folder, Message, AccountId, 'undefined').

change_folder(Folder, Message, AccountId, BoxId) ->
    Fun = [fun(J) -> kzd_box_message:apply_folder(Folder, J) end
          ],
    case update(AccountId, BoxId, Message, Fun) of
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
-spec update(ne_binary(), api_ne_binary(), message()) -> db_ret().
-spec update(ne_binary(), api_ne_binary(), message(), update_funs()) -> db_ret().
update(AccountId, BoxId, Message) ->
    update(AccountId, BoxId, Message, []).

update(AccountId, BoxId, ?NE_BINARY = MsgId, Funs) ->
    case fetch(AccountId, MsgId, BoxId) of
        {'ok', JObj} ->
            update(AccountId, BoxId, JObj, Funs);
        Error ->
            Error
    end;
update(AccountId, _BoxId, JObj, Funs) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    Db = kvm_util:get_db(AccountId, NewJObj),
    case kazoo_modb:save_doc(Db, NewJObj) of
        {'ok', _} = OK -> OK;
        {'error', _R} = Error ->
            lager:debug("failed to update voicemail message ~s: ~p", [kz_doc:id(NewJObj), _R]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc Move a message to another vmbox
%% @end
%%--------------------------------------------------------------------
-spec move_to_vmbox(ne_binary(), message(), ne_binary(), ne_binary()) ->
                           db_ret().
move_to_vmbox(AccountId, ?NE_BINARY = FromId, OldBoxId, NewBoxId) ->
    %% FIXME: maybe fetch message to make sure it's exists
    AccountDb = kvm_util:get_db(AccountId),
    case kz_datamgr:open_cache_doc(AccountDb, NewBoxId) of
        {'ok', NBoxJ} -> do_move(AccountId, FromId, OldBoxId, NewBoxId, NBoxJ);
        {'error', _Reason} = Error ->
            lager:debug("failed to open destination vmbox ~s", NewBoxId),
            Error
    end;
move_to_vmbox(AccountId, JObj, OldBoxId, NewBoxId) ->
    move_to_vmbox(AccountId, kzd_box_message:get_msg_id(JObj), OldBoxId, NewBoxId).

-spec do_move(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) -> db_ret().
do_move(AccountId, FromId, OldBoxId, NewBoxId, NBoxJ) ->
    {ToId, TransformFuns} = kvm_util:get_change_vmbox_funs(AccountId, NewBoxId, NBoxJ, OldBoxId, FromId),

    FromDb = kvm_util:get_db(AccountId, FromId),
    ToDb = kvm_util:get_db(AccountId, ToId),

    lager:debug("moving voicemail ~s/~s (vmbox ~s) to ~s/~s (vmbox ~s)"
               ,[FromDb, FromId, OldBoxId, ToDb, ToId, NewBoxId]
               ),

    Opts = [{'transform', fun(_, B) -> lists:foldl(fun(F, J) -> F(J) end, B, TransformFuns) end}],
    case kz_datamgr:move_doc(FromDb, {kzd_box_message:type(), FromId}, ToDb, ToId, Opts) of
        {'ok', _} = OK -> OK;
        {'error', 'not_found'} = NotFound ->
            lager:warning("could not copy ~s/~s to ~s/~s, modb is not exists (or maybe the original message is not exists)"
                         ,[FromDb, FromId, ToDb, ToId]
                         ),
            NotFound;
        {'error', _} = Error ->
            lager:debug("failed to copy ~s/~s to ~s/~s", [FromDb, FromId, ToDb, ToId]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc copy a message to other vmbox(es)
%% @end
%%--------------------------------------------------------------------
-spec copy_to_vmboxes(ne_binary(), ne_binary(), ne_binary(), ne_binary() | ne_binaries()) ->
                             kz_json:object().
copy_to_vmboxes(AccountId, Id, OldBoxId, ?NE_BINARY = NewBoxId) ->
    copy_to_vmboxes(AccountId, Id, OldBoxId, [NewBoxId]);
copy_to_vmboxes(AccountId, ?NE_BINARY = Id, OldBoxId, NewBoxIds) ->
    %% FIXME: maybe fetch message to make sure it's exists
    kz_json:from_list(
      dict:to_list(
        copy_to_vmboxes(AccountId, Id, OldBoxId, NewBoxIds, dict:new())
       )
     );
copy_to_vmboxes(AccountId, JObj, OldBoxId, NewBoxIds) ->
    copy_to_vmboxes(AccountId, kzd_box_message:get_msg_id(JObj), OldBoxId, NewBoxIds).

-spec copy_to_vmboxes(ne_binary(), ne_binary(), ne_binary(), ne_binaries(), dict:dict()) ->
                             dict:dict().
copy_to_vmboxes(_, _, _, [], CopiedDict) ->
    CopiedDict;
copy_to_vmboxes(AccountId, FromId, OldBoxId, [NBId | NBIds], CopiedDict) ->
    NewCopiedDict = copy_to_vmbox(AccountId, FromId, OldBoxId, NBId, CopiedDict),
    copy_to_vmboxes(AccountId, FromId, OldBoxId, NBIds, NewCopiedDict).

-spec copy_to_vmbox(ne_binary(), kz_json:object(), ne_binary(), ne_binary(), dict:dict()) ->
                           dict:dict().
copy_to_vmbox(AccountId, FromId, OldBoxId, ?NE_BINARY = NBId, CopiedDict) ->
    AccountDb = kvm_util:get_db(AccountId),
    %% FIXME: maybe bulk read vmbox in above function clause to avoid lots of cache query
    {OkErr, JObjError} = kz_datamgr:open_cache_doc(AccountDb, NBId),

    {ToId, TransformFuns} = kvm_util:get_change_vmbox_funs(AccountId, NBId, JObjError, OldBoxId),
    case OkErr =:= 'ok'
        andalso do_copy(AccountId, FromId, ToId, TransformFuns, 3)
    of
        {'ok', CopiedJObj} ->
            CopiedId = kz_doc:id(CopiedJObj),
            dict:append(<<"succeeded">>, CopiedId, CopiedDict);
        {'error', R} ->
            Failed = kz_json:from_list([{FromId, kz_term:to_binary(R)}]),
            dict:append(<<"failed">>, Failed, CopiedDict);
        'false' ->
            lager:warning("could not open destination vmbox ~s", [NBId]),
            Failed = kz_json:from_list([{FromId, kz_term:to_binary(JObjError)}]),
            dict:append(<<"failed">>, Failed, CopiedDict)
    end.

-spec do_copy(ne_binary(), ne_binary(), ne_binary(), update_funs(), 1..3) -> db_ret().
do_copy(AccountId, FromId, ToId, _Funs, 0) ->
    FromDb = kvm_util:get_db(AccountId, FromId),
    ToDb = kvm_util:get_db(AccountId, ToId),
    case fetch(AccountId, ToId) of
        {'ok', _}=OK -> OK; %% message was saved somehow(network glitch?), moving on
        {'error', _} ->
            lager:error("max retries to copy voicemail message ~s/~s to ~s/~s"
                       ,[FromDb, FromId, ToDb, ToId]
                       ),
            {'error', 'max_save_retries'}
    end;
do_copy(AccountId, FromId, ToId, Funs, Loop) ->
    FromDb = kvm_util:get_db(AccountId, FromId),
    ToDb = kvm_util:get_db(AccountId, ToId),

    lager:debug("copying voicemail ~s/~s to ~s/~s"
               ,[FromDb, FromId, ToDb, ToId]
               ),

    Opts = [{'transform', fun(_, B) -> lists:foldl(fun(F, J) -> F(J) end, B, Funs) end}],
    case kz_datamgr:copy_doc(FromDb, {kzd_box_message:type(), FromId}, ToDb, ToId, Opts) of
        {'ok', _} = OK -> OK;
        {'error', 'not_found'} = NotFound ->
            lager:warning("could not copy ~s/~s to ~s/~s, modb is not exists (or maybe the original message is not exists)"
                         ,[FromDb, FromId, ToDb, ToId]
                         ),
            NotFound;
        {'error', 'timeout'} ->
            do_copy(AccountId, FromId, ToId, Funs, Loop - 1);
        {'error', 'conflict'} ->
            Msg = io_lib:format("conflict occured during forwarding voicemail ~s / ~s to ~s / ~s"
                               ,[FromDb, FromId, ToDb, ToId]
                               ),
            Subject = <<"Conflict during forward voicemail message">>,
            send_system_alert('undefined', AccountId, Subject, kz_term:to_binary(Msg)),
            do_copy(AccountId, FromId, ToId, Funs, 0);
        {'error', _}=Error ->
            lager:debug("failed to copy ~s/~s to ~s/~s", [FromDb, FromId, ToDb, ToId]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_url(ne_binary(), message()) -> binary().
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
-type sotre_media_url() :: fun(() -> ne_binary() | {'error', any()}).

-spec create_new_message_doc(kapps_call:call(), kz_proplist()) ->
                                    {ne_binary(), sotre_media_url()} |
                                    {'error', any()}.
create_new_message_doc(Call, Props) ->
    AccountId = kapps_call:account_id(Call),
    JObj = kzd_box_message:new(AccountId, Props),

    Length = props:get_value(<<"Length">>, Props),
    CIDNumber = kvm_util:get_caller_id_number(Call),
    CIDName = kvm_util:get_caller_id_name(Call),
    Timestamp = kz_time:current_tstamp(),
    Metadata = kzd_box_message:build_metadata_object(Length, Call, kz_doc:id(JObj), CIDNumber, CIDName, Timestamp),

    MsgJObj = kzd_box_message:set_metadata(Metadata, JObj),

    AttachmentName = props:get_value(<<"Attachment-Name">>, Props),
    case try_save_document(Call, MsgJObj, 3) of
        {'ok', SavedJObj} ->
            MediaUrl = fun() -> kz_media_url:store(SavedJObj, AttachmentName) end,
            {kz_doc:id(SavedJObj), MediaUrl};
        {'error', _}=Error -> Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_forward_message_doc(kapps_call:call(), kz_json:object(), ne_binary(), kz_proplist()) ->
                                        {ne_binary(), sotre_media_url()} |
                                        {'error', any()}.
create_forward_message_doc(Call, Metadata, SrcBoxId, Props) ->
    AccountId = kapps_call:account_id(Call),
    JObj = kzd_box_message:set_metadata(Metadata, kzd_box_message:new(AccountId, Props)),

    NewBoxJObj = fake_vmbox_jobj(Call, Props),
    {_NewId, VMChangeFuns} = kvm_util:get_change_vmbox_funs(kapps_call:account_id(Call)
                                                           ,kz_doc:id(NewBoxJObj)
                                                           ,NewBoxJObj
                                                           ,SrcBoxId
                                                           ,kz_doc:id(JObj)
                                                           ),
    Updates = [fun(M) -> kz_json:set_value(<<"lenght">>, props:get_value(<<"Length">>, Props), M) end
               | VMChangeFuns
              ],
    MsgJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Updates),

    AttachmentName = props:get_value(<<"Attachment-Name">>, Props),
    case try_save_document(Call, MsgJObj, 3) of
        {'ok', SavedJObj} ->
            MediaUrl = fun() -> kz_media_url:store(SavedJObj, AttachmentName) end,
            {kz_doc:id(SavedJObj), MediaUrl};
        {'error', _}=Error -> Error
    end.

-spec try_save_document(kapps_call:call(), kz_json:object(), 1..3) -> db_ret().
try_save_document(_Call, MsgJObj, 0) ->
    case fetch(kz_doc:account_id(MsgJObj), kz_doc:id(MsgJObj)) of
        {'ok', _}=OK -> OK; %% message was saved somehow(network glitch?), moving on
        {'error', _} ->
            lager:error("max retries to save new voicemail message ~s in db ~s"
                       ,[kz_doc:id(MsgJObj), kz_doc:account_db(MsgJObj)]
                       ),
            {'error', 'max_save_retries'}
    end;
try_save_document(Call, MsgJObj, Loop) ->
    case kz_datamgr:save_doc(kz_doc:account_db(MsgJObj), MsgJObj) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            RetryFun = fun(J) -> try_save_document(Call, J, Loop - 1) end,
            retry_conflict(Call, MsgJObj, RetryFun);
        {'error', 'timeout'} ->
            try_save_document(Call, MsgJObj, Loop - 1);
        {'error', _Reason}=Error ->
            lager:error("failed to save voicemail message ~s in db ~s : ~p"
                       ,[kz_doc:id(MsgJObj), kz_doc:account_db(MsgJObj), _Reason]
                       ),
            Error
    end.

%% create a fake Destination Box JObj to pass to change vmbox functions
%% Note: set pvt_account_id and db just to make sure for case when timezone is not passed
%% so kzd_voicemail_box can find timezone from vmbox the owner or account
-spec fake_vmbox_jobj(kapps_call:call(), kz_proplist()) -> kz_json:object().
fake_vmbox_jobj(Call, Props) ->
    kz_json:from_list(
      [{<<"_id">>, props:get_value(<<"Box-Id">>, Props)}
      ,{<<"mailbox">>, props:get_value(<<"Box-Num">>, Props)}
      ,{<<"timezone">>, props:get_value(<<"Timezone">>, Props)}
      ,{<<"owner_id">>, props:get_value(<<"Owner-Id">>, Props)}
      ,{<<"pvt_account_id">>, kapps_call:account_id(Call)}
      ,{<<"pvt_account_db">>, kapps_call:account_db(Call)}
      ]
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary() | sotre_media_url(), kapps_call:call(), ne_binary()) ->
                             'ok' |
                             {'error', kapps_call:call()}.
store_recording(AttachmentName, Url, Call, MessageId) ->
    case kapps_call_command:store_file(<<"/tmp/", AttachmentName/binary>>, Url, Call) of
        'ok' -> 'ok';
        {'error', _R} ->
            lager:warning("error during storing voicemail recording ~s , checking attachment existence: ~p", [MessageId, _R]),
            check_attachment_exists(Call, MessageId)
    end.

-spec check_attachment_exists(kapps_call:call(), ne_binary()) -> 'ok' | {'error', kapps_call:call()}.
check_attachment_exists(Call, MessageId) ->
    case fetch(kapps_call:account_id(Call), MessageId) of
        {'ok', JObj} ->
            case kz_term:is_empty(kz_doc:attachments(JObj)) of
                'true' ->
                    {'error', Call};
                'false' ->
                    lager:debug("freeswitch returned error during store voicemail recording, but attachments is saved anyway")
            end;
        {'error', _R} ->
            lager:warning("failed to check attachment existence doc id ~s: ~p", [MessageId, _R]),
            {'error', Call}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec forward_to_vmbox(kapps_call:call(), kz_json:object(), ne_binary(), kz_proplist()) -> new_msg_ret().
forward_to_vmbox(Call, Metadata, SrcBoxId, Props) ->
    AccountId = kapps_call:account_id(Call),
    MediaId = kzd_box_message:media_id(Metadata),
    DestBoxId = props:get_value(<<"Box-Id">>, Props),
    Length = props:get_value(<<"Length">>, Props),
    Result = copy_to_vmboxes(AccountId, MediaId, SrcBoxId, DestBoxId),
    Failed = kz_json:get_value(<<"failed">>, Result, []),
    Succeeded = kz_json:get_value(<<"succeeded">>, Result, []),
    case {Failed, Succeeded} of
        {[], []} -> {'error', Call, 'internal_error'};
        {[], [ForwardId]} ->
            %%TODO: update lenght and caller_id
            notify_and_update_meta(Call, ForwardId, Length, Props);
        {[{_Id, Reason}], _} -> {'error', Call, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prepend_and_notify(kapps_call:call(), ne_binary(), kz_json:object(), ne_binary(), kz_proplist()) -> new_msg_ret().
prepend_and_notify(Call, ForwardId, Metadata, SrcBoxId, Props) ->
    Length = props:get_value(<<"Length">>, Props),
    try prepend_forward_message(Call, ForwardId, Metadata, SrcBoxId, Props) of
        {'ok', _} ->
            %%TODO: update lenght and caller_id
            notify_and_update_meta(Call, ForwardId, Length, Props);
        {'error', _R} ->
            %% prepend failed, so at least try to forward without a prepend message
            archive_malform_vm(Call, ForwardId, Props),
            forward_to_vmbox(Call, Metadata, SrcBoxId, Props)
    catch
        _T:_E ->
            ST = erlang:get_stacktrace(),
            lager:error("exception occured during prepend and forward message: ~p:~p", [_T, _E]),
            kz_util:log_stacktrace(ST),

            %% prepend failed, so at least try to forward without a prepend message
            archive_malform_vm(Call, ForwardId, Props),
            forward_to_vmbox(Call, Metadata, SrcBoxId, Props)
    end.

-spec prepend_forward_message(kapps_call:cal(), ne_binary(), kz_json:object(), ne_binary(), kz_proplist()) -> db_ret().
prepend_forward_message(Call, ForwardId, Metadata, _SrcBoxId, Props) ->
    lager:debug("trying to prepend a message to forwarded voicemail message ~s", [ForwardId]),
    AccountId = kapps_call:account_id(Call),

    lager:debug("saving prepend message ~s attachment to file system", [ForwardId]),
    TmpAttachmentName = props:get_ne_binary_value(<<"Attachment-Name">>, Props),
    {'ok', TmpPath} = write_attachment_to_file(AccountId, ForwardId, [TmpAttachmentName]),
    {'ok', _} = kz_datamgr:delete_attachment(kvm_util:get_db(AccountId, ForwardId), ForwardId, TmpAttachmentName),

    OrigMsgId = kzd_box_message:media_id(Metadata),
    lager:debug("saving original message ~s attachment to file system", [OrigMsgId]),
    {'ok', OrigPath} = write_attachment_to_file(AccountId, OrigMsgId),
    {'ok', OrigSampleRate} = kz_media_util:detect_file_sample_rate(OrigPath),

    TonePath = kz_binary:join([<<"/tmp/">>, <<(kz_binary:rand_hex(16))/binary, ".wav">>], <<>>),
    kz_media_util:synthesize_tone(OrigSampleRate, <<"440">>, <<"0.5">>, TonePath),

    lager:debug("joining prepend to original message"),
    case kz_media_util:join_media_files([TmpPath, TonePath, OrigPath], [{sample_rate, OrigSampleRate}]) of
        {'ok', FileContents} ->
            JoinFilename = <<(kz_binary:rand_hex(16))/binary, ".mp3">>,
            _ = [kz_util:delete_file(F) || F <- [TmpPath, OrigPath, TonePath]],
            %%TODO: update forwarded doc with lenght and media_filename
            try_put_fwd_attachment(AccountId, ForwardId, JoinFilename, FileContents, 3);
        {'error', _} ->
            _ = [kz_util:delete_file(F) || F <- [TmpPath, OrigPath, TonePath]],
            lager:warning("failed to join forward message media files"),
            {'error', 'join_failed'}

    end.

-spec try_put_fwd_attachment(ne_binary(), ne_binary(), ne_binary(), iodata(), 1..3) -> db_ret().
try_put_fwd_attachment(AccountId, ForwardId, _JoinFilename, _FileContents, 0) ->
    lager:error("max retries to save prepend forward voicemail attachmanet ~s in db ~s"
               ,[ForwardId, kvm_util:get_db(AccountId, ForwardId)]
               ),
    {'error', 'max_save_retries'};
try_put_fwd_attachment(AccountId, ForwardId, JoinFilename, FileContents, Loop) ->
    case kz_datamgr:put_attachment(kvm_util:get_db(AccountId, ForwardId), ForwardId, JoinFilename, FileContents) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            try_put_fwd_attachment(AccountId, ForwardId, JoinFilename, FileContents, Loop - 1);
        {'error', 'timeout'} ->
            try_put_fwd_attachment(AccountId, ForwardId, JoinFilename, FileContents, Loop - 1);
        {'error', _Reason}=Error ->
            lager:error("failed to save prepend forward voicemail message ~s in db ~s : ~p"
                       ,[ForwardId, kvm_util:get_db(AccountId, ForwardId), _Reason]),
            Error
    end.

-spec write_attachment_to_file(ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
write_attachment_to_file(AccountId, MessageId) ->
    case fetch(AccountId, MessageId) of
        {'ok', Doc} ->
            write_attachment_to_file(AccountId, MessageId, kz_doc:attachment_names(Doc));
        {'error', _}=Error -> Error
    end.

-spec write_attachment_to_file(ne_binary(), ne_binary(), ne_binaries()) -> {'ok', ne_binary()} | {'error', any()}.
write_attachment_to_file(AccountId, MessageId, [AttachmentId]) ->
    Db = kvm_util:get_db(AccountId, MessageId),
    {'ok', AttachmentBin} = kz_datamgr:fetch_attachment(Db, MessageId, AttachmentId),
    FilePath = kz_binary:join([<<"/tmp/_">>, AttachmentId], <<>>),
    kz_util:write_file(FilePath, AttachmentBin, ['write', 'binary']),
    {'ok', FilePath}.

-spec archive_malform_vm(kapps_call:call(), ne_binary(), kz_proplist()) -> 'ok'.
archive_malform_vm(Call, ForwardId, Props) ->
    AccountId = kapps_call:account_id(Call),
    BoxId = props:get_value(<<"Box-Id">>, Props),
    Db = kvm_util:get_db(AccountId, ForwardId),

    UpdateFuns = [fun(J) -> kz_json:set_value(<<"pvt_type">>, <<"vm_recovery">>, J) end],

    lager:debug("archiving the unsuccessful prepend voicemail ~s/~s for recovery later with pvt_type=vm_recovery"
               ,[ForwardId, Db]
               ),

    _ = update(AccountId, BoxId, ForwardId, UpdateFuns),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type notify_action() :: 'save' | 'delete' | 'nothing'.

-spec notify_and_update_meta(kapps_call:call(), ne_binary(), integer(), kz_proplist()) -> {'ok', kapps_call:call()}.
notify_and_update_meta(Call, MediaId, Length, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    NotifyAction = props:get_atom_value(<<"After-Notify-Action">>, Props),

    case kvm_util:publish_saved_notify(MediaId, BoxId, Call, Length, Props) of
        {'ok', JObjs} ->
            JObj = kvm_util:get_notify_completed_message(JObjs),
            log_notification_response(NotifyAction, MediaId, JObj, Call),
            maybe_update_meta(Length, NotifyAction, Call, MediaId, BoxId);
        {'timeout', JObjs} ->
            JObj = kvm_util:get_notify_completed_message(JObjs),
            log_notification_response(NotifyAction, MediaId, JObj, Call),
            maybe_update_meta(Length, 'nothing', Call, MediaId, BoxId);
        {'error', _R} ->
            AccountId = kapps_call:account_id(Call),
            lager:debug("failed to send new voicemail notification for message ~s in account ~s: ~p"
                       ,[MediaId, AccountId, _R]),
            maybe_update_meta(Length, 'nothing', Call, MediaId, BoxId)
    end.

-spec log_notification_response(notify_action(), ne_binary(), kz_json:object(), kapps_call:call()) -> 'ok'.
log_notification_response('nothing', _MediaId, _UpdateJObj, Call) ->
    AccountId = kapps_call:account_id(Call),
    lager:debug("successfully sent new voicemail notification for message ~s in account ~s: ~s"
               ,[_MediaId, AccountId, kz_json:encode(_UpdateJObj)]);
log_notification_response(_Action, MediaId, UpdateJObj, Call) ->
    AccountId = kapps_call:account_id(Call),
    case kz_json:get_value(<<"Status">>, UpdateJObj) of
        <<"completed">> ->
            lager:debug("successfully sent new voicemail notification for message ~s in account ~s: ~s"
                       ,[MediaId, AccountId, kz_json:encode(UpdateJObj)]);
        <<"failed">> ->
            lager:debug("failed to send new voicemail notification for message ~s in account ~s: ~s"
                       ,[MediaId
                        ,AccountId
                        ,kz_json:get_value(<<"Failure-Message">>, UpdateJObj)
                        ]);
        _ ->
            lager:info("failed to send new voicemail notification for message ~s in account ~s: timeout"
                      ,[MediaId, AccountId])
    end.

-spec maybe_update_meta(pos_integer(), notify_action(), kapps_call:call(), ne_binary(), ne_binary()) -> {'ok', kapps_call:call()}.
maybe_update_meta(Length, Action, Call, MediaId, BoxId) ->
    case Action of
        'delete' ->
            lager:debug("attachment was sent out via notification, set folder to delete"),
            Fun = [fun(JObj) ->
                           kzd_box_message:apply_folder({?VM_FOLDER_DELETED, 'false'}, JObj)
                   end
                  ],
            update_metadata(Call, BoxId, MediaId, Fun);
        'save' ->
            lager:debug("attachment was sent out via notification, set folder to saved"),
            Fun = [fun(JObj) ->
                           kzd_box_message:apply_folder(?VM_FOLDER_SAVED, JObj)
                   end
                  ],
            update_metadata(Call, BoxId, MediaId, Fun);
        'nothing' ->
            Timestamp = kz_time:current_tstamp(),
            kvm_util:publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp),
            {'ok', Call}
    end.

-spec update_metadata(kapps_call:call(), ne_binary(), ne_binary(), update_funs()) -> 'ok'.
update_metadata(Call, BoxId, MessageId, UpdateFuns) ->
    AccountId = kapps_call:account_id(Call),
    case update(AccountId, BoxId, MessageId, UpdateFuns) of
        {'ok', _} -> {'ok', Call};
        {'error', _R} ->
            lager:info("error while updating voicemail metadata: ~p", [_R]),
            {'ok', Call}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Double check document to make sure it's not exists in db
%% Using `kz_datamgr:ensure_save` is more effient here, but
%% we're doing this fetch/retry for proof of conecpt whether document's id
%% had collision or not.
%% @end
%%--------------------------------------------------------------------
-spec retry_conflict(kapps_call:call() | 'undefined', kz_json:object(), fun((kz_json:object()) -> db_ret())) -> db_ret().
retry_conflict(Call, JObj, DieAnotherDay) ->
    case fetch(kz_doc:account_id(JObj), kz_doc:id(JObj)) of
        {'ok', SavedJObj} -> check_for_collision(Call, JObj, SavedJObj, DieAnotherDay);
        {'error', 'timeout'} -> DieAnotherDay(JObj);
        {'error', 'not_found'} -> DieAnotherDay(JObj);
        {'error', _}=Error -> Error
    end.

-spec check_for_collision(kapps_call:call(), kz_json:object(), kz_json:object(), fun((kz_json:object()) -> db_ret())) -> db_ret().
check_for_collision(Call, JObj, SavedJObj, DieAnotherDay) ->
    OrigPublic = kz_doc:public_fields(JObj),
    SavedPublic = kz_doc:public_fields(SavedJObj),
    case kz_json:are_equal(OrigPublic, SavedPublic) of
        'true' ->
            Msg = io_lib:format("saving new voicemail ~s in account ~p resulted in conflict but it saved to db anyway"
                               ,[kz_doc:id(JObj), kz_doc:account_id(JObj)]
                               ),
            Subject = <<"Conflict during saving new voicemail message">>,
            send_system_alert(Call, kz_doc:account_id(JObj), Subject, kz_term:to_binary(Msg)),
            {'ok', SavedJObj};
        'false' ->
            Msg = io_lib:format("found document id collision during saving a new voicemail, id ~s account_id ~p"
                               ,[kz_doc:id(JObj), kz_doc:account_id(JObj)]
                               ),
            lager:critical(Msg),
            Subject = <<"Document ID collision detected">>,
            send_system_alert(Call, kz_doc:account_id(JObj), Subject, kz_term:to_binary(Msg)),
            NewId = give_me_another_id(kz_doc:id(JObj)),
            NewJObj = kzd_box_message:update_media_id(NewId, JObj),
            DieAnotherDay(kz_doc:set_id(NewJObj, NewId))
    end.

-spec give_me_another_id(ne_binary()) -> ne_binary().
give_me_another_id(?MATCH_MODB_PREFIX(Year, Month, _)) ->
    ?MODB_MSG_ID(Year, Month, kz_binary:rand_hex(16)).

-spec send_system_alert(kapps_call:call() | 'undefined', ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
send_system_alert('undefined', AccountId, Subject, Msg) ->
    Notify = [{<<"Message">>, Msg}
             ,{<<"Subject">>, <<"System Alert: ", Subject/binary>>}
             ,{<<"Account-ID">>, AccountId}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1);
send_system_alert(Call, AccountId, Subject, Msg) ->
    Notify = [{<<"Message">>, Msg}
             ,{<<"Subject">>, <<"System Alert: ", Subject/binary>>}
             ,{<<"Details">>, kapps_call:to_json(Call)}
             ,{<<"Account-ID">>, AccountId}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).
