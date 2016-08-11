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
        ,fetch/2, message/2
        ,set_folder/3, change_folder/3, change_folder/4

        ,move_to_modb/4

        ,media_url/2
        ]).

-include("kz_voicemail.hrl").

-define(PVT_LEGACY_TYPE, <<"private_media">>).

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

    {MediaId, MediaUrl} = create_message_doc(kapps_call:account_id(Call), Props),

    Msg = io_lib:format("failed to store voicemail media ~s in voicemail box ~s of account ~s"
                       ,[MediaId, BoxId, kapps_call:account_id(Call)]
                       ),
    Funs = [{fun kapps_call:kvs_store/3, 'mailbox_id', BoxId}
           ,{fun kapps_call:kvs_store/3, 'attachment_name', AttachmentName}
           ,{fun kapps_call:kvs_store/3, 'media_id', MediaId}
           ,{fun kapps_call:kvs_store/3, 'media_length', Length}
           ],

    lager:debug("storing voicemail media recording ~s in doc ~s", [AttachmentName, MediaId]),
    case store_recording(AttachmentName, MediaUrl, kapps_call:exec(Funs, Call)) of
        'ok' ->
            notify_and_save_meta(Call, MediaId, Length, Props);
        {'error', Call1} ->
            lager:error(Msg),
            {'error', Call1, Msg}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary(), kazoo_data:docid()) -> db_ret().
fetch(AccountId, {_, ?MATCH_MODB_PREFIX(_, _, _)} = DocId) ->
    kvm_util:open_modb_doc(AccountId, DocId, kzd_box_message:type());
fetch(AccountId, ?MATCH_MODB_PREFIX(_, _, _) = DocId) ->
    kvm_util:open_modb_doc(AccountId, DocId, kzd_box_message:type());
fetch(AccountId, MediaId) ->
    case kvm_util:open_accountdb_doc(AccountId, MediaId, ?PVT_LEGACY_TYPE) of
        {'ok', MediaJObj} ->
            SourceId = kzd_box_message:source_id(MediaJObj),
            case kvm_messages:get_from_vmbox(AccountId, SourceId) of
                {'ok', VMBoxMsgs} ->
                    merge_metadata(MediaId, MediaJObj, VMBoxMsgs);
                {'error', _}=E ->
                    E
            end;
        {'error', _R}=E ->
            lager:warning("failed to load voicemail message ~s: ~p", [MediaId, _R]),
            E
    end.

-spec merge_metadata(ne_binary(), kz_json:object(), kz_json:objects()) -> db_ret().
merge_metadata(MediaId, MediaJObj, VMBoxMsgs) ->
    case kz_json:find_value(<<"media_id">>, MediaId, VMBoxMsgs) of
        'undefined' -> {'error', 'not_found'};
        Metadata -> {'ok', kzd_box_message:set_metadata(Metadata, MediaJObj)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc fetch message metadata from db, will take care of previous
%% message metadata in vmbox.
%% @end
%%--------------------------------------------------------------------
-spec message(ne_binary(), ne_binary()) -> db_ret().
message(AccountId, MessageId) ->
    case fetch(AccountId, MessageId) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Msg} -> {'ok', kzd_box_message:metadata(Msg)};
        {'error', _}=E ->
            E
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
        {'ok', _}=OK -> OK;
        {'error', _} -> {'error', Message}
    end.

-spec maybe_set_folder(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) -> db_ret().
maybe_set_folder(_, ?VM_FOLDER_DELETED=ToFolder, MessageId, AccountId, _) ->
    % ensuring that message is really deleted
    change_folder(ToFolder, MessageId, AccountId);
maybe_set_folder(FromFolder, FromFolder, ?MATCH_MODB_PREFIX(_, _, _), _, Msg) -> {'ok', Msg};
maybe_set_folder(FromFolder, FromFolder, MessageId, AccountId, _) ->
    lager:info("folder is same, but doc is in accountdb, move it to modb"),
    update(AccountId, 'undefined', MessageId, []);
maybe_set_folder(_FromFolder, ToFolder, MessageId, AccountId, _) ->
    change_folder(ToFolder, MessageId, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec change_folder(vm_folder(), ne_binary(), ne_binary()) -> db_ret().
-spec change_folder(vm_folder(), api_ne_binary(), ne_binary(), api_binary()) -> db_ret().
change_folder(Folder, MessageId, AccountId) ->
    change_folder(Folder, MessageId, AccountId, 'undefined').

change_folder(_, 'undefined', _, _) ->
    {'error', 'attachment_undefined'};
change_folder(Folder, MessageId, AccountId, BoxId) ->
    Fun = [fun(JObj) ->
               kvm_util:apply_folder(Folder, JObj)
           end
          ],
    case update(AccountId, BoxId, MessageId, Fun) of
        {'ok', J} -> {'ok', kzd_box_message:metadata(J)};
        {'error', R}=E ->
            lager:info("error while updating folder ~s ~p", [Folder, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Update a single message doc and do migration when necessary
%% @end
%%--------------------------------------------------------------------

-spec update(ne_binary(), api_ne_binary(), ne_binary(), update_funs()) -> db_ret().
update(AccountId, BoxId, MsgId, Funs) ->
    case fetch(AccountId, MsgId) of
        {'ok', JObj} ->
            case BoxId =:= 'undefined'
                     orelse kzd_box_message:source_id(JObj) =:= BoxId
            of
                'true' -> do_update(AccountId, MsgId, JObj, Funs);
                'false' -> {'error', 'not_found'}
            end;
        {'error', _}=E -> E
    end.

-spec do_update(ne_binary(), ne_binary(), kz_json:object(), update_funs()) -> db_ret().
do_update(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _)=MsgId, JObj, Funs) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    kvm_util:handle_update_result(MsgId, kazoo_modb:save_doc(AccountId, NewJObj, Year, Month));
do_update(AccountId, MsgId, JObj, Funs) ->
    kvm_util:handle_update_result(MsgId, move_to_modb(AccountId, JObj, Funs, 'true')).

%%--------------------------------------------------------------------
%% @public
%% @doc Move old voicemail media doc with message metadata to the MODB
%% @end
%%--------------------------------------------------------------------
-spec move_to_modb(ne_binary(), kz_json:object(), update_funs(), boolean()) -> db_ret().
move_to_modb(AccountId, JObj, Funs, 'true') ->
    case do_move_to_modb(AccountId, JObj, Funs) of
        {'ok', NJObj}=OK ->
            BoxId = kzd_box_message:source_id(NJObj),
            kvm_util:cleanup_moved_msgs(AccountId, BoxId, [kz_doc:id(JObj)]),
            OK;
        {'error', _}=E -> E
    end;
move_to_modb(AccountId, JObj, Funs, 'false') ->
    do_move_to_modb(AccountId, JObj, Funs).

-spec do_move_to_modb(ne_binary(), kz_json:object(), update_funs()) -> db_ret().
do_move_to_modb(AccountId, JObj, Funs) ->
    Created = kz_doc:created(JObj),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Created),

    FromDb = kvm_util:get_db(AccountId),
    FromId = kz_doc:id(JObj),
    ToDb = kazoo_modb:get_modb(AccountId, Year, Month),
    ToId = <<(kz_util:to_binary(Year))/binary
              ,(kz_util:pad_month(Month))/binary
              ,"-"
              ,(kz_util:rand_hex_binary(16))/binary
           >>,

    TransformFuns = [fun(DestDoc) -> kzd_box_message:set_metadata(kzd_box_message:metadata(JObj), DestDoc) end
                    ,fun(DestDoc) -> update_media_id(ToId, DestDoc) end
                    ,fun(DestDoc) -> kz_json:set_value(<<"pvt_moved_to_modb">>, <<"true">>, DestDoc) end
                    ,fun(DestDoc) -> kz_json:set_value(<<"pvt_previous_id">>, FromId, DestDoc) end
                    ,fun(DestDoc) -> kz_json:set_value(<<"pvt_type">>, kzd_box_message:type(), DestDoc) end
                     | Funs
                    ],
    Options = [{'transform', fun(_, B) -> lists:foldl(fun(F, J) -> F(J) end, B, TransformFuns) end}],
    try_move(FromDb, FromId, ToDb, ToId, Options, 3).

-spec try_move(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist(), non_neg_integer()) -> db_ret().
try_move(FromDb, FromId, ToDb, ToId, Options, Tries) ->
    case kz_datamgr:move_doc(FromDb, FromId, ToDb, ToId, Options) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} when Tries > 0 ->
            maybe_create_modb(ToDb),
            try_move(FromDb, FromId, ToDb, ToId, Options, Tries - 1);
        {'error', _}=Error -> Error
    end.

-spec maybe_create_modb(ne_binary()) -> 'ok'.
maybe_create_modb(MODb) ->
    case kz_datamgr:db_exists(MODb) of
        'true' -> 'ok';
        'false' -> kazoo_modb:create(MODb)
    end.

-spec update_media_id(ne_binary(), kz_json:object()) -> kz_json:object().
update_media_id(MediaId, JObj) ->
    Metadata = kzd_box_message:set_media_id(MediaId, kzd_box_message:metadata(JObj)),
    kzd_box_message:set_metadata(Metadata, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_url(ne_binary(), ne_binary() | kz_json:object()) -> binary().
media_url(AccountId, ?JSON_WRAPPER(_)=Message) ->
    media_url(AccountId, kzd_box_message:media_id(Message));
media_url(AccountId, MessageId) ->
    case fetch(AccountId, MessageId) of
        {'ok', Message} ->
            kz_media_url:playback(Message, Message);
        {'error', _} -> <<>>
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================

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
            JObj = kvm_util:get_completed_msg(JObjs),
            maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId);
        {'timeout', JObjs} ->
            case kvm_util:get_completed_msg(JObjs) of
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

-spec maybe_save_meta(pos_integer(), atom(), kapps_call:call(), ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
maybe_save_meta(Length, 'nothing', Call, MediaId, _UpdateJObj, BoxId) ->
    save_meta(Length, 'nothing', Call, MediaId, BoxId);

maybe_save_meta(Length, Action, Call, MediaId, UpdateJObj, BoxId) ->
    case kz_json:get_value(<<"Status">>, UpdateJObj) of
        <<"completed">> ->
            save_meta(Length, Action, Call, MediaId, BoxId);
        <<"failed">> ->
            lager:debug("attachment failed to send out via notification: ~s", [kz_json:get_value(<<"Failure-Message">>, UpdateJObj)]),
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
                       kvm_util:apply_folder(?VM_FOLDER_DELETED, JObj)
                   end
                  ],
            save_metadata(Metadata, AccountId, MediaId, Fun);
        'save' ->
            lager:debug("attachment was sent out via notification, saving media file"),
            Fun = [fun(JObj) ->
                       kvm_util:apply_folder(?VM_FOLDER_SAVED, JObj)
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
    case ?RETRY_CONFLICT(update(AccountId, 'undefined', MessageId, UpdateFuns)) of
        {'ok', _} -> 'ok';
        {'error', _R} ->
            lager:info("error while storing voicemail metadata: ~p", [_R])
    end.
