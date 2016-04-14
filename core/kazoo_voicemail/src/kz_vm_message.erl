%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Voice mail message
%%% @end
%%% @contributors
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(kz_vm_message).

-export([get_message_doc/2
         ,get_metadata/2, get_messages_metadata/2
         ,media_url/2
         ,set_folder/3, update_folder/3

         ,new_message/5
         ,save_metadata/3
         ,count_all/2

         ,get_db/1
        ]).

-include("kz_voicemail.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(LISTING_BY_MAILBOX, <<"vm_messages/listing_by_mailbox">>).
-define(COUNT_VIEW, <<"vm_messages/count_per_folder">>).

get_db(AccountId) ->
    wh_util:format_account_id(AccountId, 'encoded').

%%--------------------------------------------------------------------
%% @private
%% @doc recieve a new voicemail message and store it
%% expected options:
%%  [{<<"AttachmentName">>, AttachmentName}
%%    ,{<<"Box-Id">>, BoxId}
%%    ,{<<"OwnerId">>, OwnerId}
%%    ,{<<"Length">>, Length}
%%    ,{<<"Transcribe-Voicemail">>, MaybeTranscribe}
%%    ,{<<"After-Notify-Action">>, Action}
%%    ,{<<"Default-Storage">>, ?MAILBOX_DEFAULT_STORAGE}
%%    ,{<<"Default-Extension">>, ?DEFAULT_VM_EXTENSION}
%%    ,{<<"Retry-Storage-Times">>, ?MAILBOX_RETRY_STORAGE_TIMES(AccountId)}
%%    ,{<<"Retry-Local-Storage">>, ?MAILBOX_RETRY_LOCAL_STORAGE_REMOTE_FAILS(AccountId)}
%%    ,{<<"Defualt-Min-MSG-Length">>, min_recording_length(Call)}
%%  ]
%% @end
%%--------------------------------------------------------------------
-spec new_message(ne_binary(), ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) -> any().
new_message(AttachmentName, BoxNum, Timezone, Call, Props) ->
    AccountId = whapps_call:account_id(Call),
    BoxId = props:get_value(<<"Box-Id">>, Props),
    Length = props:get_value(<<"Length">>, Props),
    DefaultExtension = props:get_value(<<"Default-Extension">>, Props),
    DefaultStorage = props:get_value(<<"Default-Storage">>, Props),

    lager:debug("saving new ~bms voicemail media and metadata", [Length]),

    {MediaId, MediaUrl} = create_message_doc(AccountId, BoxNum, BoxId, AttachmentName, Timezone, DefaultExtension, DefaultStorage),

    case store_recording(AttachmentName, MediaId, MediaUrl, Call, Props, DefaultStorage) of
        'true' -> update_mailbox(Call, MediaId, Length, Props);
        'false' ->
            lager:warning("failed to store voicemail media: ~p", [MediaId]),
            kz_datamgr:del_doc(whapps_call:account_db(Call), MediaId);
        {'error', Call1} ->
            Msg = io_lib:format("failed to store voicemail media ~s in voicemail box ~s of account ~s"
                                ,[MediaId, BoxId, whapps_call:account_id(Call1)]
                               ),
            lager:critical(Msg),
            Funs = [{fun whapps_call:kvs_store/3, 'mailbox_id', BoxId}
                    ,{fun whapps_call:kvs_store/3, 'attachment_name', AttachmentName}
                    ,{fun whapps_call:kvs_store/3, 'media_id', MediaId}
                    ,{fun whapps_call:kvs_store/3, 'media_length', Length}
                   ],
            {'error', whapps_call:exec(Funs, Call1), Msg}
    end.

-spec create_message_doc(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
create_message_doc(AccountId, BoxNum, BoxId, AttachmentName, Timezone, DefaultExtension, ExternalMediaUrl) ->
    Db = get_db(AccountId),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Doc = kzd_voice_message:new(AccountDb, BoxNum, BoxId, AttachmentName, Timezone, DefaultExtension, ExternalMediaUrl),
    {'ok', JObj} = kz_datamgr:save_doc(Db, Doc),

    MediaId = wh_doc:id(JObj),
    MediaUrl = kz_datamgr:attachment_url(Db, MediaId, AttachmentName, [{'doc_type', <<"voice_message">>}]),

    {MediaId, MediaUrl}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) -> boolean() | 'error'.
-spec store_recording(ne_binary(), ne_binary(), ne_binary(), whapps_call:call(), wh_proplist(), api_binary()) -> boolean() | 'error'.
store_recording(AttachmentName, DocId, Url, Call, Props) ->
    lager:debug("storing voicemail media recording ~s in doc ~s", [AttachmentName, DocId]),
    case try_store_recording(AttachmentName, DocId, Url, Call, Props) of
        'ok' ->
            check_attachment_length(AttachmentName, DocId, Call, Props);
        {'error', _}=Err ->
            Err
    end.

store_recording(AttachmentName, DocId, Url, Call, Props, _) ->
    store_recording(AttachmentName, DocId, Url, Call, Props).
% store_recording(AttachmentName, DocId, Url, Call, Props, StorageUrl) ->
%     Url = get_media_url(AttachmentName, DocId, Call, OwnerId, StorageUrl),
%     lager:debug("storing recording ~s at ~s", [AttachmentName, Url]),
%     case try_store_recording(AttachmentName, DocId, Url, Call, Props) of
%         'ok' ->
%             case update_doc(<<"external_media_url">>, Url, DocId, Call) of
%                 'ok' -> 'true';
%                 {'error', _}=Err -> Err
%             end;
%         {'error', _}=Err ->
%             case ?MAILBOX_RETRY_LOCAL_STORAGE_REMOTE_FAILS(whapps_call:account_id(Call)) of
%                 'true' -> store_recording(AttachmentName, DocId, Url, Call, Props);
%                 'false' -> Err
%             end
%     end.

-spec try_store_recording(ne_binary(), ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) ->
                                 'ok' | {'error', whapps_call:call()}.
-spec try_store_recording(ne_binary(), ne_binary(), ne_binary(), integer(), whapps_call:call(), wh_proplist()) ->
                                 'ok' | {'error', whapps_call:call()}.
try_store_recording(AttachmentName, DocId, Url, Call, Props) ->
    Tries = props:get_value(<<"Retry-Storage-Times">>, Props),
    Funs = [{fun whapps_call:kvs_store/3, 'media_url', Url}],
    try_store_recording(AttachmentName, DocId, Url, Tries, whapps_call:exec(Funs, Call), Props).

try_store_recording(_, _, _, 0, Call, _) -> {'error', Call};
try_store_recording(AttachmentName, DocId, Url, Tries, Call, Props) ->
    case whapps_call_command:b_store_vm(AttachmentName, Url, <<"put">>, [wh_json:new()], 'true', Call) of
        {'ok', JObj} ->
            verify_stored_recording(AttachmentName, DocId, Url, Tries, Call, JObj, Props);
        Other ->
            lager:error("error trying to store voicemail media, retrying ~B more times", [Tries - 1]),
            retry_store(AttachmentName, DocId, Url, Tries, Call, Other, Props)
    end.

% -spec get_media_url(ne_binary(), ne_binary(), whapps_call:call(), api_binary(), ne_binary()) -> ne_binary().
% get_media_url(AttachmentName, DocId, Call, OwnerId, StorageUrl) ->
%     AccountId = whapps_call:account_id(Call),
%     <<StorageUrl/binary
%       ,"/", AccountId/binary
%       ,"/", (wh_util:to_binary(OwnerId))/binary
%       ,"/", DocId/binary
%       ,"/", AttachmentName/binary
%     >>.

-spec retry_store(ne_binary(), ne_binary(), ne_binary(), pos_integer(), whapps_call:call(), any(), wh_proplist()) ->
                         'ok' | {'error', whapps_call:call()}.
retry_store(AttachmentName, DocId, Url, Tries, Call, Error, Props) ->
    timer:sleep(2000),
    Call1 = whapps_call:kvs_store('error_details', Error, Call),
    try_store_recording(AttachmentName, DocId, Url, Tries - 1, Call1, Props).

-spec verify_stored_recording(ne_binary(), ne_binary(), ne_binary(), pos_integer(), whapps_call:call(), wh_json:object(), wh_proplist()) ->
                                     'ok' |
                                     {'error', whapps_call:call()}.
verify_stored_recording(AttachmentName, DocId, Url, Tries, Call, JObj, Props) ->
    case wh_json:get_value(<<"Application-Response">>, JObj) of
        <<"success">> ->
            lager:debug("storing ~s into ~s was successful", [AttachmentName, DocId]);
        _Response ->
            case check_attachment_length(AttachmentName, DocId, Call, Props) of
                'true' ->
                    lager:debug("attachment ~s exists on ~s, saved!", [AttachmentName, DocId]);
                'false' ->
                    lager:debug("attachment ~s isn't on ~s, retry necessary", [AttachmentName, DocId]),
                    retry_store(AttachmentName, DocId, Url, Tries, Call, JObj, Props);
                {'error', Call1} ->
                    lager:debug("error fetching ~s, will retry store", [DocId]),
                    retry_store(AttachmentName, DocId, Url, Tries, Call1, JObj, Props)
            end
    end.

-spec check_attachment_length(ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) ->
                                     boolean() |
                                     {'error', whapps_call:call()}.
check_attachment_length(AttachmentName, DocId, Call, Props) ->
    AccountId = whapps_call:account_id(Call),
    MinLength = props:get_value(<<"Defualt-Min-MSG-Length">>, Props),

    case get_message_doc(AccountId, DocId) of
        {'ok', JObj} ->
            case wh_doc:attachment_length(JObj, AttachmentName) of
                'undefined' ->
                    Err = io_lib:format("voicemail media ~s is missing from doc ~s", [AttachmentName, DocId]),
                    lager:debug(Err),
                    {'error', whapps_call:kvs_store('error_details', {'error', Err}, Call)};
                AttachmentLength ->
                    lager:info("voicemail media length is ~B and must be larger than ~B to be stored", [AttachmentLength, MinLength]),
                    is_integer(AttachmentLength) andalso AttachmentLength > MinLength
            end;
        {'error', _}=Err ->
            {'error', whapps_call:kvs_store('error_details', Err, Call) }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_mailbox(whapps_call:call(), ne_binary(), integer(), wh_proplist()) ->
                            'ok'.
update_mailbox(Call, MediaId, Length, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    NotifyAction = props:get_value(<<"After-Notify-Action">>, Props),

    case publish_voicemail_saved_notify(MediaId, BoxId, Call, Length, Props) of
        {'ok', JObjs} ->
            JObj = get_completed_msg(JObjs),
            maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId);
        {'timeout', JObjs} ->
            case get_completed_msg(JObjs) of
                ?EMPTY_JSON_OBJECT ->
                    lager:info("timed out waiting for resp"),
                    save_meta(Length, Call, MediaId, BoxId);
                JObj ->
                    maybe_save_meta(Length, NotifyAction, Call, MediaId, JObj, BoxId)
            end;
        {'error', _E} ->
            lager:debug("notification error: ~p", [_E]),
            save_meta(Length, Call, MediaId, BoxId)
    end.

-spec collecting(wh_json:objects()) -> boolean().
collecting([JObj|_]) ->
    case wapi_notifications:notify_update_v(JObj)
        andalso wh_json:get_value(<<"Status">>, JObj)
    of
        <<"completed">> -> 'true';
        <<"failed">> -> 'true';
        _ -> 'false'
    end.

-spec get_completed_msg(wh_json:objects()) -> wh_json:object().
-spec get_completed_msg(wh_json:objects(), wh_json:object()) -> wh_json:object().
get_completed_msg(JObjs) ->
    get_completed_msg(JObjs, wh_json:new()).

get_completed_msg([], Acc) -> Acc;
get_completed_msg([JObj|JObjs], Acc) ->
    case wh_json:get_value(<<"Status">>, JObj) of
        <<"completed">> -> get_completed_msg([], JObj);
        _ -> get_completed_msg(JObjs, Acc)
    end.

-spec maybe_save_meta(pos_integer(), ne_binary(), whapps_call:call(), ne_binary(), wh_json:object(), ne_binary()) -> 'ok'.
maybe_save_meta(Length, 'nothing', Call, MediaId, _UpdateJObj, BoxId) ->
    save_meta(Length, Call, MediaId, BoxId);

maybe_save_meta(Length, Action, Call, MediaId, UpdateJObj, BoxId) ->
    case wh_json:get_value(<<"Status">>, UpdateJObj) of
        <<"completed">> ->
            case Action of
                'delete' ->
                    lager:debug("attachment was sent out via notification, deleteing media file"),
                    {'ok', _} = kzv_media:del_doc(whapps_call:account_id(Call), MediaId);
                'save' ->
                    lager:debug("attachment was sent out via notification, saving media file"),
                    update_folder(?VM_FOLDER_SAVED, MediaId, whapps_call:account_id(Call))
            end;
        <<"failed">> ->
            lager:debug("attachment failed to send out via notification: ~s", [wh_json:get_value(<<"Failure-Message">>, UpdateJObj)]),
            save_meta(Length, Call, MediaId, BoxId)
    end.


-spec save_meta(pos_integer(), whapps_call:call(), ne_binary(), ne_binary()) -> 'ok'.
save_meta(Length, Call, MediaId, BoxId) ->
    CIDNumber = get_caller_id_number(Call),
    CIDName = get_caller_id_name(Call),
    Timestamp = new_timestamp(),
    ExternalMediaUrl = external_media_url(whapps_call:account_id(Call), MediaId),
    Metadata = kzd_voice_message:create_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, ExternalMediaUrl, Timestamp),

    {'ok', _BoxJObj} = save_metadata(Metadata, whapps_call:account_id(Call), MediaId),
    lager:debug("stored voicemail metadata for ~s", [MediaId]),

    publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_message_doc(ne_binary(), ne_binary()) -> wh_json:objects().
get_message_doc(AccountId, MessageId) ->
    Db = get_db(AccountId),
    case kz_datamgr:open_doc(Db, MessageId) of
        {'ok', _}=OK -> OK;
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_messages_metadata(ne_binary(), ne_binary()) -> wh_json:objects().
get_messages_metadata(AccountId, BoxId) ->
    Db = get_db(AccountId),
    ViewOpts = [{'key', BoxId}, 'include_docs'],
    case kz_datamgr:get_results(Db, ?LISTING_BY_MAILBOX, ViewOpts) of
        {'ok', Msgs} -> [kzd_voice_message:metadata(wh_json:get_value(<<"doc">>, Msg)) || Msg <- Msgs];
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_metadata(ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            {'error', any()}.
get_metadata(AccountId, MessageId) ->
    case get_message_doc(AccountId, MessageId) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Msg} -> {'ok', kzd_voice_message:metadata(Msg)};
        {'error', _}=E ->
            lager:warning("failed to load voicemail message ~s", [MessageId]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_metadata(wh_json:object(), ne_binary(), ne_binary()) ->
                           {'ok', wh_json:object()} |
                           {'error', atom()}.
save_metadata(NewMessage, AccountId, MessageId) ->
    Db = get_db(AccountId),
    {'ok', JObj} = kz_datamgr:open_doc(Db, MessageId),
    case kz_datamgr:save_doc(Db, kzd_voice_message:set_metadata(NewMessage, JObj)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'conflict'} ->
            lager:info("saving resulted in a conflict, trying again"),
            save_metadata(NewMessage, AccountId, MessageId);
        {'error', R}=E ->
            lager:info("error while storing voicemail metadata: ~p", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_url(ne_binary(), ne_binary() | wh_json:object()) -> binary().
media_url(AccountId, ?JSON_WRAPPER(_)=Message) ->
    media_url(AccountId, kzd_voice_message:media_id(Message));
media_url(AccountId, MessageId) ->
    case get_metadata(AccountId, MessageId) of
        {'ok', Message} ->
            kzd_voice_message:external_media_url(Message, list_to_binary(["/", get_db(AccountId), "/", MessageId]));
        {'error', _} -> <<>>
    end.

-spec external_media_url(ne_binary(), ne_binary()) -> api_binary().
external_media_url(AccountId, MediaId) ->
    case get_message_doc(AccountId, MediaId) of
        {'ok', JObj} -> kzd_voice_message:external_media_url(JObj);
        {'error', _} -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_folder(ne_binary(), ne_binary(), ne_binary()) -> any().
set_folder(Folder, Message, AccountId) ->
    MessageId = kzd_voice_message:media_id(Message),
    lager:info("setting folder for message ~s to ~s", [MessageId, Folder]),
    not (kzd_voice_message:folder(Message) =:= Folder) andalso
        update_folder(Folder, MessageId, AccountId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_folder(ne_binary(), ne_binary(), ne_binary()) ->
                           {'ok', wh_json:object()} |
                           {'error', any()}.
update_folder(_, 'undefined', _) ->
    {'error', 'attachment_undefined'};
update_folder(Folder, MessageId, AccountId) ->
    Db = get_db(AccountId),
    case get_message_doc(AccountId, MessageId) of
        {'ok', JObj} ->
            case kz_datamgr:save_doc(Db, update_folder1(JObj, Folder)) of
                {'error', 'conflict'} ->
                    update_folder(Folder, MessageId, AccountId);
                {'ok', _}=OK ->
                    OK;
                {'error', R}=E ->
                    lager:info("error while updating folder ~s ~p", [Folder, R]),
                    E
            end;
        {'error', R}=E ->
            lager:info("failed to open voicemail message ~s: ~p", [MessageId, R]),
            E
    end.

-spec update_folder1(wh_json:object(), ne_binary()) -> wh_json:object().
update_folder1(Doc, <<"deleted">>) ->
    Metadata = kzd_voice_message:set_folder_deleted(kzd_voice_message:metadata(Doc)),
    wh_json:set_value(<<"pvt_deleted">>, <<"true">>, kzd_voice_message:set_metadata(Metadata, Doc));
update_folder1(Doc, Folder) ->
    Metadata = kzd_voice_message:set_folder(Folder, kzd_voice_message:metadata(Doc)),
    kzd_voice_message:set_metadata(Metadata, Doc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec count_all(ne_binary(), ne_binary()) -> non_neg_integer().
count_all(AccountId, BoxId) ->
    case get_count_per_folder(AccountId, BoxId, [?VM_FOLDER_NEW, ?VM_FOLDER_SAVED]) of
        {'ok', []} -> 0;
        {'ok', JObjs} -> kzd_voice_message:count_messages(JObjs);
        {'error', _} -> 0
    end.

-spec get_count_per_folder(ne_binary(), ne_binary(), ne_binary() | ne_binaries()) ->
                                {'ok', wh_json:objects()} |
                                {'error', any()}.
get_count_per_folder(AccountId, BoxId, <<_/binary>>=Folder) ->
    get_count_per_folder(AccountId, BoxId, [Folder]);
get_count_per_folder(AccountId, BoxId, Folders) ->
    Db = get_db(AccountId),
    ViewOpts = create_folder_view_option(BoxId, Folders),

    case kz_datamgr:get_results(Db, ?COUNT_VIEW, ViewOpts) of
        {'ok', _}=JObjs -> JObjs;
        {'error', _}=E -> E
    end.

create_folder_view_option(BoxId, [<<_/binary>>=Folder]) ->
    [{'key', [BoxId, Folder]}, 'reduce', 'group'];
create_folder_view_option(BoxId, Folders) ->
    [{'keys', lists:foldl(fun(F, Acc) -> [[BoxId, F] | Acc] end, [], Folders)}, 'reduce', 'group'].

-spec maybe_transcribe(ne_binary(), ne_binary(), boolean()) ->
                              api_object().
maybe_transcribe(AccountId, MediaId, 'true') ->
    Db = get_db(AccountId),
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
