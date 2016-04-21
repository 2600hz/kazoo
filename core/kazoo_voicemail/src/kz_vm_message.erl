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
         ,count_all/2
         ,set_folder/3, update_folder/3

         ,media_url/2

         ,get_db/1
         ,get_range_view/2
         ,filter_message_metadata/2
        ]).

-include("kz_voicemail.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CF_CONFIG_CAT, <<"callflow">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_MAX_RETAIN_DAYS, <<"max_message_retain_days">>).

-define(MODB_LISTING_BY_MAILBOX, <<"vm_messages/listing_by_mailbox">>).
-define(MODB_COUNT_VIEW, <<"vm_messages/count_per_folder">>).

-define(MAX_RETAIN_DAYS
        ,whapps_config:get(?CF_CONFIG_CAT
                           ,[?KEY_VOICEMAIL, ?KEY_MAX_RETAIN_DAYS]
                           ,93
                          )
       ).

get_db(AccountId) ->
    wh_util:format_account_id(AccountId, 'encoded').

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
%%    ,{<<"Default-Extension">>, ?DEFAULT_VM_EXTENSION}
%%    ,{<<"Retry-Storage-Times">>, ?MAILBOX_RETRY_STORAGE_TIMES(AccountId)}
%%  ]
%% @end
%%--------------------------------------------------------------------
-spec new_message(ne_binary(), ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) -> any().
new_message(AttachmentName, BoxNum, Timezone, Call, Props) ->
    BoxId = props:get_value(<<"Box-Id">>, Props),
    Length = props:get_value(<<"Length">>, Props),

    lager:debug("saving new ~bms voicemail media and metadata", [Length]),

    {MediaId, MediaUrl} = create_message_doc(AttachmentName, BoxNum, Call, Timezone, Props),

    lager:debug("storing voicemail media recording ~s in doc ~s", [AttachmentName, MediaId]),
    case store_recording(AttachmentName, MediaUrl, Call, Props) of
        {'ok', _JObj} -> notify_and_save(Call, MediaId, Length, Props);
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
    {'ok', _} = kz_datamgr:save_doc(Db, Doc),

    Opts = props:filter_undefined([{'doc_owner', props:get_value(<<"OwnerId">>, Props)}]),
    MediaUrl = wh_media_url:store(Db, {<<"voice_message">>, MediaId}, AttachmentName, Opts),

    {MediaId, MediaUrl}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_recording(ne_binary(), ne_binary(), whapps_call:call(), wh_proplist()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', whapps_call:call()}.
store_recording(AttachmentName, Url, Call, Props) ->
    Tries = props:get_value(<<"Retry-Storage-Times">>, Props),
    Funs = [{fun whapps_call:kvs_store/3, 'media_url', Url}],
    try_store_recording(AttachmentName, Url, Tries, whapps_call:exec(Funs, Call)).

-spec try_store_recording(ne_binary(), ne_binary(), integer(), whapps_call:call()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', whapps_call:call()}.
try_store_recording(_, _, 0, Call) -> {'error', Call};
try_store_recording(AttachmentName, Url, Tries, Call) ->
    case whapps_call_command:store_file(<<"/tmp/", AttachmentName/binary>>, Url, Call) of
        {'ok', _}=OK -> OK;
        Other ->
            lager:error("error trying to store voicemail media, retrying ~B more times: ~p", [Tries - 1, Other]),
            retry_store(AttachmentName, Url, Tries, Call, Other)
    end.

-spec retry_store(ne_binary(), ne_binary(), pos_integer(), whapps_call:call(), any()) ->
                                 {'ok', wh_json:object()} |
                                 {'error', whapps_call:call()}.
retry_store(AttachmentName, Url, Tries, Call, Error) ->
    timer:sleep(2000),
    Call1 = whapps_call:kvs_store('error_details', Error, Call),
    try_store_recording(AttachmentName, Url, Tries - 1, Call1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec notify_and_save(whapps_call:call(), ne_binary(), integer(), wh_proplist()) ->
                            'ok'.
notify_and_save(Call, MediaId, Length, Props) ->
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
get_completed_msg(JObjs) ->
    get_completed_msg(JObjs, wh_json:new()).

-spec get_completed_msg(wh_json:objects(), wh_json:object()) -> wh_json:object().
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
    Metadata = kzd_voice_message:create_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, Timestamp),

    {'ok', _BoxJObj} = save_metadata(Metadata, whapps_call:account_id(Call), MediaId),
    lager:debug("stored voicemail metadata for ~s", [MediaId]),

    publish_voicemail_saved(Length, BoxId, Call, MediaId, Timestamp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_doc(ne_binary(), kazoo_data:docid()) -> wh_json:object().
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

merge_metadata(MediaId, MediaJObj, VMBoxMsgs) ->
    case filter_message_metadata(MediaId, VMBoxMsgs) of
        {'error', _}=E -> E;
        {Metadata, _} -> {'ok', kzd_voice_message:set_metadata(Metadata, MediaJObj)}
    end.

filter_message_metadata(_MediaId, []) ->
    lager:warning("found media doc ~s but messages in vmbox is empty", [_MediaId]),
    {'error', 'not_found'};
filter_message_metadata(MediaId, [H|T]) ->
    filter_message_metadata(MediaId, kzd_voice_message:media_id(H), H, T, []).

filter_message_metadata(MediaId, MediaId, Msg, [], Acc) ->
    {Msg, Acc};
filter_message_metadata(_MediaId, _, _, [], _) ->
    lager:warning("found media doc ~s but could not find metadata in vmbox", [_MediaId]),
    {'error', 'not_found'};
filter_message_metadata(MediaId, MediaId, Msg, Tail, Acc) ->
    {Msg, lists:flatten([Acc | Tail])};
filter_message_metadata(MediaId, _, _, [H|T], Acc) ->
    filter_message_metadata(MediaId, kzd_voice_message:media_id(H), H, T, [H|Acc]).


%%--------------------------------------------------------------------
%% @public
%% @doc
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
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message(ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            {'error', any()}.
message(AccountId, MessageId) ->
    case message_doc(AccountId, MessageId) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', Msg} -> {'ok', kzd_voice_message:metadata(Msg)};
        {'error', _}=E ->
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
%% @doc Folder operations
%% @end
%%--------------------------------------------------------------------
-spec set_folder(ne_binary(), wh_json:object(), ne_binary()) -> any().
set_folder(Folder, Message, AccountId) ->
    MessageId = kzd_voice_message:media_id(Message),
    lager:info("setting folder for message ~s to ~s", [MessageId, Folder]),
    not (kzd_voice_message:folder(Message) =:= Folder) andalso
        update_folder(Folder, MessageId, AccountId).

-spec update_folder(ne_binary(), ne_binary(), ne_binary()) ->
                           {'ok', wh_json:object()} |
                           {'error', any()}.
update_folder(_, 'undefined', _) ->
    {'error', 'attachment_undefined'};
update_folder(Folder, MessageId, AccountId) ->
    Fun = [fun(JObj) ->
               update_folder1(JObj, Folder)
           end
          ],

    case update_message_doc(AccountId, MessageId, Fun) of
        {'ok', _}=OK ->OK;
        {'error', 'conflict'} ->
            lager:info("updating folder resulted in a conflict, trying again"),
            update_folder(Folder, MessageId, AccountId);
        {'error', R}=E ->
            lager:info("error while updating folder ~s ~p", [Folder, R]),
            E
    end.

update_message_doc(AccountId, DocId, Fun) ->
    case message_doc(AccountId, DocId) of
        {'ok', JObj} -> do_update_message_doc(AccountId, DocId, JObj, Fun);
        {'error', _}=E ->
            E
    end.

do_update_message_doc(AccountId, ?MATCH_MODB_PREFIX(Year, Month, _), JObj, Funs) ->
    NewJObj = lists:foldl(fun(F, J) -> F(J) end, JObj, Funs),
    kazoo_modb:save_doc(AccountId, NewJObj, Year, Month);
do_update_message_doc(AccountId, DocId, JObj, Funs) ->
    move_to_modb(AccountId, DocId, JObj, Funs).

% TODO: delete old doc from accountdb
move_to_modb(AccountId, DocId, JObj, Funs) ->
    Created = wh_doc:created(JObj),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Created),

    FromDb = wh_util:format_account_id(AccountId, 'encoded'),
    FromId = wh_doc:id(JObj),
    ToDb = kazoo_modb:get_modb(AccountId, Year, Month),
    ToId = <<(wh_util:to_binary(Year))/binary
              ,(wh_util:pad_month(Month))/binary
              ,"-"
              ,(wh_util:rand_hex_binary(16))/binary
           >>,
    io:format("FromDb ~p FromId ~p ToDb ~p ToId ~p ~n~n", [FromDb, FromId, ToDb, ToId]),
    case kz_datamgr:copy_doc(FromDb, FromId, ToDb, ToId, []) of
        {'ok', _} ->
            SourceId = kzd_voice_message:source_id(JObj),
            Result = populate_metadata_from_vmbox(AccountId, SourceId, FromId, ToId, Funs),
            maybe_delete_media_doc(AccountId, FromId, Result);
        {'error', _}=E ->
            lager:warning("failed to copy voice message ~s to modb", [DocId]),
            E
    end.

% populate_metadata_from_vmbox(AccountId, BoxId, OldId, NewId) ->
%     populate_metadata_from_vmbox(AccountId, BoxId, OldId, NewId, []).

populate_metadata_from_vmbox(AccountId, BoxId, OldId, NewId, Funs) ->
    case open_accountdb_doc(AccountId, BoxId) of
        {'ok', VMBox} ->
            try_populate_metadata(AccountId, OldId, NewId, VMBox, Funs);
        {'error', _}=E ->
            lager:warning("failed to populate voice message metadata ~s from vmbox", [OldId]),
            E
    end.

try_populate_metadata(AccountId, OldId, NewId, VMBox, Funs) ->
    Messages = wh_json:get_value(?VM_KEY_MESSAGES, VMBox, []),
    {Metadata, NewMessages} = filter_message_metadata(OldId, Messages),
    Methods = [fun(JObj) -> kzd_voice_message:set_metadata(Metadata, JObj) end
               ,fun(JObj) -> update_media_id(NewId, JObj) end
               ,fun(JObj) -> wh_json:set_value(<<"pvt_type">>, kzd_voice_message:type(), JObj) end
               | Funs
              ],
    case update_message_doc(AccountId, NewId, Methods) of
        {'ok', JObj} ->
            try_update_mailbox(AccountId, wh_json:set_value(?VM_KEY_MESSAGES, NewMessages, VMBox), JObj);
        {'error', _}=E ->
            lager:warning("failed to populate voice message metadata ~s from vmbox", [OldId]),
            E
    end.

try_update_mailbox(AccountId, NewVMBox, NewMsgJObj) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:save_doc(AccountDb, NewVMBox) of
        {'ok', _} -> {'ok', NewMsgJObj};
        {'error', R}=E ->
            lager:debug("could not update mailbox ~s: ~s", [wh_doc:id(NewVMBox), R]),
            E
    end.

update_media_id(MediaId, JObj) ->
    Metadata = kzd_voice_message:set_media_id(MediaId, kzd_voice_message:metadata(JObj)),
    kzd_voice_message:set_metadata(Metadata, JObj).

maybe_delete_media_doc(AccountId, Id, {'ok', _}=Doc) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    _ = kz_datamgr:del_doc(AccountDb, Id),
    Doc;
maybe_delete_media_doc(_, _, {'error', _}=E) ->
    E.

-spec update_folder1(wh_json:object(), ne_binary()) -> wh_json:object().
update_folder1(Doc, <<"deleted">>) ->
    Metadata = kzd_voice_message:set_folder_deleted(kzd_voice_message:metadata(Doc)),
    wh_json:set_value(<<"pvt_deleted">>, <<"true">>, kzd_voice_message:set_metadata(Metadata, Doc));
update_folder1(Doc, Folder) ->
    Metadata = kzd_voice_message:set_folder(Folder, kzd_voice_message:metadata(Doc)),
    kzd_voice_message:set_metadata(Metadata, Doc).

%%--------------------------------------------------------------------
%% @public
%% @doc Count non-deleted messages
%% @end
%%--------------------------------------------------------------------
-spec count_all(ne_binary(), ne_binary()) -> non_neg_integer().
count_all(AccountId, BoxId) ->
    % first count messages from vmbox for backward compatibility
    case fetch_vmbox_messages(AccountId, BoxId) of
        {'ok', Msgs} ->
            C = kzd_voice_message:count_messages(Msgs, [?VM_FOLDER_NEW, ?VM_FOLDER_SAVED]),
            count_modb_messages(AccountId, BoxId, C);
        _ -> count_modb_messages(AccountId, BoxId, 0)
    end.

-spec count_modb_messages(ne_binary(), ne_binary(), non_neg_integer()) -> non_neg_integer().
count_modb_messages(AccountId, BoxId, AccountDbCount) ->
    case count_per_folder(AccountId, BoxId, [?VM_FOLDER_NEW, ?VM_FOLDER_SAVED]) of
        [] -> AccountDbCount;
        ViewResults -> kzd_voice_message:count_messages(ViewResults) + AccountDbCount
    end.

-spec count_per_folder(ne_binary(), ne_binary(), ne_binary() | ne_binaries()) ->
                                {'ok', wh_json:objects()} |
                                {'error', any()}.
count_per_folder(AccountId, BoxId, <<_/binary>>=Folder) ->
    count_per_folder(AccountId, BoxId, [Folder]);
count_per_folder(AccountId, BoxId, Folders) ->
    FolderViewOpts = folder_view_option(BoxId, Folders),
    ViewOpts = get_range_view(AccountId, FolderViewOpts),

    do_fetch_from_modbs(AccountId, ?MODB_COUNT_VIEW, ViewOpts, []).

-spec folder_view_option(ne_binary(), ne_binaries()) -> wh_proplist().
folder_view_option(BoxId, [<<_/binary>>=Folder]) ->
    [{'key', [BoxId, Folder]}, 'reduce', 'group'];
folder_view_option(BoxId, Folders) ->
    [{'keys', lists:foldl(fun(F, Acc) -> [[BoxId, F] | Acc] end, [], Folders)}, 'reduce', 'group'].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec media_url(ne_binary(), ne_binary() | wh_json:object()) -> binary().
%% temp fix to unblock
%% you need to get the full doc here
media_url(AccountId, ?JSON_WRAPPER(_)=Message) ->
    media_url(AccountId, kzd_voice_message:media_id(Message));
media_url(AccountId, MessageId) ->
    case message(AccountId, MessageId) of
        {'ok', Message} ->
            lager:debug("MESSAGE ~p", [Message]),
            ?MATCH_MODB_PREFIX(Year,Month,_) = wh_doc:id(Message),
            [AName | _] = wh_doc:attachment_names(Message),
            Url = list_to_binary(
                    [kazoo_modb:get_modb(wh_doc:account_id(Message), Year, Month)
                     ,wh_doc:id(Message)
                     ,AName
                     ,[{'doc_type', wh_doc:type(Message)}]
                    ]),
            wh_media_url:playback(Url, Message);
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
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:open_doc(AccountDb, DocId) of
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
                   || Msg <- do_fetch_from_modbs(AccountId, ?MODB_LISTING_BY_MAILBOX, ViewOptsList, [])
                      ,Msg =/= []
                  ],
    VMBoxMsg ++ ModbResults.

do_fetch_from_modbs(_, _, [], ViewResults) ->
    ViewResults;
do_fetch_from_modbs(AccountId, View, [ViewOpts|ViewOptsList], Acc) ->
    case kazoo_modb:get_results(AccountId, View, ViewOpts) of
        {'ok', []} -> do_fetch_from_modbs(AccountId, View, ViewOptsList, Acc);
        {'ok', Msgs} -> do_fetch_from_modbs(AccountId, View, ViewOptsList, Msgs ++ Acc);
        {'error', _}=_E ->
            lager:debug("error when fetching voicemail message for ~s from modb ~s"
                        ,[props:get_value('key', ViewOpts), props:get_value('modb', ViewOpts)]
                       ),
            do_fetch_from_modbs(AccountId, View, ViewOptsList, Acc)
    end.

-spec get_range_view(ne_binary(), wh_proplist()) -> wh_proplists().
get_range_view(AccountId, ViewOpts) ->
    To = wh_util:current_tstamp(),
    MaxRange = ?SECONDS_IN_DAY * ?MAX_RETAIN_DAYS + ?SECONDS_IN_HOUR,
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
