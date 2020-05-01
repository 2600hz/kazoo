%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Mailbox message document manipulation
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_box_message).

-export([new/2, build_metadata_object/6
        ,count_folder/2
        ,create_message_name/3
        ,type/0

        ,folder/1, folder/2, set_folder/2
        ,set_folder_new/1, set_folder_saved/1, set_folder_deleted/1
        ,apply_folder/2
        ,filter_folder/2

        ,message_history/1, add_message_history/2
        ,message_name/1, message_name/2, set_message_name/2

        ,change_message_name/2, change_to_sip_field/3

        ,length/1
        ,media_id/1, set_media_id/2, update_media_id/2
        ,metadata/1, metadata/2, set_metadata/2
        ,source_id/1, set_source_id/2
        ,to_sip/1, to_sip/2, set_to_sip/2
        ,utc_seconds/1

        ,get_msg_id/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_DESC, <<"description">>).
-define(KEY_HISTORY, <<"history">>).
-define(KEY_MEDIA_FILENAME, <<"media_filename">>).
-define(KEY_MEDIA_ID, <<"media_id">>).
-define(KEY_MEDIA_SOURCE, <<"media_source">>).
-define(KEY_NAME, <<"name">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_PVT_TYPE, <<"pvt_type">>).
-define(KEY_SOURCE_ID, <<"source_id">>).
-define(KEY_SOURCE_TYPE, <<"source_type">>).
-define(KEY_STREAMABLE, <<"streamable">>).
-define(KEY_UTC_SEC, <<"utc_seconds">>).
-define(KEY_VOICEMAIL, <<"voicemail">>).

-define(KEY_METADATA, <<"metadata">>).
-define(KEY_TRANSCRIPTION, <<"transcription">>).
-define(KEY_META_CALL_ID, <<"call_id">>).
-define(KEY_META_CID_NAME, <<"caller_id_name">>).
-define(KEY_META_CID_NUMBER, <<"caller_id_number">>).
-define(KEY_META_FROM, <<"from">>).
-define(KEY_META_FROM_REALM, <<"from_realm">>).
-define(KEY_META_FROM_USER, <<"from_user">>).
-define(KEY_META_LENGTH, <<"length">>).
-define(KEY_META_TIMESTAMP, <<"timestamp">>).
-define(KEY_META_TO, <<"to">>).
-define(KEY_META_TO_REALM, <<"to_realm">>).
-define(KEY_META_TO_USER, <<"to_user">>).

-define(PVT_TYPE, <<"mailbox_message">>).
-define(PVT_LEGACY_TYPE, <<"private_media">>).

%%------------------------------------------------------------------------------
%% @doc Generate a mailbox message doc with the given properties.
%% Options are:
%%
%% <dl>
%%    <dt>`{<<"Attachment-Name">>, '{@link kz_term:ne_binary()}`}'</dt>
%%    <dd>Media file name</dd>
%%    <dt>`{<<"Box-Id">>, '{@link kz_term:ne_binary()}`}'</dt>
%%    <dd>The mailbox ID the message is belong to</dd>
%%    <dt>`{<<"OwnerId">>, '{@link kz_term:ne_binary()}`}'</dt>
%%    <dd>The owner ID of the mailbox</dd>
%%    <dt>`{<<"Length">>, integer()}'</dt>
%%    <dd>Media file size (or audio duration?)</dd>
%%    <dt>`{<<"Transcribe-Voicemail">>, boolean()}'</dt>
%%    <dd>Should try to transcribe the message with external service</dd>
%%    <dt>`{<<"After-Notify-Action">>, '{@link notify_action()}`}'</dt>
%%    <dd>The action to execute if sending notification was successful</dd>
%%    <dt>`{<<"Box-Num">>, '{@link kz_term:ne_binary()}`}'</dt>
%%    <dd>Extension or phone number of the mailbox</dd>
%%    <dt>`{<<"Timezone">>, '{@link kz_term:api_binary()}`}'</dt>
%%    <dd>Configured timezone of the mailbox or device or user or account. If it
%%    is `undefined' system default timezone will be used instead.</dd>
%% </dl>
%%
%% Optional options `<<"Media-Id">>', `<<"Message-Timestamp">>' and `<<"Document-Timestamp">>'
%% are useful for migrating from account's database to account's MODB.
%%
%% <div class="notice">If `<<"Media-Id">>' option is passed, it'll use for preserving
%% current `message_id' during migration, so if for any reason migration failed
%% and we run it again, it would try to write to same doc with same id
%% which result in `{error, conflict}' which in this case is safe to ignore.</div>
%%
%% Option `<<"Message-Timestamp">>' is used to preserved previous message's utc_seconds.
%%
%% Options `<<"Document-Timestamp">>' is then used to set pvt_created, pvt_modified when
%% we are moving the message to MODB.
%% @end
%%------------------------------------------------------------------------------
-spec new(kz_term:ne_binary(), kz_term:proplist()) -> doc().
new(AccountId, Props) ->
    UtcSeconds = props:get_integer_value(<<"Message-Timestamp">>, Props, kz_time:now_s()),
    Timestamp  = props:get_integer_value(<<"Document-Timestamp">>, Props, UtcSeconds),
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),

    MediaId = props:get_value(<<"Media-ID">>, Props, kz_binary:rand_hex(16)),

    Db = kazoo_modb:get_modb(AccountId, Year, Month),
    MsgId = kazoo_modb_util:modb_id(Year, Month, MediaId),

    Name = create_message_name(props:get_value(<<"Box-Num">>, Props)
                              ,props:get_value(<<"Timezone">>, Props)
                              ,UtcSeconds
                              ),
    Description = props:get_value(<<"Description">>, Props, <<"voicemail message with media">>),

    DocProps = [{<<"_id">>, MsgId}
               ,{?KEY_NAME, Name}
               ,{?KEY_DESC, Description}
               ,{?KEY_SOURCE_TYPE, ?KEY_VOICEMAIL}
               ,{?KEY_SOURCE_ID, props:get_value(<<"Box-Id">>, Props)}
               ,{?KEY_MEDIA_SOURCE, <<"recording">>}
               ,{?KEY_MEDIA_FILENAME, props:get_value(<<"Attachment-Name">>, Props)}
               ,{?KEY_STREAMABLE, 'true'}
               ,{?KEY_UTC_SEC, UtcSeconds}
               ],
    kz_doc:update_pvt_parameters(kz_json:from_list(DocProps)
                                ,Db
                                ,[{'type', type()}
                                 ,{'now', Timestamp}
                                 ]
                                ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_message_name(kz_term:ne_binary(), kz_term:api_binary(), kz_time:gregorian_seconds()) -> kz_term:ne_binary().
create_message_name(BoxNum, 'undefined', UtcSeconds) ->
    create_message_name(BoxNum, kzd_accounts:default_timezone(), UtcSeconds);
create_message_name(BoxNum, <<Timezone/binary>>, UtcSeconds) ->
    UtcDateTime = calendar:gregorian_seconds_to_datetime(kz_term:to_integer(UtcSeconds)),
    try kz_time:adjust_utc_datetime(UtcDateTime, Timezone) of
        {{_,_,_}, {_,_,_}}=LocalDateTime ->
            message_name(BoxNum, LocalDateTime, "")
    catch
        'throw':{'error', 'unknown_tz'} ->
            lager:info("unknown timezone: ~s", [Timezone]),
            message_name(BoxNum, UtcDateTime, " UTC")
    end.

-spec message_name(kz_term:ne_binary(), kz_time:datetime(), string()) -> kz_term:ne_binary().
message_name(BoxNum, {{Y,M,D},{H,I,S}}, TZ) ->
    list_to_binary(["mailbox ", BoxNum, " message "
                   ,kz_term:to_binary(M), "-"
                   ,kz_term:to_binary(D), "-"
                   ,kz_term:to_binary(Y), " "
                   ,kz_term:to_binary(H), ":"
                   ,kz_term:to_binary(I), ":"
                   ,kz_term:to_binary(S), TZ
                   ]).

%%------------------------------------------------------------------------------
%% @doc Build message metadata.
%% @end
%%------------------------------------------------------------------------------
-spec build_metadata_object(pos_integer(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_time:gregorian_seconds()) ->
          doc().
build_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, Timestamp) ->
    kz_json:from_list(
      [{?KEY_MEDIA_ID, MediaId}
      ,{?KEY_META_CALL_ID, kapps_call:call_id(Call)}

      ,{?KEY_META_CID_NAME, CIDName}
      ,{?KEY_META_CID_NUMBER, CIDNumber}

      ,{?KEY_META_FROM, kapps_call:from(Call)}
      ,{?KEY_META_FROM_USER, kapps_call:from_user(Call)}
      ,{?KEY_META_FROM_REALM, kapps_call:from_realm(Call)}

      ,{?KEY_META_LENGTH, Length}
      ,{?KEY_META_TIMESTAMP, Timestamp}

      ,{?KEY_META_TO, kapps_call:to(Call)}
      ,{?KEY_META_TO_USER, kapps_call:to_user(Call)}
      ,{?KEY_META_TO_REALM, kapps_call:to_realm(Call)}

      ,{?VM_KEY_FOLDER, ?VM_FOLDER_NEW}
      ]).

-spec get_msg_id(kz_json:object()) -> kz_term:api_ne_binary().
get_msg_id(JObj) ->
    Paths = [<<"_id">>
            ,<<"media_id">>
            ,[<<"metadata">>, <<"media_id">>]
            ],
    kz_json:get_first_defined(Paths, JObj).

%%------------------------------------------------------------------------------
%% @doc Accessors methods.
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec folder(doc()) -> kz_term:api_ne_binary().
folder(Metadata) ->
    folder(Metadata, 'undefined').

-spec folder(doc(), Default) -> doc() | Default.
folder(Metadata, Default) ->
    kz_json:get_first_defined([[?KEY_METADATA, ?VM_KEY_FOLDER], ?VM_KEY_FOLDER], Metadata, Default).

-spec set_folder(kz_term:api_ne_binary(), doc()) -> doc().
set_folder(Folder, Metadata) ->
    kz_json:set_value(?VM_KEY_FOLDER, Folder, Metadata).

-spec set_folder_new(doc()) -> doc().
set_folder_new(Metadata) ->
    kz_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_NEW, Metadata).

-spec set_folder_saved(doc()) -> doc().
set_folder_saved(Metadata) ->
    kz_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_SAVED, Metadata).

-spec set_folder_deleted(doc()) -> doc().
set_folder_deleted(Metadata) ->
    kz_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_DELETED, Metadata).

%%------------------------------------------------------------------------------
%% @doc Set folder in metadata of message's document.
%% Folder can be `{kz_term:ne_binary(), boolean()}' which the `boolean()'
%% controls whether documents should marked as soft-deleted or not.
%% @end
%%------------------------------------------------------------------------------
-spec apply_folder(kvm_message:vm_folder(), doc()) -> doc().
apply_folder({?VM_FOLDER_DELETED, 'false'}, Doc) ->
    %% only move to deleted folder not actually soft-delete it
    Metadata = set_folder_deleted(metadata(Doc)),
    set_metadata(Metadata, Doc);
apply_folder({?VM_FOLDER_DELETED, 'true'}, Doc) ->
    %% move to deleted folder and soft-delete it
    apply_folder(?VM_FOLDER_DELETED, Doc);
apply_folder(?VM_FOLDER_DELETED, Doc) ->
    Metadata = set_folder_deleted(metadata(Doc)),
    kz_doc:set_soft_deleted(set_metadata(Metadata, Doc), 'true');
apply_folder(Folder, Doc) ->
    Metadata = set_folder(Folder, metadata(Doc)),
    set_metadata(Metadata, Doc).

-spec message_history(doc()) -> kz_term:ne_binaries().
message_history(JObj) ->
    kz_json:get_value(?KEY_HISTORY, JObj, []).

-spec add_message_history(kz_term:ne_binary(), doc()) -> doc().
add_message_history(History, JObj) ->
    kz_json:set_value(?KEY_HISTORY, message_history(JObj) ++ [History], JObj).

-spec length(doc()) -> integer().
length(JObj) ->
    kz_json:get_value(?KEY_META_LENGTH, JObj).

-spec message_name(doc()) -> kz_term:api_binary().
message_name(JObj) ->
    message_name(JObj, 'undefined').

-spec message_name(doc(), Default) -> kz_term:api_binary() | Default.
message_name(JObj, Default) ->
    kz_json:get_value(?KEY_NAME, JObj, Default).

-spec set_message_name(kz_term:api_binary(), doc()) -> doc().
set_message_name(Name, JObj) ->
    kz_json:set_value(?KEY_NAME, Name, JObj).

-spec media_id(doc()) -> kz_term:api_binary().
media_id(Metadata) ->
    kz_json:get_value(?KEY_MEDIA_ID, Metadata).

-spec set_media_id(kz_term:ne_binary(), doc()) -> doc().
set_media_id(MediaId, Metadata) ->
    kz_json:set_value(?KEY_MEDIA_ID, MediaId, Metadata).

-spec update_media_id(kz_term:ne_binary(), doc()) -> doc().
update_media_id(MediaId, JObj) ->
    Metadata = set_media_id(MediaId, metadata(JObj)),
    set_metadata(Metadata, JObj).

-spec metadata(doc()) -> doc() | 'undefined'.
metadata(JObj) ->
    metadata(JObj, 'undefined').

-spec metadata(doc(), Default) -> doc() | Default.
metadata(JObj, Default) ->
    Metadata = kz_json:get_json_value(?KEY_METADATA, JObj, Default),
    maybe_add_transcription(Metadata, JObj).

-spec maybe_add_transcription(doc() | Default, doc()) -> doc() | Default.
maybe_add_transcription(Metadata, JObj) ->
    case kz_json:get_json_value(?KEY_TRANSCRIPTION, JObj) of
        'undefined' -> Metadata;
        Transcription -> kz_json:insert_value(?KEY_TRANSCRIPTION, Transcription, Metadata)
    end.

-spec set_metadata(kz_json:object(), doc()) -> doc().
set_metadata(Metadata, JObj) ->
    kz_json:set_value(?KEY_METADATA, Metadata, JObj).

-spec to_sip(doc()) -> kz_term:api_binary().
to_sip(JObj) ->
    to_sip(JObj, 'undefined').

-spec to_sip(doc(), Default) -> kz_term:api_binary() | Default.
to_sip(JObj, Default) ->
    kz_json:get_first_defined([[?KEY_METADATA, ?KEY_META_TO], ?KEY_META_TO], JObj, Default).

-spec set_to_sip(kz_term:api_binary(), doc()) -> doc().
set_to_sip(To, Metadata) ->
    kz_json:set_value(?KEY_META_TO, To, Metadata).

-spec utc_seconds(doc()) -> non_neg_integer().
utc_seconds(JObj) ->
    kz_json:get_integer_value(?KEY_UTC_SEC, JObj, 0).

-spec source_id(doc()) -> kz_term:api_ne_binary().
source_id(JObj) ->
    kz_json:get_ne_binary_value(?KEY_SOURCE_ID, JObj).

-spec set_source_id(kz_term:api_ne_binary(), doc()) -> doc().
set_source_id(SourceId, JObj) ->
    kz_json:set_value(?KEY_SOURCE_ID, SourceId, JObj).

%%------------------------------------------------------------------------------
%% @doc Filter messages based on specific folder.
%% @end
%%------------------------------------------------------------------------------
-spec filter_folder(kz_json:objects(), kz_term:ne_binary()) -> kz_json:objects().
filter_folder(Messages, Folder) ->
    [M || M <- Messages, folder(M) =:= Folder].

%%------------------------------------------------------------------------------
%% @doc Count message list in specific folder(s).
%% @end
%%------------------------------------------------------------------------------
-spec count_folder(kz_json:objects(), kz_term:ne_binary() | kz_term:ne_binaries()) -> non_neg_integer().
count_folder(Messages, Folders) when is_list(Folders) ->
    lists:sum([1 || Message <- Messages,
                    begin
                        F = kz_json:get_value(?VM_KEY_FOLDER, Message),
                        lists:member(F, Folders)
                    end
              ]);
count_folder(Messages, Folder) ->
    count_folder(Messages, [Folder]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec change_message_name(doc(), doc()) -> doc().
change_message_name(NBoxJ, MsgJObj) ->
    BoxNum = kzd_voicemail_box:mailbox_number(NBoxJ),
    Timezone = kzd_voicemail_box:timezone(NBoxJ),
    UtcSeconds = utc_seconds(MsgJObj),

    NewName = create_message_name(BoxNum, Timezone, UtcSeconds),
    set_message_name(NewName, MsgJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec change_to_sip_field(kz_term:ne_binary(), doc(), doc()) -> doc().
change_to_sip_field(AccountId, NBoxJ, MsgJObj) ->
    Realm = kzd_accounts:fetch_realm(AccountId),
    BoxNum = kzd_voicemail_box:mailbox_number(NBoxJ),
    Metadata = metadata(MsgJObj),
    To = <<BoxNum/binary, "@", Realm/binary>>,
    set_metadata(set_to_sip(To, Metadata), MsgJObj).
