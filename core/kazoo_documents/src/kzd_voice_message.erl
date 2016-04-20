%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Voicemail message document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_voice_message).

-export([new/0, new/6, create_metadata_object/7
         ,count_messages/1, count_messages/2
         ,type/0
         ,external_media_url/1, external_media_url/2, set_external_media_url/2
         ,folder/1, folder/2, set_folder/2, set_folder_saved/1, set_folder_deleted/1, filter_folder/2
         ,media_id/1
         ,metadata/1, metadata/2, set_metadata/2
         ,utc_seconds/1
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_PVT_TYPE, <<"pvt_type">>).
-define(KEY_NAME, <<"name">>).
-define(KEY_DESC, <<"description">>).
-define(KEY_SOURCE_TYPE, <<"source_type">>).
-define(KEY_SOURCE_ID, <<"source_id">>).
-define(KEY_MEDIA_SOURCE, <<"media_source">>).
-define(KEY_MEDIA_TYPE, <<"media_type">>).
-define(KEY_MEDIA_FILENAME, <<"media_filename">>).
-define(KEY_STREAMABLE, <<"streamable">>).
-define(KEY_UTC_SEC, <<"utc_seconds">>).
-define(KEY_EXTERNAL_MEDIA_URL, <<"external_media_url">>).
-define(KEY_MEDIA_ID, <<"media_id">>).

-define(KEY_METADATA, <<"metadata">>).
-define(KEY_META_TIMESTAMP, <<"timestamp">>).
-define(KEY_META_FROM, <<"from">>).
-define(KEY_META_TO, <<"to">>).
-define(KEY_META_CID_NUMBER, <<"caller_id_number">>).
-define(KEY_META_CID_NAME, <<"caller_id_name">>).
-define(KEY_META_CALL_ID, <<"call_id">>).
-define(KEY_META_LENGTH, <<"length">>).

-define(PVT_TYPE, <<"voice_message">>).

-spec new() -> doc().
new() ->
    wh_json:from_list([{?PVT_TYPE, type()}]).

-spec new(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) -> doc().
new(Db, DocId, AttachmentName, BoxNum, Timezone, Props) ->
    UtcSeconds = wh_util:current_tstamp(),
    UtcDateTime = calendar:gregorian_seconds_to_datetime(UtcSeconds),
    Name = case localtime:utc_to_local(UtcDateTime, Timezone) of
               {'error', 'unknown_tz'} ->
                   lager:info("unknown timezone: ~s", [Timezone]),
                   message_name(BoxNum, UtcDateTime, " UTC");
               DT ->
                   message_name(BoxNum, DT)
           end,

    DocProps = props:filter_undefined(
              [{<<"_id">>, DocId}
               ,{?KEY_NAME, Name}
               ,{?KEY_DESC, <<"voicemail message media">>}
               ,{?KEY_SOURCE_TYPE, ?KEY_VOICEMAIL}
               ,{?KEY_SOURCE_ID, props:get_value(<<"Box-Id">>, Props)}
               ,{?KEY_MEDIA_SOURCE, <<"recording">>}
               ,{?KEY_MEDIA_TYPE, props:get_value(<<"Default-Extension">>, Props)}
               ,{?KEY_MEDIA_FILENAME, AttachmentName}
               ,{?KEY_STREAMABLE, 'true'}
               ,{?KEY_UTC_SEC, UtcSeconds}
               ,{?KEY_EXTERNAL_MEDIA_URL, props:get_value(<<"Default-External-Storage">>, Props)}
              ]),
    wh_doc:update_pvt_parameters(wh_json:from_list(DocProps), Db, [{'type', type()}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_name(ne_binary(), wh_datetime()) -> ne_binary().
-spec message_name(ne_binary(), wh_datetime(), string()) -> ne_binary().
message_name(BoxNum, DT) ->
    message_name(BoxNum, DT, "").

message_name(BoxNum, {{Y,M,D},{H,I,S}}, TZ) ->
    list_to_binary(["mailbox ", BoxNum, " message "
                    ,wh_util:to_binary(M), "-"
                    ,wh_util:to_binary(D), "-"
                    ,wh_util:to_binary(Y), " "
                    ,wh_util:to_binary(H), ":"
                    ,wh_util:to_binary(I), ":"
                    ,wh_util:to_binary(S), TZ
                   ]).

-spec create_metadata_object(pos_integer(), whapps_call:call(), ne_binary(), ne_binary(), ne_binary(), api_binary(), gregorian_seconds()) ->
                                    doc().
create_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, ExternalMediaUrl, Timestamp) ->
    wh_json:from_list(
        props:filter_undefined(
            [{?KEY_META_TIMESTAMP, Timestamp}
             ,{?KEY_META_FROM, whapps_call:from(Call)}
             ,{?KEY_META_TO, whapps_call:to(Call)}
             ,{?KEY_META_CID_NUMBER, CIDNumber}
             ,{?KEY_META_CID_NUMBER, CIDName}
             ,{?KEY_META_CALL_ID, whapps_call:call_id(Call)}
             ,{?VM_KEY_FOLDER, ?VM_FOLDER_NEW}
             ,{?KEY_META_LENGTH, Length}
             ,{?KEY_MEDIA_ID, MediaId}
             ,{?KEY_EXTERNAL_MEDIA_URL, ExternalMediaUrl}
            ])
        ).

-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

-spec external_media_url(doc()) -> api_binary().
-spec external_media_url(doc(), Default) -> ne_binary() | Default.
external_media_url(JObj) ->
    external_media_url(JObj, 'undefined').

external_media_url(JObj, Default) ->
    case wh_json:get_value([?KEY_METADATA, ?KEY_EXTERNAL_MEDIA_URL], JObj) of
        'undefined' -> wh_json:get_value(?KEY_EXTERNAL_MEDIA_URL, JObj, Default);
        ExternalMediaUrl -> ExternalMediaUrl
    end.

-spec set_external_media_url(api_binary(), doc()) -> doc().
set_external_media_url(ExternalMediaUrl, JObj) ->
    wh_json:set_value([?KEY_EXTERNAL_MEDIA_URL], ExternalMediaUrl, JObj).

-spec folder(doc()) -> api_binary().
folder(JObj) ->
    folder(JObj, 'undefined').

-spec folder(doc(), Default) -> ne_binary() | Default.
folder(JObj, Default) ->
    wh_json:get_value(?VM_KEY_FOLDER, JObj, Default).

-spec set_folder(api_binary(), doc()) -> doc().
set_folder(Folder, JObj) ->
    wh_json:set_value(?VM_KEY_FOLDER, Folder, JObj).

-spec set_folder_saved(doc()) -> doc().
set_folder_saved(JObj) ->
    wh_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_SAVED, JObj).

-spec set_folder_deleted(doc()) -> doc().
set_folder_deleted(JObj) ->
    wh_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_DELETED, JObj).

-spec media_id(doc()) -> api_binary().
media_id(JObj) ->
    wh_json:get_value(?KEY_MEDIA_ID, JObj).

-spec metadata(doc()) -> doc().
metadata(JObj) ->
    metadata(JObj, 'undefined').

-spec metadata(doc(), doc()) -> doc().
metadata(JObj, Default) ->
    wh_json:get_value(?KEY_METADATA, JObj, Default).

-spec set_metadata(doc(), doc()) -> doc().
set_metadata(Metadata, JObj) ->
    wh_json:set_value(?KEY_METADATA, Metadata, JObj).

-spec utc_seconds(doc()) -> pos_integer().
  wh_json:get_integer_value(?KEY_UTC_SEC, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc Filter messages based on specific folder
%% @end
%%--------------------------------------------------------------------
-spec filter_folder(wh_json:objects(), ne_binary()) -> wh_json:objects().
filter_folder(Messages, Folder) ->
    [M || M <- Messages, folder(M) =:= Folder].

%%--------------------------------------------------------------------
%% @private
%% @doc Count count_per_folder view result
%% @end
%%--------------------------------------------------------------------
-spec count_messages(wh_json:objects()) -> non_neg_integer().
count_messages(Messages) ->
    lists:sum([wh_json:get_integer_value(<<"value">>, Message) || Message <- Messages]).

%%--------------------------------------------------------------------
%% @private
%% @doc Count message list in specific folder(s)
%% @end
%%--------------------------------------------------------------------
-spec count_messages(wh_json:objects(), ne_binary() | ne_binaries()) -> non_neg_integer().
count_messages(Messages, Folders) when is_list(Folders) ->
    lists:sum([1 || Message <- Messages,
                    begin
                        F = wh_json:get_value(?VM_KEY_FOLDER, Message),
                        lists:member(F, Folders)
                    end
              ]);
count_messages(Messages, Folder) ->
    count_messages(Messages, [Folder]).
