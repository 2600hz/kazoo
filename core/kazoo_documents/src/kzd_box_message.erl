%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Mailbox message document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_box_message).

-export([new/0, new/6, build_metadata_object/6
         ,count_folder/2, normalize_count/1
         ,type/0
         ,folder/1, folder/2, set_folder/2, set_folder_new/1, set_folder_saved/1, set_folder_deleted/1, filter_folder/2
         ,media_id/1, set_media_id/2
         ,metadata/1, metadata/2, set_metadata/2
         ,source_id/1, set_source_id/2
         ,utc_seconds/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_VOICEMAIL, <<"voicemail">>).
-define(KEY_PVT_TYPE, <<"pvt_type">>).
-define(KEY_NAME, <<"name">>).
-define(KEY_DESC, <<"description">>).
-define(KEY_SOURCE_TYPE, <<"source_type">>).
-define(KEY_SOURCE_ID, <<"source_id">>).
-define(KEY_MEDIA_SOURCE, <<"media_source">>).
-define(KEY_MEDIA_FILENAME, <<"media_filename">>).
-define(KEY_STREAMABLE, <<"streamable">>).
-define(KEY_UTC_SEC, <<"utc_seconds">>).
-define(KEY_MEDIA_ID, <<"media_id">>).
-define(KEY_OWNER_ID, <<"owner_id">>).

-define(KEY_METADATA, <<"metadata">>).
-define(KEY_META_TIMESTAMP, <<"timestamp">>).
-define(KEY_META_FROM, <<"from">>).
-define(KEY_META_TO, <<"to">>).
-define(KEY_META_CID_NUMBER, <<"caller_id_number">>).
-define(KEY_META_CID_NAME, <<"caller_id_name">>).
-define(KEY_META_CALL_ID, <<"call_id">>).
-define(KEY_META_LENGTH, <<"length">>).

-define(PVT_TYPE, <<"mailbox_message">>).

%%--------------------------------------------------------------------
%% @public
%% @doc Generate a mailbox message doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_json:from_list([{?PVT_TYPE, type()}]).

%%--------------------------------------------------------------------
%% @public
%% @doc Generate a mailbox message doc with the given properties
%% expected options in Props:
%%    [{<<"Box-Id">>, BoxId}]
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> doc().
new(Db, DocId, AttachmentName, BoxNum, Timezone, Props) ->
    UtcSeconds = kz_util:current_tstamp(),
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
               ,{?KEY_DESC, <<"mailbox message media">>}
               ,{?KEY_SOURCE_TYPE, ?KEY_VOICEMAIL}
               ,{?KEY_SOURCE_ID, props:get_value(<<"Box-Id">>, Props)}
               ,{?KEY_MEDIA_SOURCE, <<"recording">>}
               ,{?KEY_MEDIA_FILENAME, AttachmentName}
               ,{?KEY_STREAMABLE, 'true'}
               ,{?KEY_UTC_SEC, UtcSeconds}
              ]),
    kz_doc:update_pvt_parameters(kz_json:from_list(DocProps), Db, [{'type', type()}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message_name(ne_binary(), kz_datetime()) -> ne_binary().
message_name(BoxNum, DT) ->
    message_name(BoxNum, DT, "").

-spec message_name(ne_binary(), kz_datetime(), string()) -> ne_binary().
message_name(BoxNum, {{Y,M,D},{H,I,S}}, TZ) ->
    list_to_binary(["mailbox ", BoxNum, " message "
                    ,kz_util:to_binary(M), "-"
                    ,kz_util:to_binary(D), "-"
                    ,kz_util:to_binary(Y), " "
                    ,kz_util:to_binary(H), ":"
                    ,kz_util:to_binary(I), ":"
                    ,kz_util:to_binary(S), TZ
                   ]).

%%--------------------------------------------------------------------
%% @public
%% @doc Build message metadata
%% @end
%%--------------------------------------------------------------------
-spec build_metadata_object(pos_integer(), kapps_call:call(), ne_binary(), ne_binary(), ne_binary(), gregorian_seconds()) ->
                                    doc().
build_metadata_object(Length, Call, MediaId, CIDNumber, CIDName, Timestamp) ->
    kz_json:from_list(
        props:filter_undefined(
            [{?KEY_META_TIMESTAMP, Timestamp}
             ,{?KEY_META_FROM, kapps_call:from(Call)}
             ,{?KEY_META_TO, kapps_call:to(Call)}
             ,{?KEY_META_CID_NUMBER, CIDNumber}
             ,{?KEY_META_CID_NAME, CIDName}
             ,{?KEY_META_CALL_ID, kapps_call:call_id(Call)}
             ,{?VM_KEY_FOLDER, ?VM_FOLDER_NEW}
             ,{?KEY_META_LENGTH, Length}
             ,{?KEY_MEDIA_ID, MediaId}
            ])
        ).

%%--------------------------------------------------------------------
%% @public
%% @doc Accessors methods
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

-spec folder(doc()) -> api_object().
folder(JObj) ->
    folder(JObj, 'undefined').

-spec folder(doc(), Default) -> kz_json:object() | Default.
folder(JObj, Default) ->
    kz_json:get_value(?VM_KEY_FOLDER, JObj, Default).

-spec set_folder(api_binary(), doc()) -> doc().
set_folder(Folder, JObj) ->
    kz_json:set_value(?VM_KEY_FOLDER, Folder, JObj).

-spec set_folder_new(doc()) -> doc().
set_folder_new(JObj) ->
    kz_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_NEW, JObj).

-spec set_folder_saved(doc()) -> doc().
set_folder_saved(JObj) ->
    kz_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_SAVED, JObj).

-spec set_folder_deleted(doc()) -> doc().
set_folder_deleted(JObj) ->
    kz_json:set_value(?VM_KEY_FOLDER, ?VM_FOLDER_DELETED, JObj).

-spec media_id(doc()) -> api_binary().
media_id(JObj) ->
    kz_json:get_value(?KEY_MEDIA_ID, JObj).

-spec set_media_id(ne_binary(), doc()) -> doc().
set_media_id(MediaId, JObj) ->
    kz_json:set_value(?KEY_MEDIA_ID, MediaId, JObj).

-spec metadata(doc()) -> doc().
metadata(JObj) ->
    metadata(JObj, 'undefined').

-spec metadata(doc(), Default) -> kz_json:object() | Default.
metadata(JObj, Default) ->
    kz_json:get_value(?KEY_METADATA, JObj, Default).

-spec set_metadata(doc(), doc()) -> doc().
set_metadata(Metadata, JObj) ->
    kz_json:set_value(?KEY_METADATA, Metadata, JObj).

-spec utc_seconds(doc()) -> pos_integer().
utc_seconds(JObj) ->
    kz_json:get_integer_value(?KEY_UTC_SEC, JObj).

-spec source_id(doc()) -> ne_binary().
source_id(JObj) ->
    kz_json:get_value(?KEY_SOURCE_ID, JObj).

-spec set_source_id(api_binary(), doc()) -> doc().
set_source_id(SourceId, JObj) ->
    kz_json:set_value(?KEY_SOURCE_ID, SourceId, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc Filter messages based on specific folder
%% @end
%%--------------------------------------------------------------------
-spec filter_folder(kz_json:objects(), ne_binary()) -> kz_json:objects().
filter_folder(Messages, Folder) ->
    [M || M <- Messages, folder(M) =:= Folder].

%%--------------------------------------------------------------------
%% @public
%% @doc Count mailbox_messages/count_per_folder view results
%% @end
%%--------------------------------------------------------------------
-spec normalize_count(kz_json:objects()) -> {non_neg_integer(), non_neg_integer()}.
normalize_count(ViewRes) ->
    Props = [{kz_json:get_value([<<"key">>, 2], Msg)
              ,kz_json:get_integer_value(<<"value">>, Msg)
             }
             || Msg <- ViewRes
            ],
    {props:get_integer_value(<<"new">>, Props, 0)
     ,props:get_integer_value(<<"saved">>, Props, 0)
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc Count message list in specific folder(s)
%% @end
%%--------------------------------------------------------------------
-spec count_folder(kz_json:objects(), ne_binary() | ne_binaries()) -> non_neg_integer().
count_folder(Messages, Folders) when is_list(Folders) ->
    lists:sum([1 || Message <- Messages,
                    begin
                        F = kz_json:get_value(?VM_KEY_FOLDER, Message),
                        lists:member(F, Folders)
                    end
              ]);
count_folder(Messages, Folder) ->
    count_folder(Messages, [Folder]).
