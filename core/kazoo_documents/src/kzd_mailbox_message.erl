%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Accessors for `mailbox_message' document.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_mailbox_message).

-export([new/0]).
-export([call_id/1, call_id/2, set_call_id/2]).
-export([caller_id_name/1, caller_id_name/2, set_caller_id_name/2]).
-export([caller_id_number/1, caller_id_number/2, set_caller_id_number/2]).
-export([folder/1, folder/2, set_folder/2]).
-export([from/1, from/2, set_from/2]).
-export([length/1, length/2, set_length/2]).
-export([media_id/1, media_id/2, set_media_id/2]).
-export([timestamp/1, timestamp/2, set_timestamp/2]).
-export([to/1, to/2, set_to/2]).
-export([transcription/1, transcription/2, set_transcription/2]).
-export([transcription_result/1, transcription_result/2, set_transcription_result/2]).
-export([transcription_text/1, transcription_text/2, set_transcription_text/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"mailbox_message">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec call_id(doc()) -> kz_term:api_binary().
call_id(Doc) ->
    call_id(Doc, 'undefined').

-spec call_id(doc(), Default) -> binary() | Default.
call_id(Doc, Default) ->
    kz_json:get_binary_value([<<"call_id">>], Doc, Default).

-spec set_call_id(doc(), binary()) -> doc().
set_call_id(Doc, CallId) ->
    kz_json:set_value([<<"call_id">>], CallId, Doc).

-spec caller_id_name(doc()) -> kz_term:api_binary().
caller_id_name(Doc) ->
    caller_id_name(Doc, 'undefined').

-spec caller_id_name(doc(), Default) -> binary() | Default.
caller_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_name">>], Doc, Default).

-spec set_caller_id_name(doc(), binary()) -> doc().
set_caller_id_name(Doc, CallerIdName) ->
    kz_json:set_value([<<"caller_id_name">>], CallerIdName, Doc).

-spec caller_id_number(doc()) -> kz_term:api_binary().
caller_id_number(Doc) ->
    caller_id_number(Doc, 'undefined').

-spec caller_id_number(doc(), Default) -> binary() | Default.
caller_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_number">>], Doc, Default).

-spec set_caller_id_number(doc(), binary()) -> doc().
set_caller_id_number(Doc, CallerIdNumber) ->
    kz_json:set_value([<<"caller_id_number">>], CallerIdNumber, Doc).

-spec folder(doc()) -> kz_term:api_binary().
folder(Doc) ->
    folder(Doc, 'undefined').

-spec folder(doc(), Default) -> binary() | Default.
folder(Doc, Default) ->
    kz_json:get_binary_value([<<"folder">>], Doc, Default).

-spec set_folder(doc(), binary()) -> doc().
set_folder(Doc, Folder) ->
    kz_json:set_value([<<"folder">>], Folder, Doc).

-spec from(doc()) -> kz_term:api_binary().
from(Doc) ->
    from(Doc, 'undefined').

-spec from(doc(), Default) -> binary() | Default.
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec length(doc()) -> kz_term:api_integer().
length(Doc) ->
    length(Doc, 'undefined').

-spec length(doc(), Default) -> integer() | Default.
length(Doc, Default) ->
    kz_json:get_integer_value([<<"length">>], Doc, Default).

-spec set_length(doc(), integer()) -> doc().
set_length(Doc, Length) ->
    kz_json:set_value([<<"length">>], Length, Doc).

-spec media_id(doc()) -> kz_term:api_ne_binary().
media_id(Doc) ->
    media_id(Doc, 'undefined').

-spec media_id(doc(), Default) -> kz_term:ne_binary() | Default.
media_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"media_id">>], Doc, Default).

-spec set_media_id(doc(), kz_term:ne_binary()) -> doc().
set_media_id(Doc, MediaId) ->
    kz_json:set_value([<<"media_id">>], MediaId, Doc).

-spec timestamp(doc()) -> kz_term:api_integer().
timestamp(Doc) ->
    timestamp(Doc, 'undefined').

-spec timestamp(doc(), Default) -> integer() | Default.
timestamp(Doc, Default) ->
    kz_json:get_integer_value([<<"timestamp">>], Doc, Default).

-spec set_timestamp(doc(), integer()) -> doc().
set_timestamp(Doc, Timestamp) ->
    kz_json:set_value([<<"timestamp">>], Timestamp, Doc).

-spec to(doc()) -> kz_term:api_binary().
to(Doc) ->
    to(Doc, 'undefined').

-spec to(doc(), Default) -> binary() | Default.
to(Doc, Default) ->
    kz_json:get_binary_value([<<"to">>], Doc, Default).

-spec set_to(doc(), binary()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).

-spec transcription(doc()) -> kz_term:api_object().
transcription(Doc) ->
    transcription(Doc, 'undefined').

-spec transcription(doc(), Default) -> kz_json:object() | Default.
transcription(Doc, Default) ->
    kz_json:get_json_value([<<"transcription">>], Doc, Default).

-spec set_transcription(doc(), kz_json:object()) -> doc().
set_transcription(Doc, Transcription) ->
    kz_json:set_value([<<"transcription">>], Transcription, Doc).

-spec transcription_result(doc()) -> kz_term:api_binary().
transcription_result(Doc) ->
    transcription_result(Doc, 'undefined').

-spec transcription_result(doc(), Default) -> binary() | Default.
transcription_result(Doc, Default) ->
    kz_json:get_binary_value([<<"transcription">>, <<"result">>], Doc, Default).

-spec set_transcription_result(doc(), binary()) -> doc().
set_transcription_result(Doc, TranscriptionResult) ->
    kz_json:set_value([<<"transcription">>, <<"result">>], TranscriptionResult, Doc).

-spec transcription_text(doc()) -> kz_term:api_binary().
transcription_text(Doc) ->
    transcription_text(Doc, 'undefined').

-spec transcription_text(doc(), Default) -> binary() | Default.
transcription_text(Doc, Default) ->
    kz_json:get_binary_value([<<"transcription">>, <<"text">>], Doc, Default).

-spec set_transcription_text(doc(), binary()) -> doc().
set_transcription_text(Doc, TranscriptionText) ->
    kz_json:set_value([<<"transcription">>, <<"text">>], TranscriptionText, Doc).
