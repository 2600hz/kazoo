%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Device document manipulation
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_media).

-export([new/0]).
-export([content_length/1, content_length/2, set_content_length/2]).
-export([content_type/1, content_type/2, set_content_type/2]).
-export([description/1, description/2, set_description/2]).
-export([language/1, language/2, set_language/2]).
-export([media_source/1, media_source/2, set_media_source/2]).
-export([name/1, name/2, set_name/2]).
-export([prompt_id/1, prompt_id/2, set_prompt_id/2
        ,is_prompt/1
        ]).
-export([source_id/1, source_id/2, set_source_id/2]).
-export([source_type/1, source_type/2, set_source_type/2]).
-export([streamable/1, streamable/2, set_streamable/2]).
-export([tts/1, tts/2, set_tts/2]).
-export([tts_text/1, tts_text/2, set_tts_text/2]).
-export([tts_voice/1, tts_voice/2, set_tts_voice/2]).
-export([type/0, type/1, set_type/1]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"media">>).
-define(PVT_TYPE, <<"media">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec content_length(doc()) -> kz_term:api_integer().
content_length(Doc) ->
    content_length(Doc, 'undefined').

-spec content_length(doc(), Default) -> integer() | Default.
content_length(Doc, Default) ->
    kz_json:get_integer_value([<<"content_length">>], Doc, Default).

-spec set_content_length(doc(), integer()) -> doc().
set_content_length(Doc, ContentLength) ->
    kz_json:set_value([<<"content_length">>], ContentLength, Doc).

-spec content_type(doc()) -> kz_term:api_binary().
content_type(Doc) ->
    content_type(Doc, 'undefined').

-spec content_type(doc(), Default) -> binary() | Default.
content_type(Doc, Default) ->
    kz_json:get_binary_value([<<"content_type">>], Doc, Default).

-spec set_content_type(doc(), binary()) -> doc().
set_content_type(Doc, ContentType) ->
    kz_json:set_value([<<"content_type">>], ContentType, Doc).

-spec description(doc()) -> kz_term:api_ne_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> kz_term:ne_binary() | Default.
description(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), kz_term:ne_binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec language(doc()) -> binary().
language(Doc) ->
    language(Doc, <<"en-us">>).

-spec language(doc(), Default) -> binary() | Default.
language(Doc, Default) ->
    kz_json:get_binary_value([<<"language">>], Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value([<<"language">>], Language, Doc).

-spec media_source(doc()) -> binary().
media_source(Doc) ->
    media_source(Doc, <<"upload">>).

-spec media_source(doc(), Default) -> binary() | Default.
media_source(Doc, Default) ->
    kz_json:get_binary_value([<<"media_source">>], Doc, Default).

-spec set_media_source(doc(), binary()) -> doc().
set_media_source(Doc, MediaSource) ->
    kz_json:set_value([<<"media_source">>], MediaSource, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec prompt_id(doc()) -> kz_term:api_binary().
prompt_id(Doc) ->
    prompt_id(Doc, 'undefined').

-spec prompt_id(doc(), Default) -> kz_term:ne_binary() | Default.
prompt_id(Doc, Default) ->
    kz_json:get_binary_value([<<"prompt_id">>], Doc, Default).

-spec set_prompt_id(doc(), binary()) -> doc().
set_prompt_id(Doc, PromptId) ->
    kz_json:set_value([<<"prompt_id">>], PromptId, Doc).

-spec is_prompt(doc()) -> boolean().
is_prompt(Doc) ->
    prompt_id(Doc) =/= 'undefined'.

-spec source_id(doc()) -> kz_term:api_ne_binary().
source_id(Doc) ->
    source_id(Doc, 'undefined').

-spec source_id(doc(), Default) -> kz_term:ne_binary() | Default.
source_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"source_id">>], Doc, Default).

-spec set_source_id(doc(), kz_term:ne_binary()) -> doc().
set_source_id(Doc, SourceId) ->
    kz_json:set_value([<<"source_id">>], SourceId, Doc).

-spec source_type(doc()) -> kz_term:api_binary().
source_type(Doc) ->
    source_type(Doc, 'undefined').

-spec source_type(doc(), Default) -> binary() | Default.
source_type(Doc, Default) ->
    kz_json:get_binary_value([<<"source_type">>], Doc, Default).

-spec set_source_type(doc(), binary()) -> doc().
set_source_type(Doc, SourceType) ->
    kz_json:set_value([<<"source_type">>], SourceType, Doc).

-spec streamable(doc()) -> boolean().
streamable(Doc) ->
    streamable(Doc, true).

-spec streamable(doc(), Default) -> boolean() | Default.
streamable(Doc, Default) ->
    kz_json:get_boolean_value([<<"streamable">>], Doc, Default).

-spec set_streamable(doc(), boolean()) -> doc().
set_streamable(Doc, Streamable) ->
    kz_json:set_value([<<"streamable">>], Streamable, Doc).

-spec tts(doc()) -> kz_json:object().
tts(Doc) ->
    tts(Doc, kz_json:new()).

-spec tts(doc(), Default) -> kz_json:object() | Default.
tts(Doc, Default) ->
    kz_json:get_json_value([<<"tts">>], Doc, Default).

-spec set_tts(doc(), kz_json:object()) -> doc().
set_tts(Doc, Tts) ->
    kz_json:set_value([<<"tts">>], Tts, Doc).

-spec tts_text(doc()) -> kz_term:api_ne_binary().
tts_text(Doc) ->
    tts_text(Doc, 'undefined').

-spec tts_text(doc(), Default) -> kz_term:ne_binary() | Default.
tts_text(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"tts">>, <<"text">>], Doc, Default).

-spec set_tts_text(doc(), kz_term:ne_binary()) -> doc().
set_tts_text(Doc, TtsText) ->
    kz_json:set_value([<<"tts">>, <<"text">>], TtsText, Doc).

-spec tts_voice(doc()) -> binary().
tts_voice(Doc) ->
    tts_voice(Doc, <<"female/en-US">>).

-spec tts_voice(doc(), Default) -> binary() | Default.
tts_voice(Doc, Default) ->
    kz_json:get_binary_value([<<"tts">>, <<"voice">>], Doc, Default).

-spec set_tts_voice(doc(), binary()) -> doc().
set_tts_voice(Doc, TtsVoice) ->
    kz_json:set_value([<<"tts">>, <<"voice">>], TtsVoice, Doc).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) -> kz_doc:type(Doc, ?PVT_TYPE).

-spec set_type(doc()) -> doc().
set_type(Doc) -> kz_doc:set_type(Doc, ?PVT_TYPE).
