%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_media).

-export([new/0]).
-export([content_length/1, content_length/2, set_content_length/2]).
-export([content_type/1, content_type/2, set_content_type/2]).
-export([description/1, description/2, set_description/2]).
-export([language/1, language/2, set_language/2]).
-export([media_source/1, media_source/2, set_media_source/2]).
-export([name/1, name/2, set_name/2]).
-export([prompt_id/1, prompt_id/2, set_prompt_id/2]).
-export([source_id/1, source_id/2, set_source_id/2]).
-export([source_type/1, source_type/2, set_source_type/2]).
-export([streamable/1, streamable/2, set_streamable/2]).
-export([tts/1, tts/2, set_tts/2]).
-export([tts_text/1, tts_text/2, set_tts_text/2]).
-export([tts_voice/1, tts_voice/2, set_tts_voice/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec content_length(doc()) -> api_integer().
-spec content_length(doc(), Default) -> integer() | Default.
content_length(Doc) ->
    content_length(Doc, 'undefined').
content_length(Doc, Default) ->
    kz_json:get_integer_value(<<"content_length">>, Doc, Default).

-spec set_content_length(doc(), integer()) -> doc().
set_content_length(Doc, ContentLength) ->
    kz_json:set_value(<<"content_length">>, ContentLength, Doc).

-spec content_type(doc()) -> api_binary().
-spec content_type(doc(), Default) -> binary() | Default.
content_type(Doc) ->
    content_type(Doc, 'undefined').
content_type(Doc, Default) ->
    kz_json:get_binary_value(<<"content_type">>, Doc, Default).

-spec set_content_type(doc(), binary()) -> doc().
set_content_type(Doc, ContentType) ->
    kz_json:set_value(<<"content_type">>, ContentType, Doc).

-spec description(doc()) -> api_ne_binary().
-spec description(doc(), Default) -> ne_binary() | Default.
description(Doc) ->
    description(Doc, 'undefined').
description(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"description">>, Doc, Default).

-spec set_description(doc(), ne_binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value(<<"description">>, Description, Doc).

-spec language(doc()) -> binary().
-spec language(doc(), Default) -> binary() | Default.
language(Doc) ->
    language(Doc, <<"en-us">>).
language(Doc, Default) ->
    kz_json:get_binary_value(<<"language">>, Doc, Default).

-spec set_language(doc(), binary()) -> doc().
set_language(Doc, Language) ->
    kz_json:set_value(<<"language">>, Language, Doc).

-spec media_source(doc()) -> binary().
-spec media_source(doc(), Default) -> binary() | Default.
media_source(Doc) ->
    media_source(Doc, <<"upload">>).
media_source(Doc, Default) ->
    kz_json:get_binary_value(<<"media_source">>, Doc, Default).

-spec set_media_source(doc(), binary()) -> doc().
set_media_source(Doc, MediaSource) ->
    kz_json:set_value(<<"media_source">>, MediaSource, Doc).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec prompt_id(doc()) -> kz_term:api_ne_binary().
prompt_id(Doc) ->
    prompt_id(Doc, 'undefined').

-spec prompt_id(doc(), Default) -> kz_term:ne_binary() | Default.
prompt_id(Doc, Default) ->
    kz_json:get_binary_value(<<"prompt_id">>, Doc, Default).

-spec set_prompt_id(doc(), binary()) -> doc().
set_prompt_id(Doc, PromptId) ->
    kz_json:set_value(<<"prompt_id">>, PromptId, Doc).

-spec language(doc()) -> kz_term:api_ne_binary().
language(Doc) ->
    language(Doc, 'undefined').

-spec language(doc(), Default) -> kz_term:ne_binary() | Default.
language(Doc, Default) ->
    kz_json:get_ne_binary_value(?LANGUAGE, Doc, Default).

-spec content_type(doc()) -> kz_term:api_ne_binary().
content_type(Doc) ->
    content_type(Doc, 'undefined').

-spec content_type(doc(), Default) -> kz_term:ne_binary() | Default.
content_type(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"content_type">>, Doc, Default).
