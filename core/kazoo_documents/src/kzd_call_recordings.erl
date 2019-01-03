%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_call_recordings).

-export([new/0]).
-export([type/0, type/1]).
-export([call_id/1, call_id/2, set_call_id/2]).
-export([callee_id_name/1, callee_id_name/2, set_callee_id_name/2]).
-export([callee_id_number/1, callee_id_number/2, set_callee_id_number/2]).
-export([caller_id_name/1, caller_id_name/2, set_caller_id_name/2]).
-export([caller_id_number/1, caller_id_number/2, set_caller_id_number/2]).
-export([cdr_id/1, cdr_id/2, set_cdr_id/2]).
-export([content_type/1, content_type/2, set_content_type/2]).
-export([custom_channel_vars/1, custom_channel_vars/2, set_custom_channel_vars/2]).
-export([description/1, description/2, set_description/2]).
-export([direction/1, direction/2, set_direction/2]).
-export([duration/1, duration/2, set_duration/2]).
-export([duration_ms/1, duration_ms/2, set_duration_ms/2]).
-export([from/1, from/2, set_from/2]).
-export([interaction_id/1, interaction_id/2, set_interaction_id/2]).
-export([media_source/1, media_source/2, set_media_source/2]).
-export([media_type/1, media_type/2, set_media_type/2]).
-export([name/1, name/2, set_name/2]).
-export([origin/1, origin/2, set_origin/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([request/1, request/2, set_request/2]).
-export([source_type/1, source_type/2, set_source_type/2]).
-export([start/1, start/2, set_start/2]).
-export([to/1, to/2, set_to/2]).
-export([url/1, url/2, set_url/2]).

-export([set_id/2, set_id/4]).

-include("kz_documents.hrl").

-define(PVT_TYPE, <<"call_recording">>).
-define(SCHEMA, <<"call_recordings">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(doc()) -> kz_term:ne_binary().
type(Doc) ->
    kz_doc:type(Doc, ?PVT_TYPE).

-spec call_id(doc()) -> kz_term:api_binary().
call_id(Doc) ->
    call_id(Doc, 'undefined').

-spec call_id(doc(), Default) -> binary() | Default.
call_id(Doc, Default) ->
    kz_json:get_binary_value([<<"call_id">>], Doc, Default).

-spec set_call_id(doc(), binary()) -> doc().
set_call_id(Doc, CallId) ->
    kz_json:set_value([<<"call_id">>], CallId, Doc).

-spec callee_id_name(doc()) -> kz_term:api_binary().
callee_id_name(Doc) ->
    callee_id_name(Doc, 'undefined').

-spec callee_id_name(doc(), Default) -> binary() | Default.
callee_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"callee_id_name">>], Doc, Default).

-spec set_callee_id_name(doc(), binary()) -> doc().
set_callee_id_name(Doc, CalleeIdName) ->
    kz_json:set_value([<<"callee_id_name">>], CalleeIdName, Doc).

-spec callee_id_number(doc()) -> kz_term:api_binary().
callee_id_number(Doc) ->
    callee_id_number(Doc, 'undefined').

-spec callee_id_number(doc(), Default) -> binary() | Default.
callee_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"callee_id_number">>], Doc, Default).

-spec set_callee_id_number(doc(), binary()) -> doc().
set_callee_id_number(Doc, CalleeIdNumber) ->
    kz_json:set_value([<<"callee_id_number">>], CalleeIdNumber, Doc).

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

-spec cdr_id(doc()) -> kz_term:api_binary().
cdr_id(Doc) ->
    cdr_id(Doc, 'undefined').

-spec cdr_id(doc(), Default) -> binary() | Default.
cdr_id(Doc, Default) ->
    kz_json:get_binary_value([<<"cdr_id">>], Doc, Default).

-spec set_cdr_id(doc(), binary()) -> doc().
set_cdr_id(Doc, CdrId) ->
    kz_json:set_value([<<"cdr_id">>], CdrId, Doc).

-spec content_type(doc()) -> kz_term:api_binary().
content_type(Doc) ->
    content_type(Doc, 'undefined').

-spec content_type(doc(), Default) -> binary() | Default.
content_type(Doc, Default) ->
    kz_json:get_binary_value([<<"content_type">>], Doc, Default).

-spec set_content_type(doc(), binary()) -> doc().
set_content_type(Doc, ContentType) ->
    kz_json:set_value([<<"content_type">>], ContentType, Doc).

-spec custom_channel_vars(doc()) -> kz_term:api_object().
custom_channel_vars(Doc) ->
    custom_channel_vars(Doc, 'undefined').

-spec custom_channel_vars(doc(), Default) -> kz_json:object() | Default.
custom_channel_vars(Doc, Default) ->
    kz_json:get_json_value([<<"custom_channel_vars">>], Doc, Default).

-spec set_custom_channel_vars(doc(), kz_json:object()) -> doc().
set_custom_channel_vars(Doc, CustomChannelVars) ->
    kz_json:set_value([<<"custom_channel_vars">>], CustomChannelVars, Doc).

-spec description(doc()) -> kz_term:api_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> binary() | Default.
description(Doc, Default) ->
    kz_json:get_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec direction(doc()) -> kz_term:api_binary().
direction(Doc) ->
    direction(Doc, 'undefined').

-spec direction(doc(), Default) -> binary() | Default.
direction(Doc, Default) ->
    kz_json:get_binary_value([<<"direction">>], Doc, Default).

-spec set_direction(doc(), binary()) -> doc().
set_direction(Doc, Direction) ->
    kz_json:set_value([<<"direction">>], Direction, Doc).

-spec duration(doc()) -> kz_term:api_integer().
duration(Doc) ->
    duration(Doc, 'undefined').

-spec duration(doc(), Default) -> integer() | Default.
duration(Doc, Default) ->
    kz_json:get_integer_value([<<"duration">>], Doc, Default).

-spec set_duration(doc(), integer()) -> doc().
set_duration(Doc, Duration) ->
    kz_json:set_value([<<"duration">>], Duration, Doc).

-spec duration_ms(doc()) -> kz_term:api_integer().
duration_ms(Doc) ->
    duration_ms(Doc, 'undefined').

-spec duration_ms(doc(), Default) -> integer() | Default.
duration_ms(Doc, Default) ->
    kz_json:get_integer_value([<<"duration_ms">>], Doc, Default).

-spec set_duration_ms(doc(), integer()) -> doc().
set_duration_ms(Doc, DurationMs) ->
    kz_json:set_value([<<"duration_ms">>], DurationMs, Doc).

-spec from(doc()) -> kz_term:api_binary().
from(Doc) ->
    from(Doc, 'undefined').

-spec from(doc(), Default) -> binary() | Default.
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec interaction_id(doc()) -> kz_term:api_binary().
interaction_id(Doc) ->
    interaction_id(Doc, 'undefined').

-spec interaction_id(doc(), Default) -> binary() | Default.
interaction_id(Doc, Default) ->
    kz_json:get_binary_value([<<"interaction_id">>], Doc, Default).

-spec set_interaction_id(doc(), binary()) -> doc().
set_interaction_id(Doc, InteractionId) ->
    kz_json:set_value([<<"interaction_id">>], InteractionId, Doc).

-spec media_source(doc()) -> kz_term:api_binary().
media_source(Doc) ->
    media_source(Doc, 'undefined').

-spec media_source(doc(), Default) -> binary() | Default.
media_source(Doc, Default) ->
    kz_json:get_binary_value([<<"media_source">>], Doc, Default).

-spec set_media_source(doc(), binary()) -> doc().
set_media_source(Doc, MediaSource) ->
    kz_json:set_value([<<"media_source">>], MediaSource, Doc).

-spec media_type(doc()) -> kz_term:api_binary().
media_type(Doc) ->
    media_type(Doc, 'undefined').

-spec media_type(doc(), Default) -> binary() | Default.
media_type(Doc, Default) ->
    kz_json:get_binary_value([<<"media_type">>], Doc, Default).

-spec set_media_type(doc(), binary()) -> doc().
set_media_type(Doc, MediaType) ->
    kz_json:set_value([<<"media_type">>], MediaType, Doc).

-spec name(doc()) -> kz_term:api_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> binary() | Default.
name(Doc, Default) ->
    kz_json:get_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec origin(doc()) -> kz_term:api_binary().
origin(Doc) ->
    origin(Doc, 'undefined').

-spec origin(doc(), Default) -> binary() | Default.
origin(Doc, Default) ->
    kz_json:get_binary_value([<<"origin">>], Doc, Default).

-spec set_origin(doc(), binary()) -> doc().
set_origin(Doc, Origin) ->
    kz_json:set_value([<<"origin">>], Origin, Doc).

-spec owner_id(doc()) -> kz_term:api_binary().
owner_id(Doc) ->
    owner_id(Doc, 'undefined').

-spec owner_id(doc(), Default) -> binary() | Default.
owner_id(Doc, Default) ->
    kz_json:get_binary_value([<<"owner_id">>], Doc, Default).

-spec set_owner_id(doc(), binary()) -> doc().
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value([<<"owner_id">>], OwnerId, Doc).

-spec request(doc()) -> kz_term:api_binary().
request(Doc) ->
    request(Doc, 'undefined').

-spec request(doc(), Default) -> binary() | Default.
request(Doc, Default) ->
    kz_json:get_binary_value([<<"request">>], Doc, Default).

-spec set_request(doc(), binary()) -> doc().
set_request(Doc, Request) ->
    kz_json:set_value([<<"request">>], Request, Doc).

-spec source_type(doc()) -> kz_term:api_binary().
source_type(Doc) ->
    source_type(Doc, 'undefined').

-spec source_type(doc(), Default) -> binary() | Default.
source_type(Doc, Default) ->
    kz_json:get_binary_value([<<"source_type">>], Doc, Default).

-spec set_source_type(doc(), binary()) -> doc().
set_source_type(Doc, SourceType) ->
    kz_json:set_value([<<"source_type">>], SourceType, Doc).

-spec start(doc()) -> kz_term:api_integer().
start(Doc) ->
    start(Doc, 'undefined').

-spec start(doc(), Default) -> integer() | Default.
start(Doc, Default) ->
    kz_json:get_integer_value([<<"start">>], Doc, Default).

-spec set_start(doc(), integer()) -> doc().
set_start(Doc, Start) ->
    kz_json:set_value([<<"start">>], Start, Doc).

-spec to(doc()) -> kz_term:api_binary().
to(Doc) ->
    to(Doc, 'undefined').

-spec to(doc(), Default) -> binary() | Default.
to(Doc, Default) ->
    kz_json:get_binary_value([<<"to">>], Doc, Default).

-spec set_to(doc(), binary()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).

-spec url(doc()) -> kz_term:api_binary().
url(Doc) ->
    url(Doc, 'undefined').

-spec url(doc(), Default) -> binary() | Default.
url(Doc, Default) ->
    kz_json:get_binary_value([<<"url">>], Doc, Default).

-spec set_url(doc(), binary()) -> doc().
set_url(Doc, Url) ->
    kz_json:set_value([<<"url">>], Url, Doc).

-spec set_id(doc(), kz_term:ne_binary()) -> doc().
set_id(Doc, RecordingId) ->
    {Year, Month, _} = erlang:date(),
    set_id(Doc, RecordingId, Year, Month).

-spec set_id(doc(), kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> doc().
set_id(Doc, RecordingId, Year, Month) when is_integer(Year), is_integer(Month) ->
    kz_doc:set_id(Doc, ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), RecordingId)).
