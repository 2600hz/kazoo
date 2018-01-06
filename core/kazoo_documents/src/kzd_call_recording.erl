-module(kzd_call_recording).

-export([new/0]).
-export([any/1, any/2, set_any/2]).
-export([inbound/1, inbound/2, set_inbound/2]).
-export([outbound/1, outbound/2, set_outbound/2]).


-include("kz_documents.hrl").

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

-spec name(doc()) -> kz_term:ne_binary().
name(Doc) -> kz_json:get_ne_binary_value(<<"name">>, Doc).

-spec description(doc()) -> kz_term:ne_binary().
description(Doc) -> kz_json:get_ne_binary_value(<<"description">>, Doc).

-spec content_type(doc()) -> kz_term:ne_binary().
content_type(Doc) -> kz_json:get_ne_binary_value(<<"content_type">>, Doc).

-spec media_type(doc()) -> kz_term:ne_binary().
media_type(Doc) -> kz_json:get_ne_binary_value(<<"media_type">>, Doc).

-spec media_source(doc()) -> kz_term:ne_binary().
media_source(Doc) -> kz_json:get_ne_binary_value(<<"media_source">>, Doc).

-spec source_type(doc()) -> kz_term:ne_binary().
source_type(Doc) -> kz_json:get_ne_binary_value(<<"source_type">>, Doc).

-spec from(doc()) -> kz_term:ne_binary().
from(Doc) -> kz_json:get_ne_binary_value(<<"from">>, Doc).

-spec to(doc()) -> kz_term:ne_binary().
to(Doc) -> kz_json:get_ne_binary_value(<<"to">>, Doc).

-spec request(doc()) -> kz_term:ne_binary().
request(Doc) -> kz_json:get_ne_binary_value(<<"request">>, Doc).

-spec direction(doc()) -> kz_term:ne_binary().
direction(Doc) -> kz_json:get_ne_binary_value(<<"direction">>, Doc).

-spec start(doc()) -> kz_time:gregorian_seconds().
start(Doc) -> kz_json:get_integer_value(<<"start">>, Doc).

-spec duration(doc()) -> non_neg_integer().
duration(Doc) -> kz_json:get_integer_value(<<"duration">>, Doc).

-spec duration_ms(doc()) -> non_neg_integer().
duration_ms(Doc) -> kz_json:get_integer_value(<<"duration_ms">>, Doc).

-spec caller_id_number(doc()) -> kz_term:ne_binary().
caller_id_number(Doc) -> kz_json:get_ne_binary_value(<<"caller_id_number">>, Doc).

-spec caller_id_name(doc()) -> kz_term:ne_binary().
caller_id_name(Doc) -> kz_json:get_ne_binary_value(<<"caller_id_name">>, Doc).

-spec callee_id_number(doc()) -> kz_term:ne_binary().
callee_id_number(Doc) -> kz_json:get_ne_binary_value(<<"callee_id_number">>, Doc).

-spec callee_id_name(doc()) -> kz_term:ne_binary().
callee_id_name(Doc) -> kz_json:get_ne_binary_value(<<"callee_id_name">>, Doc).

-spec call_id(doc()) -> kz_term:ne_binary().
call_id(Doc) -> kz_json:get_ne_binary_value(<<"call_id">>, Doc).

-spec owner_id(doc()) -> kz_term:ne_binary().
owner_id(Doc) -> kz_json:get_ne_binary_value(<<"owner_id">>, Doc).

-spec url(doc()) -> kz_term:ne_binary().
url(Doc) -> kz_json:get_ne_binary_value(<<"url">>, Doc).

-spec cdr_id(doc()) -> kz_term:ne_binary().
cdr_id(Doc) -> kz_json:get_ne_binary_value(<<"cdr_id">>, Doc).

-spec interaction_id(doc()) -> kz_term:ne_binary().
interaction_id(Doc) -> kz_json:get_ne_binary_value(<<"interaction_id">>, Doc).

-spec origin(doc()) -> kz_term:ne_binary().
origin(Doc) -> kz_json:get_ne_binary_value(<<"origin">>, Doc).

-spec custom_channel_vars(doc()) -> kz_json:object().
custom_channel_vars(Doc) -> kz_json:get_json_value(<<"custom_channel_vars">>, Doc).
