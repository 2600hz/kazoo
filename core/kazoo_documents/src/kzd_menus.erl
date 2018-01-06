-module(kzd_menus).

-export([new/0]).
-export([allow_record_from_offnet/1, allow_record_from_offnet/2, set_allow_record_from_offnet/2]).
-export([hunt/1, hunt/2, set_hunt/2]).
-export([hunt_allow/1, hunt_allow/2, set_hunt_allow/2]).
-export([hunt_deny/1, hunt_deny/2, set_hunt_deny/2]).
-export([interdigit_timeout/1, interdigit_timeout/2, set_interdigit_timeout/2]).
-export([max_extension_length/1, max_extension_length/2, set_max_extension_length/2]).
-export([media/1, media/2, set_media/2]).
-export([name/1, name/2, set_name/2]).
-export([record_pin/1, record_pin/2, set_record_pin/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([timeout/1, timeout/2, set_timeout/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec allow_record_from_offnet(doc()) -> boolean().
-spec allow_record_from_offnet(doc(), Default) -> boolean() | Default.
allow_record_from_offnet(Doc) ->
    allow_record_from_offnet(Doc, false).
allow_record_from_offnet(Doc, Default) ->
    kz_json:get_boolean_value(<<"allow_record_from_offnet">>, Doc, Default).

-spec set_allow_record_from_offnet(doc(), boolean()) -> doc().
set_allow_record_from_offnet(Doc, AllowRecordFromOffnet) ->
    kz_json:set_value(<<"allow_record_from_offnet">>, AllowRecordFromOffnet, Doc).

-spec hunt(doc()) -> boolean().
-spec hunt(doc(), Default) -> boolean() | Default.
hunt(Doc) ->
    hunt(Doc, true).
hunt(Doc, Default) ->
    kz_json:get_boolean_value(<<"hunt">>, Doc, Default).

-spec set_hunt(doc(), boolean()) -> doc().
set_hunt(Doc, Hunt) ->
    kz_json:set_value(<<"hunt">>, Hunt, Doc).

-spec hunt_allow(doc()) -> api_ne_binary().
-spec hunt_allow(doc(), Default) -> ne_binary() | Default.
hunt_allow(Doc) ->
    hunt_allow(Doc, 'undefined').
hunt_allow(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"hunt_allow">>, Doc, Default).

-spec set_hunt_allow(doc(), ne_binary()) -> doc().
set_hunt_allow(Doc, HuntAllow) ->
    kz_json:set_value(<<"hunt_allow">>, HuntAllow, Doc).

-spec hunt_deny(doc()) -> api_ne_binary().
-spec hunt_deny(doc(), Default) -> ne_binary() | Default.
hunt_deny(Doc) ->
    hunt_deny(Doc, 'undefined').
hunt_deny(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"hunt_deny">>, Doc, Default).

-spec set_hunt_deny(doc(), ne_binary()) -> doc().
set_hunt_deny(Doc, HuntDeny) ->
    kz_json:set_value(<<"hunt_deny">>, HuntDeny, Doc).

-spec interdigit_timeout(doc()) -> api_integer().
-spec interdigit_timeout(doc(), Default) -> integer() | Default.
interdigit_timeout(Doc) ->
    interdigit_timeout(Doc, 'undefined').
interdigit_timeout(Doc, Default) ->
    kz_json:get_integer_value(<<"interdigit_timeout">>, Doc, Default).

-spec set_interdigit_timeout(doc(), integer()) -> doc().
set_interdigit_timeout(Doc, InterdigitTimeout) ->
    kz_json:set_value(<<"interdigit_timeout">>, InterdigitTimeout, Doc).

-spec max_extension_length(doc()) -> integer().
-spec max_extension_length(doc(), Default) -> integer() | Default.
max_extension_length(Doc) ->
    max_extension_length(Doc, 4).
max_extension_length(Doc, Default) ->
    kz_json:get_integer_value(<<"max_extension_length">>, Doc, Default).

-spec set_max_extension_length(doc(), integer()) -> doc().
set_max_extension_length(Doc, MaxExtensionLength) ->
    kz_json:set_value(<<"max_extension_length">>, MaxExtensionLength, Doc).

-spec media(doc()) -> kz_json:object().
-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc) ->
    media(Doc, kz_json:new()).
media(Doc, Default) ->
    kz_json:get_json_value(<<"media">>, Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value(<<"media">>, Media, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec record_pin(doc()) -> api_ne_binary().
-spec record_pin(doc(), Default) -> ne_binary() | Default.
record_pin(Doc) ->
    record_pin(Doc, 'undefined').
record_pin(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"record_pin">>, Doc, Default).

-spec set_record_pin(doc(), ne_binary()) -> doc().
set_record_pin(Doc, RecordPin) ->
    kz_json:set_value(<<"record_pin">>, RecordPin, Doc).

-spec retries(doc()) -> integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Doc) ->
    retries(Doc, 3).
retries(Doc, Default) ->
    kz_json:get_integer_value(<<"retries">>, Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value(<<"retries">>, Retries, Doc).

-spec timeout(doc()) -> api_integer().
-spec timeout(doc(), Default) -> integer() | Default.
timeout(Doc) ->
    timeout(Doc, 'undefined').
timeout(Doc, Default) ->
    kz_json:get_integer_value(<<"timeout">>, Doc, Default).

-spec set_timeout(doc(), integer()) -> doc().
set_timeout(Doc, Timeout) ->
    kz_json:set_value(<<"timeout">>, Timeout, Doc).
