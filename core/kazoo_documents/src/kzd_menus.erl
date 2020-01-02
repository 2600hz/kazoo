%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_menus).

-export([new/0]).
-export([allow_record_from_offnet/1, allow_record_from_offnet/2, set_allow_record_from_offnet/2]).
-export([flags/1, flags/2, set_flags/2]).
-export([hunt/1, hunt/2, set_hunt/2]).
-export([hunt_allow/1, hunt_allow/2, set_hunt_allow/2]).
-export([hunt_deny/1, hunt_deny/2, set_hunt_deny/2]).
-export([interdigit_timeout/1, interdigit_timeout/2, set_interdigit_timeout/2]).
-export([max_extension_length/1, max_extension_length/2, set_max_extension_length/2]).
-export([media/1, media/2, set_media/2]).
-export([media_exit_media/1, media_exit_media/2, set_media_exit_media/2]).
-export([media_greeting/1, media_greeting/2, set_media_greeting/2]).
-export([media_invalid_media/1, media_invalid_media/2, set_media_invalid_media/2]).
-export([media_transfer_media/1, media_transfer_media/2, set_media_transfer_media/2]).
-export([name/1, name/2, set_name/2]).
-export([record_pin/1, record_pin/2, set_record_pin/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([timeout/1, timeout/2, set_timeout/2]).

-export([schema_name/0
        ,type/0
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"menus">>).
-define(TYPE, <<"menu">>).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec allow_record_from_offnet(doc()) -> boolean().
allow_record_from_offnet(Doc) ->
    allow_record_from_offnet(Doc, false).

-spec allow_record_from_offnet(doc(), Default) -> boolean() | Default.
allow_record_from_offnet(Doc, Default) ->
    kz_json:get_boolean_value([<<"allow_record_from_offnet">>], Doc, Default).

-spec set_allow_record_from_offnet(doc(), boolean()) -> doc().
set_allow_record_from_offnet(Doc, AllowRecordFromOffnet) ->
    kz_json:set_value([<<"allow_record_from_offnet">>], AllowRecordFromOffnet, Doc).

-spec flags(doc()) -> kz_term:api_ne_binaries().
flags(Doc) ->
    flags(Doc, 'undefined').

-spec flags(doc(), Default) -> kz_term:ne_binaries() | Default.
flags(Doc, Default) ->
    kz_json:get_list_value([<<"flags">>], Doc, Default).

-spec set_flags(doc(), kz_term:ne_binaries()) -> doc().
set_flags(Doc, Flags) ->
    kz_json:set_value([<<"flags">>], Flags, Doc).

-spec hunt(doc()) -> boolean().
hunt(Doc) ->
    hunt(Doc, true).

-spec hunt(doc(), Default) -> boolean() | Default.
hunt(Doc, Default) ->
    kz_json:get_boolean_value([<<"hunt">>], Doc, Default).

-spec set_hunt(doc(), boolean()) -> doc().
set_hunt(Doc, Hunt) ->
    kz_json:set_value([<<"hunt">>], Hunt, Doc).

-spec hunt_allow(doc()) -> kz_term:api_ne_binary().
hunt_allow(Doc) ->
    hunt_allow(Doc, 'undefined').

-spec hunt_allow(doc(), Default) -> kz_term:ne_binary() | Default.
hunt_allow(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hunt_allow">>], Doc, Default).

-spec set_hunt_allow(doc(), kz_term:ne_binary()) -> doc().
set_hunt_allow(Doc, HuntAllow) ->
    kz_json:set_value([<<"hunt_allow">>], HuntAllow, Doc).

-spec hunt_deny(doc()) -> kz_term:api_ne_binary().
hunt_deny(Doc) ->
    hunt_deny(Doc, 'undefined').

-spec hunt_deny(doc(), Default) -> kz_term:ne_binary() | Default.
hunt_deny(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hunt_deny">>], Doc, Default).

-spec set_hunt_deny(doc(), kz_term:ne_binary()) -> doc().
set_hunt_deny(Doc, HuntDeny) ->
    kz_json:set_value([<<"hunt_deny">>], HuntDeny, Doc).

-spec interdigit_timeout(doc()) -> kz_term:api_integer().
interdigit_timeout(Doc) ->
    interdigit_timeout(Doc, 'undefined').

-spec interdigit_timeout(doc(), Default) -> integer() | Default.
interdigit_timeout(Doc, Default) ->
    kz_json:get_integer_value([<<"interdigit_timeout">>], Doc, Default).

-spec set_interdigit_timeout(doc(), integer()) -> doc().
set_interdigit_timeout(Doc, InterdigitTimeout) ->
    kz_json:set_value([<<"interdigit_timeout">>], InterdigitTimeout, Doc).

-spec max_extension_length(doc()) -> integer().
max_extension_length(Doc) ->
    max_extension_length(Doc, 4).

-spec max_extension_length(doc(), Default) -> integer() | Default.
max_extension_length(Doc, Default) ->
    kz_json:get_integer_value([<<"max_extension_length">>], Doc, Default).

-spec set_max_extension_length(doc(), integer()) -> doc().
set_max_extension_length(Doc, MaxExtensionLength) ->
    kz_json:set_value([<<"max_extension_length">>], MaxExtensionLength, Doc).

-spec media(doc()) -> kz_json:object().
media(Doc) ->
    media(Doc, kz_json:new()).

-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc, Default) ->
    kz_json:get_json_value([<<"media">>], Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value([<<"media">>], Media, Doc).

-spec media_exit_media(doc()) -> any().
media_exit_media(Doc) ->
    media_exit_media(Doc, 'undefined').

-spec media_exit_media(doc(), Default) -> any() | Default.
media_exit_media(Doc, Default) ->
    kz_json:get_value([<<"media">>, <<"exit_media">>], Doc, Default).

-spec set_media_exit_media(doc(), any()) -> doc().
set_media_exit_media(Doc, MediaExitMedia) ->
    kz_json:set_value([<<"media">>, <<"exit_media">>], MediaExitMedia, Doc).

-spec media_greeting(doc()) -> kz_term:api_ne_binary().
media_greeting(Doc) ->
    media_greeting(Doc, 'undefined').

-spec media_greeting(doc(), Default) -> kz_term:ne_binary() | Default.
media_greeting(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"media">>, <<"greeting">>], Doc, Default).

-spec set_media_greeting(doc(), kz_term:ne_binary()) -> doc().
set_media_greeting(Doc, MediaGreeting) ->
    kz_json:set_value([<<"media">>, <<"greeting">>], MediaGreeting, Doc).

-spec media_invalid_media(doc()) -> any().
media_invalid_media(Doc) ->
    media_invalid_media(Doc, 'undefined').

-spec media_invalid_media(doc(), Default) -> any() | Default.
media_invalid_media(Doc, Default) ->
    kz_json:get_value([<<"media">>, <<"invalid_media">>], Doc, Default).

-spec set_media_invalid_media(doc(), any()) -> doc().
set_media_invalid_media(Doc, MediaInvalidMedia) ->
    kz_json:set_value([<<"media">>, <<"invalid_media">>], MediaInvalidMedia, Doc).

-spec media_transfer_media(doc()) -> any().
media_transfer_media(Doc) ->
    media_transfer_media(Doc, 'undefined').

-spec media_transfer_media(doc(), Default) -> any() | Default.
media_transfer_media(Doc, Default) ->
    kz_json:get_value([<<"media">>, <<"transfer_media">>], Doc, Default).

-spec set_media_transfer_media(doc(), any()) -> doc().
set_media_transfer_media(Doc, MediaTransferMedia) ->
    kz_json:set_value([<<"media">>, <<"transfer_media">>], MediaTransferMedia, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec record_pin(doc()) -> kz_term:api_ne_binary().
record_pin(Doc) ->
    record_pin(Doc, 'undefined').

-spec record_pin(doc(), Default) -> kz_term:ne_binary() | Default.
record_pin(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"record_pin">>], Doc, Default).

-spec set_record_pin(doc(), kz_term:ne_binary()) -> doc().
set_record_pin(Doc, RecordPin) ->
    kz_json:set_value([<<"record_pin">>], RecordPin, Doc).

-spec retries(doc()) -> integer().
retries(Doc) ->
    retries(Doc, 3).

-spec retries(doc(), Default) -> integer() | Default.
retries(Doc, Default) ->
    kz_json:get_integer_value([<<"retries">>], Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value([<<"retries">>], Retries, Doc).

-spec timeout(doc()) -> kz_term:api_integer().
timeout(Doc) ->
    timeout(Doc, 'undefined').

-spec timeout(doc(), Default) -> integer() | Default.
timeout(Doc, Default) ->
    kz_json:get_integer_value([<<"timeout">>], Doc, Default).

-spec set_timeout(doc(), integer()) -> doc().
set_timeout(Doc, Timeout) ->
    kz_json:set_value([<<"timeout">>], Timeout, Doc).

-spec schema_name() -> kz_term:ne_binary().
schema_name() -> ?SCHEMA.

-spec type() -> kz_term:ne_binary().
type() -> ?TYPE.
