%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `clicktocall' document.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_clicktocall).

-export([new/0]).
-export([auth_required/1, auth_required/2, set_auth_required/2]).
-export([bypass_media/1, bypass_media/2, set_bypass_media/2]).
-export([caller_id_number/1, caller_id_number/2, set_caller_id_number/2]).
-export([custom_application_vars/1, custom_application_vars/2, set_custom_application_vars/2]).
-export([custom_application_var/2, custom_application_var/3, set_custom_application_var/3]).
-export([custom_sip_headers/1, custom_sip_headers/2, set_custom_sip_headers/2]).
-export([dial_first/1, dial_first/2, set_dial_first/2]).
-export([extension/1, extension/2, set_extension/2]).
-export([media/1, media/2, set_media/2]).
-export([media_ignore_early_media/1, media_ignore_early_media/2, set_media_ignore_early_media/2]).
-export([music_on_hold/1, music_on_hold/2, set_music_on_hold/2]).
-export([music_on_hold_media_id/1, music_on_hold_media_id/2, set_music_on_hold_media_id/2]).
-export([name/1, name/2, set_name/2]).
-export([outbound_callee_id_name/1, outbound_callee_id_name/2, set_outbound_callee_id_name/2]).
-export([outbound_callee_id_number/1, outbound_callee_id_number/2, set_outbound_callee_id_number/2]).
-export([presence_id/1, presence_id/2, set_presence_id/2]).
-export([ringback/1, ringback/2, set_ringback/2]).
-export([throttle/1, throttle/2, set_throttle/2]).
-export([timeout/1, timeout/2, set_timeout/2]).
-export([whitelist/1, whitelist/2, set_whitelist/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"clicktocall">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec auth_required(doc()) -> boolean().
auth_required(Doc) ->
    auth_required(Doc, true).

-spec auth_required(doc(), Default) -> boolean() | Default.
auth_required(Doc, Default) ->
    kz_json:get_boolean_value([<<"auth_required">>], Doc, Default).

-spec set_auth_required(doc(), boolean()) -> doc().
set_auth_required(Doc, AuthRequired) ->
    kz_json:set_value([<<"auth_required">>], AuthRequired, Doc).

-spec bypass_media(doc()) -> any().
bypass_media(Doc) ->
    bypass_media(Doc, 'undefined').

-spec bypass_media(doc(), Default) -> any() | Default.
bypass_media(Doc, Default) ->
    kz_json:get_value([<<"bypass_media">>], Doc, Default).

-spec set_bypass_media(doc(), any()) -> doc().
set_bypass_media(Doc, BypassMedia) ->
    kz_json:set_value([<<"bypass_media">>], BypassMedia, Doc).

-spec caller_id_number(doc()) -> kz_term:api_binary().
caller_id_number(Doc) ->
    caller_id_number(Doc, 'undefined').

-spec caller_id_number(doc(), Default) -> binary() | Default.
caller_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_number">>], Doc, Default).

-spec set_caller_id_number(doc(), binary()) -> doc().
set_caller_id_number(Doc, CallerIdNumber) ->
    kz_json:set_value([<<"caller_id_number">>], CallerIdNumber, Doc).

-spec custom_application_vars(doc()) -> kz_json:object().
custom_application_vars(Doc) ->
    custom_application_vars(Doc, kz_json:new()).

-spec custom_application_vars(doc(), Default) -> kz_json:object() | Default.
custom_application_vars(Doc, Default) ->
    kz_json:get_json_value([<<"custom_application_vars">>], Doc, Default).

-spec set_custom_application_vars(doc(), kz_json:object()) -> doc().
set_custom_application_vars(Doc, CustomApplicationVars) ->
    kz_json:set_value([<<"custom_application_vars">>], CustomApplicationVars, Doc).

-spec custom_application_var(doc(), kz_json:key()) -> kz_term:api_binary().
custom_application_var(Doc, CustomApplicationVar) ->
    custom_application_var(Doc, CustomApplicationVar, 'undefined').

-spec custom_application_var(doc(), kz_json:key(), Default) -> binary() | Default.
custom_application_var(Doc, CustomApplicationVar, Default) ->
    kz_json:get_binary_value([<<"custom_application_vars">>, CustomApplicationVar], Doc, Default).

-spec set_custom_application_var(doc(), kz_json:key(), binary()) -> doc().
set_custom_application_var(Doc, CustomApplicationVar, Value) ->
    kz_json:set_value([<<"custom_application_vars">>, CustomApplicationVar], Value, Doc).

-spec custom_sip_headers(doc()) -> kz_term:api_object().
custom_sip_headers(Doc) ->
    custom_sip_headers(Doc, 'undefined').

-spec custom_sip_headers(doc(), Default) -> kz_json:object() | Default.
custom_sip_headers(Doc, Default) ->
    kz_json:get_json_value([<<"custom_sip_headers">>], Doc, Default).

-spec set_custom_sip_headers(doc(), kz_json:object()) -> doc().
set_custom_sip_headers(Doc, CustomSipHeaders) ->
    kz_json:set_value([<<"custom_sip_headers">>], CustomSipHeaders, Doc).

-spec dial_first(doc()) -> kz_term:api_binary().
dial_first(Doc) ->
    dial_first(Doc, 'undefined').

-spec dial_first(doc(), Default) -> binary() | Default.
dial_first(Doc, Default) ->
    kz_json:get_binary_value([<<"dial_first">>], Doc, Default).

-spec set_dial_first(doc(), binary()) -> doc().
set_dial_first(Doc, DialFirst) ->
    kz_json:set_value([<<"dial_first">>], DialFirst, Doc).

-spec extension(doc()) -> kz_term:api_binary().
extension(Doc) ->
    extension(Doc, 'undefined').

-spec extension(doc(), Default) -> binary() | Default.
extension(Doc, Default) ->
    kz_json:get_binary_value([<<"extension">>], Doc, Default).

-spec set_extension(doc(), binary()) -> doc().
set_extension(Doc, Extension) ->
    kz_json:set_value([<<"extension">>], Extension, Doc).

-spec media(doc()) -> kz_term:api_object().
media(Doc) ->
    media(Doc, 'undefined').

-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc, Default) ->
    kz_json:get_json_value([<<"media">>], Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value([<<"media">>], Media, Doc).

-spec media_ignore_early_media(doc()) -> kz_term:api_boolean().
media_ignore_early_media(Doc) ->
    media_ignore_early_media(Doc, 'undefined').

-spec media_ignore_early_media(doc(), Default) -> boolean() | Default.
media_ignore_early_media(Doc, Default) ->
    kz_json:get_boolean_value([<<"media">>, <<"ignore_early_media">>], Doc, Default).

-spec set_media_ignore_early_media(doc(), boolean()) -> doc().
set_media_ignore_early_media(Doc, MediaIgnoreEarlyMedia) ->
    kz_json:set_value([<<"media">>, <<"ignore_early_media">>], MediaIgnoreEarlyMedia, Doc).

-spec music_on_hold(doc()) -> kz_term:api_object().
music_on_hold(Doc) ->
    music_on_hold(Doc, 'undefined').

-spec music_on_hold(doc(), Default) -> kz_json:object() | Default.
music_on_hold(Doc, Default) ->
    kz_json:get_json_value([<<"music_on_hold">>], Doc, Default).

-spec set_music_on_hold(doc(), kz_json:object()) -> doc().
set_music_on_hold(Doc, MusicOnHold) ->
    kz_json:set_value([<<"music_on_hold">>], MusicOnHold, Doc).

-spec music_on_hold_media_id(doc()) -> kz_term:api_binary().
music_on_hold_media_id(Doc) ->
    music_on_hold_media_id(Doc, 'undefined').

-spec music_on_hold_media_id(doc(), Default) -> binary() | Default.
music_on_hold_media_id(Doc, Default) ->
    kz_json:get_binary_value([<<"music_on_hold">>, <<"media_id">>], Doc, Default).

-spec set_music_on_hold_media_id(doc(), binary()) -> doc().
set_music_on_hold_media_id(Doc, MusicOnHoldMediaId) ->
    kz_json:set_value([<<"music_on_hold">>, <<"media_id">>], MusicOnHoldMediaId, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec outbound_callee_id_name(doc()) -> kz_term:api_binary().
outbound_callee_id_name(Doc) ->
    outbound_callee_id_name(Doc, 'undefined').

-spec outbound_callee_id_name(doc(), Default) -> binary() | Default.
outbound_callee_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"outbound_callee_id_name">>], Doc, Default).

-spec set_outbound_callee_id_name(doc(), binary()) -> doc().
set_outbound_callee_id_name(Doc, OutboundCalleeIdName) ->
    kz_json:set_value([<<"outbound_callee_id_name">>], OutboundCalleeIdName, Doc).

-spec outbound_callee_id_number(doc()) -> kz_term:api_binary().
outbound_callee_id_number(Doc) ->
    outbound_callee_id_number(Doc, 'undefined').

-spec outbound_callee_id_number(doc(), Default) -> binary() | Default.
outbound_callee_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"outbound_callee_id_number">>], Doc, Default).

-spec set_outbound_callee_id_number(doc(), binary()) -> doc().
set_outbound_callee_id_number(Doc, OutboundCalleeIdNumber) ->
    kz_json:set_value([<<"outbound_callee_id_number">>], OutboundCalleeIdNumber, Doc).

-spec presence_id(doc()) -> kz_term:api_binary().
presence_id(Doc) ->
    presence_id(Doc, 'undefined').

-spec presence_id(doc(), Default) -> binary() | Default.
presence_id(Doc, Default) ->
    kz_json:get_binary_value([<<"presence_id">>], Doc, Default).

-spec set_presence_id(doc(), binary()) -> doc().
set_presence_id(Doc, PresenceId) ->
    kz_json:set_value([<<"presence_id">>], PresenceId, Doc).

-spec ringback(doc()) -> kz_term:api_binary().
ringback(Doc) ->
    ringback(Doc, 'undefined').

-spec ringback(doc(), Default) -> binary() | Default.
ringback(Doc, Default) ->
    kz_json:get_binary_value([<<"ringback">>], Doc, Default).

-spec set_ringback(doc(), binary()) -> doc().
set_ringback(Doc, Ringback) ->
    kz_json:set_value([<<"ringback">>], Ringback, Doc).

-spec throttle(doc()) -> kz_term:api_integer().
throttle(Doc) ->
    throttle(Doc, 'undefined').

-spec throttle(doc(), Default) -> integer() | Default.
throttle(Doc, Default) ->
    kz_json:get_integer_value([<<"throttle">>], Doc, Default).

-spec set_throttle(doc(), integer()) -> doc().
set_throttle(Doc, Throttle) ->
    kz_json:set_value([<<"throttle">>], Throttle, Doc).

-spec timeout(doc()) -> kz_term:api_integer().
timeout(Doc) ->
    timeout(Doc, 'undefined').

-spec timeout(doc(), Default) -> integer() | Default.
timeout(Doc, Default) ->
    kz_json:get_integer_value([<<"timeout">>], Doc, Default).

-spec set_timeout(doc(), integer()) -> doc().
set_timeout(Doc, Timeout) ->
    kz_json:set_value([<<"timeout">>], Timeout, Doc).

-spec whitelist(doc()) -> kz_term:api_ne_binaries().
whitelist(Doc) ->
    whitelist(Doc, 'undefined').

-spec whitelist(doc(), Default) -> kz_term:ne_binaries() | Default.
whitelist(Doc, Default) ->
    kz_json:get_list_value([<<"whitelist">>], Doc, Default).

-spec set_whitelist(doc(), kz_term:ne_binaries()) -> doc().
set_whitelist(Doc, Whitelist) ->
    kz_json:set_value([<<"whitelist">>], Whitelist, Doc).
