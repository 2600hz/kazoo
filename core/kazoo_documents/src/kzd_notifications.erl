%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_notifications).

-export([new/0]).
-export([bcc/1, bcc/2, set_bcc/2]).
-export([bcc_email_addresses/1, bcc_email_addresses/2, set_bcc_email_addresses/2]).
-export([bcc_type/1, bcc_type/2, set_bcc_type/2]).
-export([category/1, category/2, set_category/2]).
-export([cc/1, cc/2, set_cc/2]).
-export([cc_email_addresses/1, cc_email_addresses/2, set_cc_email_addresses/2]).
-export([cc_type/1, cc_type/2, set_cc_type/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([friendly_name/1, friendly_name/2, set_friendly_name/2]).
-export([from/1, from/2, set_from/2]).
-export([macros/1, macros/2, set_macros/2]).
-export([reply_to/1, reply_to/2, set_reply_to/2]).
-export([subject/1, subject/2, set_subject/2]).
-export([template_charset/1, template_charset/2, set_template_charset/2]).
-export([to/1, to/2, set_to/2]).
-export([to_email_addresses/1, to_email_addresses/2, set_to_email_addresses/2]).
-export([to_type/1, to_type/2, set_to_type/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"notifications">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec bcc(doc()) -> kz_term:api_object().
bcc(Doc) ->
    bcc(Doc, 'undefined').

-spec bcc(doc(), Default) -> kz_json:object() | Default.
bcc(Doc, Default) ->
    kz_json:get_json_value([<<"bcc">>], Doc, Default).

-spec set_bcc(doc(), kz_json:object()) -> doc().
set_bcc(Doc, Bcc) ->
    kz_json:set_value([<<"bcc">>], Bcc, Doc).

-spec bcc_email_addresses(doc()) -> kz_term:api_ne_binaries().
bcc_email_addresses(Doc) ->
    bcc_email_addresses(Doc, 'undefined').

-spec bcc_email_addresses(doc(), Default) -> kz_term:ne_binaries() | Default.
bcc_email_addresses(Doc, Default) ->
    kz_json:get_list_value([<<"bcc">>, <<"email_addresses">>], Doc, Default).

-spec set_bcc_email_addresses(doc(), kz_term:ne_binaries()) -> doc().
set_bcc_email_addresses(Doc, BccEmailAddresses) ->
    kz_json:set_value([<<"bcc">>, <<"email_addresses">>], BccEmailAddresses, Doc).

-spec bcc_type(doc()) -> kz_term:api_ne_binary().
bcc_type(Doc) ->
    bcc_type(Doc, 'undefined').

-spec bcc_type(doc(), Default) -> kz_term:ne_binary() | Default.
bcc_type(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"bcc">>, <<"type">>], Doc, Default).

-spec set_bcc_type(doc(), kz_term:ne_binary()) -> doc().
set_bcc_type(Doc, BccType) ->
    kz_json:set_value([<<"bcc">>, <<"type">>], BccType, Doc).

-spec category(doc()) -> kz_term:api_ne_binary().
category(Doc) ->
    category(Doc, 'undefined').

-spec category(doc(), Default) -> kz_term:ne_binary() | Default.
category(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"category">>], Doc, Default).

-spec set_category(doc(), kz_term:ne_binary()) -> doc().
set_category(Doc, Category) ->
    kz_json:set_value([<<"category">>], Category, Doc).

-spec cc(doc()) -> kz_term:api_object().
cc(Doc) ->
    cc(Doc, 'undefined').

-spec cc(doc(), Default) -> kz_json:object() | Default.
cc(Doc, Default) ->
    kz_json:get_json_value([<<"cc">>], Doc, Default).

-spec set_cc(doc(), kz_json:object()) -> doc().
set_cc(Doc, Cc) ->
    kz_json:set_value([<<"cc">>], Cc, Doc).

-spec cc_email_addresses(doc()) -> kz_term:api_ne_binaries().
cc_email_addresses(Doc) ->
    cc_email_addresses(Doc, 'undefined').

-spec cc_email_addresses(doc(), Default) -> kz_term:ne_binaries() | Default.
cc_email_addresses(Doc, Default) ->
    kz_json:get_list_value([<<"cc">>, <<"email_addresses">>], Doc, Default).

-spec set_cc_email_addresses(doc(), kz_term:ne_binaries()) -> doc().
set_cc_email_addresses(Doc, CcEmailAddresses) ->
    kz_json:set_value([<<"cc">>, <<"email_addresses">>], CcEmailAddresses, Doc).

-spec cc_type(doc()) -> kz_term:api_binary().
cc_type(Doc) ->
    cc_type(Doc, 'undefined').

-spec cc_type(doc(), Default) -> binary() | Default.
cc_type(Doc, Default) ->
    kz_json:get_binary_value([<<"cc">>, <<"type">>], Doc, Default).

-spec set_cc_type(doc(), binary()) -> doc().
set_cc_type(Doc, CcType) ->
    kz_json:set_value([<<"cc">>, <<"type">>], CcType, Doc).

-spec enabled(doc()) -> boolean().
enabled(Doc) ->
    enabled(Doc, true).

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).

-spec friendly_name(doc()) -> kz_term:api_ne_binary().
friendly_name(Doc) ->
    friendly_name(Doc, 'undefined').

-spec friendly_name(doc(), Default) -> kz_term:ne_binary() | Default.
friendly_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"friendly_name">>], Doc, Default).

-spec set_friendly_name(doc(), kz_term:ne_binary()) -> doc().
set_friendly_name(Doc, FriendlyName) ->
    kz_json:set_value([<<"friendly_name">>], FriendlyName, Doc).

-spec from(doc()) -> kz_term:api_binary().
from(Doc) ->
    from(Doc, 'undefined').

-spec from(doc(), Default) -> binary() | Default.
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec macros(doc()) -> kz_json:object().
macros(Doc) ->
    macros(Doc, kz_json:new()).

-spec macros(doc(), Default) -> kz_json:object() | Default.
macros(Doc, Default) ->
    kz_json:get_json_value([<<"macros">>], Doc, Default).

-spec set_macros(doc(), kz_json:object()) -> doc().
set_macros(Doc, Macros) ->
    kz_json:set_value([<<"macros">>], Macros, Doc).

-spec reply_to(doc()) -> kz_term:api_binary().
reply_to(Doc) ->
    reply_to(Doc, 'undefined').

-spec reply_to(doc(), Default) -> binary() | Default.
reply_to(Doc, Default) ->
    kz_json:get_binary_value([<<"reply_to">>], Doc, Default).

-spec set_reply_to(doc(), binary()) -> doc().
set_reply_to(Doc, ReplyTo) ->
    kz_json:set_value([<<"reply_to">>], ReplyTo, Doc).

-spec subject(doc()) -> kz_term:api_ne_binary().
subject(Doc) ->
    subject(Doc, 'undefined').

-spec subject(doc(), Default) -> kz_term:ne_binary() | Default.
subject(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"subject">>], Doc, Default).

-spec set_subject(doc(), kz_term:ne_binary()) -> doc().
set_subject(Doc, Subject) ->
    kz_json:set_value([<<"subject">>], Subject, Doc).

-spec template_charset(doc()) -> kz_term:ne_binary().
template_charset(Doc) ->
    template_charset(Doc, <<"utf-8">>).

-spec template_charset(doc(), Default) -> kz_term:ne_binary() | Default.
template_charset(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"template_charset">>], Doc, Default).

-spec set_template_charset(doc(), kz_term:ne_binary()) -> doc().
set_template_charset(Doc, TemplateCharset) ->
    kz_json:set_value([<<"template_charset">>], TemplateCharset, Doc).

-spec to(doc()) -> kz_term:api_object().
to(Doc) ->
    to(Doc, 'undefined').

-spec to(doc(), Default) -> kz_json:object() | Default.
to(Doc, Default) ->
    kz_json:get_json_value([<<"to">>], Doc, Default).

-spec set_to(doc(), kz_json:object()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).

-spec to_email_addresses(doc()) -> kz_term:api_ne_binaries().
to_email_addresses(Doc) ->
    to_email_addresses(Doc, 'undefined').

-spec to_email_addresses(doc(), Default) -> kz_term:ne_binaries() | Default.
to_email_addresses(Doc, Default) ->
    kz_json:get_list_value([<<"to">>, <<"email_addresses">>], Doc, Default).

-spec set_to_email_addresses(doc(), kz_term:ne_binaries()) -> doc().
set_to_email_addresses(Doc, ToEmailAddresses) ->
    kz_json:set_value([<<"to">>, <<"email_addresses">>], ToEmailAddresses, Doc).

-spec to_type(doc()) -> kz_term:api_ne_binary().
to_type(Doc) ->
    to_type(Doc, 'undefined').

-spec to_type(doc(), Default) -> kz_term:ne_binary() | Default.
to_type(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"to">>, <<"type">>], Doc, Default).

-spec set_to_type(doc(), kz_term:ne_binary()) -> doc().
set_to_type(Doc, ToType) ->
    kz_json:set_value([<<"to">>, <<"type">>], ToType, Doc).
