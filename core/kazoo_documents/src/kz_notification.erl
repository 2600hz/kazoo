%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Device document manipulation
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_notification).

-export([macros/1, set_macros/2
        ,macro/2, set_macro/3
        ,subject/1, set_subject/2
        ,category/1, set_category/2
        ,name/1, set_name/2

        ,to/1, set_to/2, to_email_addresses/1, to_email_type/1
        ,cc/1, set_cc/2, cc_email_addresses/1, cc_email_type/1
        ,bcc/1, set_bcc/2, bcc_email_addresses/1, bcc_email_type/1

        ,from/1, set_from/2
        ,reply_to/1, set_reply_to/2

        ,id/1, db_id/1, resp_id/1

        ,set_base_properties/1, set_base_properties/2
        ,base_properties/1, base_properties/2
        ,pvt_type/0, pvt_type/1

        ,is_enabled/1, is_enabled/2
        ]).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-include("kz_documents.hrl").

-define(MACROS, <<"macros">>).
-define(SUBJECT, <<"subject">>).
-define(CATEGORY, <<"category">>).
-define(NAME, <<"friendly_name">>).
-define(TO, <<"to">>).
-define(CC, <<"cc">>).
-define(BCC, <<"bcc">>).
-define(FROM, <<"from">>).
-define(REPLY_TO, <<"reply_to">>).
-define(EMAIL_ADDRESSES, <<"email_addresses">>).
-define(EMAIL_TYPE, <<"type">>).
-define(PVT_TYPE, <<"notification">>).

-spec id(doc()) -> kz_term:api_binary().
id(JObj) -> kz_doc:id(JObj).

-spec db_id(doc() | kz_term:ne_binary()) -> kz_term:api_binary().
db_id(<<_/binary>> = Id) ->
    maybe_add_prefix(Id);
db_id(JObj) ->
    maybe_add_prefix(id(JObj)).

-spec resp_id(doc() | kz_term:ne_binary()) -> kz_term:api_binary().
resp_id(<<_/binary>> = Id) ->
    maybe_rm_prefix(Id);
resp_id(JObj) ->
    maybe_rm_prefix(id(JObj)).

-define(ID_PREFIX, "notification.").
-spec maybe_add_prefix(kz_term:api_binary()) -> kz_term:api_binary().
maybe_add_prefix('undefined') -> 'undefined';
maybe_add_prefix(<<?ID_PREFIX, _/binary>> = Id) -> Id;
maybe_add_prefix(Id) -> <<?ID_PREFIX, Id/binary>>.

-spec maybe_rm_prefix(kz_term:api_binary()) -> kz_term:api_binary().
maybe_rm_prefix('undefined') -> 'undefined';
maybe_rm_prefix(<<?ID_PREFIX, Id/binary>>) -> Id;
maybe_rm_prefix(Id) -> Id.

-spec macros(doc()) -> kz_term:api_object().
macros(JObj) ->
    kz_json:get_value(?MACROS, JObj).

-spec set_macros(doc(), kz_json:object()) -> doc().
set_macros(JObj, Macros) ->
    kz_json:set_value(?MACROS, Macros, JObj).

-spec macro(doc(), kz_json:key()) -> kz_json:api_json_term().
macro(JObj, Key) ->
    kz_json:get_value([?MACROS, Key], JObj).

-spec set_macro(doc(), kz_json:key(), kz_json:json_term()) -> doc().
set_macro(JObj, Key, Value) ->
    kz_json:set_value([?MACROS, Key], Value, JObj).

-spec subject(doc()) -> kz_term:api_binary().
subject(JObj) ->
    kz_json:get_value(?SUBJECT, JObj).

-spec set_subject(doc(), kz_term:ne_binary()) -> doc().
set_subject(JObj, Subject) ->
    kz_json:set_value(?SUBJECT, Subject, JObj).

-spec category(doc()) -> kz_term:api_binary().
category(JObj) ->
    kz_json:get_value(?CATEGORY, JObj).

-spec set_category(doc(), kz_term:ne_binary()) -> doc().
set_category(JObj, Category) ->
    kz_json:set_value(?CATEGORY, Category, JObj).

-spec name(doc()) -> kz_term:api_binary().
name(JObj) ->
    kz_json:get_value(?NAME, JObj).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(JObj, Name) ->
    kz_json:set_value(?NAME, Name, JObj).

-spec to(doc()) -> kz_term:api_object().
to(JObj) ->
    kz_json:get_value(?TO, JObj).

-spec set_to(doc(), kz_json:object()) -> doc().
set_to(JObj, To) ->
    kz_json:set_value(?TO, To, JObj).

-spec to_email_addresses(doc()) -> kz_term:api_binaries().
to_email_addresses(JObj) ->
    kz_json:get_value([?TO, ?EMAIL_ADDRESSES], JObj).

-spec to_email_type(doc()) -> kz_term:api_binary().
to_email_type(JObj) ->
    kz_json:get_value([?TO, ?EMAIL_TYPE], JObj).

-spec cc(doc()) -> kz_term:api_object().
cc(JObj) ->
    kz_json:get_value(?CC, JObj).

-spec set_cc(doc(), kz_json:object()) -> doc().
set_cc(JObj, Cc) ->
    kz_json:set_value(?CC, Cc, JObj).

-spec cc_email_addresses(doc()) -> kz_term:api_binaries().
cc_email_addresses(JObj) ->
    kz_json:get_value([?CC, ?EMAIL_ADDRESSES], JObj).

-spec cc_email_type(doc()) -> kz_term:api_binary().
cc_email_type(JObj) ->
    kz_json:get_value([?CC, ?EMAIL_TYPE], JObj).

-spec bcc(doc()) -> kz_term:api_object().
bcc(JObj) ->
    kz_json:get_value(?BCC, JObj).

-spec set_bcc(doc(), kz_json:object()) -> doc().
set_bcc(JObj, Bcc) ->
    kz_json:set_value(?BCC, Bcc, JObj).

-spec bcc_email_addresses(doc()) -> kz_term:api_binaries().
bcc_email_addresses(JObj) ->
    kz_json:get_value([?BCC, ?EMAIL_ADDRESSES], JObj).

-spec bcc_email_type(doc()) -> kz_term:api_binary().
bcc_email_type(JObj) ->
    kz_json:get_value([?BCC, ?EMAIL_TYPE], JObj).

-spec from(doc()) -> kz_term:api_binary().
from(JObj) ->
    kz_json:get_value(?FROM, JObj).

-spec set_from(doc(), kz_term:ne_binary()) -> doc().
set_from(JObj, From) ->
    kz_json:set_value(?FROM, From, JObj).

-spec reply_to(doc()) -> kz_term:api_binary().
reply_to(JObj) ->
    kz_json:get_value(?REPLY_TO, JObj).

-spec set_reply_to(doc(), kz_term:ne_binary()) -> doc().
set_reply_to(JObj, ReplyTo) ->
    kz_json:set_value(?REPLY_TO, ReplyTo, JObj).

-spec set_base_properties(doc()) -> doc().
set_base_properties(JObj) ->
    set_base_properties(JObj, id(JObj)).

-spec set_base_properties(doc(), kz_term:api_binary()) -> doc().
set_base_properties(JObj, Id) ->
    kz_json:set_values(base_properties(JObj, Id), JObj).

-spec base_properties(doc()) -> kz_json:flat_proplist().
base_properties(JObj) ->
    base_properties(JObj, id(JObj)).

-spec base_properties(doc(), kz_term:ne_binary()) -> kz_json:flat_proplist().
base_properties(_JObj, Id) ->
    [{kz_doc:path_id(), db_id(Id)}
    ,{kz_doc:path_type(), ?PVT_TYPE}
    ].

-spec pvt_type() -> kz_term:ne_binary().
pvt_type() -> ?PVT_TYPE.

-spec pvt_type(doc()) -> kz_term:ne_binary().
pvt_type(JObj) -> kz_doc:type(JObj, ?PVT_TYPE).

-spec is_enabled(doc()) -> boolean().
is_enabled(JObj) ->
    is_enabled(JObj, 'true').

-spec is_enabled(doc(), Default) -> boolean() | Default.
is_enabled(JObj, Default) ->
    kz_json:is_true(<<"enabled">>, JObj, Default).
