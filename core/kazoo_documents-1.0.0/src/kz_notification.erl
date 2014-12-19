%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
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
        ]).

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

-spec macros(wh_json:object()) -> api_object().
macros(JObj) ->
    wh_json:get_value(?MACROS, JObj).

-spec set_macros(wh_json:object(), wh_json:object()) -> wh_json:object().
set_macros(JObj, Macros) ->
    wh_json:set_value(?MACROS, Macros, JObj).

-spec macro(wh_json:object(), wh_json:key()) -> wh_json:json_term().
macro(JObj, Key) ->
    wh_json:get_value([?MACROS, Key], JObj).

-spec set_macro(wh_json:object(), wh_json:key(), wh_json:json_term()) -> wh_json:object().
set_macro(JObj, Key, Value) ->
    wh_json:set_value([?MACROS, Key], Value, JObj).

-spec subject(wh_json:object()) -> api_binary().
subject(JObj) ->
    wh_json:get_value(?SUBJECT, JObj).

-spec set_subject(wh_json:object(), ne_binary()) -> wh_json:object().
set_subject(JObj, Subject) ->
    wh_json:set_value(?SUBJECT, Subject, JObj).

-spec category(wh_json:object()) -> api_binary().
category(JObj) ->
    wh_json:get_value(?CATEGORY, JObj).

-spec set_category(wh_json:object(), ne_binary()) -> wh_json:object().
set_category(JObj, Category) ->
    wh_json:set_value(?CATEGORY, Category, JObj).

-spec name(wh_json:object()) -> api_binary().
name(JObj) ->
    wh_json:get_value(?NAME, JObj).

-spec set_name(wh_json:object(), ne_binary()) -> wh_json:object().
set_name(JObj, Name) ->
    wh_json:set_value(?NAME, Name, JObj).

-spec to(wh_json:object()) -> api_object().
to(JObj) ->
    wh_json:get_value(?TO, JObj).

-spec set_to(wh_json:object(), wh_json:object()) -> wh_json:object().
set_to(JObj, To) ->
    wh_json:set_value(?TO, To, JObj).

-spec to_email_addresses(wh_json:object()) -> api_binaries().
to_email_addresses(JObj) ->
    wh_json:get_value([?TO, ?EMAIL_ADDRESSES], JObj).

-spec to_email_type(wh_json:object()) -> api_binary().
to_email_type(JObj) ->
    wh_json:get_value([?TO, ?EMAIL_TYPE], JObj).

-spec cc(wh_json:object()) -> api_object().
cc(JObj) ->
    wh_json:get_value(?CC, JObj).

-spec set_cc(wh_json:object(), wh_json:object()) -> wh_json:object().
set_cc(JObj, Cc) ->
    wh_json:set_value(?CC, Cc, JObj).

-spec cc_email_addresses(wh_json:object()) -> api_binaries().
cc_email_addresses(JObj) ->
    wh_json:get_value([?CC, ?EMAIL_ADDRESSES], JObj).

-spec cc_email_type(wh_json:object()) -> api_binary().
cc_email_type(JObj) ->
    wh_json:get_value([?CC, ?EMAIL_TYPE], JObj).

-spec bcc(wh_json:object()) -> api_object().
bcc(JObj) ->
    wh_json:get_value(?BCC, JObj).

-spec set_bcc(wh_json:object(), wh_json:object()) -> wh_json:object().
set_bcc(JObj, Bcc) ->
    wh_json:set_value(?BCC, Bcc, JObj).

-spec bcc_email_addresses(wh_json:object()) -> api_binaries().
bcc_email_addresses(JObj) ->
    wh_json:get_value([?BCC, ?EMAIL_ADDRESSES], JObj).

-spec bcc_email_type(wh_json:object()) -> api_binary().
bcc_email_type(JObj) ->
    wh_json:get_value([?BCC, ?EMAIL_TYPE], JObj).

-spec from(wh_json:object()) -> api_binary().
from(JObj) ->
    wh_json:get_value(?FROM, JObj).

-spec set_from(wh_json:object(), ne_binary()) -> wh_json:object().
set_from(JObj, From) ->
    wh_json:set_value(?FROM, From, JObj).

-spec reply_to(wh_json:object()) -> api_binary().
reply_to(JObj) ->
    wh_json:get_value(?REPLY_TO, JObj).

-spec set_reply_to(wh_json:object(), ne_binary()) -> wh_json:object().
set_reply_to(JObj, ReplyTo) ->
    wh_json:set_value(?REPLY_TO, ReplyTo, JObj).
