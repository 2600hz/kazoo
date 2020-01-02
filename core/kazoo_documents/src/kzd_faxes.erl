%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_faxes).

-export([new/0]).
-export([attempts/1, attempts/2, set_attempts/2]).
-export([document/1, document/2, set_document/2]).
-export([document_content/1, document_content/2, set_document_content/2]).
-export([document_content_type/1, document_content_type/2, set_document_content_type/2]).
-export([document_host/1, document_host/2, set_document_host/2]).
-export([document_method/1, document_method/2, set_document_method/2]).
-export([document_referer/1, document_referer/2, set_document_referer/2]).
-export([document_url/1, document_url/2, set_document_url/2]).
-export([from_name/1, from_name/2, set_from_name/2]).
-export([from_number/1, from_number/2, set_from_number/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notifications_email/1, notifications_email/2, set_notifications_email/2]).
-export([notifications_email_send_to/1, notifications_email_send_to/2, set_notifications_email_send_to/2]).
-export([notifications_sms/1, notifications_sms/2, set_notifications_sms/2]).
-export([notifications_sms_send_to/1, notifications_sms_send_to/2, set_notifications_sms_send_to/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([to_name/1, to_name/2, set_to_name/2]).
-export([to_number/1, to_number/2, set_to_number/2]).
-export([tx_result/1, tx_result/2, set_tx_result/2]).
-export([tx_result_error_message/1, tx_result_error_message/2, set_tx_result_error_message/2]).
-export([tx_result_fax_bad_rows/1, tx_result_fax_bad_rows/2, set_tx_result_fax_bad_rows/2]).
-export([tx_result_fax_error_correction/1, tx_result_fax_error_correction/2, set_tx_result_fax_error_correction/2]).
-export([tx_result_fax_receiver_id/1, tx_result_fax_receiver_id/2, set_tx_result_fax_receiver_id/2]).
-export([tx_result_fax_speed/1, tx_result_fax_speed/2, set_tx_result_fax_speed/2]).
-export([tx_result_pages_sent/1, tx_result_pages_sent/2, set_tx_result_pages_sent/2]).
-export([tx_result_success/1, tx_result_success/2, set_tx_result_success/2]).
-export([tx_result_time_elapsed/1, tx_result_time_elapsed/2, set_tx_result_time_elapsed/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"faxes">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec attempts(doc()) -> integer().
attempts(Doc) ->
    attempts(Doc, 0).

-spec attempts(doc(), Default) -> integer() | Default.
attempts(Doc, Default) ->
    kz_json:get_integer_value([<<"attempts">>], Doc, Default).

-spec set_attempts(doc(), integer()) -> doc().
set_attempts(Doc, Attempts) ->
    kz_json:set_value([<<"attempts">>], Attempts, Doc).

-spec document(doc()) -> kz_term:api_object().
document(Doc) ->
    document(Doc, 'undefined').

-spec document(doc(), Default) -> kz_json:object() | Default.
document(Doc, Default) ->
    kz_json:get_json_value([<<"document">>], Doc, Default).

-spec set_document(doc(), kz_json:object()) -> doc().
set_document(Doc, Document) ->
    kz_json:set_value([<<"document">>], Document, Doc).

-spec document_content(doc()) -> kz_term:api_binary().
document_content(Doc) ->
    document_content(Doc, 'undefined').

-spec document_content(doc(), Default) -> binary() | Default.
document_content(Doc, Default) ->
    kz_json:get_binary_value([<<"document">>, <<"content">>], Doc, Default).

-spec set_document_content(doc(), binary()) -> doc().
set_document_content(Doc, DocumentContent) ->
    kz_json:set_value([<<"document">>, <<"content">>], DocumentContent, Doc).

-spec document_content_type(doc()) -> kz_term:api_binary().
document_content_type(Doc) ->
    document_content_type(Doc, 'undefined').

-spec document_content_type(doc(), Default) -> binary() | Default.
document_content_type(Doc, Default) ->
    kz_json:get_binary_value([<<"document">>, <<"content_type">>], Doc, Default).

-spec set_document_content_type(doc(), binary()) -> doc().
set_document_content_type(Doc, DocumentContentType) ->
    kz_json:set_value([<<"document">>, <<"content_type">>], DocumentContentType, Doc).

-spec document_host(doc()) -> kz_term:api_binary().
document_host(Doc) ->
    document_host(Doc, 'undefined').

-spec document_host(doc(), Default) -> binary() | Default.
document_host(Doc, Default) ->
    kz_json:get_binary_value([<<"document">>, <<"host">>], Doc, Default).

-spec set_document_host(doc(), binary()) -> doc().
set_document_host(Doc, DocumentHost) ->
    kz_json:set_value([<<"document">>, <<"host">>], DocumentHost, Doc).

-spec document_method(doc()) -> binary().
document_method(Doc) ->
    document_method(Doc, <<"get">>).

-spec document_method(doc(), Default) -> binary() | Default.
document_method(Doc, Default) ->
    kz_json:get_binary_value([<<"document">>, <<"method">>], Doc, Default).

-spec set_document_method(doc(), binary()) -> doc().
set_document_method(Doc, DocumentMethod) ->
    kz_json:set_value([<<"document">>, <<"method">>], DocumentMethod, Doc).

-spec document_referer(doc()) -> kz_term:api_binary().
document_referer(Doc) ->
    document_referer(Doc, 'undefined').

-spec document_referer(doc(), Default) -> binary() | Default.
document_referer(Doc, Default) ->
    kz_json:get_binary_value([<<"document">>, <<"referer">>], Doc, Default).

-spec set_document_referer(doc(), binary()) -> doc().
set_document_referer(Doc, DocumentReferer) ->
    kz_json:set_value([<<"document">>, <<"referer">>], DocumentReferer, Doc).

-spec document_url(doc()) -> kz_term:api_binary().
document_url(Doc) ->
    document_url(Doc, 'undefined').

-spec document_url(doc(), Default) -> binary() | Default.
document_url(Doc, Default) ->
    kz_json:get_binary_value([<<"document">>, <<"url">>], Doc, Default).

-spec set_document_url(doc(), binary()) -> doc().
set_document_url(Doc, DocumentUrl) ->
    kz_json:set_value([<<"document">>, <<"url">>], DocumentUrl, Doc).

-spec from_name(doc()) -> kz_term:api_binary().
from_name(Doc) ->
    from_name(Doc, 'undefined').

-spec from_name(doc(), Default) -> binary() | Default.
from_name(Doc, Default) ->
    kz_json:get_binary_value([<<"from_name">>], Doc, Default).

-spec set_from_name(doc(), binary()) -> doc().
set_from_name(Doc, FromName) ->
    kz_json:set_value([<<"from_name">>], FromName, Doc).

-spec from_number(doc()) -> kz_term:api_binary().
from_number(Doc) ->
    from_number(Doc, 'undefined').

-spec from_number(doc(), Default) -> binary() | Default.
from_number(Doc, Default) ->
    kz_json:get_binary_value([<<"from_number">>], Doc, Default).

-spec set_from_number(doc(), binary()) -> doc().
set_from_number(Doc, FromNumber) ->
    kz_json:set_value([<<"from_number">>], FromNumber, Doc).

-spec notifications(doc()) -> kz_term:api_object().
notifications(Doc) ->
    notifications(Doc, 'undefined').

-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>], Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value([<<"notifications">>], Notifications, Doc).

-spec notifications_email(doc()) -> kz_term:api_object().
notifications_email(Doc) ->
    notifications_email(Doc, 'undefined').

-spec notifications_email(doc(), Default) -> kz_json:object() | Default.
notifications_email(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"email">>], Doc, Default).

-spec set_notifications_email(doc(), kz_json:object()) -> doc().
set_notifications_email(Doc, NotificationsEmail) ->
    kz_json:set_value([<<"notifications">>, <<"email">>], NotificationsEmail, Doc).

-spec notifications_email_send_to(doc()) -> any().
notifications_email_send_to(Doc) ->
    notifications_email_send_to(Doc, 'undefined').

-spec notifications_email_send_to(doc(), Default) -> any() | Default.
notifications_email_send_to(Doc, Default) ->
    kz_json:get_value([<<"notifications">>, <<"email">>, <<"send_to">>], Doc, Default).

-spec set_notifications_email_send_to(doc(), any()) -> doc().
set_notifications_email_send_to(Doc, NotificationsEmailSendTo) ->
    kz_json:set_value([<<"notifications">>, <<"email">>, <<"send_to">>], NotificationsEmailSendTo, Doc).

-spec notifications_sms(doc()) -> kz_term:api_object().
notifications_sms(Doc) ->
    notifications_sms(Doc, 'undefined').

-spec notifications_sms(doc(), Default) -> kz_json:object() | Default.
notifications_sms(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"sms">>], Doc, Default).

-spec set_notifications_sms(doc(), kz_json:object()) -> doc().
set_notifications_sms(Doc, NotificationsSms) ->
    kz_json:set_value([<<"notifications">>, <<"sms">>], NotificationsSms, Doc).

-spec notifications_sms_send_to(doc()) -> any().
notifications_sms_send_to(Doc) ->
    notifications_sms_send_to(Doc, 'undefined').

-spec notifications_sms_send_to(doc(), Default) -> any() | Default.
notifications_sms_send_to(Doc, Default) ->
    kz_json:get_value([<<"notifications">>, <<"sms">>, <<"send_to">>], Doc, Default).

-spec set_notifications_sms_send_to(doc(), any()) -> doc().
set_notifications_sms_send_to(Doc, NotificationsSmsSendTo) ->
    kz_json:set_value([<<"notifications">>, <<"sms">>, <<"send_to">>], NotificationsSmsSendTo, Doc).

-spec retries(doc()) -> integer().
retries(Doc) ->
    retries(Doc, 1).

-spec retries(doc(), Default) -> integer() | Default.
retries(Doc, Default) ->
    kz_json:get_integer_value([<<"retries">>], Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value([<<"retries">>], Retries, Doc).

-spec to_name(doc()) -> kz_term:api_binary().
to_name(Doc) ->
    to_name(Doc, 'undefined').

-spec to_name(doc(), Default) -> binary() | Default.
to_name(Doc, Default) ->
    kz_json:get_binary_value([<<"to_name">>], Doc, Default).

-spec set_to_name(doc(), binary()) -> doc().
set_to_name(Doc, ToName) ->
    kz_json:set_value([<<"to_name">>], ToName, Doc).

-spec to_number(doc()) -> kz_term:api_binary().
to_number(Doc) ->
    to_number(Doc, 'undefined').

-spec to_number(doc(), Default) -> binary() | Default.
to_number(Doc, Default) ->
    kz_json:get_binary_value([<<"to_number">>], Doc, Default).

-spec set_to_number(doc(), binary()) -> doc().
set_to_number(Doc, ToNumber) ->
    kz_json:set_value([<<"to_number">>], ToNumber, Doc).

-spec tx_result(doc()) -> kz_term:api_object().
tx_result(Doc) ->
    tx_result(Doc, 'undefined').

-spec tx_result(doc(), Default) -> kz_json:object() | Default.
tx_result(Doc, Default) ->
    kz_json:get_json_value([<<"tx_result">>], Doc, Default).

-spec set_tx_result(doc(), kz_json:object()) -> doc().
set_tx_result(Doc, TxResult) ->
    kz_json:set_value([<<"tx_result">>], TxResult, Doc).

-spec tx_result_error_message(doc()) -> binary().
tx_result_error_message(Doc) ->
    tx_result_error_message(Doc, <<"">>).

-spec tx_result_error_message(doc(), Default) -> binary() | Default.
tx_result_error_message(Doc, Default) ->
    kz_json:get_binary_value([<<"tx_result">>, <<"error_message">>], Doc, Default).

-spec set_tx_result_error_message(doc(), binary()) -> doc().
set_tx_result_error_message(Doc, TxResultErrorMessage) ->
    kz_json:set_value([<<"tx_result">>, <<"error_message">>], TxResultErrorMessage, Doc).

-spec tx_result_fax_bad_rows(doc()) -> integer().
tx_result_fax_bad_rows(Doc) ->
    tx_result_fax_bad_rows(Doc, 0).

-spec tx_result_fax_bad_rows(doc(), Default) -> integer() | Default.
tx_result_fax_bad_rows(Doc, Default) ->
    kz_json:get_integer_value([<<"tx_result">>, <<"fax_bad_rows">>], Doc, Default).

-spec set_tx_result_fax_bad_rows(doc(), integer()) -> doc().
set_tx_result_fax_bad_rows(Doc, TxResultFaxBadRows) ->
    kz_json:set_value([<<"tx_result">>, <<"fax_bad_rows">>], TxResultFaxBadRows, Doc).

-spec tx_result_fax_error_correction(doc()) -> boolean().
tx_result_fax_error_correction(Doc) ->
    tx_result_fax_error_correction(Doc, false).

-spec tx_result_fax_error_correction(doc(), Default) -> boolean() | Default.
tx_result_fax_error_correction(Doc, Default) ->
    kz_json:get_boolean_value([<<"tx_result">>, <<"fax_error_correction">>], Doc, Default).

-spec set_tx_result_fax_error_correction(doc(), boolean()) -> doc().
set_tx_result_fax_error_correction(Doc, TxResultFaxErrorCorrection) ->
    kz_json:set_value([<<"tx_result">>, <<"fax_error_correction">>], TxResultFaxErrorCorrection, Doc).

-spec tx_result_fax_receiver_id(doc()) -> binary().
tx_result_fax_receiver_id(Doc) ->
    tx_result_fax_receiver_id(Doc, <<"">>).

-spec tx_result_fax_receiver_id(doc(), Default) -> binary() | Default.
tx_result_fax_receiver_id(Doc, Default) ->
    kz_json:get_binary_value([<<"tx_result">>, <<"fax_receiver_id">>], Doc, Default).

-spec set_tx_result_fax_receiver_id(doc(), binary()) -> doc().
set_tx_result_fax_receiver_id(Doc, TxResultFaxReceiverId) ->
    kz_json:set_value([<<"tx_result">>, <<"fax_receiver_id">>], TxResultFaxReceiverId, Doc).

-spec tx_result_fax_speed(doc()) -> integer().
tx_result_fax_speed(Doc) ->
    tx_result_fax_speed(Doc, 0).

-spec tx_result_fax_speed(doc(), Default) -> integer() | Default.
tx_result_fax_speed(Doc, Default) ->
    kz_json:get_integer_value([<<"tx_result">>, <<"fax_speed">>], Doc, Default).

-spec set_tx_result_fax_speed(doc(), integer()) -> doc().
set_tx_result_fax_speed(Doc, TxResultFaxSpeed) ->
    kz_json:set_value([<<"tx_result">>, <<"fax_speed">>], TxResultFaxSpeed, Doc).

-spec tx_result_pages_sent(doc()) -> integer().
tx_result_pages_sent(Doc) ->
    tx_result_pages_sent(Doc, 0).

-spec tx_result_pages_sent(doc(), Default) -> integer() | Default.
tx_result_pages_sent(Doc, Default) ->
    kz_json:get_integer_value([<<"tx_result">>, <<"pages_sent">>], Doc, Default).

-spec set_tx_result_pages_sent(doc(), integer()) -> doc().
set_tx_result_pages_sent(Doc, TxResultPagesSent) ->
    kz_json:set_value([<<"tx_result">>, <<"pages_sent">>], TxResultPagesSent, Doc).

-spec tx_result_success(doc()) -> boolean().
tx_result_success(Doc) ->
    tx_result_success(Doc, false).

-spec tx_result_success(doc(), Default) -> boolean() | Default.
tx_result_success(Doc, Default) ->
    kz_json:get_boolean_value([<<"tx_result">>, <<"success">>], Doc, Default).

-spec set_tx_result_success(doc(), boolean()) -> doc().
set_tx_result_success(Doc, TxResultSuccess) ->
    kz_json:set_value([<<"tx_result">>, <<"success">>], TxResultSuccess, Doc).

-spec tx_result_time_elapsed(doc()) -> integer().
tx_result_time_elapsed(Doc) ->
    tx_result_time_elapsed(Doc, 0).

-spec tx_result_time_elapsed(doc(), Default) -> integer() | Default.
tx_result_time_elapsed(Doc, Default) ->
    kz_json:get_integer_value([<<"tx_result">>, <<"time_elapsed">>], Doc, Default).

-spec set_tx_result_time_elapsed(doc(), integer()) -> doc().
set_tx_result_time_elapsed(Doc, TxResultTimeElapsed) ->
    kz_json:set_value([<<"tx_result">>, <<"time_elapsed">>], TxResultTimeElapsed, Doc).
