-module(kzd_faxbox).

-export([new/0]).
-export([attempts/1, attempts/2, set_attempts/2]).
-export([caller_id/1, caller_id/2, set_caller_id/2]).
-export([caller_name/1, caller_name/2, set_caller_name/2]).
-export([custom_smtp_email_address/1, custom_smtp_email_address/2, set_custom_smtp_email_address/2]).
-export([fax_header/1, fax_header/2, set_fax_header/2]).
-export([fax_identity/1, fax_identity/2, set_fax_identity/2]).
-export([fax_timezone/1, fax_timezone/2, set_fax_timezone/2]).
-export([media/1, media/2, set_media/2]).
-export([media_fax_option/1, media_fax_option/2, set_media_fax_option/2]).
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notifications_inbound/1, notifications_inbound/2, set_notifications_inbound/2]).
-export([notifications_inbound_callback/1, notifications_inbound_callback/2, set_notifications_inbound_callback/2]).
-export([notifications_inbound_callback_method/1, notifications_inbound_callback_method/2, set_notifications_inbound_callback_method/2]).
-export([notifications_inbound_callback_type/1, notifications_inbound_callback_type/2, set_notifications_inbound_callback_type/2]).
-export([notifications_inbound_callback_url/1, notifications_inbound_callback_url/2, set_notifications_inbound_callback_url/2]).
-export([notifications_inbound_email/1, notifications_inbound_email/2, set_notifications_inbound_email/2]).
-export([notifications_inbound_email_send_to/1, notifications_inbound_email_send_to/2, set_notifications_inbound_email_send_to/2]).
-export([notifications_inbound_sms/1, notifications_inbound_sms/2, set_notifications_inbound_sms/2]).
-export([notifications_inbound_sms_send_to/1, notifications_inbound_sms_send_to/2, set_notifications_inbound_sms_send_to/2]).
-export([notifications_outbound/1, notifications_outbound/2, set_notifications_outbound/2]).
-export([notifications_outbound_callback/1, notifications_outbound_callback/2, set_notifications_outbound_callback/2]).
-export([notifications_outbound_callback_method/1, notifications_outbound_callback_method/2, set_notifications_outbound_callback_method/2]).
-export([notifications_outbound_callback_type/1, notifications_outbound_callback_type/2, set_notifications_outbound_callback_type/2]).
-export([notifications_outbound_callback_url/1, notifications_outbound_callback_url/2, set_notifications_outbound_callback_url/2]).
-export([notifications_outbound_email/1, notifications_outbound_email/2, set_notifications_outbound_email/2]).
-export([notifications_outbound_email_send_to/1, notifications_outbound_email_send_to/2, set_notifications_outbound_email_send_to/2]).
-export([notifications_outbound_sms/1, notifications_outbound_sms/2, set_notifications_outbound_sms/2]).
-export([notifications_outbound_sms_send_to/1, notifications_outbound_sms_send_to/2, set_notifications_outbound_sms_send_to/2]).
-export([retries/1, retries/2, set_retries/2]).
-export([smtp_permission_list/1, smtp_permission_list/2, set_smtp_permission_list/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec attempts(doc()) -> integer().
-spec attempts(doc(), Default) -> integer() | Default.
attempts(Doc) ->
    attempts(Doc, 0).
attempts(Doc, Default) ->
    kz_json:get_integer_value([<<"attempts">>], Doc, Default).

-spec set_attempts(doc(), integer()) -> doc().
set_attempts(Doc, Attempts) ->
    kz_json:set_value([<<"attempts">>], Attempts, Doc).

-spec caller_id(doc()) -> api_binary().
-spec caller_id(doc(), Default) -> binary() | Default.
caller_id(Doc) ->
    caller_id(Doc, 'undefined').
caller_id(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id">>], Doc, Default).

-spec set_caller_id(doc(), binary()) -> doc().
set_caller_id(Doc, CallerId) ->
    kz_json:set_value([<<"caller_id">>], CallerId, Doc).

-spec caller_name(doc()) -> binary().
-spec caller_name(doc(), Default) -> binary() | Default.
caller_name(Doc) ->
    caller_name(Doc, <<"Kazoo Fax Printer">>).
caller_name(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_name">>], Doc, Default).

-spec set_caller_name(doc(), binary()) -> doc().
set_caller_name(Doc, CallerName) ->
    kz_json:set_value([<<"caller_name">>], CallerName, Doc).

-spec custom_smtp_email_address(doc()) -> api_binary().
-spec custom_smtp_email_address(doc(), Default) -> binary() | Default.
custom_smtp_email_address(Doc) ->
    custom_smtp_email_address(Doc, 'undefined').
custom_smtp_email_address(Doc, Default) ->
    kz_json:get_binary_value([<<"custom_smtp_email_address">>], Doc, Default).

-spec set_custom_smtp_email_address(doc(), binary()) -> doc().
set_custom_smtp_email_address(Doc, CustomSmtpEmailAddress) ->
    kz_json:set_value([<<"custom_smtp_email_address">>], CustomSmtpEmailAddress, Doc).

-spec fax_header(doc()) -> binary().
-spec fax_header(doc(), Default) -> binary() | Default.
fax_header(Doc) ->
    fax_header(Doc, <<"Kazoo Fax Printer">>).
fax_header(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_header">>], Doc, Default).

-spec set_fax_header(doc(), binary()) -> doc().
set_fax_header(Doc, FaxHeader) ->
    kz_json:set_value([<<"fax_header">>], FaxHeader, Doc).

-spec fax_identity(doc()) -> api_binary().
-spec fax_identity(doc(), Default) -> binary() | Default.
fax_identity(Doc) ->
    fax_identity(Doc, 'undefined').
fax_identity(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_identity">>], Doc, Default).

-spec set_fax_identity(doc(), binary()) -> doc().
set_fax_identity(Doc, FaxIdentity) ->
    kz_json:set_value([<<"fax_identity">>], FaxIdentity, Doc).

-spec fax_timezone(doc()) -> api_binary().
-spec fax_timezone(doc(), Default) -> binary() | Default.
fax_timezone(Doc) ->
    fax_timezone(Doc, 'undefined').
fax_timezone(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_timezone">>], Doc, Default).

-spec set_fax_timezone(doc(), binary()) -> doc().
set_fax_timezone(Doc, FaxTimezone) ->
    kz_json:set_value([<<"fax_timezone">>], FaxTimezone, Doc).

-spec media(doc()) -> kz_json:object().
-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc) ->
    media(Doc, kz_json:new()).
media(Doc, Default) ->
    kz_json:get_json_value([<<"media">>], Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value([<<"media">>], Media, Doc).

-spec media_fax_option(doc()) -> api_boolean().
-spec media_fax_option(doc(), Default) -> boolean() | Default.
media_fax_option(Doc) ->
    media_fax_option(Doc, 'undefined').
media_fax_option(Doc, Default) ->
    kz_json:get_boolean_value([<<"media">>, <<"fax_option">>], Doc, Default).

-spec set_media_fax_option(doc(), boolean()) -> doc().
set_media_fax_option(Doc, MediaFaxOption) ->
    kz_json:set_value([<<"media">>, <<"fax_option">>], MediaFaxOption, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec notifications(doc()) -> api_object().
-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc) ->
    notifications(Doc, 'undefined').
notifications(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>], Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value([<<"notifications">>], Notifications, Doc).

-spec notifications_inbound(doc()) -> api_object().
-spec notifications_inbound(doc(), Default) -> kz_json:object() | Default.
notifications_inbound(Doc) ->
    notifications_inbound(Doc, 'undefined').
notifications_inbound(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"inbound">>], Doc, Default).

-spec set_notifications_inbound(doc(), kz_json:object()) -> doc().
set_notifications_inbound(Doc, NotificationsInbound) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>], NotificationsInbound, Doc).

-spec notifications_inbound_callback(doc()) -> api_object().
-spec notifications_inbound_callback(doc(), Default) -> kz_json:object() | Default.
notifications_inbound_callback(Doc) ->
    notifications_inbound_callback(Doc, 'undefined').
notifications_inbound_callback(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"inbound">>, <<"callback">>], Doc, Default).

-spec set_notifications_inbound_callback(doc(), kz_json:object()) -> doc().
set_notifications_inbound_callback(Doc, NotificationsInboundCallback) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"callback">>], NotificationsInboundCallback, Doc).

-spec notifications_inbound_callback_method(doc()) -> api_binary().
-spec notifications_inbound_callback_method(doc(), Default) -> binary() | Default.
notifications_inbound_callback_method(Doc) ->
    notifications_inbound_callback_method(Doc, 'undefined').
notifications_inbound_callback_method(Doc, Default) ->
    kz_json:get_binary_value([<<"notifications">>, <<"inbound">>, <<"callback">>, <<"method">>], Doc, Default).

-spec set_notifications_inbound_callback_method(doc(), binary()) -> doc().
set_notifications_inbound_callback_method(Doc, NotificationsInboundCallbackMethod) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"callback">>, <<"method">>], NotificationsInboundCallbackMethod, Doc).

-spec notifications_inbound_callback_type(doc()) -> api_binary().
-spec notifications_inbound_callback_type(doc(), Default) -> binary() | Default.
notifications_inbound_callback_type(Doc) ->
    notifications_inbound_callback_type(Doc, 'undefined').
notifications_inbound_callback_type(Doc, Default) ->
    kz_json:get_binary_value([<<"notifications">>, <<"inbound">>, <<"callback">>, <<"type">>], Doc, Default).

-spec set_notifications_inbound_callback_type(doc(), binary()) -> doc().
set_notifications_inbound_callback_type(Doc, NotificationsInboundCallbackType) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"callback">>, <<"type">>], NotificationsInboundCallbackType, Doc).

-spec notifications_inbound_callback_url(doc()) -> api_binary().
-spec notifications_inbound_callback_url(doc(), Default) -> binary() | Default.
notifications_inbound_callback_url(Doc) ->
    notifications_inbound_callback_url(Doc, 'undefined').
notifications_inbound_callback_url(Doc, Default) ->
    kz_json:get_binary_value([<<"notifications">>, <<"inbound">>, <<"callback">>, <<"url">>], Doc, Default).

-spec set_notifications_inbound_callback_url(doc(), binary()) -> doc().
set_notifications_inbound_callback_url(Doc, NotificationsInboundCallbackUrl) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"callback">>, <<"url">>], NotificationsInboundCallbackUrl, Doc).

-spec notifications_inbound_email(doc()) -> api_object().
-spec notifications_inbound_email(doc(), Default) -> kz_json:object() | Default.
notifications_inbound_email(Doc) ->
    notifications_inbound_email(Doc, 'undefined').
notifications_inbound_email(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"inbound">>, <<"email">>], Doc, Default).

-spec set_notifications_inbound_email(doc(), kz_json:object()) -> doc().
set_notifications_inbound_email(Doc, NotificationsInboundEmail) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"email">>], NotificationsInboundEmail, Doc).

-spec notifications_inbound_email_send_to(doc()) -> any().
-spec notifications_inbound_email_send_to(doc(), Default) -> any() | Default.
notifications_inbound_email_send_to(Doc) ->
    notifications_inbound_email_send_to(Doc, 'undefined').
notifications_inbound_email_send_to(Doc, Default) ->
    kz_json:get_value([<<"notifications">>, <<"inbound">>, <<"email">>, <<"send_to">>], Doc, Default).

-spec set_notifications_inbound_email_send_to(doc(), any()) -> doc().
set_notifications_inbound_email_send_to(Doc, NotificationsInboundEmailSendTo) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"email">>, <<"send_to">>], NotificationsInboundEmailSendTo, Doc).

-spec notifications_inbound_sms(doc()) -> api_object().
-spec notifications_inbound_sms(doc(), Default) -> kz_json:object() | Default.
notifications_inbound_sms(Doc) ->
    notifications_inbound_sms(Doc, 'undefined').
notifications_inbound_sms(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"inbound">>, <<"sms">>], Doc, Default).

-spec set_notifications_inbound_sms(doc(), kz_json:object()) -> doc().
set_notifications_inbound_sms(Doc, NotificationsInboundSms) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"sms">>], NotificationsInboundSms, Doc).

-spec notifications_inbound_sms_send_to(doc()) -> any().
-spec notifications_inbound_sms_send_to(doc(), Default) -> any() | Default.
notifications_inbound_sms_send_to(Doc) ->
    notifications_inbound_sms_send_to(Doc, 'undefined').
notifications_inbound_sms_send_to(Doc, Default) ->
    kz_json:get_value([<<"notifications">>, <<"inbound">>, <<"sms">>, <<"send_to">>], Doc, Default).

-spec set_notifications_inbound_sms_send_to(doc(), any()) -> doc().
set_notifications_inbound_sms_send_to(Doc, NotificationsInboundSmsSendTo) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>, <<"sms">>, <<"send_to">>], NotificationsInboundSmsSendTo, Doc).

-spec notifications_outbound(doc()) -> api_object().
-spec notifications_outbound(doc(), Default) -> kz_json:object() | Default.
notifications_outbound(Doc) ->
    notifications_outbound(Doc, 'undefined').
notifications_outbound(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"outbound">>], Doc, Default).

-spec set_notifications_outbound(doc(), kz_json:object()) -> doc().
set_notifications_outbound(Doc, NotificationsOutbound) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>], NotificationsOutbound, Doc).

-spec notifications_outbound_callback(doc()) -> api_object().
-spec notifications_outbound_callback(doc(), Default) -> kz_json:object() | Default.
notifications_outbound_callback(Doc) ->
    notifications_outbound_callback(Doc, 'undefined').
notifications_outbound_callback(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"outbound">>, <<"callback">>], Doc, Default).

-spec set_notifications_outbound_callback(doc(), kz_json:object()) -> doc().
set_notifications_outbound_callback(Doc, NotificationsOutboundCallback) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"callback">>], NotificationsOutboundCallback, Doc).

-spec notifications_outbound_callback_method(doc()) -> api_binary().
-spec notifications_outbound_callback_method(doc(), Default) -> binary() | Default.
notifications_outbound_callback_method(Doc) ->
    notifications_outbound_callback_method(Doc, 'undefined').
notifications_outbound_callback_method(Doc, Default) ->
    kz_json:get_binary_value([<<"notifications">>, <<"outbound">>, <<"callback">>, <<"method">>], Doc, Default).

-spec set_notifications_outbound_callback_method(doc(), binary()) -> doc().
set_notifications_outbound_callback_method(Doc, NotificationsOutboundCallbackMethod) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"callback">>, <<"method">>], NotificationsOutboundCallbackMethod, Doc).

-spec notifications_outbound_callback_type(doc()) -> api_binary().
-spec notifications_outbound_callback_type(doc(), Default) -> binary() | Default.
notifications_outbound_callback_type(Doc) ->
    notifications_outbound_callback_type(Doc, 'undefined').
notifications_outbound_callback_type(Doc, Default) ->
    kz_json:get_binary_value([<<"notifications">>, <<"outbound">>, <<"callback">>, <<"type">>], Doc, Default).

-spec set_notifications_outbound_callback_type(doc(), binary()) -> doc().
set_notifications_outbound_callback_type(Doc, NotificationsOutboundCallbackType) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"callback">>, <<"type">>], NotificationsOutboundCallbackType, Doc).

-spec notifications_outbound_callback_url(doc()) -> api_binary().
-spec notifications_outbound_callback_url(doc(), Default) -> binary() | Default.
notifications_outbound_callback_url(Doc) ->
    notifications_outbound_callback_url(Doc, 'undefined').
notifications_outbound_callback_url(Doc, Default) ->
    kz_json:get_binary_value([<<"notifications">>, <<"outbound">>, <<"callback">>, <<"url">>], Doc, Default).

-spec set_notifications_outbound_callback_url(doc(), binary()) -> doc().
set_notifications_outbound_callback_url(Doc, NotificationsOutboundCallbackUrl) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"callback">>, <<"url">>], NotificationsOutboundCallbackUrl, Doc).

-spec notifications_outbound_email(doc()) -> api_object().
-spec notifications_outbound_email(doc(), Default) -> kz_json:object() | Default.
notifications_outbound_email(Doc) ->
    notifications_outbound_email(Doc, 'undefined').
notifications_outbound_email(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"outbound">>, <<"email">>], Doc, Default).

-spec set_notifications_outbound_email(doc(), kz_json:object()) -> doc().
set_notifications_outbound_email(Doc, NotificationsOutboundEmail) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"email">>], NotificationsOutboundEmail, Doc).

-spec notifications_outbound_email_send_to(doc()) -> any().
-spec notifications_outbound_email_send_to(doc(), Default) -> any() | Default.
notifications_outbound_email_send_to(Doc) ->
    notifications_outbound_email_send_to(Doc, 'undefined').
notifications_outbound_email_send_to(Doc, Default) ->
    kz_json:get_value([<<"notifications">>, <<"outbound">>, <<"email">>, <<"send_to">>], Doc, Default).

-spec set_notifications_outbound_email_send_to(doc(), any()) -> doc().
set_notifications_outbound_email_send_to(Doc, NotificationsOutboundEmailSendTo) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"email">>, <<"send_to">>], NotificationsOutboundEmailSendTo, Doc).

-spec notifications_outbound_sms(doc()) -> api_object().
-spec notifications_outbound_sms(doc(), Default) -> kz_json:object() | Default.
notifications_outbound_sms(Doc) ->
    notifications_outbound_sms(Doc, 'undefined').
notifications_outbound_sms(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"outbound">>, <<"sms">>], Doc, Default).

-spec set_notifications_outbound_sms(doc(), kz_json:object()) -> doc().
set_notifications_outbound_sms(Doc, NotificationsOutboundSms) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"sms">>], NotificationsOutboundSms, Doc).

-spec notifications_outbound_sms_send_to(doc()) -> any().
-spec notifications_outbound_sms_send_to(doc(), Default) -> any() | Default.
notifications_outbound_sms_send_to(Doc) ->
    notifications_outbound_sms_send_to(Doc, 'undefined').
notifications_outbound_sms_send_to(Doc, Default) ->
    kz_json:get_value([<<"notifications">>, <<"outbound">>, <<"sms">>, <<"send_to">>], Doc, Default).

-spec set_notifications_outbound_sms_send_to(doc(), any()) -> doc().
set_notifications_outbound_sms_send_to(Doc, NotificationsOutboundSmsSendTo) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>, <<"sms">>, <<"send_to">>], NotificationsOutboundSmsSendTo, Doc).

-spec retries(doc()) -> integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Doc) ->
    retries(Doc, 1).
retries(Doc, Default) ->
    kz_json:get_integer_value([<<"retries">>], Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value([<<"retries">>], Retries, Doc).

-spec smtp_permission_list(doc()) -> ne_binaries().
-spec smtp_permission_list(doc(), Default) -> ne_binaries() | Default.
smtp_permission_list(Doc) ->
    smtp_permission_list(Doc, []).
smtp_permission_list(Doc, Default) ->
    kz_json:get_list_value([<<"smtp_permission_list">>], Doc, Default).

-spec set_smtp_permission_list(doc(), ne_binaries()) -> doc().
set_smtp_permission_list(Doc, SmtpPermissionList) ->
    kz_json:set_value([<<"smtp_permission_list">>], SmtpPermissionList, Doc).
