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
-export([notificationsinbound_callback/1, notificationsinbound_callback/2, set_notificationsinbound_callback/2]).
-export([notificationsinboundcallback_method/1, notificationsinboundcallback_method/2, set_notificationsinboundcallback_method/2]).
-export([notificationsinboundcallback_type/1, notificationsinboundcallback_type/2, set_notificationsinboundcallback_type/2]).
-export([notificationsinboundcallback_url/1, notificationsinboundcallback_url/2, set_notificationsinboundcallback_url/2]).
-export([notificationsinbound_email/1, notificationsinbound_email/2, set_notificationsinbound_email/2]).
-export([notificationsinboundemail_send_to/1, notificationsinboundemail_send_to/2, set_notificationsinboundemail_send_to/2]).
-export([notificationsinbound_sms/1, notificationsinbound_sms/2, set_notificationsinbound_sms/2]).
-export([notificationsinboundsms_send_to/1, notificationsinboundsms_send_to/2, set_notificationsinboundsms_send_to/2]).
-export([notifications_outbound/1, notifications_outbound/2, set_notifications_outbound/2]).
-export([notificationsoutbound_callback/1, notificationsoutbound_callback/2, set_notificationsoutbound_callback/2]).
-export([notificationsoutboundcallback_method/1, notificationsoutboundcallback_method/2, set_notificationsoutboundcallback_method/2]).
-export([notificationsoutboundcallback_type/1, notificationsoutboundcallback_type/2, set_notificationsoutboundcallback_type/2]).
-export([notificationsoutboundcallback_url/1, notificationsoutboundcallback_url/2, set_notificationsoutboundcallback_url/2]).
-export([notificationsoutbound_email/1, notificationsoutbound_email/2, set_notificationsoutbound_email/2]).
-export([notificationsoutboundemail_send_to/1, notificationsoutboundemail_send_to/2, set_notificationsoutboundemail_send_to/2]).
-export([notificationsoutbound_sms/1, notificationsoutbound_sms/2, set_notificationsoutbound_sms/2]).
-export([notificationsoutboundsms_send_to/1, notificationsoutboundsms_send_to/2, set_notificationsoutboundsms_send_to/2]).
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
    kz_json:get_integer_value(<<"attempts">>, Doc, Default).

-spec set_attempts(doc(), integer()) -> doc().
set_attempts(Doc, Attempts) ->
    kz_json:set_value(<<"attempts">>, Attempts, Doc).

-spec caller_id(doc()) -> api_binary().
-spec caller_id(doc(), Default) -> binary() | Default.
caller_id(Doc) ->
    caller_id(Doc, 'undefined').
caller_id(Doc, Default) ->
    kz_json:get_binary_value(<<"caller_id">>, Doc, Default).

-spec set_caller_id(doc(), binary()) -> doc().
set_caller_id(Doc, CallerId) ->
    kz_json:set_value(<<"caller_id">>, CallerId, Doc).

-spec caller_name(doc()) -> binary().
-spec caller_name(doc(), Default) -> binary() | Default.
caller_name(Doc) ->
    caller_name(Doc, <<"Kazoo Fax Printer">>).
caller_name(Doc, Default) ->
    kz_json:get_binary_value(<<"caller_name">>, Doc, Default).

-spec set_caller_name(doc(), binary()) -> doc().
set_caller_name(Doc, CallerName) ->
    kz_json:set_value(<<"caller_name">>, CallerName, Doc).

-spec custom_smtp_email_address(doc()) -> api_binary().
-spec custom_smtp_email_address(doc(), Default) -> binary() | Default.
custom_smtp_email_address(Doc) ->
    custom_smtp_email_address(Doc, 'undefined').
custom_smtp_email_address(Doc, Default) ->
    kz_json:get_binary_value(<<"custom_smtp_email_address">>, Doc, Default).

-spec set_custom_smtp_email_address(doc(), binary()) -> doc().
set_custom_smtp_email_address(Doc, CustomSmtpEmailAddress) ->
    kz_json:set_value(<<"custom_smtp_email_address">>, CustomSmtpEmailAddress, Doc).

-spec fax_header(doc()) -> binary().
-spec fax_header(doc(), Default) -> binary() | Default.
fax_header(Doc) ->
    fax_header(Doc, <<"Kazoo Fax Printer">>).
fax_header(Doc, Default) ->
    kz_json:get_binary_value(<<"fax_header">>, Doc, Default).

-spec set_fax_header(doc(), binary()) -> doc().
set_fax_header(Doc, FaxHeader) ->
    kz_json:set_value(<<"fax_header">>, FaxHeader, Doc).

-spec fax_identity(doc()) -> api_binary().
-spec fax_identity(doc(), Default) -> binary() | Default.
fax_identity(Doc) ->
    fax_identity(Doc, 'undefined').
fax_identity(Doc, Default) ->
    kz_json:get_binary_value(<<"fax_identity">>, Doc, Default).

-spec set_fax_identity(doc(), binary()) -> doc().
set_fax_identity(Doc, FaxIdentity) ->
    kz_json:set_value(<<"fax_identity">>, FaxIdentity, Doc).

-spec fax_timezone(doc()) -> api_binary().
-spec fax_timezone(doc(), Default) -> binary() | Default.
fax_timezone(Doc) ->
    fax_timezone(Doc, 'undefined').
fax_timezone(Doc, Default) ->
    kz_json:get_binary_value(<<"fax_timezone">>, Doc, Default).

-spec set_fax_timezone(doc(), binary()) -> doc().
set_fax_timezone(Doc, FaxTimezone) ->
    kz_json:set_value(<<"fax_timezone">>, FaxTimezone, Doc).

-spec media(doc()) -> kz_json:object().
-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc) ->
    media(Doc, kz_json:new()).
media(Doc, Default) ->
    kz_json:get_json_value(<<"media">>, Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value(<<"media">>, Media, Doc).

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
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec notifications(doc()) -> api_object().
-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc) ->
    notifications(Doc, 'undefined').
notifications(Doc, Default) ->
    kz_json:get_json_value(<<"notifications">>, Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value(<<"notifications">>, Notifications, Doc).

-spec notifications_inbound(doc()) -> api_object().
-spec notifications_inbound(doc(), Default) -> kz_json:object() | Default.
notifications_inbound(Doc) ->
    notifications_inbound(Doc, 'undefined').
notifications_inbound(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"inbound">>], Doc, Default).

-spec set_notifications_inbound(doc(), kz_json:object()) -> doc().
set_notifications_inbound(Doc, NotificationsInbound) ->
    kz_json:set_value([<<"notifications">>, <<"inbound">>], NotificationsInbound, Doc).

-spec notificationsinbound_callback(doc()) -> api_object().
-spec notificationsinbound_callback(doc(), Default) -> kz_json:object() | Default.
notificationsinbound_callback(Doc) ->
    notificationsinbound_callback(Doc, 'undefined').
notificationsinbound_callback(Doc, Default) ->
    kz_json:get_json_value([[<<"notifications">>, <<"inbound">>], <<"callback">>], Doc, Default).

-spec set_notificationsinbound_callback(doc(), kz_json:object()) -> doc().
set_notificationsinbound_callback(Doc, NotificationsinboundCallback) ->
    kz_json:set_value([[<<"notifications">>, <<"inbound">>], <<"callback">>], NotificationsinboundCallback, Doc).

-spec notificationsinboundcallback_method(doc()) -> api_binary().
-spec notificationsinboundcallback_method(doc(), Default) -> binary() | Default.
notificationsinboundcallback_method(Doc) ->
    notificationsinboundcallback_method(Doc, 'undefined').
notificationsinboundcallback_method(Doc, Default) ->
    kz_json:get_binary_value([[[<<"notifications">>, <<"inbound">>], <<"callback">>], <<"method">>], Doc, Default).

-spec set_notificationsinboundcallback_method(doc(), binary()) -> doc().
set_notificationsinboundcallback_method(Doc, NotificationsinboundcallbackMethod) ->
    kz_json:set_value([[[<<"notifications">>, <<"inbound">>], <<"callback">>], <<"method">>], NotificationsinboundcallbackMethod, Doc).

-spec notificationsinboundcallback_type(doc()) -> api_binary().
-spec notificationsinboundcallback_type(doc(), Default) -> binary() | Default.
notificationsinboundcallback_type(Doc) ->
    notificationsinboundcallback_type(Doc, 'undefined').
notificationsinboundcallback_type(Doc, Default) ->
    kz_json:get_binary_value([[[<<"notifications">>, <<"inbound">>], <<"callback">>], <<"type">>], Doc, Default).

-spec set_notificationsinboundcallback_type(doc(), binary()) -> doc().
set_notificationsinboundcallback_type(Doc, NotificationsinboundcallbackType) ->
    kz_json:set_value([[[<<"notifications">>, <<"inbound">>], <<"callback">>], <<"type">>], NotificationsinboundcallbackType, Doc).

-spec notificationsinboundcallback_url(doc()) -> api_binary().
-spec notificationsinboundcallback_url(doc(), Default) -> binary() | Default.
notificationsinboundcallback_url(Doc) ->
    notificationsinboundcallback_url(Doc, 'undefined').
notificationsinboundcallback_url(Doc, Default) ->
    kz_json:get_binary_value([[[<<"notifications">>, <<"inbound">>], <<"callback">>], <<"url">>], Doc, Default).

-spec set_notificationsinboundcallback_url(doc(), binary()) -> doc().
set_notificationsinboundcallback_url(Doc, NotificationsinboundcallbackUrl) ->
    kz_json:set_value([[[<<"notifications">>, <<"inbound">>], <<"callback">>], <<"url">>], NotificationsinboundcallbackUrl, Doc).

-spec notificationsinbound_email(doc()) -> api_object().
-spec notificationsinbound_email(doc(), Default) -> kz_json:object() | Default.
notificationsinbound_email(Doc) ->
    notificationsinbound_email(Doc, 'undefined').
notificationsinbound_email(Doc, Default) ->
    kz_json:get_json_value([[<<"notifications">>, <<"inbound">>], <<"email">>], Doc, Default).

-spec set_notificationsinbound_email(doc(), kz_json:object()) -> doc().
set_notificationsinbound_email(Doc, NotificationsinboundEmail) ->
    kz_json:set_value([[<<"notifications">>, <<"inbound">>], <<"email">>], NotificationsinboundEmail, Doc).

-spec notificationsinboundemail_send_to(doc()) -> any().
-spec notificationsinboundemail_send_to(doc(), Default) -> any() | Default.
notificationsinboundemail_send_to(Doc) ->
    notificationsinboundemail_send_to(Doc, 'undefined').
notificationsinboundemail_send_to(Doc, Default) ->
    kz_json:get_value([[[<<"notifications">>, <<"inbound">>], <<"email">>], <<"send_to">>], Doc, Default).

-spec set_notificationsinboundemail_send_to(doc(), any()) -> doc().
set_notificationsinboundemail_send_to(Doc, NotificationsinboundemailSendTo) ->
    kz_json:set_value([[[<<"notifications">>, <<"inbound">>], <<"email">>], <<"send_to">>], NotificationsinboundemailSendTo, Doc).

-spec notificationsinbound_sms(doc()) -> api_object().
-spec notificationsinbound_sms(doc(), Default) -> kz_json:object() | Default.
notificationsinbound_sms(Doc) ->
    notificationsinbound_sms(Doc, 'undefined').
notificationsinbound_sms(Doc, Default) ->
    kz_json:get_json_value([[<<"notifications">>, <<"inbound">>], <<"sms">>], Doc, Default).

-spec set_notificationsinbound_sms(doc(), kz_json:object()) -> doc().
set_notificationsinbound_sms(Doc, NotificationsinboundSms) ->
    kz_json:set_value([[<<"notifications">>, <<"inbound">>], <<"sms">>], NotificationsinboundSms, Doc).

-spec notificationsinboundsms_send_to(doc()) -> any().
-spec notificationsinboundsms_send_to(doc(), Default) -> any() | Default.
notificationsinboundsms_send_to(Doc) ->
    notificationsinboundsms_send_to(Doc, 'undefined').
notificationsinboundsms_send_to(Doc, Default) ->
    kz_json:get_value([[[<<"notifications">>, <<"inbound">>], <<"sms">>], <<"send_to">>], Doc, Default).

-spec set_notificationsinboundsms_send_to(doc(), any()) -> doc().
set_notificationsinboundsms_send_to(Doc, NotificationsinboundsmsSendTo) ->
    kz_json:set_value([[[<<"notifications">>, <<"inbound">>], <<"sms">>], <<"send_to">>], NotificationsinboundsmsSendTo, Doc).

-spec notifications_outbound(doc()) -> api_object().
-spec notifications_outbound(doc(), Default) -> kz_json:object() | Default.
notifications_outbound(Doc) ->
    notifications_outbound(Doc, 'undefined').
notifications_outbound(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"outbound">>], Doc, Default).

-spec set_notifications_outbound(doc(), kz_json:object()) -> doc().
set_notifications_outbound(Doc, NotificationsOutbound) ->
    kz_json:set_value([<<"notifications">>, <<"outbound">>], NotificationsOutbound, Doc).

-spec notificationsoutbound_callback(doc()) -> api_object().
-spec notificationsoutbound_callback(doc(), Default) -> kz_json:object() | Default.
notificationsoutbound_callback(Doc) ->
    notificationsoutbound_callback(Doc, 'undefined').
notificationsoutbound_callback(Doc, Default) ->
    kz_json:get_json_value([[<<"notifications">>, <<"outbound">>], <<"callback">>], Doc, Default).

-spec set_notificationsoutbound_callback(doc(), kz_json:object()) -> doc().
set_notificationsoutbound_callback(Doc, NotificationsoutboundCallback) ->
    kz_json:set_value([[<<"notifications">>, <<"outbound">>], <<"callback">>], NotificationsoutboundCallback, Doc).

-spec notificationsoutboundcallback_method(doc()) -> api_binary().
-spec notificationsoutboundcallback_method(doc(), Default) -> binary() | Default.
notificationsoutboundcallback_method(Doc) ->
    notificationsoutboundcallback_method(Doc, 'undefined').
notificationsoutboundcallback_method(Doc, Default) ->
    kz_json:get_binary_value([[[<<"notifications">>, <<"outbound">>], <<"callback">>], <<"method">>], Doc, Default).

-spec set_notificationsoutboundcallback_method(doc(), binary()) -> doc().
set_notificationsoutboundcallback_method(Doc, NotificationsoutboundcallbackMethod) ->
    kz_json:set_value([[[<<"notifications">>, <<"outbound">>], <<"callback">>], <<"method">>], NotificationsoutboundcallbackMethod, Doc).

-spec notificationsoutboundcallback_type(doc()) -> api_binary().
-spec notificationsoutboundcallback_type(doc(), Default) -> binary() | Default.
notificationsoutboundcallback_type(Doc) ->
    notificationsoutboundcallback_type(Doc, 'undefined').
notificationsoutboundcallback_type(Doc, Default) ->
    kz_json:get_binary_value([[[<<"notifications">>, <<"outbound">>], <<"callback">>], <<"type">>], Doc, Default).

-spec set_notificationsoutboundcallback_type(doc(), binary()) -> doc().
set_notificationsoutboundcallback_type(Doc, NotificationsoutboundcallbackType) ->
    kz_json:set_value([[[<<"notifications">>, <<"outbound">>], <<"callback">>], <<"type">>], NotificationsoutboundcallbackType, Doc).

-spec notificationsoutboundcallback_url(doc()) -> api_binary().
-spec notificationsoutboundcallback_url(doc(), Default) -> binary() | Default.
notificationsoutboundcallback_url(Doc) ->
    notificationsoutboundcallback_url(Doc, 'undefined').
notificationsoutboundcallback_url(Doc, Default) ->
    kz_json:get_binary_value([[[<<"notifications">>, <<"outbound">>], <<"callback">>], <<"url">>], Doc, Default).

-spec set_notificationsoutboundcallback_url(doc(), binary()) -> doc().
set_notificationsoutboundcallback_url(Doc, NotificationsoutboundcallbackUrl) ->
    kz_json:set_value([[[<<"notifications">>, <<"outbound">>], <<"callback">>], <<"url">>], NotificationsoutboundcallbackUrl, Doc).

-spec notificationsoutbound_email(doc()) -> api_object().
-spec notificationsoutbound_email(doc(), Default) -> kz_json:object() | Default.
notificationsoutbound_email(Doc) ->
    notificationsoutbound_email(Doc, 'undefined').
notificationsoutbound_email(Doc, Default) ->
    kz_json:get_json_value([[<<"notifications">>, <<"outbound">>], <<"email">>], Doc, Default).

-spec set_notificationsoutbound_email(doc(), kz_json:object()) -> doc().
set_notificationsoutbound_email(Doc, NotificationsoutboundEmail) ->
    kz_json:set_value([[<<"notifications">>, <<"outbound">>], <<"email">>], NotificationsoutboundEmail, Doc).

-spec notificationsoutboundemail_send_to(doc()) -> any().
-spec notificationsoutboundemail_send_to(doc(), Default) -> any() | Default.
notificationsoutboundemail_send_to(Doc) ->
    notificationsoutboundemail_send_to(Doc, 'undefined').
notificationsoutboundemail_send_to(Doc, Default) ->
    kz_json:get_value([[[<<"notifications">>, <<"outbound">>], <<"email">>], <<"send_to">>], Doc, Default).

-spec set_notificationsoutboundemail_send_to(doc(), any()) -> doc().
set_notificationsoutboundemail_send_to(Doc, NotificationsoutboundemailSendTo) ->
    kz_json:set_value([[[<<"notifications">>, <<"outbound">>], <<"email">>], <<"send_to">>], NotificationsoutboundemailSendTo, Doc).

-spec notificationsoutbound_sms(doc()) -> api_object().
-spec notificationsoutbound_sms(doc(), Default) -> kz_json:object() | Default.
notificationsoutbound_sms(Doc) ->
    notificationsoutbound_sms(Doc, 'undefined').
notificationsoutbound_sms(Doc, Default) ->
    kz_json:get_json_value([[<<"notifications">>, <<"outbound">>], <<"sms">>], Doc, Default).

-spec set_notificationsoutbound_sms(doc(), kz_json:object()) -> doc().
set_notificationsoutbound_sms(Doc, NotificationsoutboundSms) ->
    kz_json:set_value([[<<"notifications">>, <<"outbound">>], <<"sms">>], NotificationsoutboundSms, Doc).

-spec notificationsoutboundsms_send_to(doc()) -> any().
-spec notificationsoutboundsms_send_to(doc(), Default) -> any() | Default.
notificationsoutboundsms_send_to(Doc) ->
    notificationsoutboundsms_send_to(Doc, 'undefined').
notificationsoutboundsms_send_to(Doc, Default) ->
    kz_json:get_value([[[<<"notifications">>, <<"outbound">>], <<"sms">>], <<"send_to">>], Doc, Default).

-spec set_notificationsoutboundsms_send_to(doc(), any()) -> doc().
set_notificationsoutboundsms_send_to(Doc, NotificationsoutboundsmsSendTo) ->
    kz_json:set_value([[[<<"notifications">>, <<"outbound">>], <<"sms">>], <<"send_to">>], NotificationsoutboundsmsSendTo, Doc).

-spec retries(doc()) -> integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Doc) ->
    retries(Doc, 1).
retries(Doc, Default) ->
    kz_json:get_integer_value(<<"retries">>, Doc, Default).

-spec set_retries(doc(), integer()) -> doc().
set_retries(Doc, Retries) ->
    kz_json:set_value(<<"retries">>, Retries, Doc).

-spec smtp_permission_list(doc()) -> ne_binaries().
-spec smtp_permission_list(doc(), Default) -> ne_binaries() | Default.
smtp_permission_list(Doc) ->
    smtp_permission_list(Doc, []).
smtp_permission_list(Doc, Default) ->
    kz_json:get_list_value(<<"smtp_permission_list">>, Doc, Default).

-spec set_smtp_permission_list(doc(), ne_binaries()) -> doc().
set_smtp_permission_list(Doc, SmtpPermissionList) ->
    kz_json:set_value(<<"smtp_permission_list">>, SmtpPermissionList, Doc).
