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
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
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
    media(Doc, {}).
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

-spec notifications(doc()) -> api_object().
-spec notifications(doc(), Default) -> kz_json:object() | Default.
notifications(Doc) ->
    notifications(Doc, 'undefined').
notifications(Doc, Default) ->
    kz_json:get_json_value(<<"notifications">>, Doc, Default).

-spec set_notifications(doc(), kz_json:object()) -> doc().
set_notifications(Doc, Notifications) ->
    kz_json:set_value(<<"notifications">>, Notifications, Doc).

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
