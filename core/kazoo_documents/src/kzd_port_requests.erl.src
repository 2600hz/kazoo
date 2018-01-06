-module(kzd_port_requests).

-export([new/0]).
-export([bill/1, bill/2, set_bill/2]).
-export([bill_carrier/1, bill_carrier/2, set_bill_carrier/2]).
-export([bill_extended_address/1, bill_extended_address/2, set_bill_extended_address/2]).
-export([bill_locality/1, bill_locality/2, set_bill_locality/2]).
-export([bill_name/1, bill_name/2, set_bill_name/2]).
-export([bill_postal_code/1, bill_postal_code/2, set_bill_postal_code/2]).
-export([bill_region/1, bill_region/2, set_bill_region/2]).
-export([bill_street_address/1, bill_street_address/2, set_bill_street_address/2]).
-export([comments/1, comments/2, set_comments/2]).
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notifications_email/1, notifications_email/2, set_notifications_email/2]).
-export([notificationsemail_send_to/1, notificationsemail_send_to/2, set_notificationsemail_send_to/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([port_state/1, port_state/2, set_port_state/2]).
-export([transfer_date/1, transfer_date/2, set_transfer_date/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec bill(doc()) -> api_object().
-spec bill(doc(), Default) -> kz_json:object() | Default.
bill(Doc) ->
    bill(Doc, 'undefined').
bill(Doc, Default) ->
    kz_json:get_json_value(<<"bill">>, Doc, Default).

-spec set_bill(doc(), kz_json:object()) -> doc().
set_bill(Doc, Bill) ->
    kz_json:set_value(<<"bill">>, Bill, Doc).

-spec bill_carrier(doc()) -> api_binary().
-spec bill_carrier(doc(), Default) -> binary() | Default.
bill_carrier(Doc) ->
    bill_carrier(Doc, 'undefined').
bill_carrier(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"carrier">>], Doc, Default).

-spec set_bill_carrier(doc(), binary()) -> doc().
set_bill_carrier(Doc, BillCarrier) ->
    kz_json:set_value([<<"bill">>, <<"carrier">>], BillCarrier, Doc).

-spec bill_extended_address(doc()) -> api_binary().
-spec bill_extended_address(doc(), Default) -> binary() | Default.
bill_extended_address(Doc) ->
    bill_extended_address(Doc, 'undefined').
bill_extended_address(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"extended_address">>], Doc, Default).

-spec set_bill_extended_address(doc(), binary()) -> doc().
set_bill_extended_address(Doc, BillExtendedAddress) ->
    kz_json:set_value([<<"bill">>, <<"extended_address">>], BillExtendedAddress, Doc).

-spec bill_locality(doc()) -> api_binary().
-spec bill_locality(doc(), Default) -> binary() | Default.
bill_locality(Doc) ->
    bill_locality(Doc, 'undefined').
bill_locality(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"locality">>], Doc, Default).

-spec set_bill_locality(doc(), binary()) -> doc().
set_bill_locality(Doc, BillLocality) ->
    kz_json:set_value([<<"bill">>, <<"locality">>], BillLocality, Doc).

-spec bill_name(doc()) -> api_binary().
-spec bill_name(doc(), Default) -> binary() | Default.
bill_name(Doc) ->
    bill_name(Doc, 'undefined').
bill_name(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"name">>], Doc, Default).

-spec set_bill_name(doc(), binary()) -> doc().
set_bill_name(Doc, BillName) ->
    kz_json:set_value([<<"bill">>, <<"name">>], BillName, Doc).

-spec bill_postal_code(doc()) -> api_binary().
-spec bill_postal_code(doc(), Default) -> binary() | Default.
bill_postal_code(Doc) ->
    bill_postal_code(Doc, 'undefined').
bill_postal_code(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"postal_code">>], Doc, Default).

-spec set_bill_postal_code(doc(), binary()) -> doc().
set_bill_postal_code(Doc, BillPostalCode) ->
    kz_json:set_value([<<"bill">>, <<"postal_code">>], BillPostalCode, Doc).

-spec bill_region(doc()) -> api_binary().
-spec bill_region(doc(), Default) -> binary() | Default.
bill_region(Doc) ->
    bill_region(Doc, 'undefined').
bill_region(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"region">>], Doc, Default).

-spec set_bill_region(doc(), binary()) -> doc().
set_bill_region(Doc, BillRegion) ->
    kz_json:set_value([<<"bill">>, <<"region">>], BillRegion, Doc).

-spec bill_street_address(doc()) -> api_binary().
-spec bill_street_address(doc(), Default) -> binary() | Default.
bill_street_address(Doc) ->
    bill_street_address(Doc, 'undefined').
bill_street_address(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"street_address">>], Doc, Default).

-spec set_bill_street_address(doc(), binary()) -> doc().
set_bill_street_address(Doc, BillStreetAddress) ->
    kz_json:set_value([<<"bill">>, <<"street_address">>], BillStreetAddress, Doc).

-spec comments(doc()) -> api_list().
-spec comments(doc(), Default) -> list() | Default.
comments(Doc) ->
    comments(Doc, 'undefined').
comments(Doc, Default) ->
    kz_json:get_list_value(<<"comments">>, Doc, Default).

-spec set_comments(doc(), list()) -> doc().
set_comments(Doc, Comments) ->
    kz_json:set_value(<<"comments">>, Comments, Doc).

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

-spec notifications_email(doc()) -> api_object().
-spec notifications_email(doc(), Default) -> kz_json:object() | Default.
notifications_email(Doc) ->
    notifications_email(Doc, 'undefined').
notifications_email(Doc, Default) ->
    kz_json:get_json_value([<<"notifications">>, <<"email">>], Doc, Default).

-spec set_notifications_email(doc(), kz_json:object()) -> doc().
set_notifications_email(Doc, NotificationsEmail) ->
    kz_json:set_value([<<"notifications">>, <<"email">>], NotificationsEmail, Doc).

-spec notificationsemail_send_to(doc()) -> any().
-spec notificationsemail_send_to(doc(), Default) -> any() | Default.
notificationsemail_send_to(Doc) ->
    notificationsemail_send_to(Doc, 'undefined').
notificationsemail_send_to(Doc, Default) ->
    kz_json:get_value([[<<"notifications">>, <<"email">>], <<"send_to">>], Doc, Default).

-spec set_notificationsemail_send_to(doc(), any()) -> doc().
set_notificationsemail_send_to(Doc, NotificationsemailSendTo) ->
    kz_json:set_value([[<<"notifications">>, <<"email">>], <<"send_to">>], NotificationsemailSendTo, Doc).

-spec numbers(doc()) -> api_object().
-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc) ->
    numbers(Doc, 'undefined').
numbers(Doc, Default) ->
    kz_json:get_json_value(<<"numbers">>, Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value(<<"numbers">>, Numbers, Doc).

-spec port_state(doc()) -> binary().
-spec port_state(doc(), Default) -> binary() | Default.
port_state(Doc) ->
    port_state(Doc, <<"unconfirmed">>).
port_state(Doc, Default) ->
    kz_json:get_binary_value(<<"port_state">>, Doc, Default).

-spec set_port_state(doc(), binary()) -> doc().
set_port_state(Doc, PortState) ->
    kz_json:set_value(<<"port_state">>, PortState, Doc).

-spec transfer_date(doc()) -> api_integer().
-spec transfer_date(doc(), Default) -> integer() | Default.
transfer_date(Doc) ->
    transfer_date(Doc, 'undefined').
transfer_date(Doc, Default) ->
    kz_json:get_integer_value(<<"transfer_date">>, Doc, Default).

-spec set_transfer_date(doc(), integer()) -> doc().
set_transfer_date(Doc, TransferDate) ->
    kz_json:set_value(<<"transfer_date">>, TransferDate, Doc).
