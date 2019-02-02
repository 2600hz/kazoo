%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `port_requests' document.
%%% @end
%%%-----------------------------------------------------------------------------
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
-export([bill_street_number/1, bill_street_number/2, set_bill_street_number/2]).
-export([bill_street_type/1, bill_street_type/2, set_bill_street_type/2]).
-export([comments/1, comments/2, set_comments/2]).
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notifications_email/1, notifications_email/2, set_notifications_email/2]).
-export([notifications_email_send_to/1, notifications_email_send_to/2, set_notifications_email_send_to/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([number/2, number/3, set_number/3]).
-export([port_state/1, port_state/2, set_port_state/2]).
-export([signee_name/1, signee_name/2, set_signee_name/2]).
-export([signing_date/1, signing_date/2, set_signing_date/2]).
-export([transfer_date/1, transfer_date/2, set_transfer_date/2]).

-export([port_authority/1, port_authority/2, set_port_authority/2]).
-export([find_port_authority/1]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"port_requests">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec bill(doc()) -> kz_term:api_object().
bill(Doc) ->
    bill(Doc, 'undefined').

-spec bill(doc(), Default) -> kz_json:object() | Default.
bill(Doc, Default) ->
    kz_json:get_json_value([<<"bill">>], Doc, Default).

-spec set_bill(doc(), kz_json:object()) -> doc().
set_bill(Doc, Bill) ->
    kz_json:set_value([<<"bill">>], Bill, Doc).

-spec bill_carrier(doc()) -> kz_term:api_binary().
bill_carrier(Doc) ->
    bill_carrier(Doc, 'undefined').

-spec bill_carrier(doc(), Default) -> binary() | Default.
bill_carrier(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"carrier">>], Doc, Default).

-spec set_bill_carrier(doc(), binary()) -> doc().
set_bill_carrier(Doc, BillCarrier) ->
    kz_json:set_value([<<"bill">>, <<"carrier">>], BillCarrier, Doc).

-spec bill_extended_address(doc()) -> kz_term:api_binary().
bill_extended_address(Doc) ->
    bill_extended_address(Doc, 'undefined').

-spec bill_extended_address(doc(), Default) -> binary() | Default.
bill_extended_address(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"extended_address">>], Doc, Default).

-spec set_bill_extended_address(doc(), binary()) -> doc().
set_bill_extended_address(Doc, BillExtendedAddress) ->
    kz_json:set_value([<<"bill">>, <<"extended_address">>], BillExtendedAddress, Doc).

-spec bill_locality(doc()) -> kz_term:api_binary().
bill_locality(Doc) ->
    bill_locality(Doc, 'undefined').

-spec bill_locality(doc(), Default) -> binary() | Default.
bill_locality(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"locality">>], Doc, Default).

-spec set_bill_locality(doc(), binary()) -> doc().
set_bill_locality(Doc, BillLocality) ->
    kz_json:set_value([<<"bill">>, <<"locality">>], BillLocality, Doc).

-spec bill_name(doc()) -> kz_term:api_binary().
bill_name(Doc) ->
    bill_name(Doc, 'undefined').

-spec bill_name(doc(), Default) -> binary() | Default.
bill_name(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"name">>], Doc, Default).

-spec set_bill_name(doc(), binary()) -> doc().
set_bill_name(Doc, BillName) ->
    kz_json:set_value([<<"bill">>, <<"name">>], BillName, Doc).

-spec bill_postal_code(doc()) -> kz_term:api_binary().
bill_postal_code(Doc) ->
    bill_postal_code(Doc, 'undefined').

-spec bill_postal_code(doc(), Default) -> binary() | Default.
bill_postal_code(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"postal_code">>], Doc, Default).

-spec set_bill_postal_code(doc(), binary()) -> doc().
set_bill_postal_code(Doc, BillPostalCode) ->
    kz_json:set_value([<<"bill">>, <<"postal_code">>], BillPostalCode, Doc).

-spec bill_region(doc()) -> kz_term:api_binary().
bill_region(Doc) ->
    bill_region(Doc, 'undefined').

-spec bill_region(doc(), Default) -> binary() | Default.
bill_region(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"region">>], Doc, Default).

-spec set_bill_region(doc(), binary()) -> doc().
set_bill_region(Doc, BillRegion) ->
    kz_json:set_value([<<"bill">>, <<"region">>], BillRegion, Doc).

-spec bill_street_address(doc()) -> kz_term:api_binary().
bill_street_address(Doc) ->
    bill_street_address(Doc, 'undefined').

-spec bill_street_address(doc(), Default) -> binary() | Default.
bill_street_address(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"street_address">>], Doc, Default).

-spec set_bill_street_address(doc(), binary()) -> doc().
set_bill_street_address(Doc, BillStreetAddress) ->
    kz_json:set_value([<<"bill">>, <<"street_address">>], BillStreetAddress, Doc).

-spec bill_street_number(doc()) -> kz_term:api_binary().
bill_street_number(Doc) ->
    bill_street_number(Doc, 'undefined').

-spec bill_street_number(doc(), Default) -> binary() | Default.
bill_street_number(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"street_number">>], Doc, Default).

-spec set_bill_street_number(doc(), binary()) -> doc().
set_bill_street_number(Doc, BillStreetNumber) ->
    kz_json:set_value([<<"bill">>, <<"street_number">>], BillStreetNumber, Doc).

-spec bill_street_type(doc()) -> kz_term:api_binary().
bill_street_type(Doc) ->
    bill_street_type(Doc, 'undefined').

-spec bill_street_type(doc(), Default) -> binary() | Default.
bill_street_type(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"street_type">>], Doc, Default).

-spec set_bill_street_type(doc(), binary()) -> doc().
set_bill_street_type(Doc, BillStreetType) ->
    kz_json:set_value([<<"bill">>, <<"street_type">>], BillStreetType, Doc).

-spec comments(doc()) -> kz_term:api_objects().
comments(Doc) ->
    comments(Doc, 'undefined').

-spec comments(doc(), Default) -> kz_json:objects() | Default.
comments(Doc, Default) ->
    kz_json:get_list_value([<<"comments">>], Doc, Default).

-spec set_comments(doc(), kz_json:objects()) -> doc().
set_comments(Doc, Comments) ->
    kz_json:set_value([<<"comments">>], Comments, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

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

-spec numbers(doc()) -> kz_term:api_object().
numbers(Doc) ->
    numbers(Doc, 'undefined').

-spec numbers(doc(), Default) -> kz_json:object() | Default.
numbers(Doc, Default) ->
    kz_json:get_json_value([<<"numbers">>], Doc, Default).

-spec set_numbers(doc(), kz_json:object()) -> doc().
set_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"numbers">>], Numbers, Doc).

-spec number(doc(), kz_json:key()) -> kz_term:api_object().
number(Doc, Number) ->
    number(Doc, Number, 'undefined').

-spec number(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
number(Doc, Number, Default) ->
    kz_json:get_json_value([<<"numbers">>, Number], Doc, Default).

-spec set_number(doc(), kz_json:key(), kz_json:object()) -> doc().
set_number(Doc, Number, Value) ->
    kz_json:set_value([<<"numbers">>, Number], Value, Doc).

-spec port_state(doc()) -> binary().
port_state(Doc) ->
    port_state(Doc, <<"unconfirmed">>).

-spec port_state(doc(), Default) -> binary() | Default.
port_state(Doc, Default) ->
    kz_json:get_binary_value([<<"port_state">>], Doc, Default).

-spec set_port_state(doc(), binary()) -> doc().
set_port_state(Doc, PortState) ->
    kz_json:set_value([<<"port_state">>], PortState, Doc).

-spec signee_name(doc()) -> kz_term:api_binary().
signee_name(Doc) ->
    signee_name(Doc, 'undefined').

-spec signee_name(doc(), Default) -> binary() | Default.
signee_name(Doc, Default) ->
    kz_json:get_binary_value([<<"signee_name">>], Doc, Default).

-spec set_signee_name(doc(), binary()) -> doc().
set_signee_name(Doc, SigneeName) ->
    kz_json:set_value([<<"signee_name">>], SigneeName, Doc).

-spec signing_date(doc()) -> kz_term:api_integer().
signing_date(Doc) ->
    signing_date(Doc, 'undefined').

-spec signing_date(doc(), Default) -> integer() | Default.
signing_date(Doc, Default) ->
    kz_json:get_integer_value([<<"signing_date">>], Doc, Default).

-spec set_signing_date(doc(), integer()) -> doc().
set_signing_date(Doc, SigningDate) ->
    kz_json:set_value([<<"signing_date">>], SigningDate, Doc).

-spec transfer_date(doc()) -> kz_term:api_integer().
transfer_date(Doc) ->
    transfer_date(Doc, 'undefined').

-spec transfer_date(doc(), Default) -> integer() | Default.
transfer_date(Doc, Default) ->
    kz_json:get_integer_value([<<"transfer_date">>], Doc, Default).

-spec set_transfer_date(doc(), integer()) -> doc().
set_transfer_date(Doc, TransferDate) ->
    kz_json:set_value([<<"transfer_date">>], TransferDate, Doc).

-spec port_authority(doc()) -> kz_term:api_ne_binary().
port_authority(Doc) ->
    port_authority(Doc, 'undefined').

-spec port_authority(doc(), Default) -> kz_term:api_ne_binary() | Default.
port_authority(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_port_authority">>], Doc, Default).

-spec set_port_authority(doc(), kz_term:api_binary()) -> doc().
set_port_authority(Doc, PortAuthority) ->
    kz_json:set_value([<<"pvt_port_authority">>], PortAuthority, Doc).

-spec find_port_authority(doc()) -> kz_term:api_binary().
find_port_authority(Doc) ->
    case port_authority(Doc) of
        'undefined' ->
            AccountId = kz_doc:account_id(Doc),
            case kapps_util:get_master_account_id() of
                {'ok', MasterAccountId} ->
                    PortAuthority = find_port_authority(MasterAccountId, AccountId),
                    lager:debug("using account ~s as port authority", [PortAuthority]),
                    PortAuthority;
                {'error', _} ->
                    lager:debug("failed to find port authority, master account is undefined"),
                    'undefined'
            end;
        PortAuthority ->
            lager:debug("using account ~s as port authority", [PortAuthority]),
            PortAuthority
    end.

-spec find_port_authority(kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_binary().
find_port_authority('undefined', 'undefined') ->
    lager:debug("master and account id is undefined"),
    'undefined';
find_port_authority(?NE_BINARY = MasterAccountId, 'undefined') ->
    lager:debug("account id is undefined, checking master"),
    find_port_authority(MasterAccountId, MasterAccountId);
find_port_authority(MasterAccountId, MasterAccountId) ->
    case kzd_whitelabel:fetch(MasterAccountId) of
        {'error', _R} ->
            lager:debug("failed to find master whitelabel, assuming master is port authority"),
            MasterAccountId;
        {'ok', JObj} ->
            lager:debug("checking master whitelabel port authority if defined"),
            kzd_whitelabel:port_authority(JObj, MasterAccountId)
    end;
find_port_authority(MasterAccountId, AccountId) ->
    case kzd_whitelabel:fetch(AccountId) of
        {'error', _R} ->
            ParentId = kzd_accounts:get_authoritative_parent_id(AccountId, MasterAccountId),
            lager:debug("failed to find whitelabel for ~s, checking parent ~s", [AccountId, ParentId]),
            find_port_authority(MasterAccountId, ParentId);
        {'ok', JObj} ->
            case kzd_whitelabel:port_authority(JObj) of
                'undefined' ->
                    ParentId = kzd_accounts:get_authoritative_parent_id(AccountId, MasterAccountId),
                    lager:debug("undefined port authority in account ~s, checking parent ~s"
                               ,[AccountId, ParentId]
                               ),
                    find_port_authority(MasterAccountId, ParentId);
                Id -> Id
            end
    end.
