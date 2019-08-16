%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_port_requests).

-export([new/0]).
-export([bill/1, bill/2, set_bill/2]).
-export([bill_account_number/1, bill_account_number/2, set_bill_account_number/2]).
-export([bill_btn/1, bill_btn/2, set_bill_btn/2]).
-export([bill_carrier/1, bill_carrier/2, set_bill_carrier/2]).
-export([bill_locality/1, bill_locality/2, set_bill_locality/2]).
-export([bill_name/1, bill_name/2, set_bill_name/2]).
-export([bill_pin/1, bill_pin/2, set_bill_pin/2]).
-export([bill_postal_code/1, bill_postal_code/2, set_bill_postal_code/2]).
-export([bill_region/1, bill_region/2, set_bill_region/2]).
-export([bill_street_address/1, bill_street_address/2, set_bill_street_address/2]).
-export([bill_street_number/1, bill_street_number/2, set_bill_street_number/2]).
-export([bill_street_post_dir/1, bill_street_post_dir/2, set_bill_street_post_dir/2]).
-export([bill_street_pre_dir/1, bill_street_pre_dir/2, set_bill_street_pre_dir/2]).
-export([bill_street_type/1, bill_street_type/2, set_bill_street_type/2]).
-export([comments/1, comments/2, set_comments/2]).
-export([name/1, name/2, set_name/2]).
-export([notifications/1, notifications/2, set_notifications/2]).
-export([notifications_email/1, notifications_email/2, set_notifications_email/2]).
-export([notifications_email_send_to/1, notifications_email_send_to/2, set_notifications_email_send_to/2]).
-export([numbers/1, numbers/2, set_numbers/2]).
-export([number/2, number/3, set_number/3]).
-export([reference_number/1, reference_number/2, set_reference_number/2]).
-export([signee_name/1, signee_name/2, set_signee_name/2]).
-export([signing_date/1, signing_date/2, set_signing_date/2]).
-export([transfer_date/1, transfer_date/2, set_transfer_date/2]).
-export([winning_carrier/1, winning_carrier/2, set_winning_carrier/2]).

%% Private fields
-export([pvt_account_name/1, pvt_account_name/2, set_pvt_account_name/2]).
-export([pvt_last_phonebook_error/1, pvt_last_phonebook_error/2, set_pvt_last_phonebook_error/2]).
-export([pvt_port_authority/1, pvt_port_authority/2, set_pvt_port_authority/2]).
-export([pvt_port_authority_name/1, pvt_port_authority_name/2, set_pvt_port_authority_name/2]).
-export([pvt_port_state/1, pvt_port_state/2, set_pvt_port_state/2]).
-export([pvt_ported_numbers/1, pvt_ported_numbers/2, set_pvt_ported_numbers/2]).
-export([pvt_sent/1, pvt_sent/2, set_pvt_sent/2]).
-export([pvt_transitions/1, pvt_transitions/2, set_pvt_tranisitions/2]).
-export([pvt_tree/1, pvt_tree/2, set_pvt_tree/2]).

%% Utilities
-export([get_transition/2]).
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

-spec bill_account_number(doc()) -> kz_term:api_binary().
bill_account_number(Doc) ->
    bill_account_number(Doc, 'undefined').

-spec bill_account_number(doc(), Default) -> binary() | Default.
bill_account_number(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"account_number">>], Doc, Default).

-spec set_bill_account_number(doc(), binary()) -> doc().
set_bill_account_number(Doc, BillAccountNumber) ->
    kz_json:set_value([<<"bill">>, <<"account_number">>], BillAccountNumber, Doc).

-spec bill_btn(doc()) -> kz_term:api_binary().
bill_btn(Doc) ->
    bill_btn(Doc, 'undefined').

-spec bill_btn(doc(), Default) -> binary() | Default.
bill_btn(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"btn">>], Doc, Default).

-spec set_bill_btn(doc(), binary()) -> doc().
set_bill_btn(Doc, BillBtn) ->
    kz_json:set_value([<<"bill">>, <<"btn">>], BillBtn, Doc).

-spec bill_carrier(doc()) -> kz_term:api_binary().
bill_carrier(Doc) ->
    bill_carrier(Doc, 'undefined').

-spec bill_carrier(doc(), Default) -> binary() | Default.
bill_carrier(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"carrier">>], Doc, Default).

-spec set_bill_carrier(doc(), binary()) -> doc().
set_bill_carrier(Doc, BillCarrier) ->
    kz_json:set_value([<<"bill">>, <<"carrier">>], BillCarrier, Doc).

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

-spec bill_pin(doc()) -> kz_term:api_binary().
bill_pin(Doc) ->
    bill_pin(Doc, 'undefined').

-spec bill_pin(doc(), Default) -> binary() | Default.
bill_pin(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"pin">>], Doc, Default).

-spec set_bill_pin(doc(), binary()) -> doc().
set_bill_pin(Doc, BillPin) ->
    kz_json:set_value([<<"bill">>, <<"pin">>], BillPin, Doc).

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

-spec bill_street_post_dir(doc()) -> kz_term:api_binary().
bill_street_post_dir(Doc) ->
    bill_street_post_dir(Doc, 'undefined').

-spec bill_street_post_dir(doc(), Default) -> binary() | Default.
bill_street_post_dir(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"street_post_dir">>], Doc, Default).

-spec set_bill_street_post_dir(doc(), binary()) -> doc().
set_bill_street_post_dir(Doc, BillStreetPostDir) ->
    kz_json:set_value([<<"bill">>, <<"street_post_dir">>], BillStreetPostDir, Doc).

-spec bill_street_pre_dir(doc()) -> kz_term:api_binary().
bill_street_pre_dir(Doc) ->
    bill_street_pre_dir(Doc, 'undefined').

-spec bill_street_pre_dir(doc(), Default) -> binary() | Default.
bill_street_pre_dir(Doc, Default) ->
    kz_json:get_binary_value([<<"bill">>, <<"street_pre_dir">>], Doc, Default).

-spec set_bill_street_pre_dir(doc(), binary()) -> doc().
set_bill_street_pre_dir(Doc, BillStreetPreDir) ->
    kz_json:set_value([<<"bill">>, <<"street_pre_dir">>], BillStreetPreDir, Doc).

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

-spec reference_number(doc()) -> kz_term:api_binary().
reference_number(Doc) ->
    reference_number(Doc, 'undefined').

-spec reference_number(doc(), Default) -> binary() | Default.
reference_number(Doc, Default) ->
    kz_json:get_binary_value([<<"reference_number">>], Doc, Default).

-spec set_reference_number(doc(), binary()) -> doc().
set_reference_number(Doc, ReferenceNumber) ->
    kz_json:set_value([<<"reference_number">>], ReferenceNumber, Doc).

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

-spec winning_carrier(doc()) -> kz_term:api_binary().
winning_carrier(Doc) ->
    winning_carrier(Doc, 'undefined').

-spec winning_carrier(doc(), Default) -> binary() | Default.
winning_carrier(Doc, Default) ->
    kz_json:get_binary_value([<<"winning_carrier">>], Doc, Default).

-spec set_winning_carrier(doc(), binary()) -> doc().
set_winning_carrier(Doc, WinningCarrier) ->
    kz_json:set_value([<<"winning_carrier">>], WinningCarrier, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_account_name(doc()) -> kz_term:api_ne_binary().
pvt_account_name(Doc) ->
    pvt_account_name(Doc, 'undefined').

-spec pvt_account_name(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_account_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_account_name">>], Doc, Default).

-spec set_pvt_account_name(doc(), kz_term:ne_binary()) -> doc().
set_pvt_account_name(Doc, Name) ->
    kz_json:set_value([<<"pvt_account_name">>], Name, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_last_phonebook_error(doc()) -> kz_term:api_ne_binary().
pvt_last_phonebook_error(Doc) ->
    pvt_last_phonebook_error(Doc, 'undefined').

-spec pvt_last_phonebook_error(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_last_phonebook_error(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_last_phonebook_error">>], Doc, Default).

-spec set_pvt_last_phonebook_error(doc(), kz_term:ne_binary()) -> doc().
set_pvt_last_phonebook_error(Doc, Name) ->
    kz_json:set_value([<<"pvt_last_phonebook_error">>], Name, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_port_authority(doc()) -> kz_term:api_ne_binary().
pvt_port_authority(Doc) ->
    pvt_port_authority(Doc, 'undefined').

-spec pvt_port_authority(doc(), Default) -> kz_term:api_ne_binary() | Default.
pvt_port_authority(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_port_authority">>], Doc, Default).

-spec set_pvt_port_authority(doc(), kz_term:api_binary()) -> doc().
set_pvt_port_authority(Doc, PortAuthority) ->
    kz_json:set_value([<<"pvt_port_authority">>], PortAuthority, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_port_authority_name(doc()) -> kz_term:api_ne_binary().
pvt_port_authority_name(Doc) ->
    pvt_port_authority_name(Doc, 'undefined').

-spec pvt_port_authority_name(doc(), Default) -> kz_term:api_ne_binary() | Default.
pvt_port_authority_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_port_authority_name">>], Doc, Default).

-spec set_pvt_port_authority_name(doc(), kz_term:api_binary()) -> doc().
set_pvt_port_authority_name(Doc, PortAuthority) ->
    kz_json:set_value([<<"pvt_port_authority_name">>], PortAuthority, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_port_state(doc()) -> kz_term:api_ne_binary().
pvt_port_state(Doc) ->
    pvt_port_state(Doc, 'undefined').

-spec pvt_port_state(doc(), Default) -> kz_term:api_ne_binary() | Default.
pvt_port_state(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_port_state">>], Doc, Default).

-spec set_pvt_port_state(doc(), kz_term:api_binary()) -> doc().
set_pvt_port_state(Doc, PortAuthority) ->
    kz_json:set_value([<<"pvt_port_state">>], PortAuthority, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_ported_numbers(doc()) -> kz_term:api_object().
pvt_ported_numbers(Doc) ->
    pvt_ported_numbers(Doc, 'undefined').

-spec pvt_ported_numbers(doc(), Default) -> kz_json:object() | Default.
pvt_ported_numbers(Doc, Default) ->
    kz_json:get_json_value([<<"pvt_ported_numbers">>], Doc, Default).

-spec set_pvt_ported_numbers(doc(), kz_json:object()) -> doc().
set_pvt_ported_numbers(Doc, Numbers) ->
    kz_json:set_value([<<"pvt_ported_numbers">>], Numbers, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_tree(doc()) -> kz_term:ne_binaries().
pvt_tree(Doc) ->
    pvt_tree(Doc, []).

-spec pvt_tree(doc(), Default) -> kz_term:ne_binaries() | Default.
pvt_tree(Doc, Default) ->
    kz_json:get_list_value([<<"pvt_tree">>], Doc, Default).

-spec set_pvt_tree(doc(), kz_term:api_ne_binaries()) -> doc().
set_pvt_tree(Doc, Tree) ->
    kz_json:set_value([<<"pvt_sent">>], Tree, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_sent(doc()) -> kz_term:api_boolean().
pvt_sent(Doc) ->
    pvt_sent(Doc, 'undefined').

-spec pvt_sent(doc(), Default) -> kz_term:api_boolean() | Default.
pvt_sent(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"pvt_sent">>], Doc, Default).

-spec set_pvt_sent(doc(), kz_term:api_boolean()) -> doc().
set_pvt_sent(Doc, IsSent) ->
    kz_json:set_value([<<"pvt_sent">>], IsSent, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_transitions(doc()) -> kz_term:api_objects().
pvt_transitions(Doc) ->
    pvt_transitions(Doc, 'undefined').

-spec pvt_transitions(doc(), Default) -> kz_term:api_objects() | Default.
pvt_transitions(Doc, Default) ->
    kz_json:get_list_value([<<"pvt_transitions">>], Doc, Default).

-spec set_pvt_tranisitions(doc(), kz_term:api_objects()) -> doc().
set_pvt_tranisitions(Doc, Transitions) ->
    kz_json:set_value([<<"pvt_transitions">>], Transitions, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_transition(doc(), kz_term:ne_binary()) -> kz_json:objects().
get_transition(Doc, ToState) ->
    ToStatePath = [<<"transition">>, <<"new">>],
    [Transition
     || Transition <- pvt_transitions(Doc, []),
        kz_json:get_ne_binary_value(ToStatePath, Transition) =:= ToState
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_port_authority(doc() | kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
find_port_authority('undefined') ->
    case kapps_util:get_master_account_id() of
        {'ok', ?NE_BINARY = MasterAccountId} ->
            lager:debug("account id is undefined, checking master"),
            find_port_authority(MasterAccountId, MasterAccountId, MasterAccountId);
        {'ok', _} ->
            lager:debug("master and account id is undefined"),
            'undefined';
        {'error', _} ->
            lager:debug("master and account id is undefined"),
            'undefined'
    end;
find_port_authority(AccountId) when is_binary(AccountId) ->
    case kapps_util:get_master_account_id() of
        {'ok', ?NE_BINARY = MasterAccountId} ->
            find_port_authority(MasterAccountId, AccountId, AccountId);
        {'ok', _} ->
            lager:debug("master and account id is undefined"),
            'undefined';
        {'error', _} ->
            lager:debug("failed to find port authority, master account is undefined"),
            'undefined'
    end;
find_port_authority(Doc) ->
    case kz_json:is_json_object(Doc) of
        'true' ->
            case pvt_port_authority(Doc) of
                'undefined' ->
                    find_port_authority(kz_doc:account_id(Doc));
                PortAuthority ->
                    lager:debug("using account ~s as port authority", [PortAuthority]),
                    PortAuthority
            end;
        'false' ->
            'undefined'
    end.

-spec find_port_authority(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) ->
                                 kz_term:api_binary().
find_port_authority(MasterAccountId, SubmittedAccountId, 'undefined') ->
    lager:debug("account id is undefined, checking master"),
    find_port_authority(MasterAccountId, SubmittedAccountId, MasterAccountId);
find_port_authority(MasterAccountId, _, MasterAccountId) ->
    lager:debug("using master account ~s as port authority", [MasterAccountId]),
    MasterAccountId;
find_port_authority(MasterAccountId, SubmittedAccountId, AccountId) ->
    WhiteAuthority = kzd_whitelabel:fetch_port_authority(AccountId, 'undefined'),
    find_port_authority(MasterAccountId, SubmittedAccountId, AccountId, WhiteAuthority).

-spec find_port_authority(kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
                                 kz_term:api_binary().
find_port_authority(MasterAccountId, SubmittedAccountId, AccountId, 'undefined') ->
    ParentId = kzd_accounts:get_authoritative_parent_id(AccountId, MasterAccountId),
    lager:debug("no port authority key found for ~s, checking parent ~s", [AccountId, ParentId]),
    case ParentId of
        AccountId ->
            lager:debug("reached to top level account"),
            find_port_authority(MasterAccountId, SubmittedAccountId, MasterAccountId);
        ParentId ->
            find_port_authority(MasterAccountId, SubmittedAccountId, ParentId)
    end;
find_port_authority(MasterAccountId, AccountId, AccountId, AccountId) ->
    ParentId = kzd_accounts:get_authoritative_parent_id(AccountId, MasterAccountId),
    lager:debug("port authority is same as submitted account ~s, checking parent ~s port authority"
               ,[AccountId, ParentId]
               ),
    case ParentId of
        AccountId ->
            lager:debug("reached to top level account"),
            find_port_authority(MasterAccountId, AccountId, MasterAccountId);
        ParentId ->
            find_port_authority(MasterAccountId, AccountId, ParentId)
    end;
find_port_authority(MasterAccountId, SubmittedAccountId, _, MasterAccountId) ->
    find_port_authority(MasterAccountId, SubmittedAccountId, MasterAccountId);
find_port_authority(_, _, _, PortAuthority) ->
    lager:debug("using account ~s as port authority", [PortAuthority]),
    PortAuthority.
