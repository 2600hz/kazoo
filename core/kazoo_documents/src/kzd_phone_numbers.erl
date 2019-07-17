%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_phone_numbers).

-export([new/0
        ,type/0
        ]).

%% Private field accessors
-export([pvt_assigned_to/1, pvt_assigned_to/2, set_pvt_assigned_to/2, pvt_assigned_to_path/0]).
-export([pvt_authorizing_account/1, pvt_authorizing_account/2, set_pvt_authorizing_account/2, pvt_authorizing_account_path/0]).
-export([pvt_carrier_data/1, pvt_carrier_data/2, set_pvt_carrier_data/2, pvt_carrier_data_path/0]).
-export([pvt_db_name/1, pvt_db_name/2, set_pvt_db_name/2, pvt_db_name_path/0]).
-export([pvt_features/1, pvt_features/2, set_pvt_features/2, pvt_features_path/0]).
-export([pvt_features_allowed/1, pvt_features_allowed/2, set_pvt_features_allowed/2, pvt_features_allowed_path/0]).
-export([pvt_features_denied/1, pvt_features_denied/2, set_pvt_features_denied/2, pvt_features_denied_path/0]).
-export([pvt_module_name/1, pvt_module_name/2, set_pvt_module_name/2, pvt_module_name_path/0]).
-export([pvt_ported_in/1, pvt_ported_in/2, set_pvt_ported_in/2, pvt_ported_in_path/0]).
-export([pvt_previously_assigned_to/1, pvt_previously_assigned_to/2, set_pvt_previously_assigned_to/2, pvt_previously_assigned_to_path/0]).
-export([pvt_region/1, pvt_region/2, set_pvt_region/2, pvt_region_path/0]).
-export([pvt_reserve_history/1, pvt_reserve_history/2, set_pvt_reserve_history/2, pvt_reserve_history_path/0]).
-export([pvt_state/1, pvt_state/2, set_pvt_state/2, pvt_state_path/0]).
-export([pvt_used_by/1, pvt_used_by/2, set_pvt_used_by/2, pvt_used_by_path/0]).

%% Public field accessors
-export([carrier_name/1, carrier_name/2, set_carrier_name/2]).
-export([cnam/1, cnam/2, set_cnam/2]).
-export([cnam_display_name/1, cnam_display_name/2, set_cnam_display_name/2]).
-export([cnam_inbound_lookup/1, cnam_inbound_lookup/2, set_cnam_inbound_lookup/2]).
-export([create_with_state/1, create_with_state/2, set_create_with_state/2]).
-export([e911/1, e911/2, set_e911/2]).
-export([e911_activated_time/1, e911_activated_time/2, set_e911_activated_time/2]).
-export([e911_caller_name/1, e911_caller_name/2, set_e911_caller_name/2]).
-export([e911_extended_address/1, e911_extended_address/2, set_e911_extended_address/2]).
-export([e911_latitude/1, e911_latitude/2, set_e911_latitude/2]).
-export([e911_legacy_data/1, e911_legacy_data/2, set_e911_legacy_data/2]).
-export([e911_legacy_data_house_number/1, e911_legacy_data_house_number/2, set_e911_legacy_data_house_number/2]).
-export([e911_legacy_data_predirectional/1, e911_legacy_data_predirectional/2, set_e911_legacy_data_predirectional/2]).
-export([e911_legacy_data_streetname/1, e911_legacy_data_streetname/2, set_e911_legacy_data_streetname/2]).
-export([e911_legacy_data_suite/1, e911_legacy_data_suite/2, set_e911_legacy_data_suite/2]).
-export([e911_locality/1, e911_locality/2, set_e911_locality/2]).
-export([e911_location_id/1, e911_location_id/2, set_e911_location_id/2]).
-export([e911_longitude/1, e911_longitude/2, set_e911_longitude/2]).
-export([e911_plus_four/1, e911_plus_four/2, set_e911_plus_four/2]).
-export([e911_postal_code/1, e911_postal_code/2, set_e911_postal_code/2]).
-export([e911_region/1, e911_region/2, set_e911_region/2]).
-export([e911_status/1, e911_status/2, set_e911_status/2]).
-export([e911_street_address/1, e911_street_address/2, set_e911_street_address/2]).
-export([porting/1, porting/2, set_porting/2]).
-export([porting_billing_account_id/1, porting_billing_account_id/2, set_porting_billing_account_id/2]).
-export([porting_billing_extended_address/1, porting_billing_extended_address/2, set_porting_billing_extended_address/2]).
-export([porting_billing_locality/1, porting_billing_locality/2, set_porting_billing_locality/2]).
-export([porting_billing_name/1, porting_billing_name/2, set_porting_billing_name/2]).
-export([porting_billing_postal_code/1, porting_billing_postal_code/2, set_porting_billing_postal_code/2]).
-export([porting_billing_region/1, porting_billing_region/2, set_porting_billing_region/2]).
-export([porting_billing_street_address/1, porting_billing_street_address/2, set_porting_billing_street_address/2]).
-export([porting_billing_telephone_number/1, porting_billing_telephone_number/2, set_porting_billing_telephone_number/2]).
-export([porting_comments/1, porting_comments/2, set_porting_comments/2]).
-export([porting_customer_contact/1, porting_customer_contact/2, set_porting_customer_contact/2]).
-export([porting_port_id/1, porting_port_id/2, set_porting_port_id/2]).
-export([porting_requested_port_date/1, porting_requested_port_date/2, set_porting_requested_port_date/2]).
-export([porting_service_provider/1, porting_service_provider/2, set_porting_service_provider/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"phone_numbers">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec type() -> kz_term:ne_binary().
type() -> <<"number">>.


%%%=============================================================================
%%% Private field accessor functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_assigned_to(doc()) -> kz_term:api_ne_binary().
pvt_assigned_to(Doc) ->
    pvt_assigned_to(Doc, 'undefined').

-spec pvt_assigned_to(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_assigned_to(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_assigned_to_path(), Doc, Default).

-spec set_pvt_assigned_to(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_assigned_to(Doc, Value) ->
    kz_json:set_value(pvt_assigned_to_path(), Value, Doc).

-spec pvt_assigned_to_path() -> kz_json:path().
pvt_assigned_to_path() -> [<<"pvt_assigned_to">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_authorizing_account(doc()) -> kz_term:api_ne_binary().
pvt_authorizing_account(Doc) ->
    pvt_authorizing_account(Doc, 'undefined').

-spec pvt_authorizing_account(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_authorizing_account(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_authorizing_account_path(), Doc, Default).

-spec set_pvt_authorizing_account(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_authorizing_account(Doc, Value) ->
    kz_json:set_value(pvt_authorizing_account_path(), Value, Doc).

-spec pvt_authorizing_account_path() -> kz_json:path().
pvt_authorizing_account_path() -> [<<"pvt_authorizing_account">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_carrier_data(doc()) -> kz_term:api_object().
pvt_carrier_data(Doc) ->
    pvt_carrier_data(Doc, 'undefined').

-spec pvt_carrier_data(doc(), Default) -> kz_json:object() | Default.
pvt_carrier_data(Doc, Default) ->
    kz_json:get_ne_json_value(pvt_carrier_data_path(), Doc, Default).

-spec set_pvt_carrier_data(doc(), kz_term:api_object()) -> doc().
set_pvt_carrier_data(Doc, Value) ->
    kz_json:set_value(pvt_carrier_data_path(), Value, Doc).

-spec pvt_carrier_data_path() -> kz_json:path().
pvt_carrier_data_path() -> [<<"pvt_carrier_data">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_db_name(doc()) -> kz_term:api_ne_binary().
pvt_db_name(Doc) ->
    pvt_db_name(Doc, 'undefined').

-spec pvt_db_name(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_db_name(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_db_name_path(), Doc, Default).

-spec set_pvt_db_name(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_db_name(Doc, Value) ->
    kz_json:set_value(pvt_db_name_path(), Value, Doc).

-spec pvt_db_name_path() -> kz_json:path().
pvt_db_name_path() -> [<<"pvt_db_name">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_features(doc()) -> kz_term:api_ne_binaries() | kz_json:object().
pvt_features(Doc) ->
    pvt_features(Doc, 'undefined').

-spec pvt_features(doc(), Default) -> kz_term:ne_binaries() | kz_json:object() | Default.
pvt_features(Doc, Default) ->
    kz_json:get_ne_value(pvt_features_path(), Doc, Default).

-spec set_pvt_features(doc(), kz_term:api_ne_binaries() | kz_json:object()) -> doc().
set_pvt_features(Doc, Value) ->
    kz_json:set_value(pvt_features_path(), Value, Doc).

-spec pvt_features_path() -> kz_json:path().
pvt_features_path() -> [<<"pvt_features">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_features_allowed(doc()) -> kz_term:api_ne_binaries().
pvt_features_allowed(Doc) ->
    pvt_features_allowed(Doc, 'undefined').

-spec pvt_features_allowed(doc(), Default) -> kz_term:ne_binaries() | Default.
pvt_features_allowed(Doc, Default) ->
    kz_json:get_ne_value(pvt_features_allowed_path(), Doc, Default).

-spec set_pvt_features_allowed(doc(), kz_term:api_ne_binaries()) -> doc().
set_pvt_features_allowed(Doc, Value) ->
    kz_json:set_value(pvt_features_allowed_path(), Value, Doc).

-spec pvt_features_allowed_path() -> kz_json:path().
pvt_features_allowed_path() -> [<<"pvt_features_allowed">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_features_denied(doc()) -> kz_term:api_ne_binaries().
pvt_features_denied(Doc) ->
    pvt_features_denied(Doc, 'undefined').

-spec pvt_features_denied(doc(), Default) -> kz_term:ne_binaries() | Default.
pvt_features_denied(Doc, Default) ->
    kz_json:get_ne_value(pvt_features_denied_path(), Doc, Default).

-spec set_pvt_features_denied(doc(), kz_term:api_ne_binaries()) -> doc().
set_pvt_features_denied(Doc, Value) ->
    kz_json:set_value(pvt_features_denied_path(), Value, Doc).

-spec pvt_features_denied_path() -> kz_json:path().
pvt_features_denied_path() -> [<<"pvt_features_denied">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_module_name(doc()) -> kz_term:api_ne_binary().
pvt_module_name(Doc) ->
    pvt_module_name(Doc, 'undefined').

-spec pvt_module_name(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_module_name(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_module_name_path(), Doc, Default).

-spec set_pvt_module_name(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_module_name(Doc, Value) ->
    kz_json:set_value(pvt_module_name_path(), Value, Doc).

-spec pvt_module_name_path() -> kz_json:path().
pvt_module_name_path() -> [<<"pvt_module_name">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_ported_in(doc()) -> kz_term:api_boolean().
pvt_ported_in(Doc) ->
    pvt_ported_in(Doc, 'undefined').

-spec pvt_ported_in(doc(), Default) -> boolean() | Default.
pvt_ported_in(Doc, Default) ->
    kz_json:is_true(pvt_ported_in_path(), Doc, Default).

-spec set_pvt_ported_in(doc(), kz_term:api_boolean()) -> doc().
set_pvt_ported_in(Doc, Value) ->
    kz_json:set_value(pvt_ported_in_path(), Value, Doc).

-spec pvt_ported_in_path() -> kz_json:path().
pvt_ported_in_path() -> [<<"pvt_ported_in">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_previously_assigned_to(doc()) -> kz_term:api_ne_binary().
pvt_previously_assigned_to(Doc) ->
    pvt_previously_assigned_to(Doc, 'undefined').

-spec pvt_previously_assigned_to(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_previously_assigned_to(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_previously_assigned_to_path(), Doc, Default).

-spec set_pvt_previously_assigned_to(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_previously_assigned_to(Doc, Value) ->
    kz_json:set_value(pvt_previously_assigned_to_path(), Value, Doc).

-spec pvt_previously_assigned_to_path() -> kz_json:path().
pvt_previously_assigned_to_path() -> [<<"pvt_previously_assigned_to">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_region(doc()) -> kz_term:api_ne_binary().
pvt_region(Doc) ->
    pvt_region(Doc, 'undefined').

-spec pvt_region(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_region(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_region_path(), Doc, Default).

-spec set_pvt_region(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_region(Doc, Value) ->
    kz_json:set_value(pvt_region_path(), Value, Doc).

-spec pvt_region_path() -> kz_json:path().
pvt_region_path() -> [<<"pvt_region">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_reserve_history(doc()) -> kz_term:api_ne_binaries().
pvt_reserve_history(Doc) ->
    pvt_reserve_history(Doc, 'undefined').

-spec pvt_reserve_history(doc(), Default) -> kz_term:ne_binaries() | Default.
pvt_reserve_history(Doc, Default) ->
    kz_json:get_ne_value(pvt_reserve_history_path(), Doc, Default).

-spec set_pvt_reserve_history(doc(), kz_term:api_ne_binaries()) -> doc().
set_pvt_reserve_history(Doc, Value) ->
    kz_json:set_value(pvt_reserve_history_path(), Value, Doc).

-spec pvt_reserve_history_path() -> kz_json:path().
pvt_reserve_history_path() -> [<<"pvt_reserve_history">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_state(doc()) -> kz_term:api_ne_binary().
pvt_state(Doc) ->
    pvt_state(Doc, 'undefined').

-spec pvt_state(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_state(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_state_path(), Doc, Default).

-spec set_pvt_state(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_state(Doc, Value) ->
    kz_json:set_value(pvt_state_path(), Value, Doc).

-spec pvt_state_path() -> kz_json:path().
pvt_state_path() -> [<<"pvt_state">>].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pvt_used_by(doc()) -> kz_term:api_ne_binary().
pvt_used_by(Doc) ->
    pvt_used_by(Doc, 'undefined').

-spec pvt_used_by(doc(), Default) -> kz_term:ne_binary() | Default.
pvt_used_by(Doc, Default) ->
    kz_json:get_ne_binary_value(pvt_used_by_path(), Doc, Default).

-spec set_pvt_used_by(doc(), kz_term:api_ne_binary()) -> doc().
set_pvt_used_by(Doc, Value) ->
    kz_json:set_value(pvt_used_by_path(), Value, Doc).

-spec pvt_used_by_path() -> kz_json:path().
pvt_used_by_path() -> [<<"pvt_used_by">>].


%%%=============================================================================
%%% Public field accessor
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec carrier_name(doc()) -> kz_term:api_ne_binary().
carrier_name(Doc) ->
    carrier_name(Doc, 'undefined').

-spec carrier_name(doc(), Default) -> kz_term:ne_binary() | Default.
carrier_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"carrier_name">>], Doc, Default).

-spec set_carrier_name(doc(), kz_term:ne_binary()) -> doc().
set_carrier_name(Doc, CarrierName) ->
    kz_json:set_value([<<"carrier_name">>], CarrierName, Doc).

-spec cnam(doc()) -> kz_term:api_object().
cnam(Doc) ->
    cnam(Doc, 'undefined').

-spec cnam(doc(), Default) -> kz_json:object() | Default.
cnam(Doc, Default) ->
    kz_json:get_json_value([<<"cnam">>], Doc, Default).

-spec set_cnam(doc(), kz_json:object()) -> doc().
set_cnam(Doc, Cnam) ->
    kz_json:set_value([<<"cnam">>], Cnam, Doc).

-spec cnam_display_name(doc()) -> kz_term:api_ne_binary().
cnam_display_name(Doc) ->
    cnam_display_name(Doc, 'undefined').

-spec cnam_display_name(doc(), Default) -> kz_term:ne_binary() | Default.
cnam_display_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"cnam">>, <<"display_name">>], Doc, Default).

-spec set_cnam_display_name(doc(), kz_term:ne_binary()) -> doc().
set_cnam_display_name(Doc, CnamDisplayName) ->
    kz_json:set_value([<<"cnam">>, <<"display_name">>], CnamDisplayName, Doc).

-spec cnam_inbound_lookup(doc()) -> kz_term:api_boolean().
cnam_inbound_lookup(Doc) ->
    cnam_inbound_lookup(Doc, 'undefined').

-spec cnam_inbound_lookup(doc(), Default) -> boolean() | Default.
cnam_inbound_lookup(Doc, Default) ->
    kz_json:get_boolean_value([<<"cnam">>, <<"inbound_lookup">>], Doc, Default).

-spec set_cnam_inbound_lookup(doc(), boolean()) -> doc().
set_cnam_inbound_lookup(Doc, CnamInboundLookup) ->
    kz_json:set_value([<<"cnam">>, <<"inbound_lookup">>], CnamInboundLookup, Doc).

-spec create_with_state(doc()) -> kz_term:api_binary().
create_with_state(Doc) ->
    create_with_state(Doc, 'undefined').

-spec create_with_state(doc(), Default) -> binary() | Default.
create_with_state(Doc, Default) ->
    kz_json:get_binary_value([<<"create_with_state">>], Doc, Default).

-spec set_create_with_state(doc(), binary()) -> doc().
set_create_with_state(Doc, CreateWithState) ->
    kz_json:set_value([<<"create_with_state">>], CreateWithState, Doc).

-spec e911(doc()) -> kz_term:api_object().
e911(Doc) ->
    e911(Doc, 'undefined').

-spec e911(doc(), Default) -> kz_json:object() | Default.
e911(Doc, Default) ->
    kz_json:get_json_value([<<"e911">>], Doc, Default).

-spec set_e911(doc(), kz_json:object()) -> doc().
set_e911(Doc, E911) ->
    kz_json:set_value([<<"e911">>], E911, Doc).

-spec e911_activated_time(doc()) -> kz_term:api_binary().
e911_activated_time(Doc) ->
    e911_activated_time(Doc, 'undefined').

-spec e911_activated_time(doc(), Default) -> binary() | Default.
e911_activated_time(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"activated_time">>], Doc, Default).

-spec set_e911_activated_time(doc(), binary()) -> doc().
set_e911_activated_time(Doc, E911ActivatedTime) ->
    kz_json:set_value([<<"e911">>, <<"activated_time">>], E911ActivatedTime, Doc).

-spec e911_caller_name(doc()) -> kz_term:api_ne_binary().
e911_caller_name(Doc) ->
    e911_caller_name(Doc, 'undefined').

-spec e911_caller_name(doc(), Default) -> kz_term:ne_binary() | Default.
e911_caller_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"e911">>, <<"caller_name">>], Doc, Default).

-spec set_e911_caller_name(doc(), kz_term:ne_binary()) -> doc().
set_e911_caller_name(Doc, E911CallerName) ->
    kz_json:set_value([<<"e911">>, <<"caller_name">>], E911CallerName, Doc).

-spec e911_extended_address(doc()) -> kz_term:api_binary().
e911_extended_address(Doc) ->
    e911_extended_address(Doc, 'undefined').

-spec e911_extended_address(doc(), Default) -> binary() | Default.
e911_extended_address(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"extended_address">>], Doc, Default).

-spec set_e911_extended_address(doc(), binary()) -> doc().
set_e911_extended_address(Doc, E911ExtendedAddress) ->
    kz_json:set_value([<<"e911">>, <<"extended_address">>], E911ExtendedAddress, Doc).

-spec e911_latitude(doc()) -> kz_term:api_binary().
e911_latitude(Doc) ->
    e911_latitude(Doc, 'undefined').

-spec e911_latitude(doc(), Default) -> binary() | Default.
e911_latitude(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"latitude">>], Doc, Default).

-spec set_e911_latitude(doc(), binary()) -> doc().
set_e911_latitude(Doc, E911Latitude) ->
    kz_json:set_value([<<"e911">>, <<"latitude">>], E911Latitude, Doc).

-spec e911_legacy_data(doc()) -> kz_term:api_object().
e911_legacy_data(Doc) ->
    e911_legacy_data(Doc, 'undefined').

-spec e911_legacy_data(doc(), Default) -> kz_json:object() | Default.
e911_legacy_data(Doc, Default) ->
    kz_json:get_json_value([<<"e911">>, <<"legacy_data">>], Doc, Default).

-spec set_e911_legacy_data(doc(), kz_json:object()) -> doc().
set_e911_legacy_data(Doc, E911LegacyData) ->
    kz_json:set_value([<<"e911">>, <<"legacy_data">>], E911LegacyData, Doc).

-spec e911_legacy_data_house_number(doc()) -> kz_term:api_binary().
e911_legacy_data_house_number(Doc) ->
    e911_legacy_data_house_number(Doc, 'undefined').

-spec e911_legacy_data_house_number(doc(), Default) -> binary() | Default.
e911_legacy_data_house_number(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"legacy_data">>, <<"house_number">>], Doc, Default).

-spec set_e911_legacy_data_house_number(doc(), binary()) -> doc().
set_e911_legacy_data_house_number(Doc, E911LegacyDataHouseNumber) ->
    kz_json:set_value([<<"e911">>, <<"legacy_data">>, <<"house_number">>], E911LegacyDataHouseNumber, Doc).

-spec e911_legacy_data_predirectional(doc()) -> kz_term:api_binary().
e911_legacy_data_predirectional(Doc) ->
    e911_legacy_data_predirectional(Doc, 'undefined').

-spec e911_legacy_data_predirectional(doc(), Default) -> binary() | Default.
e911_legacy_data_predirectional(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"legacy_data">>, <<"predirectional">>], Doc, Default).

-spec set_e911_legacy_data_predirectional(doc(), binary()) -> doc().
set_e911_legacy_data_predirectional(Doc, E911LegacyDataPredirectional) ->
    kz_json:set_value([<<"e911">>, <<"legacy_data">>, <<"predirectional">>], E911LegacyDataPredirectional, Doc).

-spec e911_legacy_data_streetname(doc()) -> kz_term:api_binary().
e911_legacy_data_streetname(Doc) ->
    e911_legacy_data_streetname(Doc, 'undefined').

-spec e911_legacy_data_streetname(doc(), Default) -> binary() | Default.
e911_legacy_data_streetname(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"legacy_data">>, <<"streetname">>], Doc, Default).

-spec set_e911_legacy_data_streetname(doc(), binary()) -> doc().
set_e911_legacy_data_streetname(Doc, E911LegacyDataStreetname) ->
    kz_json:set_value([<<"e911">>, <<"legacy_data">>, <<"streetname">>], E911LegacyDataStreetname, Doc).

-spec e911_legacy_data_suite(doc()) -> kz_term:api_binary().
e911_legacy_data_suite(Doc) ->
    e911_legacy_data_suite(Doc, 'undefined').

-spec e911_legacy_data_suite(doc(), Default) -> binary() | Default.
e911_legacy_data_suite(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"legacy_data">>, <<"suite">>], Doc, Default).

-spec set_e911_legacy_data_suite(doc(), binary()) -> doc().
set_e911_legacy_data_suite(Doc, E911LegacyDataSuite) ->
    kz_json:set_value([<<"e911">>, <<"legacy_data">>, <<"suite">>], E911LegacyDataSuite, Doc).

-spec e911_locality(doc()) -> kz_term:api_binary().
e911_locality(Doc) ->
    e911_locality(Doc, 'undefined').

-spec e911_locality(doc(), Default) -> binary() | Default.
e911_locality(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"locality">>], Doc, Default).

-spec set_e911_locality(doc(), binary()) -> doc().
set_e911_locality(Doc, E911Locality) ->
    kz_json:set_value([<<"e911">>, <<"locality">>], E911Locality, Doc).

-spec e911_location_id(doc()) -> kz_term:api_binary().
e911_location_id(Doc) ->
    e911_location_id(Doc, 'undefined').

-spec e911_location_id(doc(), Default) -> binary() | Default.
e911_location_id(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"location_id">>], Doc, Default).

-spec set_e911_location_id(doc(), binary()) -> doc().
set_e911_location_id(Doc, E911LocationId) ->
    kz_json:set_value([<<"e911">>, <<"location_id">>], E911LocationId, Doc).

-spec e911_longitude(doc()) -> kz_term:api_binary().
e911_longitude(Doc) ->
    e911_longitude(Doc, 'undefined').

-spec e911_longitude(doc(), Default) -> binary() | Default.
e911_longitude(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"longitude">>], Doc, Default).

-spec set_e911_longitude(doc(), binary()) -> doc().
set_e911_longitude(Doc, E911Longitude) ->
    kz_json:set_value([<<"e911">>, <<"longitude">>], E911Longitude, Doc).

-spec e911_plus_four(doc()) -> kz_term:api_binary().
e911_plus_four(Doc) ->
    e911_plus_four(Doc, 'undefined').

-spec e911_plus_four(doc(), Default) -> binary() | Default.
e911_plus_four(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"plus_four">>], Doc, Default).

-spec set_e911_plus_four(doc(), binary()) -> doc().
set_e911_plus_four(Doc, E911PlusFour) ->
    kz_json:set_value([<<"e911">>, <<"plus_four">>], E911PlusFour, Doc).

-spec e911_postal_code(doc()) -> kz_term:api_binary().
e911_postal_code(Doc) ->
    e911_postal_code(Doc, 'undefined').

-spec e911_postal_code(doc(), Default) -> binary() | Default.
e911_postal_code(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"postal_code">>], Doc, Default).

-spec set_e911_postal_code(doc(), binary()) -> doc().
set_e911_postal_code(Doc, E911PostalCode) ->
    kz_json:set_value([<<"e911">>, <<"postal_code">>], E911PostalCode, Doc).

-spec e911_region(doc()) -> kz_term:api_ne_binary().
e911_region(Doc) ->
    e911_region(Doc, 'undefined').

-spec e911_region(doc(), Default) -> kz_term:ne_binary() | Default.
e911_region(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"e911">>, <<"region">>], Doc, Default).

-spec set_e911_region(doc(), kz_term:ne_binary()) -> doc().
set_e911_region(Doc, E911Region) ->
    kz_json:set_value([<<"e911">>, <<"region">>], E911Region, Doc).

-spec e911_status(doc()) -> kz_term:api_binary().
e911_status(Doc) ->
    e911_status(Doc, 'undefined').

-spec e911_status(doc(), Default) -> binary() | Default.
e911_status(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"status">>], Doc, Default).

-spec set_e911_status(doc(), binary()) -> doc().
set_e911_status(Doc, E911Status) ->
    kz_json:set_value([<<"e911">>, <<"status">>], E911Status, Doc).

-spec e911_street_address(doc()) -> kz_term:api_binary().
e911_street_address(Doc) ->
    e911_street_address(Doc, 'undefined').

-spec e911_street_address(doc(), Default) -> binary() | Default.
e911_street_address(Doc, Default) ->
    kz_json:get_binary_value([<<"e911">>, <<"street_address">>], Doc, Default).

-spec set_e911_street_address(doc(), binary()) -> doc().
set_e911_street_address(Doc, E911StreetAddress) ->
    kz_json:set_value([<<"e911">>, <<"street_address">>], E911StreetAddress, Doc).

-spec porting(doc()) -> kz_term:api_object().
porting(Doc) ->
    porting(Doc, 'undefined').

-spec porting(doc(), Default) -> kz_json:object() | Default.
porting(Doc, Default) ->
    kz_json:get_json_value([<<"porting">>], Doc, Default).

-spec set_porting(doc(), kz_json:object()) -> doc().
set_porting(Doc, Porting) ->
    kz_json:set_value([<<"porting">>], Porting, Doc).

-spec porting_billing_account_id(doc()) -> kz_term:api_binary().
porting_billing_account_id(Doc) ->
    porting_billing_account_id(Doc, 'undefined').

-spec porting_billing_account_id(doc(), Default) -> binary() | Default.
porting_billing_account_id(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_account_id">>], Doc, Default).

-spec set_porting_billing_account_id(doc(), binary()) -> doc().
set_porting_billing_account_id(Doc, PortingBillingAccountId) ->
    kz_json:set_value([<<"porting">>, <<"billing_account_id">>], PortingBillingAccountId, Doc).

-spec porting_billing_extended_address(doc()) -> kz_term:api_binary().
porting_billing_extended_address(Doc) ->
    porting_billing_extended_address(Doc, 'undefined').

-spec porting_billing_extended_address(doc(), Default) -> binary() | Default.
porting_billing_extended_address(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_extended_address">>], Doc, Default).

-spec set_porting_billing_extended_address(doc(), binary()) -> doc().
set_porting_billing_extended_address(Doc, PortingBillingExtendedAddress) ->
    kz_json:set_value([<<"porting">>, <<"billing_extended_address">>], PortingBillingExtendedAddress, Doc).

-spec porting_billing_locality(doc()) -> kz_term:api_binary().
porting_billing_locality(Doc) ->
    porting_billing_locality(Doc, 'undefined').

-spec porting_billing_locality(doc(), Default) -> binary() | Default.
porting_billing_locality(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_locality">>], Doc, Default).

-spec set_porting_billing_locality(doc(), binary()) -> doc().
set_porting_billing_locality(Doc, PortingBillingLocality) ->
    kz_json:set_value([<<"porting">>, <<"billing_locality">>], PortingBillingLocality, Doc).

-spec porting_billing_name(doc()) -> kz_term:api_binary().
porting_billing_name(Doc) ->
    porting_billing_name(Doc, 'undefined').

-spec porting_billing_name(doc(), Default) -> binary() | Default.
porting_billing_name(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_name">>], Doc, Default).

-spec set_porting_billing_name(doc(), binary()) -> doc().
set_porting_billing_name(Doc, PortingBillingName) ->
    kz_json:set_value([<<"porting">>, <<"billing_name">>], PortingBillingName, Doc).

-spec porting_billing_postal_code(doc()) -> kz_term:api_binary().
porting_billing_postal_code(Doc) ->
    porting_billing_postal_code(Doc, 'undefined').

-spec porting_billing_postal_code(doc(), Default) -> binary() | Default.
porting_billing_postal_code(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_postal_code">>], Doc, Default).

-spec set_porting_billing_postal_code(doc(), binary()) -> doc().
set_porting_billing_postal_code(Doc, PortingBillingPostalCode) ->
    kz_json:set_value([<<"porting">>, <<"billing_postal_code">>], PortingBillingPostalCode, Doc).

-spec porting_billing_region(doc()) -> kz_term:api_binary().
porting_billing_region(Doc) ->
    porting_billing_region(Doc, 'undefined').

-spec porting_billing_region(doc(), Default) -> binary() | Default.
porting_billing_region(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_region">>], Doc, Default).

-spec set_porting_billing_region(doc(), binary()) -> doc().
set_porting_billing_region(Doc, PortingBillingRegion) ->
    kz_json:set_value([<<"porting">>, <<"billing_region">>], PortingBillingRegion, Doc).

-spec porting_billing_street_address(doc()) -> kz_term:api_binary().
porting_billing_street_address(Doc) ->
    porting_billing_street_address(Doc, 'undefined').

-spec porting_billing_street_address(doc(), Default) -> binary() | Default.
porting_billing_street_address(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_street_address">>], Doc, Default).

-spec set_porting_billing_street_address(doc(), binary()) -> doc().
set_porting_billing_street_address(Doc, PortingBillingStreetAddress) ->
    kz_json:set_value([<<"porting">>, <<"billing_street_address">>], PortingBillingStreetAddress, Doc).

-spec porting_billing_telephone_number(doc()) -> kz_term:api_binary().
porting_billing_telephone_number(Doc) ->
    porting_billing_telephone_number(Doc, 'undefined').

-spec porting_billing_telephone_number(doc(), Default) -> binary() | Default.
porting_billing_telephone_number(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"billing_telephone_number">>], Doc, Default).

-spec set_porting_billing_telephone_number(doc(), binary()) -> doc().
set_porting_billing_telephone_number(Doc, PortingBillingTelephoneNumber) ->
    kz_json:set_value([<<"porting">>, <<"billing_telephone_number">>], PortingBillingTelephoneNumber, Doc).

-spec porting_comments(doc()) -> kz_term:api_ne_binaries().
porting_comments(Doc) ->
    porting_comments(Doc, 'undefined').

-spec porting_comments(doc(), Default) -> kz_term:ne_binaries() | Default.
porting_comments(Doc, Default) ->
    kz_json:get_list_value([<<"porting">>, <<"comments">>], Doc, Default).

-spec set_porting_comments(doc(), kz_term:ne_binaries()) -> doc().
set_porting_comments(Doc, PortingComments) ->
    kz_json:set_value([<<"porting">>, <<"comments">>], PortingComments, Doc).

-spec porting_customer_contact(doc()) -> kz_term:api_binary().
porting_customer_contact(Doc) ->
    porting_customer_contact(Doc, 'undefined').

-spec porting_customer_contact(doc(), Default) -> binary() | Default.
porting_customer_contact(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"customer_contact">>], Doc, Default).

-spec set_porting_customer_contact(doc(), binary()) -> doc().
set_porting_customer_contact(Doc, PortingCustomerContact) ->
    kz_json:set_value([<<"porting">>, <<"customer_contact">>], PortingCustomerContact, Doc).

-spec porting_port_id(doc()) -> kz_term:api_binary().
porting_port_id(Doc) ->
    porting_port_id(Doc, 'undefined').

-spec porting_port_id(doc(), Default) -> binary() | Default.
porting_port_id(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"port_id">>], Doc, Default).

-spec set_porting_port_id(doc(), binary()) -> doc().
set_porting_port_id(Doc, PortingPortId) ->
    kz_json:set_value([<<"porting">>, <<"port_id">>], PortingPortId, Doc).

-spec porting_requested_port_date(doc()) -> kz_term:api_binary().
porting_requested_port_date(Doc) ->
    porting_requested_port_date(Doc, 'undefined').

-spec porting_requested_port_date(doc(), Default) -> binary() | Default.
porting_requested_port_date(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"requested_port_date">>], Doc, Default).

-spec set_porting_requested_port_date(doc(), binary()) -> doc().
set_porting_requested_port_date(Doc, PortingRequestedPortDate) ->
    kz_json:set_value([<<"porting">>, <<"requested_port_date">>], PortingRequestedPortDate, Doc).

-spec porting_service_provider(doc()) -> kz_term:api_binary().
porting_service_provider(Doc) ->
    porting_service_provider(Doc, 'undefined').

-spec porting_service_provider(doc(), Default) -> binary() | Default.
porting_service_provider(Doc, Default) ->
    kz_json:get_binary_value([<<"porting">>, <<"service_provider">>], Doc, Default).

-spec set_porting_service_provider(doc(), binary()) -> doc().
set_porting_service_provider(Doc, PortingServiceProvider) ->
    kz_json:set_value([<<"porting">>, <<"service_provider">>], PortingServiceProvider, Doc).
