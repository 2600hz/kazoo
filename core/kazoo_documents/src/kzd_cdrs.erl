%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_cdrs).

-export([new/0]).
-export([app_name/1, app_name/2, set_app_name/2]).
-export([app_version/1, app_version/2, set_app_version/2]).
-export([billing_seconds/1, billing_seconds/2, set_billing_seconds/2]).
-export([call_direction/1, call_direction/2, set_call_direction/2]).
-export([call_id/1, call_id/2, set_call_id/2]).
-export([callee_id_name/1, callee_id_name/2, set_callee_id_name/2]).
-export([callee_id_number/1, callee_id_number/2, set_callee_id_number/2]).
-export([caller_id_name/1, caller_id_name/2, set_caller_id_name/2]).
-export([caller_id_number/1, caller_id_number/2, set_caller_id_number/2]).
-export([custom_application_vars/1, custom_application_vars/2, set_custom_application_vars/2]).
-export([custom_channel_vars/1, custom_channel_vars/2, set_custom_channel_vars/2]).
-export([custom_sip_headers/1, custom_sip_headers/2, set_custom_sip_headers/2]).
-export([digits_dialed/1, digits_dialed/2, set_digits_dialed/2]).
-export([disposition/1, disposition/2, set_disposition/2]).
-export([duration_seconds/1, duration_seconds/2, set_duration_seconds/2]).
-export([fax_bad_rows/1, fax_bad_rows/2, set_fax_bad_rows/2]).
-export([fax_ecm_used/1, fax_ecm_used/2, set_fax_ecm_used/2]).
-export([fax_result_code/1, fax_result_code/2, set_fax_result_code/2]).
-export([fax_result_text/1, fax_result_text/2, set_fax_result_text/2]).
-export([fax_success/1, fax_success/2, set_fax_success/2]).
-export([fax_total_pages/1, fax_total_pages/2, set_fax_total_pages/2]).
-export([fax_transfer_rate/1, fax_transfer_rate/2, set_fax_transfer_rate/2]).
-export([fax_transferred_pages/1, fax_transferred_pages/2, set_fax_transferred_pages/2]).
-export([from/1, from/2, set_from/2]).
-export([from_tag/1, from_tag/2, set_from_tag/2]).
-export([from_uri/1, from_uri/2, set_from_uri/2]).
-export([hangup_cause/1, hangup_cause/2, set_hangup_cause/2]).
-export([hangup_code/1, hangup_code/2, set_hangup_code/2]).
-export([interaction_id/1, interaction_id/2, set_interaction_id/2]).
-export([local_sdp/1, local_sdp/2, set_local_sdp/2]).
-export([media_server/1, media_server/2, set_media_server/2]).
-export([node/1, node/2, set_node/2]).
-export([other_leg_call_id/1, other_leg_call_id/2, set_other_leg_call_id/2]).
-export([other_leg_caller_id_name/1, other_leg_caller_id_name/2, set_other_leg_caller_id_name/2]).
-export([other_leg_caller_id_number/1, other_leg_caller_id_number/2, set_other_leg_caller_id_number/2]).
-export([other_leg_destination_number/1, other_leg_destination_number/2, set_other_leg_destination_number/2]).
-export([other_leg_direction/1, other_leg_direction/2, set_other_leg_direction/2]).
-export([presence_id/1, presence_id/2, set_presence_id/2]).
-export([remote_sdp/1, remote_sdp/2, set_remote_sdp/2]).
-export([request/1, request/2, set_request/2]).
-export([ringing_seconds/1, ringing_seconds/2, set_ringing_seconds/2]).
-export([timestamp/1, timestamp/2, set_timestamp/2]).
-export([to/1, to/2, set_to/2]).
-export([to_tag/1, to_tag/2, set_to_tag/2]).
-export([to_uri/1, to_uri/2, set_to_uri/2]).
-export([user_agent/1, user_agent/2, set_user_agent/2]).

-export([create_doc_id/1, create_doc_id/2, create_doc_id/3
        ,type/0
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"cdrs">>).
-define(PVT_TYPE, <<"cdr">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec app_name(doc()) -> kz_term:api_binary().
app_name(Doc) ->
    app_name(Doc, 'undefined').

-spec app_name(doc(), Default) -> binary() | Default.
app_name(Doc, Default) ->
    kz_json:get_binary_value([<<"app_name">>], Doc, Default).

-spec set_app_name(doc(), binary()) -> doc().
set_app_name(Doc, AppName) ->
    kz_json:set_value([<<"app_name">>], AppName, Doc).

-spec app_version(doc()) -> kz_term:api_binary().
app_version(Doc) ->
    app_version(Doc, 'undefined').

-spec app_version(doc(), Default) -> binary() | Default.
app_version(Doc, Default) ->
    kz_json:get_binary_value([<<"app_version">>], Doc, Default).

-spec set_app_version(doc(), binary()) -> doc().
set_app_version(Doc, AppVersion) ->
    kz_json:set_value([<<"app_version">>], AppVersion, Doc).

-spec billing_seconds(doc()) -> kz_term:api_integer().
billing_seconds(Doc) ->
    billing_seconds(Doc, 'undefined').

-spec billing_seconds(doc(), Default) -> integer() | Default.
billing_seconds(Doc, Default) ->
    kz_json:get_integer_value([<<"billing_seconds">>], Doc, Default).

-spec set_billing_seconds(doc(), integer()) -> doc().
set_billing_seconds(Doc, BillingSeconds) ->
    kz_json:set_value([<<"billing_seconds">>], BillingSeconds, Doc).

-spec call_direction(doc()) -> kz_term:api_binary().
call_direction(Doc) ->
    call_direction(Doc, 'undefined').

-spec call_direction(doc(), Default) -> binary() | Default.
call_direction(Doc, Default) ->
    kz_json:get_binary_value([<<"call_direction">>], Doc, Default).

-spec set_call_direction(doc(), binary()) -> doc().
set_call_direction(Doc, CallDirection) ->
    kz_json:set_value([<<"call_direction">>], CallDirection, Doc).

-spec call_id(doc()) -> kz_term:api_binary().
call_id(Doc) ->
    call_id(Doc, 'undefined').

-spec call_id(doc(), Default) -> binary() | Default.
call_id(Doc, Default) ->
    kz_json:get_binary_value([<<"call_id">>], Doc, Default).

-spec set_call_id(doc(), binary()) -> doc().
set_call_id(Doc, CallId) ->
    kz_json:set_value([<<"call_id">>], CallId, Doc).

-spec callee_id_name(doc()) -> kz_term:api_binary().
callee_id_name(Doc) ->
    callee_id_name(Doc, 'undefined').

-spec callee_id_name(doc(), Default) -> binary() | Default.
callee_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"callee_id_name">>], Doc, Default).

-spec set_callee_id_name(doc(), binary()) -> doc().
set_callee_id_name(Doc, CalleeIdName) ->
    kz_json:set_value([<<"callee_id_name">>], CalleeIdName, Doc).

-spec callee_id_number(doc()) -> kz_term:api_binary().
callee_id_number(Doc) ->
    callee_id_number(Doc, 'undefined').

-spec callee_id_number(doc(), Default) -> binary() | Default.
callee_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"callee_id_number">>], Doc, Default).

-spec set_callee_id_number(doc(), binary()) -> doc().
set_callee_id_number(Doc, CalleeIdNumber) ->
    kz_json:set_value([<<"callee_id_number">>], CalleeIdNumber, Doc).

-spec caller_id_name(doc()) -> kz_term:api_binary().
caller_id_name(Doc) ->
    caller_id_name(Doc, 'undefined').

-spec caller_id_name(doc(), Default) -> binary() | Default.
caller_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_name">>], Doc, Default).

-spec set_caller_id_name(doc(), binary()) -> doc().
set_caller_id_name(Doc, CallerIdName) ->
    kz_json:set_value([<<"caller_id_name">>], CallerIdName, Doc).

-spec caller_id_number(doc()) -> kz_term:api_binary().
caller_id_number(Doc) ->
    caller_id_number(Doc, 'undefined').

-spec caller_id_number(doc(), Default) -> binary() | Default.
caller_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_id_number">>], Doc, Default).

-spec set_caller_id_number(doc(), binary()) -> doc().
set_caller_id_number(Doc, CallerIdNumber) ->
    kz_json:set_value([<<"caller_id_number">>], CallerIdNumber, Doc).

-spec custom_application_vars(doc()) -> kz_term:api_object().
custom_application_vars(Doc) ->
    custom_application_vars(Doc, 'undefined').

-spec custom_application_vars(doc(), Default) -> kz_json:object() | Default.
custom_application_vars(Doc, Default) ->
    kz_json:get_json_value([<<"custom_application_vars">>], Doc, Default).

-spec set_custom_application_vars(doc(), kz_json:object()) -> doc().
set_custom_application_vars(Doc, CustomApplicationVars) ->
    kz_json:set_value([<<"custom_application_vars">>], CustomApplicationVars, Doc).

-spec custom_channel_vars(doc()) -> kz_term:api_object().
custom_channel_vars(Doc) ->
    custom_channel_vars(Doc, 'undefined').

-spec custom_channel_vars(doc(), Default) -> kz_json:object() | Default.
custom_channel_vars(Doc, Default) ->
    kz_json:get_json_value([<<"custom_channel_vars">>], Doc, Default).

-spec set_custom_channel_vars(doc(), kz_json:object()) -> doc().
set_custom_channel_vars(Doc, CustomChannelVars) ->
    kz_json:set_value([<<"custom_channel_vars">>], CustomChannelVars, Doc).

-spec custom_sip_headers(doc()) -> kz_term:api_object().
custom_sip_headers(Doc) ->
    custom_sip_headers(Doc, 'undefined').

-spec custom_sip_headers(doc(), Default) -> kz_json:object() | Default.
custom_sip_headers(Doc, Default) ->
    kz_json:get_json_value([<<"custom_sip_headers">>], Doc, Default).

-spec set_custom_sip_headers(doc(), kz_json:object()) -> doc().
set_custom_sip_headers(Doc, CustomSipHeaders) ->
    kz_json:set_value([<<"custom_sip_headers">>], CustomSipHeaders, Doc).

-spec digits_dialed(doc()) -> kz_term:api_binary().
digits_dialed(Doc) ->
    digits_dialed(Doc, 'undefined').

-spec digits_dialed(doc(), Default) -> binary() | Default.
digits_dialed(Doc, Default) ->
    kz_json:get_binary_value([<<"digits_dialed">>], Doc, Default).

-spec set_digits_dialed(doc(), binary()) -> doc().
set_digits_dialed(Doc, DigitsDialed) ->
    kz_json:set_value([<<"digits_dialed">>], DigitsDialed, Doc).

-spec disposition(doc()) -> kz_term:api_binary().
disposition(Doc) ->
    disposition(Doc, 'undefined').

-spec disposition(doc(), Default) -> binary() | Default.
disposition(Doc, Default) ->
    kz_json:get_binary_value([<<"disposition">>], Doc, Default).

-spec set_disposition(doc(), binary()) -> doc().
set_disposition(Doc, Disposition) ->
    kz_json:set_value([<<"disposition">>], Disposition, Doc).

-spec duration_seconds(doc()) -> kz_term:api_integer().
duration_seconds(Doc) ->
    duration_seconds(Doc, 'undefined').

-spec duration_seconds(doc(), Default) -> integer() | Default.
duration_seconds(Doc, Default) ->
    kz_json:get_integer_value([<<"duration_seconds">>], Doc, Default).

-spec set_duration_seconds(doc(), integer()) -> doc().
set_duration_seconds(Doc, DurationSeconds) ->
    kz_json:set_value([<<"duration_seconds">>], DurationSeconds, Doc).

-spec fax_bad_rows(doc()) -> kz_term:api_binary().
fax_bad_rows(Doc) ->
    fax_bad_rows(Doc, 'undefined').

-spec fax_bad_rows(doc(), Default) -> binary() | Default.
fax_bad_rows(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_bad_rows">>], Doc, Default).

-spec set_fax_bad_rows(doc(), binary()) -> doc().
set_fax_bad_rows(Doc, FaxBadRows) ->
    kz_json:set_value([<<"fax_bad_rows">>], FaxBadRows, Doc).

-spec fax_ecm_used(doc()) -> kz_term:api_binary().
fax_ecm_used(Doc) ->
    fax_ecm_used(Doc, 'undefined').

-spec fax_ecm_used(doc(), Default) -> binary() | Default.
fax_ecm_used(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_ecm_used">>], Doc, Default).

-spec set_fax_ecm_used(doc(), binary()) -> doc().
set_fax_ecm_used(Doc, FaxEcmUsed) ->
    kz_json:set_value([<<"fax_ecm_used">>], FaxEcmUsed, Doc).

-spec fax_result_code(doc()) -> kz_term:api_binary().
fax_result_code(Doc) ->
    fax_result_code(Doc, 'undefined').

-spec fax_result_code(doc(), Default) -> binary() | Default.
fax_result_code(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_result_code">>], Doc, Default).

-spec set_fax_result_code(doc(), binary()) -> doc().
set_fax_result_code(Doc, FaxResultCode) ->
    kz_json:set_value([<<"fax_result_code">>], FaxResultCode, Doc).

-spec fax_result_text(doc()) -> kz_term:api_binary().
fax_result_text(Doc) ->
    fax_result_text(Doc, 'undefined').

-spec fax_result_text(doc(), Default) -> binary() | Default.
fax_result_text(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_result_text">>], Doc, Default).

-spec set_fax_result_text(doc(), binary()) -> doc().
set_fax_result_text(Doc, FaxResultText) ->
    kz_json:set_value([<<"fax_result_text">>], FaxResultText, Doc).

-spec fax_success(doc()) -> kz_term:api_binary().
fax_success(Doc) ->
    fax_success(Doc, 'undefined').

-spec fax_success(doc(), Default) -> binary() | Default.
fax_success(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_success">>], Doc, Default).

-spec set_fax_success(doc(), binary()) -> doc().
set_fax_success(Doc, FaxSuccess) ->
    kz_json:set_value([<<"fax_success">>], FaxSuccess, Doc).

-spec fax_total_pages(doc()) -> kz_term:api_binary().
fax_total_pages(Doc) ->
    fax_total_pages(Doc, 'undefined').

-spec fax_total_pages(doc(), Default) -> binary() | Default.
fax_total_pages(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_total_pages">>], Doc, Default).

-spec set_fax_total_pages(doc(), binary()) -> doc().
set_fax_total_pages(Doc, FaxTotalPages) ->
    kz_json:set_value([<<"fax_total_pages">>], FaxTotalPages, Doc).

-spec fax_transfer_rate(doc()) -> kz_term:api_binary().
fax_transfer_rate(Doc) ->
    fax_transfer_rate(Doc, 'undefined').

-spec fax_transfer_rate(doc(), Default) -> binary() | Default.
fax_transfer_rate(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_transfer_rate">>], Doc, Default).

-spec set_fax_transfer_rate(doc(), binary()) -> doc().
set_fax_transfer_rate(Doc, FaxTransferRate) ->
    kz_json:set_value([<<"fax_transfer_rate">>], FaxTransferRate, Doc).

-spec fax_transferred_pages(doc()) -> kz_term:api_binary().
fax_transferred_pages(Doc) ->
    fax_transferred_pages(Doc, 'undefined').

-spec fax_transferred_pages(doc(), Default) -> binary() | Default.
fax_transferred_pages(Doc, Default) ->
    kz_json:get_binary_value([<<"fax_transferred_pages">>], Doc, Default).

-spec set_fax_transferred_pages(doc(), binary()) -> doc().
set_fax_transferred_pages(Doc, FaxTransferredPages) ->
    kz_json:set_value([<<"fax_transferred_pages">>], FaxTransferredPages, Doc).

-spec from(doc()) -> kz_term:api_binary().
from(Doc) ->
    from(Doc, 'undefined').

-spec from(doc(), Default) -> binary() | Default.
from(Doc, Default) ->
    kz_json:get_binary_value([<<"from">>], Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value([<<"from">>], From, Doc).

-spec from_tag(doc()) -> kz_term:api_binary().
from_tag(Doc) ->
    from_tag(Doc, 'undefined').

-spec from_tag(doc(), Default) -> binary() | Default.
from_tag(Doc, Default) ->
    kz_json:get_binary_value([<<"from_tag">>], Doc, Default).

-spec set_from_tag(doc(), binary()) -> doc().
set_from_tag(Doc, FromTag) ->
    kz_json:set_value([<<"from_tag">>], FromTag, Doc).

-spec from_uri(doc()) -> kz_term:api_binary().
from_uri(Doc) ->
    from_uri(Doc, 'undefined').

-spec from_uri(doc(), Default) -> binary() | Default.
from_uri(Doc, Default) ->
    kz_json:get_binary_value([<<"from_uri">>], Doc, Default).

-spec set_from_uri(doc(), binary()) -> doc().
set_from_uri(Doc, FromUri) ->
    kz_json:set_value([<<"from_uri">>], FromUri, Doc).

-spec hangup_cause(doc()) -> kz_term:api_binary().
hangup_cause(Doc) ->
    hangup_cause(Doc, 'undefined').

-spec hangup_cause(doc(), Default) -> binary() | Default.
hangup_cause(Doc, Default) ->
    kz_json:get_binary_value([<<"hangup_cause">>], Doc, Default).

-spec set_hangup_cause(doc(), binary()) -> doc().
set_hangup_cause(Doc, HangupCause) ->
    kz_json:set_value([<<"hangup_cause">>], HangupCause, Doc).

-spec hangup_code(doc()) -> kz_term:api_binary().
hangup_code(Doc) ->
    hangup_code(Doc, 'undefined').

-spec hangup_code(doc(), Default) -> binary() | Default.
hangup_code(Doc, Default) ->
    kz_json:get_binary_value([<<"hangup_code">>], Doc, Default).

-spec set_hangup_code(doc(), binary()) -> doc().
set_hangup_code(Doc, HangupCode) ->
    kz_json:set_value([<<"hangup_code">>], HangupCode, Doc).

-spec interaction_id(doc()) -> kz_term:api_binary().
interaction_id(Doc) ->
    interaction_id(Doc, 'undefined').

-spec interaction_id(doc(), Default) -> binary() | Default.
interaction_id(Doc, Default) ->
    kz_json:get_binary_value([<<"interaction_id">>], Doc, Default).

-spec set_interaction_id(doc(), binary()) -> doc().
set_interaction_id(Doc, InteractionId) ->
    kz_json:set_value([<<"interaction_id">>], InteractionId, Doc).

-spec local_sdp(doc()) -> kz_term:api_binary().
local_sdp(Doc) ->
    local_sdp(Doc, 'undefined').

-spec local_sdp(doc(), Default) -> binary() | Default.
local_sdp(Doc, Default) ->
    kz_json:get_binary_value([<<"local_sdp">>], Doc, Default).

-spec set_local_sdp(doc(), binary()) -> doc().
set_local_sdp(Doc, LocalSdp) ->
    kz_json:set_value([<<"local_sdp">>], LocalSdp, Doc).

-spec media_server(doc()) -> kz_term:api_binary().
media_server(Doc) ->
    media_server(Doc, 'undefined').

-spec media_server(doc(), Default) -> binary() | Default.
media_server(Doc, Default) ->
    kz_json:get_binary_value([<<"media_server">>], Doc, Default).

-spec set_media_server(doc(), binary()) -> doc().
set_media_server(Doc, MediaServer) ->
    kz_json:set_value([<<"media_server">>], MediaServer, Doc).

-spec node(doc()) -> kz_term:api_binary().
node(Doc) ->
    node(Doc, 'undefined').

-spec node(doc(), Default) -> binary() | Default.
node(Doc, Default) ->
    kz_json:get_binary_value([<<"node">>], Doc, Default).

-spec set_node(doc(), binary()) -> doc().
set_node(Doc, Node) ->
    kz_json:set_value([<<"node">>], Node, Doc).

-spec other_leg_call_id(doc()) -> kz_term:api_binary().
other_leg_call_id(Doc) ->
    other_leg_call_id(Doc, 'undefined').

-spec other_leg_call_id(doc(), Default) -> binary() | Default.
other_leg_call_id(Doc, Default) ->
    kz_json:get_binary_value([<<"other_leg_call_id">>], Doc, Default).

-spec set_other_leg_call_id(doc(), binary()) -> doc().
set_other_leg_call_id(Doc, OtherLegCallId) ->
    kz_json:set_value([<<"other_leg_call_id">>], OtherLegCallId, Doc).

-spec other_leg_caller_id_name(doc()) -> kz_term:api_binary().
other_leg_caller_id_name(Doc) ->
    other_leg_caller_id_name(Doc, 'undefined').

-spec other_leg_caller_id_name(doc(), Default) -> binary() | Default.
other_leg_caller_id_name(Doc, Default) ->
    kz_json:get_binary_value([<<"other_leg_caller_id_name">>], Doc, Default).

-spec set_other_leg_caller_id_name(doc(), binary()) -> doc().
set_other_leg_caller_id_name(Doc, OtherLegCallerIdName) ->
    kz_json:set_value([<<"other_leg_caller_id_name">>], OtherLegCallerIdName, Doc).

-spec other_leg_caller_id_number(doc()) -> kz_term:api_binary().
other_leg_caller_id_number(Doc) ->
    other_leg_caller_id_number(Doc, 'undefined').

-spec other_leg_caller_id_number(doc(), Default) -> binary() | Default.
other_leg_caller_id_number(Doc, Default) ->
    kz_json:get_binary_value([<<"other_leg_caller_id_number">>], Doc, Default).

-spec set_other_leg_caller_id_number(doc(), binary()) -> doc().
set_other_leg_caller_id_number(Doc, OtherLegCallerIdNumber) ->
    kz_json:set_value([<<"other_leg_caller_id_number">>], OtherLegCallerIdNumber, Doc).

-spec other_leg_destination_number(doc()) -> kz_term:api_binary().
other_leg_destination_number(Doc) ->
    other_leg_destination_number(Doc, 'undefined').

-spec other_leg_destination_number(doc(), Default) -> binary() | Default.
other_leg_destination_number(Doc, Default) ->
    kz_json:get_binary_value([<<"other_leg_destination_number">>], Doc, Default).

-spec set_other_leg_destination_number(doc(), binary()) -> doc().
set_other_leg_destination_number(Doc, OtherLegDestinationNumber) ->
    kz_json:set_value([<<"other_leg_destination_number">>], OtherLegDestinationNumber, Doc).

-spec other_leg_direction(doc()) -> kz_term:api_binary().
other_leg_direction(Doc) ->
    other_leg_direction(Doc, 'undefined').

-spec other_leg_direction(doc(), Default) -> binary() | Default.
other_leg_direction(Doc, Default) ->
    kz_json:get_binary_value([<<"other_leg_direction">>], Doc, Default).

-spec set_other_leg_direction(doc(), binary()) -> doc().
set_other_leg_direction(Doc, OtherLegDirection) ->
    kz_json:set_value([<<"other_leg_direction">>], OtherLegDirection, Doc).

-spec presence_id(doc()) -> kz_term:api_binary().
presence_id(Doc) ->
    presence_id(Doc, 'undefined').

-spec presence_id(doc(), Default) -> binary() | Default.
presence_id(Doc, Default) ->
    kz_json:get_binary_value([<<"presence_id">>], Doc, Default).

-spec set_presence_id(doc(), binary()) -> doc().
set_presence_id(Doc, PresenceId) ->
    kz_json:set_value([<<"presence_id">>], PresenceId, Doc).

-spec remote_sdp(doc()) -> kz_term:api_binary().
remote_sdp(Doc) ->
    remote_sdp(Doc, 'undefined').

-spec remote_sdp(doc(), Default) -> binary() | Default.
remote_sdp(Doc, Default) ->
    kz_json:get_binary_value([<<"remote_sdp">>], Doc, Default).

-spec set_remote_sdp(doc(), binary()) -> doc().
set_remote_sdp(Doc, RemoteSdp) ->
    kz_json:set_value([<<"remote_sdp">>], RemoteSdp, Doc).

-spec request(doc()) -> kz_term:api_binary().
request(Doc) ->
    request(Doc, 'undefined').

-spec request(doc(), Default) -> binary() | Default.
request(Doc, Default) ->
    kz_json:get_binary_value([<<"request">>], Doc, Default).

-spec set_request(doc(), binary()) -> doc().
set_request(Doc, Request) ->
    kz_json:set_value([<<"request">>], Request, Doc).

-spec ringing_seconds(doc()) -> kz_term:api_integer().
ringing_seconds(Doc) ->
    ringing_seconds(Doc, 'undefined').

-spec ringing_seconds(doc(), Default) -> integer() | Default.
ringing_seconds(Doc, Default) ->
    kz_json:get_integer_value([<<"ringing_seconds">>], Doc, Default).

-spec set_ringing_seconds(doc(), integer()) -> doc().
set_ringing_seconds(Doc, RingingSeconds) ->
    kz_json:set_value([<<"ringing_seconds">>], RingingSeconds, Doc).

-spec timestamp(doc()) -> kz_term:api_integer().
timestamp(Doc) ->
    timestamp(Doc, 'undefined').

-spec timestamp(doc(), Default) -> kz_time:gregorian_seconds() | Default.
timestamp(Doc, Default) ->
    kz_json:get_integer_value([<<"timestamp">>], Doc, Default).

-spec set_timestamp(doc(), binary()) -> doc().
set_timestamp(Doc, Timestamp) ->
    kz_json:set_value([<<"timestamp">>], Timestamp, Doc).

-spec to(doc()) -> kz_term:api_binary().
to(Doc) ->
    to(Doc, 'undefined').

-spec to(doc(), Default) -> binary() | Default.
to(Doc, Default) ->
    kz_json:get_binary_value([<<"to">>], Doc, Default).

-spec set_to(doc(), binary()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value([<<"to">>], To, Doc).

-spec to_tag(doc()) -> kz_term:api_binary().
to_tag(Doc) ->
    to_tag(Doc, 'undefined').

-spec to_tag(doc(), Default) -> binary() | Default.
to_tag(Doc, Default) ->
    kz_json:get_binary_value([<<"to_tag">>], Doc, Default).

-spec set_to_tag(doc(), binary()) -> doc().
set_to_tag(Doc, ToTag) ->
    kz_json:set_value([<<"to_tag">>], ToTag, Doc).

-spec to_uri(doc()) -> kz_term:api_binary().
to_uri(Doc) ->
    to_uri(Doc, 'undefined').

-spec to_uri(doc(), Default) -> binary() | Default.
to_uri(Doc, Default) ->
    kz_json:get_binary_value([<<"to_uri">>], Doc, Default).

-spec set_to_uri(doc(), binary()) -> doc().
set_to_uri(Doc, ToUri) ->
    kz_json:set_value([<<"to_uri">>], ToUri, Doc).

-spec user_agent(doc()) -> kz_term:api_binary().
user_agent(Doc) ->
    user_agent(Doc, 'undefined').

-spec user_agent(doc(), Default) -> binary() | Default.
user_agent(Doc, Default) ->
    kz_json:get_binary_value([<<"user_agent">>], Doc, Default).

-spec set_user_agent(doc(), binary()) -> doc().
set_user_agent(Doc, UserAgent) ->
    kz_json:set_value([<<"user_agent">>], UserAgent, Doc).


-spec create_doc_id(kz_term:ne_binary()) -> kz_term:ne_binary().
create_doc_id(?NE_BINARY=CallId) ->
    {Year, Month, _} = erlang:date(),
    create_doc_id(CallId, Year, Month).

-spec create_doc_id(kz_term:ne_binary(), kz_time:gregorian_seconds()) -> kz_term:ne_binary().
create_doc_id(?NE_BINARY=CallId, Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    create_doc_id(CallId, Year, Month).

-spec create_doc_id(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> kz_term:ne_binary().
create_doc_id(?NE_BINARY=CallId, Year, Month)
  when is_integer(Year),
       is_integer(Month) ->
    list_to_binary([kz_term:to_binary(Year)
                   ,kz_date:pad_month(Month)
                   ,"-"
                   ,CallId
                   ]).

-spec type() -> binary().
type() -> ?PVT_TYPE.
