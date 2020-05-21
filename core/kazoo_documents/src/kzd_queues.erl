%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_queues).

-export([new/0]).
-export([agent_ring_timeout/1, agent_ring_timeout/2, set_agent_ring_timeout/2]).
-export([agent_wrapup_time/1, agent_wrapup_time/2, set_agent_wrapup_time/2]).
-export([announce/1, announce/2, set_announce/2]).
-export([announcements/1, announcements/2, set_announcements/2]).
-export([announcements_interval/1, announcements_interval/2, set_announcements_interval/2]).
-export([announcements_media/1, announcements_media/2, set_announcements_media/2]).
-export([announcements_media_in_the_queue/1, announcements_media_in_the_queue/2, set_announcements_media_in_the_queue/2]).
-export([announcements_media_increase_in_call_volume/1, announcements_media_increase_in_call_volume/2, set_announcements_media_increase_in_call_volume/2]).
-export([announcements_media_the_estimated_wait_time_is/1, announcements_media_the_estimated_wait_time_is/2, set_announcements_media_the_estimated_wait_time_is/2]).
-export([announcements_media_you_are_at_position/1, announcements_media_you_are_at_position/2, set_announcements_media_you_are_at_position/2]).
-export([announcements_position_announcements_enabled/1, announcements_position_announcements_enabled/2, set_announcements_position_announcements_enabled/2]).
-export([announcements_wait_time_announcements_enabled/1, announcements_wait_time_announcements_enabled/2, set_announcements_wait_time_announcements_enabled/2]).
-export([caller_exit_key/1, caller_exit_key/2, set_caller_exit_key/2]).
-export([cdr_url/1, cdr_url/2, set_cdr_url/2]).
-export([connection_timeout/1, connection_timeout/2, set_connection_timeout/2]).
-export([enter_when_empty/1, enter_when_empty/2, set_enter_when_empty/2]).
-export([max_priority/1, max_priority/2, set_max_priority/2]).
-export([max_queue_size/1, max_queue_size/2, set_max_queue_size/2]).
-export([moh/1, moh/2, set_moh/2]).
-export([name/1, name/2, set_name/2]).
-export([record_caller/1, record_caller/2, set_record_caller/2]).
-export([recording_url/1, recording_url/2, set_recording_url/2]).
-export([ring_simultaneously/1, ring_simultaneously/2, set_ring_simultaneously/2]).
-export([strategy/1, strategy/2, set_strategy/2]).


-export([type/0]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"queues">>).

-spec type() -> kz_term:ne_binary().
type() -> <<"queue">>.

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec agent_ring_timeout(doc()) -> integer().
agent_ring_timeout(Doc) ->
    agent_ring_timeout(Doc, 15).

-spec agent_ring_timeout(doc(), Default) -> integer() | Default.
agent_ring_timeout(Doc, Default) ->
    kz_json:get_integer_value([<<"agent_ring_timeout">>], Doc, Default).

-spec set_agent_ring_timeout(doc(), integer()) -> doc().
set_agent_ring_timeout(Doc, AgentRingTimeout) ->
    kz_json:set_value([<<"agent_ring_timeout">>], AgentRingTimeout, Doc).

-spec agent_wrapup_time(doc()) -> integer().
agent_wrapup_time(Doc) ->
    agent_wrapup_time(Doc, 0).

-spec agent_wrapup_time(doc(), Default) -> integer() | Default.
agent_wrapup_time(Doc, Default) ->
    kz_json:get_integer_value([<<"agent_wrapup_time">>], Doc, Default).

-spec set_agent_wrapup_time(doc(), integer()) -> doc().
set_agent_wrapup_time(Doc, AgentWrapupTime) ->
    kz_json:set_value([<<"agent_wrapup_time">>], AgentWrapupTime, Doc).

-spec announce(doc()) -> kz_term:api_binary().
announce(Doc) ->
    announce(Doc, 'undefined').

-spec announce(doc(), Default) -> binary() | Default.
announce(Doc, Default) ->
    kz_json:get_binary_value([<<"announce">>], Doc, Default).

-spec set_announce(doc(), binary()) -> doc().
set_announce(Doc, Announce) ->
    kz_json:set_value([<<"announce">>], Announce, Doc).

-spec announcements(doc()) -> kz_term:api_object().
announcements(Doc) ->
    announcements(Doc, 'undefined').

-spec announcements(doc(), Default) -> kz_json:object() | Default.
announcements(Doc, Default) ->
    kz_json:get_json_value([<<"announcements">>], Doc, Default).

-spec set_announcements(doc(), kz_json:object()) -> doc().
set_announcements(Doc, Announcements) ->
    kz_json:set_value([<<"announcements">>], Announcements, Doc).

-spec announcements_interval(doc()) -> integer().
announcements_interval(Doc) ->
    announcements_interval(Doc, 30).

-spec announcements_interval(doc(), Default) -> integer() | Default.
announcements_interval(Doc, Default) ->
    kz_json:get_integer_value([<<"announcements">>, <<"interval">>], Doc, Default).

-spec set_announcements_interval(doc(), integer()) -> doc().
set_announcements_interval(Doc, AnnouncementsInterval) ->
    kz_json:set_value([<<"announcements">>, <<"interval">>], AnnouncementsInterval, Doc).

-spec announcements_media(doc()) -> kz_term:api_object().
announcements_media(Doc) ->
    announcements_media(Doc, 'undefined').

-spec announcements_media(doc(), Default) -> kz_json:object() | Default.
announcements_media(Doc, Default) ->
    kz_json:get_json_value([<<"announcements">>, <<"media">>], Doc, Default).

-spec set_announcements_media(doc(), kz_json:object()) -> doc().
set_announcements_media(Doc, AnnouncementsMedia) ->
    kz_json:set_value([<<"announcements">>, <<"media">>], AnnouncementsMedia, Doc).

-spec announcements_media_in_the_queue(doc()) -> kz_term:api_binary().
announcements_media_in_the_queue(Doc) ->
    announcements_media_in_the_queue(Doc, 'undefined').

-spec announcements_media_in_the_queue(doc(), Default) -> binary() | Default.
announcements_media_in_the_queue(Doc, Default) ->
    kz_json:get_binary_value([<<"announcements">>, <<"media">>, <<"in_the_queue">>], Doc, Default).

-spec set_announcements_media_in_the_queue(doc(), binary()) -> doc().
set_announcements_media_in_the_queue(Doc, AnnouncementsMediaInTheQueue) ->
    kz_json:set_value([<<"announcements">>, <<"media">>, <<"in_the_queue">>], AnnouncementsMediaInTheQueue, Doc).

-spec announcements_media_increase_in_call_volume(doc()) -> kz_term:api_binary().
announcements_media_increase_in_call_volume(Doc) ->
    announcements_media_increase_in_call_volume(Doc, 'undefined').

-spec announcements_media_increase_in_call_volume(doc(), Default) -> binary() | Default.
announcements_media_increase_in_call_volume(Doc, Default) ->
    kz_json:get_binary_value([<<"announcements">>, <<"media">>, <<"increase_in_call_volume">>], Doc, Default).

-spec set_announcements_media_increase_in_call_volume(doc(), binary()) -> doc().
set_announcements_media_increase_in_call_volume(Doc, AnnouncementsMediaIncreaseInCallVolume) ->
    kz_json:set_value([<<"announcements">>, <<"media">>, <<"increase_in_call_volume">>], AnnouncementsMediaIncreaseInCallVolume, Doc).

-spec announcements_media_the_estimated_wait_time_is(doc()) -> kz_term:api_binary().
announcements_media_the_estimated_wait_time_is(Doc) ->
    announcements_media_the_estimated_wait_time_is(Doc, 'undefined').

-spec announcements_media_the_estimated_wait_time_is(doc(), Default) -> binary() | Default.
announcements_media_the_estimated_wait_time_is(Doc, Default) ->
    kz_json:get_binary_value([<<"announcements">>, <<"media">>, <<"the_estimated_wait_time_is">>], Doc, Default).

-spec set_announcements_media_the_estimated_wait_time_is(doc(), binary()) -> doc().
set_announcements_media_the_estimated_wait_time_is(Doc, AnnouncementsMediaTheEstimatedWaitTimeIs) ->
    kz_json:set_value([<<"announcements">>, <<"media">>, <<"the_estimated_wait_time_is">>], AnnouncementsMediaTheEstimatedWaitTimeIs, Doc).

-spec announcements_media_you_are_at_position(doc()) -> kz_term:api_binary().
announcements_media_you_are_at_position(Doc) ->
    announcements_media_you_are_at_position(Doc, 'undefined').

-spec announcements_media_you_are_at_position(doc(), Default) -> binary() | Default.
announcements_media_you_are_at_position(Doc, Default) ->
    kz_json:get_binary_value([<<"announcements">>, <<"media">>, <<"you_are_at_position">>], Doc, Default).

-spec set_announcements_media_you_are_at_position(doc(), binary()) -> doc().
set_announcements_media_you_are_at_position(Doc, AnnouncementsMediaYouAreAtPosition) ->
    kz_json:set_value([<<"announcements">>, <<"media">>, <<"you_are_at_position">>], AnnouncementsMediaYouAreAtPosition, Doc).

-spec announcements_position_announcements_enabled(doc()) -> kz_term:api_boolean().
announcements_position_announcements_enabled(Doc) ->
    announcements_position_announcements_enabled(Doc, 'undefined').

-spec announcements_position_announcements_enabled(doc(), Default) -> boolean() | Default.
announcements_position_announcements_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"announcements">>, <<"position_announcements_enabled">>], Doc, Default).

-spec set_announcements_position_announcements_enabled(doc(), boolean()) -> doc().
set_announcements_position_announcements_enabled(Doc, AnnouncementsPositionAnnouncementsEnabled) ->
    kz_json:set_value([<<"announcements">>, <<"position_announcements_enabled">>], AnnouncementsPositionAnnouncementsEnabled, Doc).

-spec announcements_wait_time_announcements_enabled(doc()) -> kz_term:api_boolean().
announcements_wait_time_announcements_enabled(Doc) ->
    announcements_wait_time_announcements_enabled(Doc, 'undefined').

-spec announcements_wait_time_announcements_enabled(doc(), Default) -> boolean() | Default.
announcements_wait_time_announcements_enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"announcements">>, <<"wait_time_announcements_enabled">>], Doc, Default).

-spec set_announcements_wait_time_announcements_enabled(doc(), boolean()) -> doc().
set_announcements_wait_time_announcements_enabled(Doc, AnnouncementsWaitTimeAnnouncementsEnabled) ->
    kz_json:set_value([<<"announcements">>, <<"wait_time_announcements_enabled">>], AnnouncementsWaitTimeAnnouncementsEnabled, Doc).

-spec caller_exit_key(doc()) -> binary().
caller_exit_key(Doc) ->
    caller_exit_key(Doc, <<"#">>).

-spec caller_exit_key(doc(), Default) -> binary() | Default.
caller_exit_key(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_exit_key">>], Doc, Default).

-spec set_caller_exit_key(doc(), binary()) -> doc().
set_caller_exit_key(Doc, CallerExitKey) ->
    kz_json:set_value([<<"caller_exit_key">>], CallerExitKey, Doc).

-spec cdr_url(doc()) -> kz_term:api_binary().
cdr_url(Doc) ->
    cdr_url(Doc, 'undefined').

-spec cdr_url(doc(), Default) -> binary() | Default.
cdr_url(Doc, Default) ->
    kz_json:get_binary_value([<<"cdr_url">>], Doc, Default).

-spec set_cdr_url(doc(), binary()) -> doc().
set_cdr_url(Doc, CdrUrl) ->
    kz_json:set_value([<<"cdr_url">>], CdrUrl, Doc).

-spec connection_timeout(doc()) -> integer().
connection_timeout(Doc) ->
    connection_timeout(Doc, 3600).

-spec connection_timeout(doc(), Default) -> integer() | Default.
connection_timeout(Doc, Default) ->
    kz_json:get_integer_value([<<"connection_timeout">>], Doc, Default).

-spec set_connection_timeout(doc(), integer()) -> doc().
set_connection_timeout(Doc, ConnectionTimeout) ->
    kz_json:set_value([<<"connection_timeout">>], ConnectionTimeout, Doc).

-spec enter_when_empty(doc()) -> boolean().
enter_when_empty(Doc) ->
    enter_when_empty(Doc, true).

-spec enter_when_empty(doc(), Default) -> boolean() | Default.
enter_when_empty(Doc, Default) ->
    kz_json:get_boolean_value([<<"enter_when_empty">>], Doc, Default).

-spec set_enter_when_empty(doc(), boolean()) -> doc().
set_enter_when_empty(Doc, EnterWhenEmpty) ->
    kz_json:set_value([<<"enter_when_empty">>], EnterWhenEmpty, Doc).

-spec max_priority(doc()) -> kz_term:api_integer().
max_priority(Doc) ->
    max_priority(Doc, 'undefined').

-spec max_priority(doc(), Default) -> integer() | Default.
max_priority(Doc, Default) ->
    kz_json:get_integer_value([<<"max_priority">>], Doc, Default).

-spec set_max_priority(doc(), integer()) -> doc().
set_max_priority(Doc, MaxPriority) ->
    kz_json:set_value([<<"max_priority">>], MaxPriority, Doc).

-spec max_queue_size(doc()) -> integer().
max_queue_size(Doc) ->
    max_queue_size(Doc, 0).

-spec max_queue_size(doc(), Default) -> integer() | Default.
max_queue_size(Doc, Default) ->
    kz_json:get_integer_value([<<"max_queue_size">>], Doc, Default).

-spec set_max_queue_size(doc(), integer()) -> doc().
set_max_queue_size(Doc, MaxQueueSize) ->
    kz_json:set_value([<<"max_queue_size">>], MaxQueueSize, Doc).

-spec moh(doc()) -> kz_term:api_binary().
moh(Doc) ->
    moh(Doc, 'undefined').

-spec moh(doc(), Default) -> binary() | Default.
moh(Doc, Default) ->
    kz_json:get_binary_value([<<"moh">>], Doc, Default).

-spec set_moh(doc(), binary()) -> doc().
set_moh(Doc, Moh) ->
    kz_json:set_value([<<"moh">>], Moh, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec record_caller(doc()) -> boolean().
record_caller(Doc) ->
    record_caller(Doc, false).

-spec record_caller(doc(), Default) -> boolean() | Default.
record_caller(Doc, Default) ->
    kz_json:get_boolean_value([<<"record_caller">>], Doc, Default).

-spec set_record_caller(doc(), boolean()) -> doc().
set_record_caller(Doc, RecordCaller) ->
    kz_json:set_value([<<"record_caller">>], RecordCaller, Doc).

-spec recording_url(doc()) -> kz_term:api_binary().
recording_url(Doc) ->
    recording_url(Doc, 'undefined').

-spec recording_url(doc(), Default) -> binary() | Default.
recording_url(Doc, Default) ->
    kz_json:get_binary_value([<<"recording_url">>], Doc, Default).

-spec set_recording_url(doc(), binary()) -> doc().
set_recording_url(Doc, RecordingUrl) ->
    kz_json:set_value([<<"recording_url">>], RecordingUrl, Doc).

-spec ring_simultaneously(doc()) -> integer().
ring_simultaneously(Doc) ->
    ring_simultaneously(Doc, 1).

-spec ring_simultaneously(doc(), Default) -> integer() | Default.
ring_simultaneously(Doc, Default) ->
    kz_json:get_integer_value([<<"ring_simultaneously">>], Doc, Default).

-spec set_ring_simultaneously(doc(), integer()) -> doc().
set_ring_simultaneously(Doc, RingSimultaneously) ->
    kz_json:set_value([<<"ring_simultaneously">>], RingSimultaneously, Doc).

-spec strategy(doc()) -> binary().
strategy(Doc) ->
    strategy(Doc, <<"round_robin">>).

-spec strategy(doc(), Default) -> binary() | Default.
strategy(Doc, Default) ->
    kz_json:get_binary_value([<<"strategy">>], Doc, Default).

-spec set_strategy(doc(), binary()) -> doc().
set_strategy(Doc, Strategy) ->
    kz_json:set_value([<<"strategy">>], Strategy, Doc).
