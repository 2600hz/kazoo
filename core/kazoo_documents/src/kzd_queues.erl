-module(kzd_queues).

-export([new/0]).
-export([agent_ring_timeout/1, agent_ring_timeout/2, set_agent_ring_timeout/2]).
-export([agent_wrapup_time/1, agent_wrapup_time/2, set_agent_wrapup_time/2]).
-export([announce/1, announce/2, set_announce/2]).
-export([announcements/1, announcements/2, set_announcements/2]).
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


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec agent_ring_timeout(doc()) -> integer().
-spec agent_ring_timeout(doc(), Default) -> integer() | Default.
agent_ring_timeout(Doc) ->
    agent_ring_timeout(Doc, 15).
agent_ring_timeout(Doc, Default) ->
    kz_json:get_integer_value(<<"agent_ring_timeout">>, Doc, Default).

-spec set_agent_ring_timeout(doc(), integer()) -> doc().
set_agent_ring_timeout(Doc, AgentRingTimeout) ->
    kz_json:set_value(<<"agent_ring_timeout">>, AgentRingTimeout, Doc).

-spec agent_wrapup_time(doc()) -> integer().
-spec agent_wrapup_time(doc(), Default) -> integer() | Default.
agent_wrapup_time(Doc) ->
    agent_wrapup_time(Doc, 0).
agent_wrapup_time(Doc, Default) ->
    kz_json:get_integer_value(<<"agent_wrapup_time">>, Doc, Default).

-spec set_agent_wrapup_time(doc(), integer()) -> doc().
set_agent_wrapup_time(Doc, AgentWrapupTime) ->
    kz_json:set_value(<<"agent_wrapup_time">>, AgentWrapupTime, Doc).

-spec announce(doc()) -> api_binary().
-spec announce(doc(), Default) -> binary() | Default.
announce(Doc) ->
    announce(Doc, 'undefined').
announce(Doc, Default) ->
    kz_json:get_binary_value(<<"announce">>, Doc, Default).

-spec set_announce(doc(), binary()) -> doc().
set_announce(Doc, Announce) ->
    kz_json:set_value(<<"announce">>, Announce, Doc).

-spec announcements(doc()) -> api_object().
-spec announcements(doc(), Default) -> kz_json:object() | Default.
announcements(Doc) ->
    announcements(Doc, 'undefined').
announcements(Doc, Default) ->
    kz_json:get_json_value(<<"announcements">>, Doc, Default).

-spec set_announcements(doc(), kz_json:object()) -> doc().
set_announcements(Doc, Announcements) ->
    kz_json:set_value(<<"announcements">>, Announcements, Doc).

-spec caller_exit_key(doc()) -> binary().
-spec caller_exit_key(doc(), Default) -> binary() | Default.
caller_exit_key(Doc) ->
    caller_exit_key(Doc, <<"#">>).
caller_exit_key(Doc, Default) ->
    kz_json:get_binary_value(<<"caller_exit_key">>, Doc, Default).

-spec set_caller_exit_key(doc(), binary()) -> doc().
set_caller_exit_key(Doc, CallerExitKey) ->
    kz_json:set_value(<<"caller_exit_key">>, CallerExitKey, Doc).

-spec cdr_url(doc()) -> api_binary().
-spec cdr_url(doc(), Default) -> binary() | Default.
cdr_url(Doc) ->
    cdr_url(Doc, 'undefined').
cdr_url(Doc, Default) ->
    kz_json:get_binary_value(<<"cdr_url">>, Doc, Default).

-spec set_cdr_url(doc(), binary()) -> doc().
set_cdr_url(Doc, CdrUrl) ->
    kz_json:set_value(<<"cdr_url">>, CdrUrl, Doc).

-spec connection_timeout(doc()) -> integer().
-spec connection_timeout(doc(), Default) -> integer() | Default.
connection_timeout(Doc) ->
    connection_timeout(Doc, 3600).
connection_timeout(Doc, Default) ->
    kz_json:get_integer_value(<<"connection_timeout">>, Doc, Default).

-spec set_connection_timeout(doc(), integer()) -> doc().
set_connection_timeout(Doc, ConnectionTimeout) ->
    kz_json:set_value(<<"connection_timeout">>, ConnectionTimeout, Doc).

-spec enter_when_empty(doc()) -> boolean().
-spec enter_when_empty(doc(), Default) -> boolean() | Default.
enter_when_empty(Doc) ->
    enter_when_empty(Doc, true).
enter_when_empty(Doc, Default) ->
    kz_json:get_boolean_value(<<"enter_when_empty">>, Doc, Default).

-spec set_enter_when_empty(doc(), boolean()) -> doc().
set_enter_when_empty(Doc, EnterWhenEmpty) ->
    kz_json:set_value(<<"enter_when_empty">>, EnterWhenEmpty, Doc).

-spec max_priority(doc()) -> api_integer().
-spec max_priority(doc(), Default) -> integer() | Default.
max_priority(Doc) ->
    max_priority(Doc, 'undefined').
max_priority(Doc, Default) ->
    kz_json:get_integer_value(<<"max_priority">>, Doc, Default).

-spec set_max_priority(doc(), integer()) -> doc().
set_max_priority(Doc, MaxPriority) ->
    kz_json:set_value(<<"max_priority">>, MaxPriority, Doc).

-spec max_queue_size(doc()) -> integer().
-spec max_queue_size(doc(), Default) -> integer() | Default.
max_queue_size(Doc) ->
    max_queue_size(Doc, 0).
max_queue_size(Doc, Default) ->
    kz_json:get_integer_value(<<"max_queue_size">>, Doc, Default).

-spec set_max_queue_size(doc(), integer()) -> doc().
set_max_queue_size(Doc, MaxQueueSize) ->
    kz_json:set_value(<<"max_queue_size">>, MaxQueueSize, Doc).

-spec moh(doc()) -> api_binary().
-spec moh(doc(), Default) -> binary() | Default.
moh(Doc) ->
    moh(Doc, 'undefined').
moh(Doc, Default) ->
    kz_json:get_binary_value(<<"moh">>, Doc, Default).

-spec set_moh(doc(), binary()) -> doc().
set_moh(Doc, Moh) ->
    kz_json:set_value(<<"moh">>, Moh, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec record_caller(doc()) -> boolean().
-spec record_caller(doc(), Default) -> boolean() | Default.
record_caller(Doc) ->
    record_caller(Doc, false).
record_caller(Doc, Default) ->
    kz_json:get_boolean_value(<<"record_caller">>, Doc, Default).

-spec set_record_caller(doc(), boolean()) -> doc().
set_record_caller(Doc, RecordCaller) ->
    kz_json:set_value(<<"record_caller">>, RecordCaller, Doc).

-spec recording_url(doc()) -> api_binary().
-spec recording_url(doc(), Default) -> binary() | Default.
recording_url(Doc) ->
    recording_url(Doc, 'undefined').
recording_url(Doc, Default) ->
    kz_json:get_binary_value(<<"recording_url">>, Doc, Default).

-spec set_recording_url(doc(), binary()) -> doc().
set_recording_url(Doc, RecordingUrl) ->
    kz_json:set_value(<<"recording_url">>, RecordingUrl, Doc).

-spec ring_simultaneously(doc()) -> integer().
-spec ring_simultaneously(doc(), Default) -> integer() | Default.
ring_simultaneously(Doc) ->
    ring_simultaneously(Doc, 1).
ring_simultaneously(Doc, Default) ->
    kz_json:get_integer_value(<<"ring_simultaneously">>, Doc, Default).

-spec set_ring_simultaneously(doc(), integer()) -> doc().
set_ring_simultaneously(Doc, RingSimultaneously) ->
    kz_json:set_value(<<"ring_simultaneously">>, RingSimultaneously, Doc).

-spec strategy(doc()) -> binary().
-spec strategy(doc(), Default) -> binary() | Default.
strategy(Doc) ->
    strategy(Doc, <<"round_robin">>).
strategy(Doc, Default) ->
    kz_json:get_binary_value(<<"strategy">>, Doc, Default).

-spec set_strategy(doc(), binary()) -> doc().
set_strategy(Doc, Strategy) ->
    kz_json:set_value(<<"strategy">>, Strategy, Doc).