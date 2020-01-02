%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Track the FreeSWITCH channel information, and provide accessors
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_event_stream_registered).

-export([init/0]).

-export([notify_call_event/1
        ,notify_conference_event/1
        ]).

-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"event_stream.registered.call_event.*">>, ?MODULE, 'notify_call_event'),
    _ = kazoo_bindings:bind(<<"event_stream.registered.conference.event">>, ?MODULE, 'notify_conference_event'),
    'ok'.

-spec notify_call_event(map()) -> any().
notify_call_event(#{node := Node, call_id := UUID, event := Event, payload := JObj}) ->
    kz_log:put_callid(JObj),
    gproc:send({'p', 'l', ?FS_EVENT_REG_MSG(Node, Event)}, {'event', UUID, JObj}),
    maybe_send_call_event(UUID, Event, JObj, Node).

-spec maybe_send_call_event(kz_term:api_binary(), kz_term:ne_binary(), kz_json:object(), atom()) -> any().
maybe_send_call_event('undefined', _, _, _) -> 'ok';
maybe_send_call_event(CallId, Event, JObj, Node) ->
    gproc:send({'p', 'l', ?FS_CALL_EVENT_MSG(Node, Event, CallId)}, {'event', Event, CallId, JObj}),
    gproc:send({'p', 'l', ?FS_CALL_EVENT_REG_MSG(Node, CallId)}, {'event', CallId, JObj}).

-spec notify_conference_event(map()) -> any().
notify_conference_event(#{node := Node, payload := JObj}) ->
    kz_log:put_callid(JObj),
    ConferenceId = kz_conference_event:conference_id(JObj),
    Event = kz_conference_event:event(JObj),
    gproc:send({'p', 'l', ?FS_CONFERENCE_EVENT_REG_MSG(Node, ConferenceId, Event)}, {'conference', ConferenceId, Event, JObj}),
    gproc:send({'p', 'l', ?FS_CONFERENCE_EVENT_ALL_REG_MSG(Node, ConferenceId)}, {'conference', ConferenceId, Event, JObj}),
    gproc:send({'p', 'l', ?FS_CONFERENCE_ALL_EVENT_REG_MSG(Node, Event)}, {'conference', ConferenceId, Event, JObj}),
    gproc:send({'p', 'l', ?FS_CONFERENCE_ALL_REG_MSG(Node)}, {'conference', ConferenceId, Event, JObj}).
