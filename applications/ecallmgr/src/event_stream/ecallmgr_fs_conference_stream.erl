%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Execute conference commands
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_conference_stream).

%% API
-export([init/0
        ,handle_event/1
        ]).

-include("ecallmgr.hrl").


-define(MEMBER_UPDATE_EVENTS, [<<"stop-talking">>
                              ,<<"start-talking">>
                              ,<<"mute-member">>
                              ,<<"unmute-member">>
                              ,<<"deaf-member">>
                              ,<<"undeaf-member">>
                              ]).

-define(CONFERENCE_EVENTS, [<<"conference-create">>
                           ,<<"conference-destroy">>
                           ,<<"lock">>
                           ,<<"unlock">>
                           ,<<"add-member">>
                           ,<<"del-member">>
                                | ?MEMBER_UPDATE_EVENTS
                           ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"event_stream.process.conference.event">>, ?MODULE, 'handle_event'),
    'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(map()) -> 'ok'.
handle_event(#{node := Node, payload := JObj}) ->
    Event = kz_conference_event:event(JObj),
    lager:debug("handle conference event ~s ~s", [Node, Event]),
    process_event(Event, JObj, Node).

-spec process_event(kz_term:ne_binary(), kz_json:object(), atom()) -> any().
process_event(<<"conference-create">>, JObj, Node) ->
    _ = ecallmgr_fs_conferences:create(JObj, Node),
    ConferenceId = kz_conference_event:conference_id(JObj),
    UUID = kz_conference_event:instance_id(JObj),
    ecallmgr_conference_control_sup:start_conference_control(Node, ConferenceId, UUID);
process_event(<<"conference-destroy">>, JObj, Node) ->
    ConferenceId = kz_conference_event:conference_id(JObj),
    InstanceId = kz_conference_event:instance_id(JObj),
    _ = ecallmgr_fs_conferences:destroy(InstanceId),
    _ = ecallmgr_conference_control_sup:stop_conference_control(Node, ConferenceId, InstanceId);

process_event(<<"add-member">>, JObj, Node) ->
    ecallmgr_fs_conferences:participant_create(JObj, Node);
process_event(<<"del-member">>, JObj, _Node) ->
    ecallmgr_fs_conferences:participant_destroy(kz_conference_event:call_id(JObj));

process_event(<<"lock">>, JObj, _) ->
    UUID = kz_conference_event:instance_id(JObj),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'true'});
process_event(<<"unlock">>, JObj, _) ->
    UUID = kz_conference_event:instance_id(JObj),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'false'});
process_event(Event, JObj, _Node) ->
    case lists:member(Event, ?MEMBER_UPDATE_EVENTS) of
        'true' -> update_participant(JObj);
        'false' -> 'ok'
    end.

update_participant(JObj) ->
    ConferenceVars = kz_conference_event:conference_channel_vars(JObj),
    CustomVars = kz_conference_event:custom_channel_vars(JObj),
    AppVars = kz_conference_event:custom_application_vars(JObj),
    UUID = kz_conference_event:call_id(JObj),
    Update = [{#participant.conference_channel_vars, ConferenceVars}
             ,{#participant.custom_channel_vars, CustomVars}
             ,{#participant.custom_application_vars, AppVars}
             ],
    ecallmgr_fs_conferences:participant_update(UUID, Update).
