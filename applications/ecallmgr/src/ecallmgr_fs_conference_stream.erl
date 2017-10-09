%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017 2600Hz INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_conference).

%% API
-export([init/0
        ,handle_event/1
        ,publish_event/1
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

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.process.conference.event">>, ?MODULE, 'handle_event'),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_event(tuple()) -> 'ok'.
handle_event({Node, _UUID, _Category, _Event, JObj}) ->
    Event = kzd_conference:event(JObj),
    process_event(Event, JObj, Node).

-spec process_event(ne_binary(), kz_json:object(), atom()) -> any().
process_event(<<"conference-create">>, JObj, Node) ->
    ecallmgr_fs_conferences:create(JObj, Node),
    ConferenceId = kzd_conference:conference_id(JObj),
    UUID = kzd_conference:instance_id(JObj),
    ecallmgr_conference_sup:start_conference_control(Node, ConferenceId, UUID);
process_event(<<"conference-destroy">>, JObj, Node) ->
    ConferenceId = kzd_conference:conference_id(JObj),
    InstanceId = kzd_conference:instance_id(JObj),
    _ = ecallmgr_fs_conferences:destroy(InstanceId),
    _ = ecallmgr_conference_sup:stop_conference_control(Node, ConferenceId, InstanceId);

process_event(<<"add-member">>, JObj, Node) ->
    ecallmgr_fs_conferences:participant_create(JObj, Node);
process_event(<<"del-member">>, JObj, _Node) ->
    ecallmgr_fs_conferences:participant_destroy(kzd_conference:call_id(JObj));

process_event(<<"lock">>, JObj, _) ->
    UUID = kzd_conference:instance_id(JObj),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'true'});
process_event(<<"unlock">>, JObj, _) ->
    UUID = kzd_conference:instance_id(JObj),
    ecallmgr_fs_conferences:update(UUID, {#conference.locked, 'false'});
process_event(Event, JObj, _Node) ->
    case lists:member(Event, ?MEMBER_UPDATE_EVENTS) of
        'true' -> update_participant(JObj);
        'false' -> 'ok'
    end.

update_participant(JObj) ->
    ConferenceVars = kzd_conference:conference_channel_vars(JObj),
    CustomVars = kzd_conference:custom_channel_vars(JObj),
    UUID = kzd_conference:call_id(JObj),
    Update = [{#participant.conference_channel_vars, ConferenceVars}
             ,{#participant.custom_channel_vars, CustomVars}
             ],
    ecallmgr_fs_conferences:participant_update(UUID, Update).
