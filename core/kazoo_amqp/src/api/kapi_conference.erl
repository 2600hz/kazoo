%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_conference).

-export([event/1, event_v/1]).
-export([focus_queue_name/1]).
-export([search_req/1, search_req_v/1]).
-export([search_resp/1, search_resp_v/1]).
-export([discovery_req/1, discovery_req_v/1]).
-export([discovery_resp/1, discovery_resp_v/1]).

-export([add_participant/1, add_participant_v/1]).
-export([deaf_participant/1, deaf_participant_v/1]).
-export([participant_energy/1, participant_energy_v/1]).
-export([kick/1, kick_v/1]).
-export([participants_req/1, participants_req_v/1]).
-export([participants_resp/1, participants_resp_v/1]).
-export([lock/1, lock_v/1]).
-export([mute_participant/1, mute_participant_v/1]).
-export([play/1, play_v/1
        ,tones/1, tones_v/1
        ,say/1, say_v/1
        ,tts/1, tts_v/1
        ]).
-export([record/1, record_v/1]).
-export([recordstop/1, recordstop_v/1]).
-export([relate_participants/1, relate_participants_v/1]).
-export([set/1, set_v/1]).
-export([stop_play/1, stop_play_v/1]).
-export([undeaf_participant/1, undeaf_participant_v/1]).
-export([unlock/1, unlock_v/1]).
-export([unmute_participant/1, unmute_participant_v/1]).
-export([participant_volume_in/1, participant_volume_in_v/1]).
-export([participant_volume_out/1, participant_volume_out_v/1]).
-export([participant_event/1, participant_event_v/1]).
-export([conference_error/1, conference_error_v/1]).
-export([config_req/1, config_req_v/1
        ,config_resp/1, config_resp_v/1
        ]).
-export([play_macro_req/1, play_macro_req_v/1]).
-export([dial/1, dial_v/1
        ,dial_resp/1, dial_resp_v/1
        ]).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([publish_search_req/1, publish_search_req/2]).
-export([publish_search_resp/2, publish_search_resp/3]).
-export([publish_discovery_req/1, publish_discovery_req/2]).
-export([publish_discovery_resp/2, publish_discovery_resp/3]).

-export([publish_add_participant/2, publish_add_participant/3]).
-export([publish_deaf_participant/2, publish_deaf_participant/3]).
-export([publish_participant_energy/2, publish_participant_energy/3]).
-export([publish_kick/2, publish_kick/3]).
-export([publish_participants_req/2, publish_participants_req/3]).
-export([publish_participants_resp/2, publish_participants_resp/3]).
-export([publish_lock/2, publish_lock/3]).
-export([publish_mute_participant/2, publish_mute_participant/3]).
-export([publish_play/2, publish_play/3]).
-export([publish_record/2, publish_record/3]).
-export([publish_recordstop/2, publish_recordstop/3]).
-export([publish_relate_participants/2, publish_relate_participants/3]).
-export([publish_set/2, publish_set/3]).
-export([publish_stop_play/2, publish_stop_play/3]).
-export([publish_undeaf_participant/2, publish_undeaf_participant/3]).
-export([publish_unlock/2, publish_unlock/3]).
-export([publish_unmute_participant/2, publish_unmute_participant/3]).
-export([publish_participant_volume_in/2, publish_participant_volume_in/3]).
-export([publish_participant_volume_out/2, publish_participant_volume_out/3]).
-export([publish_error/2, publish_error/3]).
-export([publish_participant_event/3, publish_participant_event/4]).
-export([publish_command/2, publish_command/3]).
-export([publish_event/1, publish_event/2]).
-export([publish_targeted_command/2, publish_targeted_command/3]).
-export([publish_config_req/1, publish_config_req/2
        ,publish_config_resp/2, publish_config_resp/3
        ,publish_dial/2, publish_dial/3
        ,publish_dial_resp/2, publish_dial_resp/3
        ]).

-include_lib("kz_amqp_util.hrl").
-include("kapi_dialplan.hrl").
-include("kapi_call.hrl").

-type doc() :: kz_json:object().
-type discovery_req() :: kz_json:object().
-export_type([doc/0
             ,discovery_req/0
             ]).

%% Conference Search Request
-define(SEARCH_REQ_HEADERS, [ [<<"Conference-ID">>, <<"Account-ID">>] ]).
-define(OPTIONAL_SEARCH_REQ_HEADERS, []).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                           ,{<<"Event-Name">>, <<"search_req">>}
                           ]).
-define(SEARCH_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Search Response
-define(SEARCH_RESP_HEADERS, [ [<<"Conferences">>, <<"Conference-ID">>] ]).
-define(OPTIONAL_SEARCH_RESP_HEADERS, [<<"Answered">>
                                      ,<<"Dynamic">>
                                      ,<<"Locked">>
                                      ,<<"Participant-Count">>
                                      ,<<"Participants">>
                                      ,<<"Run-Time">>
                                      ,<<"Running">>
                                      ,<<"Start-Time">>
                                      ,<<"Switch-External-IP">>
                                      ,<<"Switch-Hostname">>
                                      ,<<"Switch-URL">>
                                      ,<<"UUID">>
                                      ]).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                            ,{<<"Event-Name">>, <<"search_resp">>}
                            ]).
-define(SEARCH_RESP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Discovery Request
-define(DISCOVERY_REQ_HEADERS, [<<"Call">>]).
-define(OPTIONAL_DISCOVERY_REQ_HEADERS, [<<"Conference-Doc">>
                                        ,<<"Conference-ID">>
                                        ,<<"Conference-Name">>
                                        ,<<"Moderator">>
                                        ,<<"Play-Entry-Tone">>
                                        ,<<"Play-Exit-Tone">>
                                        ,<<"Play-Welcome">>
                                        ,<<"Play-Welcome-Media">>
                                        ]).
-define(DISCOVERY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                              ,{<<"Event-Name">>, <<"discovery_req">>}
                              ]).
-define(DISCOVERY_REQ_TYPES, [{<<"Moderator">>, fun kz_term:is_boolean/1}
                             ,{<<"Play-Welcome">>, fun kz_term:is_boolean/1}
                             ]).

%% Conference Discovery Request
-define(DISCOVERY_RESP_HEADERS, [<<"Participant-ID">>]).
-define(OPTIONAL_DISCOVERY_RESP_HEADERS, [<<"Conference-ID">>]).
-define(DISCOVERY_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"discovery_resp">>}
                               ]).
-define(DISCOVERY_RESP_TYPES, []).

-define(ADD_PARTICIPANT_HEADERS, [<<"Call-ID">>
                                 ,<<"Conference-ID">>
                                 ,<<"Control-Queue">>
                                 ]).
-define(OPTIONAL_ADD_PARTICIPANT_HEADERS, [<<"Account-ID">>
                                          ,<<"Answered">>
                                          ,<<"Authorizing-ID">>
                                          ,<<"Authorizing-Type">>
                                          ,<<"Bridge-ID">>
                                          ,<<"Call-Direction">>
                                          ,<<"Call-ID">>
                                          ,<<"Callee-ID-Name">>
                                          ,<<"Callee-ID-Number">>
                                          ,<<"Caller-ID-Name">>
                                          ,<<"Caller-ID-Number">>
                                          ,<<"Channel-Authorized">>
                                          ,<<"Channel-Call-State">>
                                          ,<<"Channel-Created-Time">>
                                          ,<<"Channel-Is-Loopback">>
                                          ,<<"Channel-Loopback-Bowout">>
                                          ,<<"Channel-Loopback-Bowout-Execute">>
                                          ,<<"Channel-Loopback-Leg">>
                                          ,<<"Channel-Loopback-Other-Leg-ID">>
                                          ,<<"Channel-Moving">>
                                          ,<<"Channel-Name">>
                                          ,<<"Channel-State">>
                                          ,<<"Custom-Application-Vars">>
                                          ,<<"Custom-Channel-Vars">>
                                          ,<<"Custom-SIP-Headers">>
                                          ,<<"Destination">>
                                          ,<<"Disposition">>
                                          ,<<"Ecallmgr-Node">>
                                          ,<<"From">>
                                          ,<<"From-Tag">>
                                          ,<<"Hangup-Cause">>
                                          ,<<"Hangup-Code">>
                                          ,<<"Is-Loopback">>
                                          ,<<"Is-On-Hold">>
                                          ,<<"Media-Node">>
                                          ,<<"Media-Server">>
                                          ,<<"Msg-ID">>
                                          ,<<"Node">>
                                          ,<<"Origination-Call-ID">>
                                          ,<<"Other-Leg-Call-ID">>
                                          ,<<"Other-Leg-Caller-ID-Name">>
                                          ,<<"Other-Leg-Caller-ID-Number">>
                                          ,<<"Other-Leg-Destination-Number">>
                                          ,<<"Other-Leg-Direction">>
                                          ,<<"Participant-Flags">>
                                          ,<<"Profile-Name">>
                                          ,<<"Presence-ID">>
                                          ,<<"Raw-Application-Data">>
                                          ,<<"Raw-Application-Name">>
                                          ,<<"Realm">>
                                          ,<<"Replaced-By">>
                                          ,<<"Request">>
                                          ,<<"Switch-Hostname">>
                                          ,<<"Switch-Nodename">>
                                          ,<<"Switch-URI">>
                                          ,<<"Switch-URL">>
                                          ,<<"Timestamp">>
                                          ,<<"To">>
                                          ,<<"To-Tag">>
                                          ,<<"Transfer-History">>
                                          ,<<"Username">>
                                          ]).

-define(ADD_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                ,{<<"Event-Name">>, <<"add_participant">>}
                                ]).
-define(ADD_PARTICIPANT_TYPES, []).

%% Conference Deaf
-define(DEAF_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_DEAF_PARTICIPANT_HEADERS, []).
-define(DEAF_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"deaf_participant">>}
                                 ]).
-define(DEAF_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ]).

%% Conference Energy
-define(PARTICIPANT_ENERGY_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>
                                    ,<<"Participant-ID">>, <<"Energy-Level">>
                                    ]).
-define(OPTIONAL_PARTICIPANT_ENERGY_HEADERS, []).
-define(PARTICIPANT_ENERGY_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                   ,{<<"Event-Name">>, <<"command">>}
                                   ,{<<"Application-Name">>, <<"participant_energy">>}
                                   ]).
-define(PARTICIPANT_ENERGY_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                  ]).

%% Conference Kick
-define(KICK_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_KICK_HEADERS, [<<"Participant-ID">>]).
-define(KICK_VALUES, [{<<"Event-Category">>, <<"conference">>}
                     ,{<<"Event-Name">>, <<"command">>}
                     ,{<<"Application-Name">>, <<"kick">>}
                     ]).
-define(KICK_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                    ]).

%% Conference Participants Req
-define(PARTICIPANTS_REQ_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_PARTICIPANTS_REQ_HEADERS, []).
-define(PARTICIPANTS_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"participants">>}
                                 ]).
-define(PARTICIPANTS_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Participants Resp
-define(PARTICIPANTS_RESP_HEADERS, [<<"Participants">>]).
-define(OPTIONAL_PARTICIPANTS_RESP_HEADERS, []).
-define(PARTICIPANTS_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                  ,{<<"Event-Name">>, <<"participants_resp">>}
                                  ]).
-define(PARTICIPANTS_RESP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Lock
-define(LOCK_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_LOCK_HEADERS, []).
-define(LOCK_VALUES, [{<<"Event-Category">>, <<"conference">>}
                     ,{<<"Event-Name">>, <<"command">>}
                     ,{<<"Application-Name">>, <<"lock">>}
                     ]).
-define(LOCK_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                    ]).

%% Conference Mute
-define(MUTE_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_MUTE_PARTICIPANT_HEADERS, []).
-define(MUTE_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"command">>}
                                 ,{<<"Application-Name">>, <<"mute_participant">>}
                                 ]).
-define(MUTE_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ]).

%% Conference Play
-define(PLAY_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_PLAY_HEADERS, [<<"Participant-ID">>, <<"Call-ID">>]).
-define(PLAY_VALUES, [{<<"Event-Category">>, <<"conference">>}
                     ,{<<"Event-Name">>, <<"command">>}
                     ,{<<"Application-Name">>, <<"play">>}
                     ]).
-define(PLAY_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                    ,{<<"Media-Name">>, fun is_binary/1}
                    ]).

%% Conference Record
-define(RECORD_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_RECORD_HEADERS, [<<"Call-ID">>]).
-define(RECORD_VALUES, [{<<"Event-Category">>, <<"conference">>}
                       ,{<<"Event-Name">>, <<"command">>}
                       ,{<<"Application-Name">>, <<"record">>}
                       ]).
-define(RECORD_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference RecordStop
-define(RECORDSTOP_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_RECORDSTOP_HEADERS, [<<"Call-ID">>]).
-define(RECORDSTOP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"recordstop">>}
                           ]).
-define(RECORDSTOP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).


%% Conference Relate Participants
-define(RELATE_PARTICIPANTS_HEADERS, [<<"Application-Name">>
                                     ,<<"Conference-ID">>
                                     ,<<"Other-Participant">>
                                     ,<<"Participant-ID">>
                                     ]).
-define(OPTIONAL_RELATE_PARTICIPANTS_HEADERS, [<<"Relationship">>]).
-define(RELATE_PARTICIPANTS_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                    ,{<<"Event-Name">>, <<"command">>}
                                    ,{<<"Application-Name">>, <<"relate_participants">>}
                                    ,{<<"Relationship">>, [<<"deaf">>, <<"mute">>, <<"clear">>]}
                                    ]).
-define(RELATE_PARTICIPANTS_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                   ,{<<"Participant-ID">> ,fun(ID) -> is_integer(ID)
                                                                          orelse is_binary(ID)
                                                           end
                                    }
                                   ,{<<"Other-Participant">>, fun(ID) -> is_integer(ID)
                                                                             orelse is_binary(ID)
                                                              end
                                    }
                                   ]).

%% Conference Set
-define(SET_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Parameter">>, <<"Value">>]).
-define(OPTIONAL_SET_HEADERS, []).
-define(SET_VALUES, [{<<"Event-Category">>, <<"conference">>}
                    ,{<<"Event-Name">>, <<"command">>}
                    ,{<<"Application-Name">>, <<"set">>}
                    ,{<<"Parameter">>, [<<"Max-Members">>, <<"Caller-ID-Name">>, <<"Caller-ID-Number">>]}
                    ]).
-define(SET_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                   ,{<<"Value">>, fun is_binary/1}
                   ]).

%% Conference Stop Play
-define(STOP_PLAY_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_STOP_PLAY_HEADERS, [<<"Participant-ID">>, <<"Affects">>]).
-define(STOP_PLAY_VALUES, [{<<"Event-Category">>, <<"conference">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"stop_play">>}
                          ,{<<"Affects">>, [<<"current">>, <<"all">>]}
                          ]).
-define(STOP_PLAY_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                         ]).
%% Conference Undeaf
-define(UNDEAF_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_UNDEAF_PARTICIPANT_HEADERS, []).
-define(UNDEAF_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                   ,{<<"Event-Name">>, <<"command">>}
                                   ,{<<"Application-Name">>, <<"undeaf_participant">>}
                                   ]).
-define(UNDEAF_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                  ]).

%% Conference Unlock
-define(UNLOCK_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_UNLOCK_HEADERS, []).
-define(UNLOCK_VALUES, [{<<"Event-Category">>, <<"conference">>}
                       ,{<<"Event-Name">>, <<"command">>}
                       ,{<<"Application-Name">>, <<"unlock">>}
                       ]).
-define(UNLOCK_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                      ]).

%% Conference Unmute
-define(UNMUTE_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant-ID">>]).
-define(OPTIONAL_UNMUTE_PARTICIPANT_HEADERS, []).
-define(UNMUTE_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                   ,{<<"Event-Name">>, <<"command">>}
                                   ,{<<"Application-Name">>, <<"unmute_participant">>}
                                   ]).
-define(UNMUTE_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                  ]).

%% Conference Set Volume In
-define(PARTICIPANT_VOLUME_IN_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>
                                       ,<<"Participant-ID">>, <<"Volume-In-Level">>
                                       ]).
-define(OPTIONAL_PARTICIPANT_VOLUME_IN_HEADERS, []).
-define(PARTICIPANT_VOLUME_IN_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                      ,{<<"Event-Name">>, <<"command">>}
                                      ,{<<"Application-Name">>, <<"participant_volume_in">>}
                                      ]).
-define(PARTICIPANT_VOLUME_IN_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                     ]).

%% Conference Set Volume Out
-define(PARTICIPANT_VOLUME_OUT_HEADERS, [<<"Application-Name">>
                                        ,<<"Conference-ID">>
                                        ,<<"Participant-ID">>
                                        ,<<"Volume-Out-Level">>
                                        ]).
-define(OPTIONAL_PARTICIPANT_VOLUME_OUT_HEADERS, []).
-define(PARTICIPANT_VOLUME_OUT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                       ,{<<"Event-Name">>, <<"command">>}
                                       ,{<<"Application-Name">>, <<"participant_volume_out">>}
                                       ]).
-define(PARTICIPANT_VOLUME_OUT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

-define(DIAL_HEADERS, [<<"Endpoints">>
                      ,<<"Conference-ID">>
                      ,<<"Application-Name">>
                      ]).
-define(OPTIONAL_DIAL_HEADERS, [<<"Account-ID">>
                               ,<<"Caller-ID-Name">>
                               ,<<"Caller-ID-Number">>
                               ,<<"Custom-Channel-Vars">>
                               ,<<"Custom-Application-Vars">>
                               ,<<"Outbound-Call-ID">>
                               ,<<"Participant-Flags">>
                               ,<<"Profile-Name">>
                               ,<<"Target-Call-ID">>
                               ,<<"Timeout">>
                               ]).
-define(DIAL_VALUES, [{<<"Event-Category">>, <<"conference">>}
                     ,{<<"Event-Name">>, <<"command">>}
                     ,{<<"Application-Name">>, <<"dial">>}
                     ]).
-define(DIAL_TYPES, [{<<"Caller-ID-Name">>, fun is_binary/1}
                    ,{<<"Caller-ID-Number">>, fun is_binary/1}
                    ,{<<"Endpoints">>, fun(Es) ->
                                               kz_term:is_ne_list(Es)
                                                   andalso kz_json:are_json_objects(Es)
                                       end
                     }
                    ,{<<"Custom-Channel-Vars">>, fun kz_json:is_json_object/1}
                    ,{<<"Custom-Application-Vars">>, fun kz_json:is_json_object/1}
                    ,{<<"Timeout">>, fun is_integer/1}
                    ]).

-define(DIAL_RESP_HEADERS, [<<"Endpoint-Responses">>]).
-define(OPTIONAL_DIAL_RESP_HEADERS, []).
-define(DIAL_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"dial_resp">>}
                          ]).
-define(DIAL_RESP_TYPES, [{<<"Endpoint-Responses">>
                          ,fun(Es) ->
                                   kz_term:is_ne_list(Es)
                                       andalso kz_json:are_json_objects(Es)
                           end
                          }
                         ]).

%% Conference Participants Event
-define(PARTICIPANT_EVENT_HEADERS, [<<"Event">>
                                   ,<<"Call-ID">>
                                   ,<<"Focus">>
                                   ,<<"Conference-ID">>
                                   ,<<"Instance-ID">>
                                   ,<<"Participant-ID">>
                                   ,<<"Floor">>
                                   ,<<"Hear">>
                                   ,<<"Speak">>
                                   ,<<"Talking">>
                                   ,<<"Current-Energy">>
                                   ,<<"Energy-Level">>
                                   ,<<"Video">>
                                   ,<<"Mute-Detect">>
                                   ,<<"Caller-ID-Name">>
                                   ,<<"Caller-ID-Number">>
                                   ,<<"Channel-Presence-ID">>
                                   ,<<"Custom-Channel-Vars">>
                                   ]).
-define(OPTIONAL_PARTICIPANT_EVENT_HEADERS, []).
-define(PARTICIPANT_EVENT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                  ,{<<"Event-Name">>, <<"participant_event">>}
                                  ]).
-define(PARTICIPANT_EVENT_TYPES, []).

%% Conference Event
-define(CONFERENCE_EVENT_KEY(Event, AccountId, ConferenceId, CallId)
       ,list_to_binary([Event, "."
                       ,AccountId, "."
                       ,ConferenceId, "."
                       ,kz_amqp_util:encode(CallId)
                       ])
       ).
-define(CONFERENCE_EVENT_HEADERS, [<<"Event">>
                                  ,<<"Conference-ID">>
                                  ,<<"Instance-ID">>
                                  ]).
-define(OPTIONAL_CONFERENCE_EVENT_HEADERS, [<<"Call-ID">>
                                           ,<<"Participant-ID">>
                                           ,<<"Caller-ID-Name">>
                                           ,<<"Caller-ID-Number">>
                                           ,<<"Channel-Presence-ID">>
                                           ,<<"Custom-Channel-Vars">>
                                           ,<<"Conference-Channel-Vars">>
                                           ]).
-define(CONFERENCE_EVENT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"event">>}
                                 ]).
-define(CONFERENCE_EVENT_TYPES, []).

%% Conference Error
-define(CONFERENCE_ERROR_HEADERS, [<<"Error-Message">>, <<"Request">>, <<"Conference-ID">>]).
-define(OPTIONAL_CONFERENCE_ERROR_HEADERS, []).
-define(CONFERENCE_ERROR_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                 ,{<<"Event-Name">>, <<"error">>}
                                 ]).
-define(CONFERENCE_ERROR_TYPES, []).

-define(CONFIG_REQ_HEADERS, [<<"Request">>, <<"Profile">>]).
-define(OPTIONAL_CONFIG_REQ_HEADERS, [<<"Call-ID">>
                                     ,<<"Conference-ID">>
                                     ,<<"Controls">>
                                     ]).
-define(CONFIG_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                           ,{<<"Event-Name">>, <<"config_req">>}
                           ]).
-define(CONFIG_REQ_TYPES, []).

-define(CONFIG_RESP_HEADERS, [[<<"Profiles">>, <<"Caller-Controls">>]]).
-define(OPTIONAL_CONFIG_RESP_HEADERS, [<<"Advertise">>, <<"Chat-Permissions">>]).
-define(CONFIG_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                            ,{<<"Event-Name">>, <<"config_resp">>}
                            ]).
-define(CONFIG_RESP_TYPES, []).

-define(APPLICTION_MAP, [{<<"deaf_participant">>, ?DEAF_PARTICIPANT_VALUES, fun deaf_participant/1}
                        ,{<<"participant_energy">>, ?PARTICIPANT_ENERGY_VALUES, fun participant_energy/1}
                        ,{<<"kick">>, ?KICK_VALUES, fun kick/1}
                        ,{<<"participants">>, ?PARTICIPANTS_REQ_VALUES, fun participants_req/1}
                        ,{<<"lock">>, ?LOCK_VALUES, fun lock/1}
                        ,{<<"mute_participant">>, ?MUTE_PARTICIPANT_VALUES, fun mute_participant/1}
                        ,{<<"play">>, ?PLAY_VALUES, fun play/1}
                        ,{<<"record">>, ?RECORD_VALUES, fun record/1}
                        ,{<<"recordstop">>, ?RECORDSTOP_VALUES, fun recordstop/1}
                        ,{<<"relate_participants">>, ?RELATE_PARTICIPANTS_VALUES, fun relate_participants/1}
                        ,{<<"stop_play">>, ?STOP_PLAY_VALUES, fun stop_play/1}
                        ,{<<"undeaf_participant">>, ?UNDEAF_PARTICIPANT_VALUES, fun undeaf_participant/1}
                        ,{<<"unlock">>, ?UNLOCK_VALUES, fun unlock/1}
                        ,{<<"unmute_participant">>, ?UNMUTE_PARTICIPANT_VALUES, fun unmute_participant/1}
                        ,{<<"participant_volume_in">>, ?PARTICIPANT_VOLUME_IN_VALUES, fun participant_volume_in/1}
                        ,{<<"participant_volume_out">>, ?PARTICIPANT_VOLUME_OUT_VALUES, fun participant_volume_out/1}
                        ,{<<"dial">>, ?DIAL_VALUES, fun dial/1}
                        ,{<<"dial_resp">>, ?DIAL_RESP_VALUES, fun dial_resp/1}
                        ,{<<"tones">>, ?CONF_TONES_REQ_VALUES, fun tones/1}
                        ,{<<"say">>, ?CONF_SAY_REQ_VALUES, fun say/1}
                        ,{<<"tts">>, ?CONF_SAY_REQ_VALUES, fun tts/1}
                        ,{<<"play_macro">>, ?CONF_PLAY_MACRO_REQ_VALUES, fun play_macro_req/1}
                        ]).

-define(CONF_PLAY_MACRO_REQ_HEADERS, [<<"Application-Name">>
                                     ,<<"Conference-ID">>
                                     ,<<"Media-Macro">>
                                     ]).
-define(OPTIONAL_CONF_PLAY_MACRO_REQ_HEADERS, []).
-define(CONF_PLAY_MACRO_REQ_VALUES, []).
-define(CONF_PLAY_MACRO_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

-spec focus_queue_name(atom()) -> kz_term:ne_binary().
focus_queue_name(Focus) -> <<(kz_term:to_binary(Focus))/binary, "_conference">>.

%%------------------------------------------------------------------------------
%% @doc Create a tone on the channel.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-define(CONF_SAY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                             ,{<<"Application-Name">>, [<<"say">>, <<"tts">>]}
                              | props:delete_keys([<<"Event-Category">>
                                                  ,<<"Application-Name">>
                                                  ]
                                                 ,?TONES_REQ_VALUES
                                                 )
                             ]).
-spec say(kz_term:api_terms()) -> api_formatter_return() .
say(Prop) when is_list(Prop) ->
    case say_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?TTS_REQ_HEADERS, ?OPTIONAL_TTS_REQ_HEADERS);
        'false' -> {'error', "Prop failed validation for say_req"}
    end;
say(JObj) -> say(kz_json:to_proplist(JObj)).

-spec tts(kz_term:api_terms()) -> api_formatter_return().
tts(API) -> say(API).

-spec say_v(kz_term:api_terms()) -> boolean().
say_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?TTS_REQ_HEADERS, ?CONF_SAY_REQ_VALUES, ?TTS_REQ_TYPES);
say_v(JObj) -> say_v(kz_json:to_proplist(JObj)).

-spec tts_v(kz_term:api_terms()) -> boolean().
tts_v(API) -> say_v(API).

%%------------------------------------------------------------------------------
%% @doc Create a tone on the channel.
%% Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------

-define(CONF_TONES_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                | props:delete(<<"Event-Category">>, ?TONES_REQ_VALUES)
                               ]).
-spec tones(kz_term:api_terms()) -> api_formatter_return() .
tones(Prop) when is_list(Prop) ->
    Tones = [begin
                 {'ok', TonesProp} = kapi_dialplan:tones_req_tone_headers(Tone),
                 kz_json:from_list(TonesProp)
             end
             || Tone <- props:get_value(<<"Tones">>, Prop, []),
                kapi_dialplan:tones_req_tone_v(Tone)
            ],
    Prop1 = [ {<<"Tones">>, Tones} | props:delete(<<"Tones">>, Prop)],
    case tones_v(Prop1) of
        'true' -> kz_api:build_message(Prop1, ?TONES_REQ_HEADERS, ?OPTIONAL_TONES_REQ_HEADERS);
        'false' -> {'error', "Prop failed validation for tones_req"}
    end;
tones(JObj) -> tones(kz_json:to_proplist(JObj)).

-spec tones_v(kz_term:api_terms()) -> boolean().
tones_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?TONES_REQ_HEADERS, ?CONF_TONES_REQ_VALUES, ?TONES_REQ_TYPES);
tones_v(JObj) -> tones_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec search_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_req(Prop) when is_list(Prop) ->
    case search_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_REQ_HEADERS, ?OPTIONAL_SEARCH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for search request"}
    end;
search_req(JObj) -> search_req(kz_json:to_proplist(JObj)).

-spec search_req_v(kz_term:api_terms()) -> boolean().
search_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_VALUES, ?SEARCH_REQ_TYPES);
search_req_v(JObj) -> search_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec search_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_resp(Prop) when is_list(Prop) ->
    case search_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SEARCH_RESP_HEADERS, ?OPTIONAL_SEARCH_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search response"}
    end;
search_resp(JObj) -> search_resp(kz_json:to_proplist(JObj)).

-spec search_resp_v(kz_term:api_terms()) -> boolean().
search_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_VALUES, ?SEARCH_RESP_TYPES);
search_resp_v(JObj) -> search_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec discovery_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
discovery_req(Prop) when is_list(Prop) ->
    case discovery_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DISCOVERY_REQ_HEADERS, ?OPTIONAL_DISCOVERY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery"}
    end;
discovery_req(JObj) -> discovery_req(kz_json:to_proplist(JObj)).

-spec discovery_req_v(kz_term:api_terms()) -> boolean().
discovery_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DISCOVERY_REQ_HEADERS, ?DISCOVERY_REQ_VALUES, ?DISCOVERY_REQ_TYPES);
discovery_req_v(JObj) -> discovery_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec discovery_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
discovery_resp(Prop) when is_list(Prop) ->
    case discovery_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DISCOVERY_RESP_HEADERS, ?OPTIONAL_DISCOVERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery resp"}
    end;
discovery_resp(JObj) -> discovery_resp(kz_json:to_proplist(JObj)).

-spec discovery_resp_v(kz_term:api_terms()) -> boolean().
discovery_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DISCOVERY_RESP_HEADERS, ?DISCOVERY_RESP_VALUES, ?DISCOVERY_RESP_TYPES);
discovery_resp_v(JObj) -> discovery_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec add_participant(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
add_participant(Prop) when is_list(Prop) ->
    case add_participant_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ADD_PARTICIPANT_HEADERS, ?OPTIONAL_ADD_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for add participant"}
    end;
add_participant(JObj) -> add_participant(kz_json:to_proplist(JObj)).

-spec add_participant_v(kz_term:api_terms()) -> boolean().
add_participant_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ADD_PARTICIPANT_HEADERS, ?ADD_PARTICIPANT_VALUES, ?ADD_PARTICIPANT_TYPES);
add_participant_v(JObj) -> add_participant_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec deaf_participant(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
deaf_participant(Prop) when is_list(Prop) ->
    case deaf_participant_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DEAF_PARTICIPANT_HEADERS, ?OPTIONAL_DEAF_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for deaf participant"}
    end;
deaf_participant(JObj) -> deaf_participant(kz_json:to_proplist(JObj)).

-spec deaf_participant_v(kz_term:api_terms()) -> boolean().
deaf_participant_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DEAF_PARTICIPANT_HEADERS, ?DEAF_PARTICIPANT_VALUES, ?DEAF_PARTICIPANT_TYPES);
deaf_participant_v(JObj) -> deaf_participant_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec participant_energy(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_energy(Prop) when is_list(Prop) ->
    case participant_energy_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PARTICIPANT_ENERGY_HEADERS, ?OPTIONAL_PARTICIPANT_ENERGY_HEADERS);
        'false' -> {'error', "Proplist failed validation for participant energy"}
    end;
participant_energy(JObj) -> participant_energy(kz_json:to_proplist(JObj)).

-spec participant_energy_v(kz_term:api_terms()) -> boolean().
participant_energy_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PARTICIPANT_ENERGY_HEADERS, ?PARTICIPANT_ENERGY_VALUES, ?PARTICIPANT_ENERGY_TYPES);
participant_energy_v(JObj) -> participant_energy_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec kick(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
kick(Prop) when is_list(Prop) ->
    case kick_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?KICK_HEADERS, ?OPTIONAL_KICK_HEADERS);
        'false' -> {'error', "Proplist failed validation for kick"}
    end;
kick(JObj) -> kick(kz_json:to_proplist(JObj)).

-spec kick_v(kz_term:api_terms()) -> boolean().
kick_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?KICK_HEADERS, ?KICK_VALUES, ?KICK_TYPES);
kick_v(JObj) -> kick_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec participants_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
participants_req(Prop) when is_list(Prop) ->
    case participants_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PARTICIPANTS_REQ_HEADERS, ?OPTIONAL_PARTICIPANTS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for participants request"}
    end;
participants_req(JObj) -> participants_req(kz_json:to_proplist(JObj)).

-spec participants_req_v(kz_term:api_terms()) -> boolean().
participants_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PARTICIPANTS_REQ_HEADERS, ?PARTICIPANTS_REQ_VALUES, ?PARTICIPANTS_REQ_TYPES);
participants_req_v(JObj) -> participants_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec participants_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
participants_resp(Prop) when is_list(Prop) ->
    case participants_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PARTICIPANTS_RESP_HEADERS, ?OPTIONAL_PARTICIPANTS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for participants response"}
    end;
participants_resp(JObj) -> participants_resp(kz_json:to_proplist(JObj)).

-spec participants_resp_v(kz_term:api_terms()) -> boolean().
participants_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PARTICIPANTS_RESP_HEADERS, ?PARTICIPANTS_RESP_VALUES, ?PARTICIPANTS_RESP_TYPES);
participants_resp_v(JObj) -> participants_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec lock(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
lock(Prop) when is_list(Prop) ->
    case lock_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?LOCK_HEADERS, ?OPTIONAL_LOCK_HEADERS);
        'false' -> {'error', "Proplist failed validation for lock"}
    end;
lock(JObj) -> lock(kz_json:to_proplist(JObj)).

-spec lock_v(kz_term:api_terms()) -> boolean().
lock_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?LOCK_HEADERS, ?LOCK_VALUES, ?LOCK_TYPES);
lock_v(JObj) -> lock_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec mute_participant(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
mute_participant(Prop) when is_list(Prop) ->
    case mute_participant_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MUTE_PARTICIPANT_HEADERS, ?OPTIONAL_MUTE_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for mute participant"}
    end;
mute_participant(JObj) -> mute_participant(kz_json:to_proplist(JObj)).

-spec mute_participant_v(kz_term:api_terms()) -> boolean().
mute_participant_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MUTE_PARTICIPANT_HEADERS, ?MUTE_PARTICIPANT_VALUES, ?MUTE_PARTICIPANT_TYPES);
mute_participant_v(JObj) -> mute_participant_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec play(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
play(Prop) when is_list(Prop) ->
    case play_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PLAY_HEADERS, ?OPTIONAL_PLAY_HEADERS);
        'false' -> {'error', "Proplist failed validation for play"}
    end;
play(JObj) -> play(kz_json:to_proplist(JObj)).

-spec play_v(kz_term:api_terms()) -> boolean().
play_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PLAY_HEADERS, ?PLAY_VALUES, ?PLAY_TYPES);
play_v(JObj) -> play_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec play_macro_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
play_macro_req(Prop) when is_list(Prop) ->
    case play_macro_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONF_PLAY_MACRO_REQ_HEADERS, ?OPTIONAL_CONF_PLAY_MACRO_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for play_macro_req"}
    end;
play_macro_req(JObj) -> play_macro_req(kz_json:to_proplist(JObj)).

-spec play_macro_req_v(kz_term:api_terms()) -> boolean().
play_macro_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONF_PLAY_MACRO_REQ_HEADERS, ?CONF_PLAY_MACRO_REQ_VALUES, ?CONF_PLAY_MACRO_REQ_TYPES);
play_macro_req_v(JObj) -> play_macro_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec record(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
record(Prop) when is_list(Prop) ->
    case record_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RECORD_HEADERS, ?OPTIONAL_RECORD_HEADERS);
        'false' -> {'error', "Proplist failed validation for record"}
    end;
record(JObj) -> record(kz_json:to_proplist(JObj)).

-spec record_v(kz_term:api_terms()) -> boolean().
record_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RECORD_HEADERS, ?RECORD_VALUES, ?RECORD_TYPES);
record_v(JObj) -> record_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec recordstop(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
recordstop(Prop) when is_list(Prop) ->
    case recordstop_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RECORDSTOP_HEADERS, ?OPTIONAL_RECORDSTOP_HEADERS);
        'false' -> {'error', "Proplist failed validation for recordstop"}
    end;
recordstop(JObj) -> recordstop(kz_json:to_proplist(JObj)).

-spec recordstop_v(kz_term:api_terms()) -> boolean().
recordstop_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RECORDSTOP_HEADERS, ?RECORDSTOP_VALUES, ?RECORDSTOP_TYPES);
recordstop_v(JObj) -> recordstop_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec relate_participants(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
relate_participants(Prop) when is_list(Prop) ->
    case relate_participants_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?RELATE_PARTICIPANTS_HEADERS, ?OPTIONAL_RELATE_PARTICIPANTS_HEADERS);
        'false' -> {'error', "Proplist failed validation for relate participants"}
    end;
relate_participants(JObj) -> relate_participants(kz_json:to_proplist(JObj)).

-spec relate_participants_v(kz_term:api_terms()) -> boolean().
relate_participants_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?RELATE_PARTICIPANTS_HEADERS, ?RELATE_PARTICIPANTS_VALUES, ?RELATE_PARTICIPANTS_TYPES);
relate_participants_v(JObj) -> relate_participants_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec set(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
set(Prop) when is_list(Prop) ->
    case set_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SET_HEADERS, ?OPTIONAL_SET_HEADERS);
        'false' -> {'error', "Proplist failed validation for set"}
    end;
set(JObj) -> set(kz_json:to_proplist(JObj)).

-spec set_v(kz_term:api_terms()) -> boolean().
set_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SET_HEADERS, ?SET_VALUES, ?SET_TYPES);
set_v(JObj) -> set_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec stop_play(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
stop_play(Prop) when is_list(Prop) ->
    case stop_play_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?STOP_PLAY_HEADERS, ?OPTIONAL_STOP_PLAY_HEADERS);
        'false' -> {'error', "Proplist failed validation for stop play"}
    end;
stop_play(JObj) -> stop_play(kz_json:to_proplist(JObj)).

-spec stop_play_v(kz_term:api_terms()) -> boolean().
stop_play_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?STOP_PLAY_HEADERS, ?STOP_PLAY_VALUES, ?STOP_PLAY_TYPES);
stop_play_v(JObj) -> stop_play_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec undeaf_participant(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
undeaf_participant(Prop) when is_list(Prop) ->
    case undeaf_participant_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UNDEAF_PARTICIPANT_HEADERS, ?OPTIONAL_UNDEAF_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for undeaf participant"}
    end;
undeaf_participant(JObj) -> undeaf_participant(kz_json:to_proplist(JObj)).

-spec undeaf_participant_v(kz_term:api_terms()) -> boolean().
undeaf_participant_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UNDEAF_PARTICIPANT_HEADERS, ?UNDEAF_PARTICIPANT_VALUES, ?UNDEAF_PARTICIPANT_TYPES);
undeaf_participant_v(JObj) -> undeaf_participant_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec unlock(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
unlock(Prop) when is_list(Prop) ->
    case unlock_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UNLOCK_HEADERS, ?OPTIONAL_UNLOCK_HEADERS);
        'false' -> {'error', "Proplist failed validation for unlock"}
    end;
unlock(JObj) -> unlock(kz_json:to_proplist(JObj)).

-spec unlock_v(kz_term:api_terms()) -> boolean().
unlock_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UNLOCK_HEADERS, ?UNLOCK_VALUES, ?UNLOCK_TYPES);
unlock_v(JObj) -> unlock_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec unmute_participant(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
unmute_participant(Prop) when is_list(Prop) ->
    case unmute_participant_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?UNMUTE_PARTICIPANT_HEADERS, ?OPTIONAL_UNMUTE_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for unmute participant"}
    end;
unmute_participant(JObj) -> unmute_participant(kz_json:to_proplist(JObj)).

-spec unmute_participant_v(kz_term:api_terms()) -> boolean().
unmute_participant_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?UNMUTE_PARTICIPANT_HEADERS, ?UNMUTE_PARTICIPANT_VALUES, ?UNMUTE_PARTICIPANT_TYPES);
unmute_participant_v(JObj) -> unmute_participant_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec participant_volume_in(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_volume_in(Prop) when is_list(Prop) ->
    case participant_volume_in_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PARTICIPANT_VOLUME_IN_HEADERS, ?OPTIONAL_PARTICIPANT_VOLUME_IN_HEADERS);
        'false' -> {'error', "Proplist failed validation for volume in"}
    end;
participant_volume_in(JObj) -> participant_volume_in(kz_json:to_proplist(JObj)).

-spec participant_volume_in_v(kz_term:api_terms()) -> boolean().
participant_volume_in_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PARTICIPANT_VOLUME_IN_HEADERS, ?PARTICIPANT_VOLUME_IN_VALUES, ?PARTICIPANT_VOLUME_IN_TYPES);
participant_volume_in_v(JObj) -> participant_volume_in_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec participant_volume_out(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_volume_out(Prop) when is_list(Prop) ->
    case participant_volume_out_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PARTICIPANT_VOLUME_OUT_HEADERS, ?OPTIONAL_PARTICIPANT_VOLUME_OUT_HEADERS);
        'false' -> {'error', "Proplist failed validation for volume out"}
    end;
participant_volume_out(JObj) -> participant_volume_out(kz_json:to_proplist(JObj)).

-spec participant_volume_out_v(kz_term:api_terms()) -> boolean().
participant_volume_out_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PARTICIPANT_VOLUME_OUT_HEADERS, ?PARTICIPANT_VOLUME_OUT_VALUES, ?PARTICIPANT_VOLUME_OUT_TYPES);
participant_volume_out_v(JObj) -> participant_volume_out_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec participant_event(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_event(Prop) when is_list(Prop) ->
    case participant_event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PARTICIPANT_EVENT_HEADERS, ?OPTIONAL_PARTICIPANT_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for participant_event response"}
    end;
participant_event(JObj) -> participant_event(kz_json:to_proplist(JObj)).

-spec participant_event_v(kz_term:api_terms()) -> boolean().
participant_event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PARTICIPANT_EVENT_HEADERS, ?PARTICIPANT_EVENT_VALUES, ?PARTICIPANT_EVENT_TYPES);
participant_event_v(JObj) -> participant_event_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec event(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
event(Prop) when is_list(Prop) ->
    case event_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONFERENCE_EVENT_HEADERS, ?OPTIONAL_CONFERENCE_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for conference event"}
    end;
event(JObj) -> event(kz_json:to_proplist(JObj)).

-spec event_v(kz_term:api_terms()) -> boolean().
event_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONFERENCE_EVENT_HEADERS, ?CONFERENCE_EVENT_VALUES, ?CONFERENCE_EVENT_TYPES);
event_v(JObj) -> event_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec conference_error(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
conference_error(Prop) when is_list(Prop) ->
    case conference_error_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONFERENCE_ERROR_HEADERS, ?OPTIONAL_CONFERENCE_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for conference error"}
    end;
conference_error(JObj) -> conference_error(kz_json:to_proplist(JObj)).

-spec conference_error_v(kz_term:api_terms()) -> boolean().
conference_error_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONFERENCE_ERROR_HEADERS, ?CONFERENCE_ERROR_VALUES, ?CONFERENCE_ERROR_TYPES);
conference_error_v(JObj) -> conference_error_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec config_req(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
config_req(Prop) when is_list(Prop) ->
    case config_req_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONFIG_REQ_HEADERS, ?OPTIONAL_CONFIG_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for config req"}
    end;
config_req(JObj) -> config_req(kz_json:to_proplist(JObj)).

-spec config_req_v(kz_term:api_terms()) -> boolean().
config_req_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONFIG_REQ_HEADERS, ?CONFIG_REQ_VALUES, ?CONFIG_REQ_TYPES);
config_req_v(JObj) -> config_req_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Takes {@link kz_term:proplist()}, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec config_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
config_resp(Prop) when is_list(Prop) ->
    case config_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CONFIG_RESP_HEADERS, ?OPTIONAL_CONFIG_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for config resp"}
    end;
config_resp(JObj) -> config_resp(kz_json:to_proplist(JObj)).

-spec config_resp_v(kz_term:api_terms()) -> boolean().
config_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CONFIG_RESP_HEADERS, ?CONFIG_RESP_VALUES, ?CONFIG_RESP_TYPES);
config_resp_v(JObj) -> config_resp_v(kz_json:to_proplist(JObj)).

-spec dial(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
dial(Prop) when is_list(Prop) ->
    case dial_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DIAL_HEADERS, ?OPTIONAL_DIAL_HEADERS);
        'false' -> {'error', "Proplist failed validation for dial"}
    end;
dial(JObj) -> dial(kz_json:to_proplist(JObj)).

-spec dial_v(kz_term:api_terms()) -> boolean().
dial_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DIAL_HEADERS, ?DIAL_VALUES, ?DIAL_TYPES);
dial_v(JObj) -> dial_v(kz_json:to_proplist(JObj)).

-spec dial_resp(kz_term:api_terms()) -> {'ok', iolist()} | {'error', string()}.
dial_resp(Prop) when is_list(Prop) ->
    case dial_resp_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DIAL_RESP_HEADERS, ?OPTIONAL_DIAL_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for dial_resp"}
    end;
dial_resp(JObj) -> dial_resp(kz_json:to_proplist(JObj)).

-spec dial_resp_v(kz_term:api_terms()) -> boolean().
dial_resp_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DIAL_RESP_HEADERS, ?DIAL_RESP_VALUES, ?DIAL_RESP_TYPES);
dial_resp_v(JObj) -> dial_resp_v(kz_json:to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc Bind a queue to the conference exchange.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Q, 'undefined', _) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'discovery'),
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'command'),
    kz_amqp_util:bind_q_to_conference(Q, 'event');
bind_to_q(Q, ['discovery'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'discovery'),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['command'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'command'),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['event'|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'event', <<"#">>),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['config'|T], Props) ->
    Profile = props:get_value('profile', Props, <<"*">>),
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'config', Profile),
    bind_to_q(Q, T, Props);
bind_to_q(Q, [{'config', Profile}|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'config', Profile),
    bind_to_q(Q, T, Props);
bind_to_q(Q, [{'event', {_ConfId, _CallId}=Key}|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'event', event_binding_key(Key)),
    bind_to_q(Q, T, Props);
bind_to_q(Q, [{'event', ConfIdOrProps}|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'event', event_binding_key(ConfIdOrProps)),
    bind_to_q(Q, T, Props);
bind_to_q(Q, [{'command', ConfId}|T], Props) ->
    'ok' = kz_amqp_util:bind_q_to_conference(Q, 'command', ConfId),
    bind_to_q(Q, T, Props);
bind_to_q(_Q, [], _) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Unbind a queue from the conference exchange.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_from_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_from_q(Q, 'undefined', _) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'discovery'),
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'command'),
    kz_amqp_util:unbind_q_from_conference(Q, 'event');
unbind_from_q(Q, ['discovery'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'discovery'),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, ['command'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'command'),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, ['event'|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'event', <<"#">>),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, ['config'|T], Props) ->
    Profile = props:get_value('profile', Props, <<"*">>),
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'config', Profile),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, [{'config', Profile}|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'config', Profile),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, [{'event', {_ConfId, _CallId}=Key}|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'event', event_binding_key(Key)),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, [{'event', ConfIdOrProps}|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'event', event_binding_key(ConfIdOrProps)),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, [{'command', ConfId}|T], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_conference(Q, 'command', ConfId),
    unbind_from_q(Q, T, Props);
unbind_from_q(_Q, [], _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:conference_exchange().

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_search_req(kz_term:api_terms()) -> 'ok'.
publish_search_req(JObj) ->
    publish_search_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_search_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun search_req/1),
    kz_amqp_util:conference_publish(Payload, 'discovery', 'undefined', [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_search_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_search_resp(Queue, Resp) ->
    publish_search_resp(Queue, Resp, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_search_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun search_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_discovery_req(kz_term:api_terms()) -> 'ok'.
publish_discovery_req(JObj) ->
    publish_discovery_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_discovery_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_discovery_req(Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DISCOVERY_REQ_VALUES, fun discovery_req/1),
    kz_amqp_util:conference_publish(Payload, 'discovery', 'undefined', [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish the response to requestor.
%% @end
%%------------------------------------------------------------------------------

-spec publish_discovery_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_discovery_resp(Q, JObj) ->
    publish_discovery_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_discovery_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_discovery_resp(Q, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DISCOVERY_RESP_VALUES, fun discovery_resp/1),
    kz_amqp_util:targeted_publish(Q, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_add_participant(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_add_participant(Zone, JObj) ->
    publish_add_participant(Zone, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_add_participant(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_add_participant(Zone, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?ADD_PARTICIPANT_VALUES, fun add_participant/1),
    kz_amqp_util:conference_publish(Payload, 'command', Zone, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_deaf_participant(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_deaf_participant(ConferenceId, JObj) ->
    publish_deaf_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_deaf_participant(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_deaf_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DEAF_PARTICIPANT_VALUES, fun deaf_participant/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_participant_energy(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_participant_energy(ConferenceId, JObj) ->
    publish_participant_energy(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_participant_energy(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_participant_energy(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PARTICIPANT_ENERGY_VALUES, fun participant_energy/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_kick(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_kick(ConferenceId, JObj) ->
    publish_kick(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_kick(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_kick(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?KICK_VALUES, fun kick/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_participants_req(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_participants_req(ConferenceId, JObj) ->
    publish_participants_req(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_participants_req(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_participants_req(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PARTICIPANTS_REQ_VALUES, fun participants_req/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_participants_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_participants_resp(Queue, Resp) ->
    publish_participants_resp(Queue, Resp, ?DEFAULT_CONTENT_TYPE).

-spec publish_participants_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_participants_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Resp, ?PARTICIPANTS_RESP_VALUES, fun participants_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_lock(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_lock(ConferenceId, JObj) ->
    publish_lock(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_lock(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_lock(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?LOCK_VALUES, fun lock/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_mute_participant(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_mute_participant(ConferenceId, JObj) ->
    publish_mute_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_mute_participant(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_mute_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?MUTE_PARTICIPANT_VALUES, fun mute_participant/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_play(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_play(ConferenceId, JObj) ->
    publish_play(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_play(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_play(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PLAY_VALUES, fun play/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_record(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_record(ConferenceId, JObj) ->
    publish_record(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_record(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_record(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RECORD_VALUES, fun record/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_recordstop(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_recordstop(ConferenceId, JObj) ->
    publish_recordstop(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_recordstop(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_recordstop(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RECORDSTOP_VALUES, fun recordstop/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_relate_participants(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_relate_participants(ConferenceId, JObj) ->
    publish_relate_participants(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_relate_participants(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_relate_participants(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?RELATE_PARTICIPANTS_VALUES, fun relate_participants/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_set(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_set(ConferenceId, JObj) ->
    publish_set(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_set(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_set(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?SET_VALUES, fun set/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_stop_play(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_stop_play(ConferenceId, JObj) ->
    publish_stop_play(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_stop_play(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_stop_play(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?STOP_PLAY_VALUES, fun stop_play/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_undeaf_participant(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_undeaf_participant(ConferenceId, JObj) ->
    publish_undeaf_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_undeaf_participant(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_undeaf_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?UNDEAF_PARTICIPANT_VALUES, fun undeaf_participant/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_unlock(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_unlock(ConferenceId, JObj) ->
    publish_unlock(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_unlock(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_unlock(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?UNLOCK_VALUES, fun unlock/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_unmute_participant(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_unmute_participant(ConferenceId, JObj) ->
    publish_unmute_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_unmute_participant(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_unmute_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?UNMUTE_PARTICIPANT_VALUES, fun unmute_participant/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_participant_volume_in(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_participant_volume_in(ConferenceId, JObj) ->
    publish_participant_volume_in(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_participant_volume_in(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_participant_volume_in(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PARTICIPANT_VOLUME_IN_VALUES, fun participant_volume_in/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_participant_volume_out(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_participant_volume_out(ConferenceId, JObj) ->
    publish_participant_volume_out(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_participant_volume_out(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_participant_volume_out(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?PARTICIPANT_VOLUME_OUT_VALUES, fun participant_volume_out/1),
    kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_participant_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_participant_event(ConferenceId, CallId, JObj) ->
    publish_participant_event(ConferenceId, CallId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_participant_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_participant_event(ConferenceId, CallId, Event, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Event, ?PARTICIPANT_EVENT_VALUES, fun participant_event/1),
    kz_amqp_util:conference_publish(Payload, 'event', ConferenceId, kz_amqp_util:encode(CallId), [], ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_event(kz_term:api_terms()) -> 'ok'.
publish_event(API) ->
    publish_event(API, ?DEFAULT_CONTENT_TYPE).

-spec publish_event(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_event(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?CONFERENCE_EVENT_VALUES, fun event/1),
    kz_amqp_util:conference_publish(Payload, 'event', event_key(API), [], ContentType).

event_binding_key(ConferenceId)
  when is_binary(ConferenceId) ->
    ?CONFERENCE_EVENT_KEY(<<"*">>, <<"*">>, ConferenceId, <<"*">>);
event_binding_key({ConferenceId, CallId}) ->
    ?CONFERENCE_EVENT_KEY(<<"*">>, <<"*">>, ConferenceId, CallId);
event_binding_key(Props) ->
    Event = props:get_value('event', Props, <<"*">>),
    AccountId = props:get_value('account_id', Props, <<"*">>),
    ConferenceId = props:get_value('conference_id', Props, <<"*">>),
    CallId = props:get_value('call_id', Props, <<"*">>),
    ?CONFERENCE_EVENT_KEY(Event, AccountId, ConferenceId, CallId).

event_key(API)
  when is_list(API) ->
    event_key(kz_json:from_list(API));
event_key(API) ->
    Event = kz_json:get_ne_binary_value(<<"Event">>, API),
    AccountId = kz_json:get_first_defined([<<"Account-ID">>
                                          ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                                          ]
                                         ,API
                                         ),
    ConferenceId = kz_json:get_value(<<"Conference-ID">>, API),
    CallId = kz_json:get_value(<<"Call-ID">>, API, ConferenceId),
    ?CONFERENCE_EVENT_KEY(Event, AccountId, ConferenceId, CallId).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_error(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_error(Queue, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CONFERENCE_ERROR_VALUES, fun conference_error/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_command(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_command(ConferenceId, JObj) ->
    publish_command(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_command(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_command(ConferenceId, Req, ContentType) ->
    App = props:get_value(<<"Application-Name">>, Req),
    case lists:keyfind(App, 1, ?APPLICTION_MAP) of
        'false' -> throw({'invalid_conference_command', Req});
        {_, Values, Fun} ->
            {'ok', Payload} = kz_api:prepare_api_payload(Req, Values, Fun),
            kz_amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType)
    end.

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_targeted_command(atom(), kz_term:api_terms()) -> 'ok'.
publish_targeted_command(Focus, JObj) ->
    publish_targeted_command(Focus, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_targeted_command(atom(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_targeted_command(Focus, Req, ContentType) ->
    App = props:get_value(<<"Application-Name">>, Req),
    case lists:keyfind(App, 1, ?APPLICTION_MAP) of
        'false' -> throw({'invalid_conference_command', Req});
        {_, Values, Fun} ->
            {'ok', Payload} = kz_api:prepare_api_payload(Req, Values, Fun),
            Queue = focus_queue_name(Focus),
            kz_amqp_util:targeted_publish(Queue, Payload, ContentType)
    end.

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_config_req(kz_term:api_terms()) -> 'ok'.
publish_config_req(JObj) ->
    publish_config_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_config_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_config_req(Req, ContentType) ->
    Profile = profile(Req),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CONFIG_REQ_VALUES, fun config_req/1),
    kz_amqp_util:conference_publish(Payload, 'config', Profile, [], ContentType).

profile(Props) when is_list(Props) -> props:get_value(<<"Profile">>, Props);
profile(JObj) -> kz_json:get_value(<<"Profile">>, JObj).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_config_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_config_resp(Queue, JObj) ->
    publish_config_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_config_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_config_resp(Queue, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?CONFIG_RESP_VALUES, fun config_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Publish to the conference exchange.
%% @end
%%------------------------------------------------------------------------------

-spec publish_dial(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_dial(Zone, JObj) ->
    publish_dial(Zone, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_dial(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_dial(Zone, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DIAL_VALUES, fun dial/1),
    kz_amqp_util:conference_publish(Payload, 'command', Zone, [], ContentType).

-spec publish_dial_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_dial_resp(Queue, JObj) ->
    publish_dial_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_dial_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_dial_resp(Queue, Req, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(Req, ?DIAL_RESP_VALUES, fun dial_resp/1),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).
