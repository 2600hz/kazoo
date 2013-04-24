%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_conference).

-export([focus_queue_name/1]).
-export([search_req/1, search_req_v/1]).
-export([search_resp/1, search_resp_v/1]).
-export([discovery_req/1, discovery_req_v/1]).
-export([discovery_resp/1, discovery_resp_v/1]).
-export([deaf_participant/1, deaf_participant_v/1]).
-export([participant_energy/1, participant_energy_v/1]).
-export([kick/1, kick_v/1]).
-export([hup/1, hup_v/1]).
-export([participants_req/1, participants_req_v/1]).
-export([participants_resp/1, participants_resp_v/1]).
-export([lock/1, lock_v/1]).
-export([mute_participant/1, mute_participant_v/1]).
-export([play/1, play_v/1
         ,tones/1, tones_v/1
         ,say/1, say_v/1, tts/1, tts_v/1
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
-export([participants_event/1, participants_event_v/1]).
-export([conference_error/1, conference_error_v/1]).
-export([config_req/1, config_req_v/1
         ,config_resp/1, config_resp_v/1
        ]).

-export([bind_q/2, unbind_q/2]).

-export([publish_search_req/1, publish_search_req/2]).
-export([publish_search_resp/2, publish_search_resp/3]).
-export([publish_discovery_req/1, publish_discovery_req/2]).
-export([publish_discovery_resp/2, publish_discovery_resp/3]).
-export([publish_deaf_participant/2, publish_deaf_participant/3]).
-export([publish_participant_energy/2, publish_participant_energy/3]).
-export([publish_kick/2, publish_kick/3]).
-export([publish_hup/2, publish_hup/3]).
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
-export([publish_participants_event/2, publish_participants_event/3]).
-export([publish_command/2, publish_command/3]).
-export([publish_targeted_command/2, publish_targeted_command/3]).
-export([publish_config_req/1, publish_config_req/2
         ,publish_config_resp/2, publish_config_resp/3
        ]).

-include_lib("whistle/include/wh_api.hrl").
-include("wapi_dialplan.hrl").

%% Conference Search Request
-define(SEARCH_REQ_HEADERS, [<<"Conference-ID">>]).
-define(OPTIONAL_SEARCH_REQ_HEADERS, []).
-define(SEARCH_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                            ,{<<"Event-Name">>, <<"search_req">>}
                           ]).
-define(SEARCH_REQ_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Search Response
-define(SEARCH_RESP_HEADERS, [<<"Conference-ID">>, <<"Participant-Count">>]).
-define(OPTIONAL_SEARCH_RESP_HEADERS, [<<"UUID">>, <<"Running">>, <<"Answered">>, <<"Dynamic">>
                                       ,<<"Run-Time">>, <<"Participants">>, <<"Locked">>
                                       ,<<"Switch-Hostname">>, <<"Switch-URL">>, <<"Switch-External-IP">>
                                      ]).
-define(SEARCH_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                             ,{<<"Event-Name">>, <<"search_resp">>}
                            ]).
-define(SEARCH_RESP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Discovery Request
-define(DISCOVERY_REQ_HEADERS, [<<"Call">>]).
-define(OPTIONAL_DISCOVERY_REQ_HEADERS, [<<"Conference-ID">>, <<"Moderator">>
                                         ,<<"Conference-Doc">> % ad-hoc conferencing
                                        ]).
-define(DISCOVERY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                               ,{<<"Event-Name">>, <<"discovery_req">>}
                              ]).
-define(DISCOVERY_REQ_TYPES, [{<<"Moderator">>, fun wh_util:is_boolean/1}]).

%% Conference Discovery Request
-define(DISCOVERY_RESP_HEADERS, [<<"Participant-ID">>]).
-define(OPTIONAL_DISCOVERY_RESP_HEADERS, [<<"Conference-ID">>]).
-define(DISCOVERY_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                ,{<<"Event-Name">>, <<"discovery_resp">>}
                              ]).
-define(DISCOVERY_RESP_TYPES, []).

%% Conference Deaf
-define(DEAF_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant">>]).
-define(OPTIONAL_DEAF_PARTICIPANT_HEADERS, []).
-define(DEAF_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                  ,{<<"Event-Name">>, <<"command">>}
                                  ,{<<"Application-Name">>, <<"deaf_participant">>}
                                 ]).
-define(DEAF_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ]).

%% Conference Energy
-define(PARTICIPANT_ENERGY_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>
                                         ,<<"Participant">>, <<"Energy-Level">>
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
-define(OPTIONAL_KICK_HEADERS, [<<"Participant">>]).
-define(KICK_VALUES, [{<<"Event-Category">>, <<"conference">>}
                      ,{<<"Event-Name">>, <<"command">>}
                      ,{<<"Application-Name">>, <<"kick">>}
                     ]).
-define(KICK_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                    ]).

%% Conference Hup
-define(HUP_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>]).
-define(OPTIONAL_HUP_HEADERS, [<<"Participant">>]).
-define(HUP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                      ,{<<"Event-Name">>, <<"command">>}
                      ,{<<"Application-Name">>, <<"hup">>}
                     ]).
-define(HUP_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
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
-define(MUTE_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant">>]).
-define(OPTIONAL_MUTE_PARTICIPANT_HEADERS, []).
-define(MUTE_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                  ,{<<"Event-Name">>, <<"command">>}
                                  ,{<<"Application-Name">>, <<"mute_participant">>}
                                 ]).
-define(MUTE_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                ]).

%% Conference Play
-define(PLAY_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Media-Name">>]).
-define(OPTIONAL_PLAY_HEADERS, [<<"Participant">>, <<"Call-ID">>]).
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
-define(RELATE_PARTICIPANTS_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant">>, <<"Other-Participant">>]).
-define(OPTIONAL_RELATE_PARTICIPANTS_HEADERS, [<<"Relationship">>]).
-define(RELATE_PARTICIPANTS_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                     ,{<<"Event-Name">>, <<"command">>}
                                     ,{<<"Application-Name">>, <<"relate_participants">>}
                                     ,{<<"Relationship">>, [<<"deaf">>, <<"mute">>, <<"clear">>]}
                                    ]).
-define(RELATE_PARTICIPANTS_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
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
-define(OPTIONAL_STOP_PLAY_HEADERS, [<<"Participant">>, <<"Affects">>]).
-define(STOP_PLAY_VALUES, [{<<"Event-Category">>, <<"conference">>}
                           ,{<<"Event-Name">>, <<"command">>}
                           ,{<<"Application-Name">>, <<"stop_play">>}
                           ,{<<"Affects">>, [<<"current">>, <<"all">>]}
                          ]).
-define(STOP_PLAY_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                         ]).
%% Conference Undeaf
-define(UNDEAF_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant">>]).
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
-define(UNMUTE_PARTICIPANT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>, <<"Participant">>]).
-define(OPTIONAL_UNMUTE_PARTICIPANT_HEADERS, []).
-define(UNMUTE_PARTICIPANT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                    ,{<<"Event-Name">>, <<"command">>}
                                    ,{<<"Application-Name">>, <<"unmute_participant">>}
                                   ]).
-define(UNMUTE_PARTICIPANT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                  ]).

%% Conference Set Volume In
-define(PARTICIPANT_VOLUME_IN_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>
                                            ,<<"Participant">>, <<"Volume-In-Level">>
                                       ]).
-define(OPTIONAL_PARTICIPANT_VOLUME_IN_HEADERS, []).
-define(PARTICIPANT_VOLUME_IN_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                       ,{<<"Event-Name">>, <<"command">>}
                                       ,{<<"Application-Name">>, <<"participant_volume_in">>}
                                      ]).
-define(PARTICIPANT_VOLUME_IN_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                     ]).

%% Conference Set Volume Out
-define(PARTICIPANT_VOLUME_OUT_HEADERS, [<<"Application-Name">>, <<"Conference-ID">>
                                             ,<<"Participant">>, <<"Volume-Out-Level">>
                                        ]).
-define(OPTIONAL_PARTICIPANT_VOLUME_OUT_HEADERS, []).
-define(PARTICIPANT_VOLUME_OUT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                        ,{<<"Event-Name">>, <<"command">>}
                                        ,{<<"Application-Name">>, <<"participant_volume_out">>}
                                       ]).
-define(PARTICIPANT_VOLUME_OUT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}
                                      ]).

%% Conference Participants Event
-define(PARTICIPANTS_EVENT_HEADERS, [<<"Participants">>, <<"Conference-ID">>, <<"Focus">>]).
-define(OPTIONAL_PARTICIPANTS_EVENT_HEADERS, []).
-define(PARTICIPANTS_EVENT_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                    ,{<<"Event-Name">>, <<"participants_event">>}
                                   ]).
-define(PARTICIPANTS_EVENT_TYPES, [{<<"Conference-ID">>, fun is_binary/1}]).

%% Conference Error
-define(CONFERENCE_ERROR_HEADERS, [<<"Error-Message">>, <<"Request">>]).
-define(OPTIONAL_CONFERENCE_ERROR_HEADERS, []).
-define(CONFERENCE_ERROR_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                  ,{<<"Event-Name">>, <<"error">>}
                                 ]).
-define(CONFERENCE_ERROR_TYPES, []).

-define(CONFIG_REQ_HEADERS, [<<"Profile">>]).
-define(OPTIONAL_CONFIG_REQ_HEADERS, []).
-define(CONFIG_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                            ,{<<"Event-Name">>, <<"config_req">>}
                           ]).
-define(CONFIG_REQ_TYPES, []).

-define(CONFIG_RESP_HEADERS, [<<"Profiles">>]).
-define(OPTIONAL_CONFIG_RESP_HEADERS, [<<"Caller-Controls">>, <<"Advertise">>, <<"Chat-Permissions">>]).
-define(CONFIG_RESP_VALUES, [{<<"Event-Category">>, <<"conference">>}
                             ,{<<"Event-Name">>, <<"config_resp">>}
                            ]).
-define(CONFIG_RESP_TYPES, []).

-define(APPLICTION_MAP, [{<<"deaf_participant">>, ?DEAF_PARTICIPANT_VALUES, fun ?MODULE:deaf_participant/1}
                         ,{<<"participant_energy">>, ?PARTICIPANT_ENERGY_VALUES, fun ?MODULE:participant_energy/1}
                         ,{<<"kick">>, ?KICK_VALUES, fun ?MODULE:kick/1}
                         ,{<<"hup">>, ?HUP_VALUES, fun ?MODULE:hup/1}
                         ,{<<"participants">>, ?PARTICIPANTS_REQ_VALUES, fun ?MODULE:participants_req/1}
                         ,{<<"lock">>, ?LOCK_VALUES, fun ?MODULE:lock/1}
                         ,{<<"mute_participant">>, ?MUTE_PARTICIPANT_VALUES, fun ?MODULE:mute_participant/1}
                         ,{<<"play">>, ?PLAY_VALUES, fun ?MODULE:play/1}
                         ,{<<"record">>, ?RECORD_VALUES, fun ?MODULE:record/1}
                         ,{<<"recordstop">>, ?RECORDSTOP_VALUES, fun ?MODULE:recordstop/1}
                         ,{<<"relate_participants">>, ?RELATE_PARTICIPANTS_VALUES, fun ?MODULE:relate_participants/1}
                         ,{<<"stop_play">>, ?STOP_PLAY_VALUES, fun ?MODULE:stop_play/1}
                         ,{<<"undeaf_participant">>, ?UNDEAF_PARTICIPANT_VALUES, fun ?MODULE:undeaf_participant/1}
                         ,{<<"unlock">>, ?UNLOCK_VALUES, fun ?MODULE:unlock/1}
                         ,{<<"unmute_participant">>, ?UNMUTE_PARTICIPANT_VALUES, fun ?MODULE:unmute_participant/1}
                         ,{<<"participant_volume_in">>, ?PARTICIPANT_VOLUME_IN_VALUES, fun ?MODULE:participant_volume_in/1}
                         ,{<<"participant_volume_out">>, ?PARTICIPANT_VOLUME_OUT_VALUES, fun ?MODULE:participant_volume_out/1}
                         ,{<<"tones">>, ?CONF_TONES_REQ_VALUES, fun ?MODULE:tones/1}
                         ,{<<"say">>, ?CONF_SAY_REQ_VALUES, fun ?MODULE:say/1}
                         ,{<<"tts">>, ?CONF_SAY_REQ_VALUES, fun ?MODULE:tts/1}
                        ]).

focus_queue_name(Focus) -> <<(wh_util:to_binary(Focus))/binary, "_conference">>.

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-define(CONF_SAY_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                              ,{<<"Application-Name">>, [<<"say">>, <<"tts">>]}
                              | props:delete_keys([<<"Event-Category">>
                                                       ,<<"Application-Name">>
                                                  ], ?TONES_REQ_VALUES)
                             ]).
-spec say(api_terms()) -> api_formatter_return() .
say(Prop) when is_list(Prop) ->
    case say_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?TTS_REQ_HEADERS, ?OPTIONAL_TTS_REQ_HEADERS);
        'false' -> {'error', "Prop failed validation for say_req"}
    end;
say(JObj) -> say(wh_json:to_proplist(JObj)).

tts(API) -> say(API).

-spec say_v(api_terms()) -> boolean().
say_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TTS_REQ_HEADERS, ?CONF_SAY_REQ_VALUES, ?TTS_REQ_TYPES);
say_v(JObj) -> say_v(wh_json:to_proplist(JObj)).

tts_v(API) -> say_v(API).

%%--------------------------------------------------------------------
%% @doc Create a tone on the channel - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------

-define(CONF_TONES_REQ_VALUES, [{<<"Event-Category">>, <<"conference">>}
                                | props:delete(<<"Event-Category">>, ?TONES_REQ_VALUES)
                               ]).
-spec tones(api_terms()) -> api_formatter_return() .
tones(Prop) when is_list(Prop) ->
    Tones = [begin
                 {'ok', TonesProp} = wapi_dialplan:tones_req_tone_headers(Tone),
                 wh_json:from_list(TonesProp)
             end
             || Tone <- props:get_value(<<"Tones">>, Prop, []),
                wapi_dialplan:tones_req_tone_v(Tone)
            ],
    Prop1 = [ {<<"Tones">>, Tones} | props:delete(<<"Tones">>, Prop)],
    case tones_v(Prop1) of
        'true' -> wh_api:build_message(Prop1, ?TONES_REQ_HEADERS, ?OPTIONAL_TONES_REQ_HEADERS);
        'false' -> {'error', "Prop failed validation for tones_req"}
    end;
tones(JObj) -> tones(wh_json:to_proplist(JObj)).

-spec tones_v(api_terms()) -> boolean().
tones_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TONES_REQ_HEADERS, ?CONF_TONES_REQ_VALUES, ?TONES_REQ_TYPES);
tones_v(JObj) -> tones_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec search_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_req(Prop) when is_list(Prop) ->
    case search_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SEARCH_REQ_HEADERS, ?OPTIONAL_SEARCH_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for search request"}
    end;
search_req(JObj) -> search_req(wh_json:to_proplist(JObj)).

-spec search_req_v(api_terms()) -> boolean().
search_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SEARCH_REQ_HEADERS, ?SEARCH_REQ_VALUES, ?SEARCH_REQ_TYPES);
search_req_v(JObj) -> search_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec search_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
search_resp(Prop) when is_list(Prop) ->
    case search_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SEARCH_RESP_HEADERS, ?OPTIONAL_SEARCH_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for search response"}
    end;
search_resp(JObj) -> search_resp(wh_json:to_proplist(JObj)).

-spec search_resp_v(api_terms()) -> boolean().
search_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SEARCH_RESP_HEADERS, ?SEARCH_RESP_VALUES, ?SEARCH_RESP_TYPES);
search_resp_v(JObj) -> search_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec discovery_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
discovery_req(Prop) when is_list(Prop) ->
    case discovery_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?DISCOVERY_REQ_HEADERS, ?OPTIONAL_DISCOVERY_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery"}
    end;
discovery_req(JObj) -> discovery_req(wh_json:to_proplist(JObj)).

-spec discovery_req_v(api_terms()) -> boolean().
discovery_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DISCOVERY_REQ_HEADERS, ?DISCOVERY_REQ_VALUES, ?DISCOVERY_REQ_TYPES);
discovery_req_v(JObj) -> discovery_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec discovery_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
discovery_resp(Prop) when is_list(Prop) ->
    case discovery_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?DISCOVERY_RESP_HEADERS, ?OPTIONAL_DISCOVERY_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for discovery resp"}
    end;
discovery_resp(JObj) -> discovery_resp(wh_json:to_proplist(JObj)).

-spec discovery_resp_v(api_terms()) -> boolean().
discovery_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DISCOVERY_RESP_HEADERS, ?DISCOVERY_RESP_VALUES, ?DISCOVERY_RESP_TYPES);
discovery_resp_v(JObj) -> discovery_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec deaf_participant(api_terms()) -> {'ok', iolist()} | {'error', string()}.
deaf_participant(Prop) when is_list(Prop) ->
    case deaf_participant_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?DEAF_PARTICIPANT_HEADERS, ?OPTIONAL_DEAF_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for deaf participant"}
    end;
deaf_participant(JObj) -> deaf_participant(wh_json:to_proplist(JObj)).

-spec deaf_participant_v(api_terms()) -> boolean().
deaf_participant_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DEAF_PARTICIPANT_HEADERS, ?DEAF_PARTICIPANT_VALUES, ?DEAF_PARTICIPANT_TYPES);
deaf_participant_v(JObj) -> deaf_participant_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec participant_energy(api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_energy(Prop) when is_list(Prop) ->
    case participant_energy_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARTICIPANT_ENERGY_HEADERS, ?OPTIONAL_PARTICIPANT_ENERGY_HEADERS);
        'false' -> {'error', "Proplist failed validation for participant energy"}
    end;
participant_energy(JObj) -> participant_energy(wh_json:to_proplist(JObj)).

-spec participant_energy_v(api_terms()) -> boolean().
participant_energy_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANT_ENERGY_HEADERS, ?PARTICIPANT_ENERGY_VALUES, ?PARTICIPANT_ENERGY_TYPES);
participant_energy_v(JObj) -> participant_energy_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec kick(api_terms()) -> {'ok', iolist()} | {'error', string()}.
kick(Prop) when is_list(Prop) ->
    case kick_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?KICK_HEADERS, ?OPTIONAL_KICK_HEADERS);
        'false' -> {'error', "Proplist failed validation for kick"}
    end;
kick(JObj) -> kick(wh_json:to_proplist(JObj)).

-spec kick_v(api_terms()) -> boolean().
kick_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?KICK_HEADERS, ?KICK_VALUES, ?KICK_TYPES);
kick_v(JObj) -> kick_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec hup(api_terms()) -> {'ok', iolist()} | {'error', string()}.
hup(Prop) when is_list(Prop) ->
    case hup_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?HUP_HEADERS, ?OPTIONAL_HUP_HEADERS);
        'false' -> {'error', "Proplist failed validation for hup"}
    end;
hup(JObj) -> hup(wh_json:to_proplist(JObj)).

-spec hup_v(api_terms()) -> boolean().
hup_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?HUP_HEADERS, ?HUP_VALUES, ?HUP_TYPES);
hup_v(JObj) -> hup_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec participants_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
participants_req(Prop) when is_list(Prop) ->
    case participants_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARTICIPANTS_REQ_HEADERS, ?OPTIONAL_PARTICIPANTS_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for participants request"}
    end;
participants_req(JObj) -> participants_req(wh_json:to_proplist(JObj)).

-spec participants_req_v(api_terms()) -> boolean().
participants_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANTS_REQ_HEADERS, ?PARTICIPANTS_REQ_VALUES, ?PARTICIPANTS_REQ_TYPES);
participants_req_v(JObj) -> participants_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec participants_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
participants_resp(Prop) when is_list(Prop) ->
    case participants_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARTICIPANTS_RESP_HEADERS, ?OPTIONAL_PARTICIPANTS_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for participants response"}
    end;
participants_resp(JObj) -> participants_resp(wh_json:to_proplist(JObj)).

-spec participants_resp_v(api_terms()) -> boolean().
participants_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANTS_RESP_HEADERS, ?PARTICIPANTS_RESP_VALUES, ?PARTICIPANTS_RESP_TYPES);
participants_resp_v(JObj) -> participants_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec lock(api_terms()) -> {'ok', iolist()} | {'error', string()}.
lock(Prop) when is_list(Prop) ->
    case lock_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?LOCK_HEADERS, ?OPTIONAL_LOCK_HEADERS);
        'false' -> {'error', "Proplist failed validation for lock"}
    end;
lock(JObj) -> lock(wh_json:to_proplist(JObj)).

-spec lock_v(api_terms()) -> boolean().
lock_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?LOCK_HEADERS, ?LOCK_VALUES, ?LOCK_TYPES);
lock_v(JObj) -> lock_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec mute_participant(api_terms()) -> {'ok', iolist()} | {'error', string()}.
mute_participant(Prop) when is_list(Prop) ->
    case mute_participant_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?MUTE_PARTICIPANT_HEADERS, ?OPTIONAL_MUTE_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for mute participant"}
    end;
mute_participant(JObj) -> mute_participant(wh_json:to_proplist(JObj)).

-spec mute_participant_v(api_terms()) -> boolean().
mute_participant_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MUTE_PARTICIPANT_HEADERS, ?MUTE_PARTICIPANT_VALUES, ?MUTE_PARTICIPANT_TYPES);
mute_participant_v(JObj) -> mute_participant_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec play(api_terms()) -> {'ok', iolist()} | {'error', string()}.
play(Prop) when is_list(Prop) ->
    case play_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PLAY_HEADERS, ?OPTIONAL_PLAY_HEADERS);
        'false' -> {'error', "Proplist failed validation for play"}
    end;
play(JObj) -> play(wh_json:to_proplist(JObj)).

-spec play_v(api_terms()) -> boolean().
play_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PLAY_HEADERS, ?PLAY_VALUES, ?PLAY_TYPES);
play_v(JObj) -> play_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec record(api_terms()) -> {'ok', iolist()} | {'error', string()}.
record(Prop) when is_list(Prop) ->
    case record_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RECORD_HEADERS, ?OPTIONAL_RECORD_HEADERS);
        'false' -> {'error', "Proplist failed validation for record"}
    end;
record(JObj) -> record(wh_json:to_proplist(JObj)).

-spec record_v(api_terms()) -> boolean().
record_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RECORD_HEADERS, ?RECORD_VALUES, ?RECORD_TYPES);
record_v(JObj) -> record_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec recordstop(api_terms()) -> {'ok', iolist()} | {'error', string()}.
recordstop(Prop) when is_list(Prop) ->
    case recordstop_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RECORDSTOP_HEADERS, ?OPTIONAL_RECORDSTOP_HEADERS);
        'false' -> {'error', "Proplist failed validation for recordstop"}
    end;
recordstop(JObj) -> recordstop(wh_json:to_proplist(JObj)).

-spec recordstop_v(api_terms()) -> boolean().
recordstop_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RECORDSTOP_HEADERS, ?RECORDSTOP_VALUES, ?RECORDSTOP_TYPES);
recordstop_v(JObj) -> recordstop_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec relate_participants(api_terms()) -> {'ok', iolist()} | {'error', string()}.
relate_participants(Prop) when is_list(Prop) ->
    case relate_participants_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?RELATE_PARTICIPANTS_HEADERS, ?OPTIONAL_RELATE_PARTICIPANTS_HEADERS);
        'false' -> {'error', "Proplist failed validation for relate participants"}
    end;
relate_participants(JObj) -> relate_participants(wh_json:to_proplist(JObj)).

-spec relate_participants_v(api_terms()) -> boolean().
relate_participants_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?RELATE_PARTICIPANTS_HEADERS, ?RELATE_PARTICIPANTS_VALUES, ?RELATE_PARTICIPANTS_TYPES);
relate_participants_v(JObj) -> relate_participants_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec set(api_terms()) -> {'ok', iolist()} | {'error', string()}.
set(Prop) when is_list(Prop) ->
    case set_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?SET_HEADERS, ?OPTIONAL_SET_HEADERS);
        'false' -> {'error', "Proplist failed validation for set"}
    end;
set(JObj) -> set(wh_json:to_proplist(JObj)).

-spec set_v(api_terms()) -> boolean().
set_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SET_HEADERS, ?SET_VALUES, ?SET_TYPES);
set_v(JObj) -> set_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec stop_play(api_terms()) -> {'ok', iolist()} | {'error', string()}.
stop_play(Prop) when is_list(Prop) ->
    case stop_play_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?STOP_PLAY_HEADERS, ?OPTIONAL_STOP_PLAY_HEADERS);
        'false' -> {'error', "Proplist failed validation for stop play"}
    end;
stop_play(JObj) -> stop_play(wh_json:to_proplist(JObj)).

-spec stop_play_v(api_terms()) -> boolean().
stop_play_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?STOP_PLAY_HEADERS, ?STOP_PLAY_VALUES, ?STOP_PLAY_TYPES);
stop_play_v(JObj) -> stop_play_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec undeaf_participant(api_terms()) -> {'ok', iolist()} | {'error', string()}.
undeaf_participant(Prop) when is_list(Prop) ->
    case undeaf_participant_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UNDEAF_PARTICIPANT_HEADERS, ?OPTIONAL_UNDEAF_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for undeaf participant"}
    end;
undeaf_participant(JObj) -> undeaf_participant(wh_json:to_proplist(JObj)).

-spec undeaf_participant_v(api_terms()) -> boolean().
undeaf_participant_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UNDEAF_PARTICIPANT_HEADERS, ?UNDEAF_PARTICIPANT_VALUES, ?UNDEAF_PARTICIPANT_TYPES);
undeaf_participant_v(JObj) -> undeaf_participant_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec unlock(api_terms()) -> {'ok', iolist()} | {'error', string()}.
unlock(Prop) when is_list(Prop) ->
    case unlock_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UNLOCK_HEADERS, ?OPTIONAL_UNLOCK_HEADERS);
        'false' -> {'error', "Proplist failed validation for unlock"}
    end;
unlock(JObj) -> unlock(wh_json:to_proplist(JObj)).

-spec unlock_v(api_terms()) -> boolean().
unlock_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UNLOCK_HEADERS, ?UNLOCK_VALUES, ?UNLOCK_TYPES);
unlock_v(JObj) -> unlock_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec unmute_participant(api_terms()) -> {'ok', iolist()} | {'error', string()}.
unmute_participant(Prop) when is_list(Prop) ->
    case unmute_participant_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UNMUTE_PARTICIPANT_HEADERS, ?OPTIONAL_UNMUTE_PARTICIPANT_HEADERS);
        'false' -> {'error', "Proplist failed validation for unmute participant"}
    end;
unmute_participant(JObj) -> unmute_participant(wh_json:to_proplist(JObj)).

-spec unmute_participant_v(api_terms()) -> boolean().
unmute_participant_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UNMUTE_PARTICIPANT_HEADERS, ?UNMUTE_PARTICIPANT_VALUES, ?UNMUTE_PARTICIPANT_TYPES);
unmute_participant_v(JObj) -> unmute_participant_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec participant_volume_in(api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_volume_in(Prop) when is_list(Prop) ->
    case participant_volume_in_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARTICIPANT_VOLUME_IN_HEADERS, ?OPTIONAL_PARTICIPANT_VOLUME_IN_HEADERS);
        'false' -> {'error', "Proplist failed validation for volume in"}
    end;
participant_volume_in(JObj) -> participant_volume_in(wh_json:to_proplist(JObj)).

-spec participant_volume_in_v(api_terms()) -> boolean().
participant_volume_in_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANT_VOLUME_IN_HEADERS, ?PARTICIPANT_VOLUME_IN_VALUES, ?PARTICIPANT_VOLUME_IN_TYPES);
participant_volume_in_v(JObj) -> participant_volume_in_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec participant_volume_out(api_terms()) -> {'ok', iolist()} | {'error', string()}.
participant_volume_out(Prop) when is_list(Prop) ->
    case participant_volume_out_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARTICIPANT_VOLUME_OUT_HEADERS, ?OPTIONAL_PARTICIPANT_VOLUME_OUT_HEADERS);
        'false' -> {'error', "Proplist failed validation for volume out"}
    end;
participant_volume_out(JObj) -> participant_volume_out(wh_json:to_proplist(JObj)).

-spec participant_volume_out_v(api_terms()) -> boolean().
participant_volume_out_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANT_VOLUME_OUT_HEADERS, ?PARTICIPANT_VOLUME_OUT_VALUES, ?PARTICIPANT_VOLUME_OUT_TYPES);
participant_volume_out_v(JObj) -> participant_volume_out_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec participants_event(api_terms()) -> {'ok', iolist()} | {'error', string()}.
participants_event(Prop) when is_list(Prop) ->
    case participants_event_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?PARTICIPANTS_EVENT_HEADERS, ?OPTIONAL_PARTICIPANTS_EVENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for participants_event response"}
    end;
participants_event(JObj) -> participants_event(wh_json:to_proplist(JObj)).

-spec participants_event_v(api_terms()) -> boolean().
participants_event_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PARTICIPANTS_EVENT_HEADERS, ?PARTICIPANTS_EVENT_VALUES, ?PARTICIPANTS_EVENT_TYPES);
participants_event_v(JObj) -> participants_event_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec conference_error(api_terms()) -> {'ok', iolist()} | {'error', string()}.
conference_error(Prop) when is_list(Prop) ->
    case conference_error_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONFERENCE_ERROR_HEADERS, ?OPTIONAL_CONFERENCE_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for conference error"}
    end;
conference_error(JObj) -> conference_error(wh_json:to_proplist(JObj)).

-spec conference_error_v(api_terms()) -> boolean().
conference_error_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONFERENCE_ERROR_HEADERS, ?CONFERENCE_ERROR_VALUES, ?CONFERENCE_ERROR_TYPES);
conference_error_v(JObj) -> conference_error_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec config_req(api_terms()) -> {'ok', iolist()} | {'error', string()}.
config_req(Prop) when is_list(Prop) ->
    case config_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONFIG_REQ_HEADERS, ?OPTIONAL_CONFIG_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for config req"}
    end;
config_req(JObj) -> config_req(wh_json:to_proplist(JObj)).

-spec config_req_v(api_terms()) -> boolean().
config_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONFIG_REQ_HEADERS, ?CONFIG_REQ_VALUES, ?CONFIG_REQ_TYPES);
config_req_v(JObj) -> config_req_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec config_resp(api_terms()) -> {'ok', iolist()} | {'error', string()}.
config_resp(Prop) when is_list(Prop) ->
    case config_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?CONFIG_RESP_HEADERS, ?OPTIONAL_CONFIG_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for config resp"}
    end;
config_resp(JObj) -> config_resp(wh_json:to_proplist(JObj)).

-spec config_resp_v(api_terms()) -> boolean().
config_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CONFIG_RESP_HEADERS, ?CONFIG_RESP_VALUES, ?CONFIG_RESP_TYPES);
config_resp_v(JObj) -> config_resp_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Bind a queue to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec bind_q(ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    amqp_util:conference_exchange(),
    bind_to_q(Queue, props:get_value('restrict_to', Props), Props).

bind_to_q(Q, 'undefined', _) ->
    'ok' = amqp_util:bind_q_to_conference(Q, 'discovery'),
    'ok' = amqp_util:bind_q_to_conference(Q, 'command'),
    amqp_util:bind_q_to_conference(Q, 'event');
bind_to_q(Q, ['discovery'|T], Props) ->
    'ok' = amqp_util:bind_q_to_conference(Q, 'discovery'),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['command'|T], Props) ->
    'ok' = amqp_util:bind_q_to_conference(Q, 'command'),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['event'|T], Props) ->
    'ok' = amqp_util:bind_q_to_conference(Q, 'event'),
    bind_to_q(Q, T, Props);
bind_to_q(Q, ['config'|T], Props) ->
    Profile = props:get_value('profile', Props, <<"*">>),
    'ok' = amqp_util:bind_q_to_conference(Q, 'config', Profile),
    bind_to_q(Q, T, Props);
bind_to_q(Q, [{'conference', ConfId}|T], Props) ->
    'ok' = amqp_util:bind_q_to_conference(Q, 'event', ConfId),
    bind_to_q(Q, T, Props);
bind_to_q(_Q, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% Unbind a queue from the conference exhange
%% @end
%%--------------------------------------------------------------------
-spec unbind_q(ne_binary(), wh_proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    amqp_util:conference_exchange(),
    unbind_from_q(Queue, props:get_value('restrict_to', Props), Props).

unbind_from_q(Q, 'undefined', _) ->
    'ok' = amqp_util:unbind_q_from_conference(Q, 'discovery'),
    'ok' = amqp_util:unbind_q_from_conference(Q, 'command'),
    amqp_util:unbind_q_from_conference(Q, 'event');
unbind_from_q(Q, ['discovery'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_conference(Q, 'discovery'),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, ['command'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_conference(Q, 'command'),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, ['event'|T], Props) ->
    'ok' = amqp_util:unbind_q_from_conference(Q, 'event'),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, [{'conference', ConfId}|T], Props) ->
    'ok' = amqp_util:unbind_q_from_conference(Q, 'event', ConfId),
    unbind_from_q(Q, T, Props);
unbind_from_q(Q, ['config'|T], Props) ->
    Profile = props:get_value('profile', Props, <<"*">>),
    'ok' = amqp_util:unbind_q_from_conference(Q, 'config', Profile),
    unbind_from_q(Q, T, Props);
unbind_from_q(_Q, [], _) -> 'ok'.

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_search_req(api_terms()) -> 'ok'.
-spec publish_search_req(api_terms(), ne_binary()) -> 'ok'.
publish_search_req(JObj) ->
    publish_search_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_search_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?SEARCH_REQ_VALUES, fun ?MODULE:search_req/1),
    amqp_util:conference_publish(Payload, 'discovery', 'undefined', [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_search_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_search_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_search_resp(Queue, Resp) ->
    publish_search_resp(Queue, Resp, ?DEFAULT_CONTENT_TYPE).
publish_search_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?SEARCH_RESP_VALUES, fun ?MODULE:search_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_discovery_req(api_terms()) -> 'ok'.
-spec publish_discovery_req(api_terms(), ne_binary()) -> 'ok'.
publish_discovery_req(JObj) ->
    publish_discovery_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_discovery_req(Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?DISCOVERY_REQ_VALUES, fun ?MODULE:discovery_req/1),
    amqp_util:conference_publish(Payload, 'discovery', 'undefined', [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish the response to requestor
%% @end
%%--------------------------------------------------------------------
-spec publish_discovery_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_discovery_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_discovery_resp(Q, JObj) ->
    publish_discovery_resp(Q, JObj, ?DEFAULT_CONTENT_TYPE).
publish_discovery_resp(Q, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?DISCOVERY_RESP_VALUES, fun ?MODULE:discovery_resp/1),
    amqp_util:targeted_publish(Q, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_deaf_participant(ne_binary(), api_terms()) -> 'ok'.
-spec publish_deaf_participant(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_deaf_participant(ConferenceId, JObj) ->
    publish_deaf_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_deaf_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?DEAF_PARTICIPANT_VALUES, fun ?MODULE:deaf_participant/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_participant_energy(ne_binary(), api_terms()) -> 'ok'.
-spec publish_participant_energy(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_participant_energy(ConferenceId, JObj) ->
    publish_participant_energy(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_participant_energy(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?PARTICIPANT_ENERGY_VALUES, fun ?MODULE:participant_energy/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_kick(ne_binary(), api_terms()) -> 'ok'.
-spec publish_kick(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_kick(ConferenceId, JObj) ->
    publish_kick(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_kick(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?KICK_VALUES, fun ?MODULE:kick/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_hup(ne_binary(), api_terms()) -> 'ok'.
-spec publish_hup(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_hup(ConferenceId, JObj) ->
    publish_hup(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_hup(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?HUP_VALUES, fun ?MODULE:hup/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_participants_req(ne_binary(), api_terms()) -> 'ok'.
-spec publish_participants_req(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_participants_req(ConferenceId, JObj) ->
    publish_participants_req(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_participants_req(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?PARTICIPANTS_REQ_VALUES, fun ?MODULE:participants_req/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_participants_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_participants_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_participants_resp(Queue, Resp) ->
    publish_participants_resp(Queue, Resp, ?DEFAULT_CONTENT_TYPE).
publish_participants_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?PARTICIPANTS_RESP_VALUES, fun ?MODULE:participants_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_lock(ne_binary(), api_terms()) -> 'ok'.
-spec publish_lock(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_lock(ConferenceId, JObj) ->
    publish_lock(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_lock(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?LOCK_VALUES, fun ?MODULE:lock/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_mute_participant(ne_binary(), api_terms()) -> 'ok'.
-spec publish_mute_participant(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_mute_participant(ConferenceId, JObj) ->
    publish_mute_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_mute_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?MUTE_PARTICIPANT_VALUES, fun ?MODULE:mute_participant/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_play(ne_binary(), api_terms()) -> 'ok'.
-spec publish_play(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_play(ConferenceId, JObj) ->
    publish_play(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_play(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?PLAY_VALUES, fun ?MODULE:play/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_record(ne_binary(), api_terms()) -> 'ok'.
-spec publish_record(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_record(ConferenceId, JObj) ->
    publish_record(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_record(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?RECORD_VALUES, fun ?MODULE:record/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_recordstop(ne_binary(), api_terms()) -> 'ok'.
-spec publish_recordstop(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_recordstop(ConferenceId, JObj) ->
    publish_recordstop(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_recordstop(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?RECORDSTOP_VALUES, fun ?MODULE:recordstop/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_relate_participants(ne_binary(), api_terms()) -> 'ok'.
-spec publish_relate_participants(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_relate_participants(ConferenceId, JObj) ->
    publish_relate_participants(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_relate_participants(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?RELATE_PARTICIPANTS_VALUES, fun ?MODULE:relate_participants/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_set(ne_binary(), api_terms()) -> 'ok'.
-spec publish_set(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_set(ConferenceId, JObj) ->
    publish_set(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_set(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?SET_VALUES, fun ?MODULE:set/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_stop_play(ne_binary(), api_terms()) -> 'ok'.
-spec publish_stop_play(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_stop_play(ConferenceId, JObj) ->
    publish_stop_play(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_stop_play(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?STOP_PLAY_VALUES, fun ?MODULE:stop_play/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_undeaf_participant(ne_binary(), api_terms()) -> 'ok'.
-spec publish_undeaf_participant(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_undeaf_participant(ConferenceId, JObj) ->
    publish_undeaf_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_undeaf_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?UNDEAF_PARTICIPANT_VALUES, fun ?MODULE:undeaf_participant/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_unlock(ne_binary(), api_terms()) -> 'ok'.
-spec publish_unlock(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_unlock(ConferenceId, JObj) ->
    publish_unlock(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_unlock(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?UNLOCK_VALUES, fun ?MODULE:unlock/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_unmute_participant(ne_binary(), api_terms()) -> 'ok'.
-spec publish_unmute_participant(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_unmute_participant(ConferenceId, JObj) ->
    publish_unmute_participant(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_unmute_participant(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?UNMUTE_PARTICIPANT_VALUES, fun ?MODULE:unmute_participant/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_participant_volume_in(ne_binary(), api_terms()) -> 'ok'.
-spec publish_participant_volume_in(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_participant_volume_in(ConferenceId, JObj) ->
    publish_participant_volume_in(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_participant_volume_in(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?PARTICIPANT_VOLUME_IN_VALUES, fun ?MODULE:participant_volume_in/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_participant_volume_out(ne_binary(), api_terms()) -> 'ok'.
-spec publish_participant_volume_out(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_participant_volume_out(ConferenceId, JObj) ->
    publish_participant_volume_out(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_participant_volume_out(ConferenceId, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?PARTICIPANT_VOLUME_OUT_VALUES, fun ?MODULE:participant_volume_out/1),
    amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_participants_event(ne_binary(), api_terms()) -> 'ok'.
-spec publish_participants_event(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_participants_event(ConferenceId, JObj) ->
    publish_participants_event(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_participants_event(ConferenceId, Event, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Event, ?PARTICIPANTS_EVENT_VALUES, fun ?MODULE:participants_event/1),
    amqp_util:conference_publish(Payload, 'event', ConferenceId, [], ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_error(ne_binary(), api_terms()) -> 'ok'.
-spec publish_error(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_error(Queue, JObj) ->
    publish_error(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_error(Queue, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?CONFERENCE_ERROR_VALUES, fun ?MODULE:conference_error/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_command(ne_binary(), api_terms()) -> 'ok'.
-spec publish_command(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_command(ConferenceId, JObj) ->
    publish_command(ConferenceId, JObj, ?DEFAULT_CONTENT_TYPE).
publish_command(ConferenceId, Req, ContentType) ->
    App = props:get_value(<<"Application-Name">>, Req),
    case lists:keyfind(App, 1, ?APPLICTION_MAP) of
        'false' -> throw({'invalid_conference_command', Req});
        {_, Values, Fun} ->
            {'ok', Payload} = wh_api:prepare_api_payload(Req, Values, Fun),
            amqp_util:conference_publish(Payload, 'command', ConferenceId, [], ContentType)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_targeted_command(ne_binary(), api_terms()) -> 'ok'.
-spec publish_targeted_command(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_targeted_command(Focus, JObj) ->
    publish_targeted_command(Focus, JObj, ?DEFAULT_CONTENT_TYPE).
publish_targeted_command(Focus, Req, ContentType) ->
    App = props:get_value(<<"Application-Name">>, Req),
    case lists:keyfind(App, 1, ?APPLICTION_MAP) of
        'false' -> throw({'invalid_conference_command', Req});
        {_, Values, Fun} ->
            {'ok', Payload} = wh_api:prepare_api_payload(Req, Values, Fun),
            Queue = focus_queue_name(Focus),
            amqp_util:targeted_publish(Queue, Payload, ContentType)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_config_req(api_terms()) -> 'ok'.
-spec publish_config_req(api_terms(), ne_binary()) -> 'ok'.
publish_config_req(JObj) ->
    publish_config_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_config_req(Req, ContentType) ->
    Profile = profile(Req),
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?CONFIG_REQ_VALUES, fun ?MODULE:config_req/1),
    amqp_util:conference_publish(Payload, 'config', Profile, [], ContentType).

profile(Props) when is_list(Props) -> props:get_value(<<"Profile">>, Props);
profile(JObj) -> wh_json:get_value(<<"Profile">>, JObj).

%%--------------------------------------------------------------------
%% @doc
%% Publish to the conference exchange
%% @end
%%--------------------------------------------------------------------
-spec publish_config_resp(ne_binary(), api_terms()) -> 'ok'.
-spec publish_config_resp(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_config_resp(Queue, JObj) ->
    publish_config_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_config_resp(Queue, Req, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Req, ?CONFIG_RESP_VALUES, fun ?MODULE:config_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).
