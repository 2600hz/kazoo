%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_conference_command).

-include("kapps_call_command.hrl").

-export([search/1]).
-export([deaf_participant/2]).
-export([participant_energy/3]).
-export([kick/1, kick/2]).
-export([lock/1]).
-export([mute_participant/2]).
-export([prompt/2, prompt/3]).
-export([play_command/1, play_command/2]).
-export([play/2, play/3]).
-export([record/1, recordstop/1]).
-export([relate_participants/3, relate_participants/4]).
-export([stop_play/1, stop_play/2, stop_play/3]).
-export([undeaf_participant/2]).
-export([unlock/1]).
-export([unmute_participant/2]).
-export([participant_volume_in/3]).
-export([participant_volume_out/3]).
-export([dial/2, dial/3, dial/4, dial/5]).

-export([send_command/2]).
-export([play_macro/2]).

-spec search(kapps_conference:conference()) ->
                    {'ok', kz_json:object()} |
                    {'error', any()}.
search(Conference) ->
    AppName = kapps_conference:application_name(Conference),
    AppVersion = kapps_conference:application_version(Conference),
    ConferenceId = kapps_conference:id(Conference),
    Req = [{<<"Conference-ID">>, ConferenceId}
           | kz_api:default_headers(AppName, AppVersion)
          ],
    ReqResp = kz_amqp_worker:call_collect(Req
                                         ,fun kapi_conference:publish_search_req/1
                                         ,{'ecallmgr', fun kapi_conference:search_resp_v/1, 'true'}
                                         ),
    lager:debug("searching for conference ~s", [ConferenceId]),
    case ReqResp of
        {'error', _R}=E ->
            lager:info("received error while searching for ~s: ~-800p", [ConferenceId, _R]),
            E;
        {_, JObjs} -> conference_search_filter(JObjs, ConferenceId)
    end.

-spec conference_search_filter(kz_json:objects(), kz_term:ne_binary()) ->
                                      {'ok', kz_json:object()} |
                                      {'error', 'not_found'}.
conference_search_filter([], ConferenceId) ->
    lager:info("received invalid conference search response for ~s", [ConferenceId]),
    {'error', 'not_found'};
conference_search_filter([JObj|JObjs], ConferenceId) ->
    case kapi_conference:search_resp_v(JObj) of
        'true' ->
            lager:info("received valid conference search response for ~s", [ConferenceId]),
            {'ok', JObj};
        'false' ->
            conference_search_filter(JObjs, ConferenceId)
    end.

-spec deaf_participant(non_neg_integer(), kapps_conference:conference()) -> 'ok'.
deaf_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"deaf_participant">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participant_energy(non_neg_integer(),  kz_term:ne_binary(), kapps_conference:conference()) -> 'ok'.
participant_energy(ParticipantId, EnergyLevel, Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ,{<<"Energy-Level">>, EnergyLevel}
              ],
    send_command(Command, Conference).


-spec kick(kapps_conference:conference()) -> 'ok'.
kick(Conference) ->
    kick('undefined', Conference).

-spec kick(non_neg_integer() | 'undefined', kapps_conference:conference()) -> 'ok'.
kick(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"kick">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec lock(kapps_conference:conference()) -> 'ok'.
lock(Conference) ->
    Command = [{<<"Application-Name">>, <<"lock">>}],
    send_command(Command, Conference).

-spec mute_participant(non_neg_integer(), kapps_conference:conference()) -> 'ok'.
mute_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"mute_participant">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ],
    send_command(Command, Conference).


-spec prompt(kz_term:ne_binary(), kapps_conference:conference()) -> 'ok'.
prompt(Media, Conference) ->
    prompt(Media, 'undefined', Conference).

-spec prompt(kz_term:ne_binary(), non_neg_integer() | 'undefined', kapps_conference:conference()) -> 'ok'.
prompt(Media, ParticipantId, Conference) ->
    Prompt = get_media_prompt(Media, Conference),
    play(Prompt, ParticipantId, Conference).

-spec get_media_prompt(kz_term:ne_binary(), kapps_conference:conference()) -> kz_term:ne_binary().
get_media_prompt(Media, Conference) ->
    Call = kapps_conference:call(Conference),
    kapps_call:get_prompt(Call, Media).

-spec play_command(kz_term:ne_binary()) -> kz_term:proplist().
play_command(Media) ->
    play_command(Media, 'undefined').

-spec play_command(kz_term:ne_binary(), non_neg_integer() | 'undefined') -> kz_term:proplist().
play_command(Media, ParticipantId) ->
    [{<<"Application-Name">>, <<"play">>}
    ,{<<"Media-Name">>, Media}
    ,{<<"Participant-ID">>, ParticipantId}
    ].

-spec play(kz_term:ne_binary(), kapps_conference:conference()) -> 'ok'.
play(Media, Conference) ->
    play(Media, 'undefined', Conference).

-spec play(kz_term:ne_binary(), non_neg_integer() | 'undefined', kapps_conference:conference()) -> 'ok'.
play(Media, ParticipantId, Conference) ->
    Command = play_command(Media, ParticipantId),
    send_command(Command, Conference).

-spec record(kapps_conference:conference()) -> 'ok'.
record(Conference) ->
    Command = [{<<"Application-Name">>, <<"record">>}],
    send_command(Command, Conference).

-spec recordstop(kapps_conference:conference()) -> 'ok'.
recordstop(Conference) ->
    Command = [{<<"Application-Name">>, <<"recordstop">>}],
    send_command(Command, Conference).

-spec relate_participants(non_neg_integer(), non_neg_integer(), kapps_conference:conference()) -> 'ok'.
relate_participants(ParticipantId, OtherParticipantId, Conference) ->
    relate_participants(ParticipantId, OtherParticipantId, 'undefined', Conference).

-spec relate_participants(non_neg_integer(), non_neg_integer(), kz_term:api_binary(), kapps_conference:conference()) -> 'ok'.
relate_participants(ParticipantId, OtherParticipantId, Relationship, Conference) ->
    Command = [{<<"Application-Name">>, <<"relate_participants">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ,{<<"Other-Participant">>, OtherParticipantId}
              ,{<<"Relationship">>, Relationship}
              ],
    send_command(Command, Conference).

-spec stop_play(kapps_conference:conference()) -> 'ok'.
stop_play(Conference) ->
    stop_play('undefined', Conference).

-spec stop_play(non_neg_integer() | 'undefined', kapps_conference:conference()) -> 'ok'.
stop_play(ParticipantId, Conference) ->
    stop_play(ParticipantId, 'undefined', Conference).

-spec stop_play(non_neg_integer() | 'undefined', kz_term:api_binary(), kapps_conference:conference()) -> 'ok'.
stop_play(ParticipantId, Affects, Conference) ->
    Command = [{<<"Application-Name">>, <<"stop_play">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ,{<<"Affects">>, Affects}
              ],
    send_command(Command, Conference).

-spec undeaf_participant(non_neg_integer(), kapps_conference:conference()) -> 'ok'.
undeaf_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"undeaf_participant">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec unlock(kapps_conference:conference()) -> 'ok'.
unlock(Conference) ->
    Command = [{<<"Application-Name">>, <<"unlock">>}],
    send_command(Command, Conference).

-spec unmute_participant(non_neg_integer(), kapps_conference:conference()) -> 'ok'.
unmute_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"unmute_participant">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participant_volume_in(non_neg_integer(),  kz_term:ne_binary(), kapps_conference:conference()) -> 'ok'.
participant_volume_in(ParticipantId, VolumeIn, Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ,{<<"Volume-In-Level">>, VolumeIn}
              ],
    send_command(Command, Conference).

-spec participant_volume_out(non_neg_integer(),  kz_term:ne_binary(), kapps_conference:conference()) -> 'ok'.
participant_volume_out(ParticipantId, VolumeOut,Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
              ,{<<"Participant-ID">>, ParticipantId}
              ,{<<"Volume-Out-Level">>, VolumeOut}
              ],
    send_command(Command, Conference).

-spec dial(kz_json:objects(), kapps_conference:conference()) -> 'ok'.
dial(Endpoints, Conference) when is_list(Endpoints) ->
    dial(Endpoints, 'undefined', 'undefined', Conference).

-spec dial(kz_json:objects(), kz_term:api_ne_binary(), kapps_conference:conference()) -> 'ok'.
dial(Endpoints, CallerIdNumber, Conference) when is_list(Endpoints) ->
    dial(Endpoints, CallerIdNumber, 'undefined', Conference).

-spec dial(kz_json:objects(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kapps_conference:conference()) -> 'ok'.
dial(Endpoints, CallerIdNumber, CallerIdName, ConferenceId) ->
    dial(Endpoints, CallerIdNumber, CallerIdName, 'undefined', ConferenceId).

-spec dial(kz_json:objects(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_object(), kapps_conference:conference() | kz_term:ne_binary()) -> 'ok'.
dial(Endpoints, CallerIdNumber, CallerIdName, CCVs, ConferenceId)
  when is_list(Endpoints),
       is_binary(ConferenceId) ->
    Command = [{<<"Application-Name">>, <<"dial">>}
              ,{<<"Endpoints">>, Endpoints}
              ,{<<"Caller-ID-Name">>, CallerIdName}
              ,{<<"Caller-ID-Number">>, CallerIdNumber}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ,{<<"Conference-ID">>, ConferenceId}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    'ok' = kz_amqp_worker:cast(Command, fun(P) -> kapi_conference:publish_dial(ConferenceId, P) end);
dial(Endpoints, CallerIdNumber, CallerIdName, CCVs, Conference) ->
    dial(Endpoints, CallerIdNumber, CallerIdName, CCVs, kapps_conference:id(Conference)).

-spec send_command(kz_term:api_terms(), kapps_conference:conference()) -> 'ok'.
send_command([_|_]=Command, Conference) ->
    Q = kapps_conference:controller_queue(Conference),
    ConferenceId = kapps_conference:id(Conference),
    AppName = kapps_conference:application_name(Conference),
    AppVersion = kapps_conference:application_version(Conference),
    Focus = kapps_conference:focus(Conference),
    Prop = Command ++ [{<<"Conference-ID">>, ConferenceId}
                       | kz_api:default_headers(Q, <<"conference">>, <<"command">>, AppName, AppVersion)
                      ],
    case kz_term:is_empty(Focus) of
        'true' -> kz_amqp_worker:cast(Prop, fun(P) -> kapi_conference:publish_command(ConferenceId, P) end);
        'false' -> kz_amqp_worker:cast(Prop, fun(P) -> kapi_conference:publish_targeted_command(Focus, P) end)
    end;
send_command(JObj, Conference) -> send_command(kz_json:to_proplist(JObj), Conference).

-spec play_macro(kz_term:ne_binaries(), kapps_conference:conference()) -> 'ok'.
play_macro(Macro, Conference) ->
    Values = [{<<"Event-Category">>, <<"conference">>}
             ,{<<"Event-Name">>, <<"command">>}
             ,{<<"Conference-ID">>, kapps_conference:id(Conference)}
             ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    Prop = [{<<"Application-Name">>, <<"play_macro">>}
           ,{<<"Media-Macro">>, Macro}
            | Values
           ],
    send_command(Prop, Conference).
