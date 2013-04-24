%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whapps_conference_command).

-include("whapps_call_command.hrl").

-export([search/1]).
-export([deaf_participant/2]).
-export([participant_energy/3]).
-export([kick/1, kick/2]).
-export([hup/1, hup/2]).
-export([participants/1]).
-export([lock/1]).
-export([mute_participant/2]).
-export([prompt/2, prompt/3]).
-export([play/2, play/3]).
-export([record/1, recordstop/1]).
-export([relate_participants/3, relate_participants/4]).
-export([stop_play/1, stop_play/2, stop_play/3]).
-export([undeaf_participant/2]).
-export([unlock/1]).
-export([unmute_participant/2]).
-export([participant_volume_in/3]).
-export([participant_volume_out/3]).

-export([send_command/2]).

-spec search(whapps_conference:conference()) ->
                    {'ok', wh_json:object()} |
                    {'error', _}.
search(Conference) ->
    AppName = whapps_conference:application_name(Conference),
    AppVersion = whapps_conference:application_version(Conference),
    ConferenceId = whapps_conference:id(Conference),
    Req = [{<<"Conference-ID">>, ConferenceId}
           | wh_api:default_headers(AppName, AppVersion)
          ],
    ReqResp = whapps_util:amqp_pool_collect(Req
                                            ,fun wapi_conference:publish_search_req/1
                                            ,{'ecallmgr', fun wapi_conference:search_resp_v/1}
                                           ),
    lager:debug("searching for conference ~s", [ConferenceId]),
    case ReqResp of
        {'ok', [Response|_]} ->
            case wapi_conference:search_resp_v(Response) of
                'true' ->
                    lager:info("recieved valid conference search response for ~s", [ConferenceId]),
                    {'ok', Response};
                'false' ->
                    lager:info("recieved invalid conference search response for ~s", [ConferenceId]),
                    {'error', Response}
            end;
        {'timeout', _} ->
            lager:warning("timeout while searching for conference ~s", [ConferenceId]),
            timer:sleep(500),
            search(Conference);
        {'error', 'timeout'} ->
            lager:warning("timeout while searching for conference ~s", [ConferenceId]),
            timer:sleep(500),
            search(Conference);
        {'error', _R}=E ->
            lager:info("recieved error while searching for ~s: ~-800p", [ConferenceId, _R]),
            E
    end.

-spec deaf_participant(non_neg_integer(), whapps_conference:conference()) -> 'ok'.
deaf_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"deaf_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participant_energy(non_neg_integer(),  ne_binary(), whapps_conference:conference()) -> 'ok'.
participant_energy(ParticipantId, EnergyLevel, Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Energy-Level">>, EnergyLevel}
              ],
    send_command(Command, Conference).

-spec kick(whapps_conference:conference()) -> 'ok'.
-spec kick(non_neg_integer() | 'undefined', whapps_conference:conference()) -> 'ok'.

kick(Conference) ->
    kick('undefined', Conference).
kick(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"kick">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec hup(whapps_conference:conference()) -> 'ok'.
-spec hup(ne_binary() | integer(), whapps_conference:conference()) -> 'ok'.
hup(Conference) ->
    hup(<<"all">>, Conference).
hup(<<"all">> = Who, Conference) ->
    hup_conf(Who, Conference);
hup(<<"last">> = Who, Conference) ->
    hup_conf(Who, Conference);
hup(<<"non_moderator">> = Who, Conference) ->
    hup_conf(Who, Conference);
hup(ParticipantId, Conference) when is_integer(ParticipantId) ->
    hup_conf(ParticipantId, Conference);
hup(Who, Conference) ->
    hup(wh_util:to_integer(Who), Conference).

hup_conf(Who, Conference) ->
    Command = [{<<"Application-Name">>, <<"hup">>}
               ,{<<"Participant">>, Who}
              ],
    send_command(Command, Conference).

-spec participants(whapps_conference:conference()) -> 'ok'.
participants(Conference) ->
    Command = [{<<"Application-Name">>, <<"participants">>}],
    send_command(Command, Conference).

-spec lock(whapps_conference:conference()) -> 'ok'.
lock(Conference) ->
    Command = [{<<"Application-Name">>, <<"lock">>}],
    send_command(Command, Conference).

-spec mute_participant(non_neg_integer(), whapps_conference:conference()) -> 'ok'.
mute_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"mute_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec prompt(ne_binary(), whapps_conference:conference()) -> 'ok'.
-spec prompt(ne_binary(), non_neg_integer() | 'undefined', whapps_conference:conference()) -> 'ok'.

prompt(Media, Conference) ->
    prompt(Media, 'undefined', Conference).
prompt(Media, ParticipantId, Conference) ->
    play(whapps_util:get_prompt(Media, 'undefined'), ParticipantId, Conference).

-spec play(ne_binary(), whapps_conference:conference()) -> 'ok'.
-spec play(ne_binary(), non_neg_integer() | 'undefined', whapps_conference:conference()) -> 'ok'.

play(Media, Conference) ->
    play(Media, 'undefined', Conference).
play(Media, ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec record(whapps_conference:conference()) -> 'ok'.
record(Conference) ->
    Command = [{<<"Application-Name">>, <<"record">>}],
    send_command(Command, Conference).

-spec recordstop(whapps_conference:conference()) -> 'ok'.
recordstop(Conference) ->
    Command = [{<<"Application-Name">>, <<"recordstop">>}],
    send_command(Command, Conference).

-spec relate_participants(non_neg_integer(), non_neg_integer(), whapps_conference:conference()) -> 'ok'.
-spec relate_participants(non_neg_integer(), non_neg_integer(), api_binary(), whapps_conference:conference()) -> 'ok'.

relate_participants(ParticipantId, OtherParticipantId, Conference) ->
    relate_participants(ParticipantId, OtherParticipantId, 'undefined', Conference).

relate_participants(ParticipantId, OtherParticipantId, Relationship, Conference) ->
    Command = [{<<"Application-Name">>, <<"relate_participants">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Other-Participant">>, OtherParticipantId}
               ,{<<"Relationship">>, Relationship}
              ],
    send_command(Command, Conference).

-spec stop_play(whapps_conference:conference()) -> 'ok'.
-spec stop_play(non_neg_integer() | 'undefined', whapps_conference:conference()) -> 'ok'.
-spec stop_play(non_neg_integer() | 'undefined', api_binary(), whapps_conference:conference()) -> 'ok'.

stop_play(Conference) ->
    stop_play('undefined', Conference).

stop_play(ParticipantId, Conference) ->
    stop_play(ParticipantId, undefined, Conference).

stop_play(ParticipantId, Affects, Conference) ->
    Command = [{<<"Application-Name">>, <<"stop_play">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Affects">>, Affects}
              ],
    send_command(Command, Conference).

-spec undeaf_participant(non_neg_integer(), whapps_conference:conference()) -> 'ok'.
undeaf_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"undeaf_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec unlock(whapps_conference:conference()) -> 'ok'.
unlock(Conference) ->
    Command = [{<<"Application-Name">>, <<"unlock">>}],
    send_command(Command, Conference).

-spec unmute_participant(non_neg_integer(), whapps_conference:conference()) -> 'ok'.
unmute_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"unmute_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participant_volume_in(non_neg_integer(),  ne_binary(), whapps_conference:conference()) -> 'ok'.
participant_volume_in(ParticipantId, VolumeIn, Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Volume-In-Level">>, VolumeIn}
              ],
    send_command(Command, Conference).

-spec participant_volume_out(non_neg_integer(),  ne_binary(), whapps_conference:conference()) -> 'ok'.
participant_volume_out(ParticipantId, VolumeOut,Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Volume-Out-Level">>, VolumeOut}
              ],
    send_command(Command, Conference).

-spec send_command(api_terms(), whapps_conference:conference()) -> 'ok'.
send_command([_|_]=Command, Conference) ->
    Q = whapps_conference:controller_queue(Conference),
    ConferenceId = whapps_conference:id(Conference),
    AppName = whapps_conference:application_name(Conference),
    AppVersion = whapps_conference:application_version(Conference),
    Focus = whapps_conference:focus(Conference),
    Prop = Command ++ [{<<"Conference-ID">>, ConferenceId}
                       | wh_api:default_headers(Q, <<"conference">>, <<"command">>, AppName, AppVersion)
                      ],
    case wh_util:is_empty(Focus) of
        'true' -> wapi_conference:publish_command(ConferenceId, Prop);
        'false' -> wapi_conference:publish_targeted_command(Focus, Prop)
    end;
send_command(JObj, Conference) -> send_command(wh_json:to_proplist(JObj), Conference).
