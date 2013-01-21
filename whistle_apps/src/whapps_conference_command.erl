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

-export([search/1, search/2]).
-export([deaf_participant/2]).
-export([participant_energy/3]).
-export([kick/1, kick/2]).
-export([participants/1]).
-export([lock/1]).
-export([mute_participant/2]).
-export([prompt/2, prompt/3]).
-export([play/2, play/3]).
-export([record/1]).
-export([relate_participants/3, relate_participants/4]).
-export([stop_play/1, stop_play/2, stop_play/3]).
-export([undeaf_participant/2]).
-export([unlock/1]).
-export([unmute_participant/2]).
-export([participant_volume_in/3]).
-export([participant_volume_out/3]).

-spec search/1 :: (whapps_conference:conference()) -> 'undefined'.
-spec search/2 :: (SearchId, whapps_conference:conference()) -> SearchId.

search(Conference) ->
    search(undefined, Conference).

search(undefined, Conference) ->
    search(couch_mgr:get_uuid(), Conference);
search(SearchId, Conference) ->
    Q = whapps_conference:controller_queue(Conference),
    AppName = whapps_conference:application_name(Conference),
    AppVersion = whapps_conference:application_version(Conference),

    lager:debug("searching for conference: ~s", [whapps_conference:id(Conference)]),
    Search = [{<<"Conference-ID">>, whapps_conference:id(Conference)}
              ,{<<"Msg-ID">>, SearchId}
              | wh_api:default_headers(Q, AppName, AppVersion)
             ],
    wapi_conference:publish_search_req(Search), 
    SearchId.

-spec deaf_participant/2 :: (ne_binary(), whapps_conference:conference()) -> 'ok'.
deaf_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"deaf_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participant_energy/3 :: (ne_binary(),  ne_binary(), whapps_conference:conference()) -> 'ok'.
participant_energy(ParticipantId, EnergyLevel, Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Energy-Level">>, EnergyLevel}
              ],
    send_command(Command, Conference).

-spec kick/1 :: (whapps_conference:conference()) -> 'ok'.
-spec kick/2 :: (api_binary(), whapps_conference:conference()) -> 'ok'.

kick(Conference) ->
    kick(undefined, Conference).
kick(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"kick">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participants/1 :: (whapps_conference:conference()) -> 'ok'.
participants(Conference) ->
    Command = [{<<"Application-Name">>, <<"participants">>}],
    send_command(Command, Conference).

-spec lock/1 :: (whapps_conference:conference()) -> 'ok'.
lock(Conference) ->
    Command = [{<<"Application-Name">>, <<"lock">>}],
    send_command(Command, Conference).

-spec mute_participant/2 :: (ne_binary(), whapps_conference:conference()) -> 'ok'.
mute_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"mute_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec prompt/2 :: (ne_binary(), whapps_conference:conference()) -> 'ok'.
-spec prompt/3 :: (ne_binary(), api_binary(), whapps_conference:conference()) -> 'ok'.

prompt(Media, Conference) ->
    prompt(Media, undefined, Conference).
prompt(Media, ParticipantId, Conference) ->
    play(whapps_util:get_prompt(Media, undefined), ParticipantId, Conference).


-spec play/2 :: (ne_binary(), whapps_conference:conference()) -> 'ok'.
-spec play/3 :: (ne_binary(), api_binary(), whapps_conference:conference()) -> 'ok'.

play(Media, Conference) ->
    play(Media, undefined, Conference).
play(Media, ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"play">>}
               ,{<<"Media-Name">>, Media}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).    

-spec record/1 :: (whapps_conference:conference()) -> 'ok'.
record(Conference) ->
    Command = [{<<"Application-Name">>, <<"record">>}],
    send_command(Command, Conference).

-spec relate_participants/3 :: (ne_binary(), ne_binary(), whapps_conference:conference()) -> 'ok'.
-spec relate_participants/4 :: (ne_binary(), ne_binary(), api_binary(), whapps_conference:conference()) -> 'ok'.

relate_participants(ParticipantId, OtherParticipantId, Conference) ->
    relate_participants(ParticipantId, OtherParticipantId, undefined, Conference).

relate_participants(ParticipantId, OtherParticipantId, Relationship, Conference) ->
    Command = [{<<"Application-Name">>, <<"relate_participants">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Other-Participant">>, OtherParticipantId}
               ,{<<"Relationship">>, Relationship}
              ],
    send_command(Command, Conference).    

-spec stop_play/1 :: (whapps_conference:conference()) -> 'ok'.
-spec stop_play/2 :: (api_binary(), whapps_conference:conference()) -> 'ok'.
-spec stop_play/3 :: (api_binary(), api_binary(), whapps_conference:conference()) -> 'ok'.

stop_play(Conference) ->
    stop_play(undefined, Conference).

stop_play(ParticipantId, Conference) ->
    stop_play(ParticipantId, undefined, Conference).

stop_play(ParticipantId, Affects, Conference) ->
    Command = [{<<"Application-Name">>, <<"stop_play">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Affects">>, Affects}
              ],
    send_command(Command, Conference).

-spec undeaf_participant/2 :: (ne_binary(), whapps_conference:conference()) -> 'ok'.
undeaf_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"undeaf_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec unlock/1 :: (whapps_conference:conference()) -> 'ok'.
unlock(Conference) ->
    Command = [{<<"Application-Name">>, <<"unlock">>}],
    send_command(Command, Conference).

-spec unmute_participant/2 :: (ne_binary(), whapps_conference:conference()) -> 'ok'.
unmute_participant(ParticipantId, Conference) ->
    Command = [{<<"Application-Name">>, <<"unmute_participant">>}
               ,{<<"Participant">>, ParticipantId}
              ],
    send_command(Command, Conference).

-spec participant_volume_in/3 :: (ne_binary(),  ne_binary(), whapps_conference:conference()) -> 'ok'.
participant_volume_in(ParticipantId, VolumeIn, Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Volume-In-Level">>, VolumeIn}
              ],
    send_command(Command, Conference).

-spec participant_volume_out/3 :: (ne_binary(),  ne_binary(), whapps_conference:conference()) -> 'ok'.
participant_volume_out(ParticipantId, VolumeOut,Conference) ->
    Command = [{<<"Application-Name">>, <<"participant_energy">>}
               ,{<<"Participant">>, ParticipantId}
               ,{<<"Volume-Out-Level">>, VolumeOut}
              ],
    send_command(Command, Conference).

-spec send_command/2 :: (proplist(), whapps_conference:conference()) -> 'ok'.
send_command(Command, Conference) ->
    Q = whapps_conference:controller_queue(Conference),
    ConferenceId = whapps_conference:id(Conference),
    AppName = whapps_conference:application_name(Conference),
    AppVersion = whapps_conference:application_version(Conference),
    Prop = Command ++ [{<<"Conference-ID">>, ConferenceId}
                       | wh_api:default_headers(Q, <<"conference">>, <<"command">>, AppName, AppVersion)
                      ],
    wapi_conference:publish_command(ConferenceId, Prop).
