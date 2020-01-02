%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_tts_google_utils).

-export([getJson/1]).

-include("kazoo_tts_google.hrl").

-spec getJson(synthesisInput() | voiceSelectionParams() | audioConfig()) -> {'ok', kz_json:object()} |
          {'error', kz_term:ne_binary()}.
getJson(#synthesisInput{}=SysthesisInput) ->
    synthesis_input_to_json(SysthesisInput, validate_synthesis_input(SysthesisInput));
getJson(#voiceSelectionParams{}=VoiceSelection) ->
    voice_selection_to_json(VoiceSelection, validate_voice_selection(VoiceSelection));
getJson(#audioConfig{}=AudioConfig) ->
    audio_config_to_json(AudioConfig, validate_audio_config(AudioConfig)).

%% Checking is provided value is float in range 0.25 .. 4.0
-spec is_speakingRate(speakingRate()) -> boolean().
is_speakingRate('undefined') ->
    'true';
is_speakingRate(Value) when Value > 4.0 ->
    'false';
is_speakingRate(Value) when Value < 0.25 ->
    'false';
is_speakingRate(_) ->
    'true'.

%% Checking is provided value is float in range -20.0 .. 20.0
-spec is_pitch(pitch()) -> boolean().
is_pitch('undefined') ->
    'true';
is_pitch(Value) when Value > 20.0 ->
    'false';
is_pitch(Value) when Value < -20.0 ->
    'false';
is_pitch(_) ->
    'true'.

%% Checking is provided value is float in range -96.0 .. 16.0
-spec is_volumeGainDb(volumeGainDb()) -> boolean().
is_volumeGainDb('undefined') ->
    'true';
is_volumeGainDb(Value) when Value > 16.0 ->
    'false';
is_volumeGainDb(Value) when Value < -96.0 ->
    'false';
is_volumeGainDb(_) ->
    'true'.

-spec validate_voice_selection(voiceSelectionParams()) -> 'ok' | {'error', kz_term:ne_binary()}.
validate_voice_selection(#voiceSelectionParams{languageCode='undefined'}) ->
    {'error', <<"LanguageCode is mandatory">>};
validate_voice_selection(#voiceSelectionParams{}) -> 'ok'.

-spec validate_synthesis_input(synthesisInput()) -> 'ok' | {'error', kz_term:ne_binary()}.
validate_synthesis_input(#synthesisInput{text='undefined', ssml='undefined'}) ->
    {'error', <<"Text or Ssml must be set">>};
validate_synthesis_input(#synthesisInput{text='undefined'}) -> 'ok';
validate_synthesis_input(#synthesisInput{ssml='undefined'}) -> 'ok';
validate_synthesis_input(#synthesisInput{}) ->
    {'error', <<"Text and Ssml value cannot be set both">>}.

-spec validate_audio_config(audioConfig()) -> 'ok' | {'error', kz_term:ne_binary()}.
validate_audio_config(#audioConfig{audioEncoding='AUDIO_ENCODING_UNSPECIFIED'}) ->
    {'error', <<"Unsupported AudioEncoding value">>};
validate_audio_config(#audioConfig{speakingRate=SpeakingRate
                                  ,pitch=Pitch
                                  ,volumeGainDb=VolumeGainDb
                                  }) ->
    Fs = [{fun is_speakingRate/1, SpeakingRate, <<"SpeakingRate">>}
         ,{fun is_pitch/1, Pitch, <<"Pitch">>}
         ,{fun is_volumeGainDb/1, VolumeGainDb, <<"VolumeGainDb">>}
         ],
    validate_funs(Fs).

-type validate_fun() :: {fun((Value) -> boolean()), Value, kz_term:ne_binary()}.
-type validate_funs() :: [validate_fun()].
-spec validate_funs(validate_funs()) -> 'ok' | {'error', kz_term:ne_binary()}.
validate_funs([]) -> 'ok';
validate_funs([{Fun, Value, Var} | Fs]) ->
    case Fun(Value) of
        'true' -> validate_funs(Fs);
        'false' -> {'error', error_message(Var)}
    end.

-spec error_message(kz_term:ne_binary()) -> kz_term:ne_binary().
error_message(Var) -> <<"Wrong ", Var/binary, " value">>.

synthesis_input_to_json(_SynthesisInput, {'error', _Message}=Error) -> Error;
synthesis_input_to_json(#synthesisInput{text=Text
                                       ,ssml=SSML
                                       }
                       ,'ok') ->
    kz_json:set_values([{<<"text">>, Text}
                       ,{<<"ssml">>, SSML}
                       ]
                      ,kz_json:new()
                      ).

voice_selection_to_json(_VoiceSelection, {'error', _Message}=Error) -> Error;
voice_selection_to_json(#voiceSelectionParams{languageCode=Code
                                             ,name=Name
                                             ,ssmlGender=Gender
                                             }
                       ,'ok') ->
    kz_json:set_values([{<<"languageCode">>, Code}
                       ,{<<"name">>, Name}
                       ,{<<"ssmlGender">>, Gender}
                       ]
                      ,kz_json:new()
                      ).

audio_config_to_json(_AudioConfig, {'error', _Message}=Error) -> Error;
audio_config_to_json(#audioConfig{audioEncoding=Encoding
                                 ,speakingRate=SpeakingRate
                                 ,pitch=Pitch
                                 ,volumeGainDb=Gain
                                 ,sampleRateHertz=SampleRate
                                 }
                    ,'ok') ->
    kz_json:set_values([{<<"audioEncoding">>, Encoding}
                       ,{<<"speakingRate">>, SpeakingRate}
                       ,{<<"pitch">>, Pitch}
                       ,{<<"volumeGainDb">>, Gain}
                       ,{<<"sampleRateHertz">>, SampleRate}
                       ]
                      ,kz_json:new()
                      ).
