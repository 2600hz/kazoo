-module(kazoo_tts_google_utils).

-export([getJson/1
        ]).

-include("kazoo_tts_google.hrl").

%% Checking is provided value is float in range 0.25 .. 4.0
-spec is_speakingRate(speakingRate()) -> boolean().
is_speakingRate('undefined') ->
    true;
is_speakingRate(Value) when Value > 4.0 ->
    false;
is_speakingRate(Value) when Value < 0.25 ->
    false;
is_speakingRate(_) ->
    true.

%% Checking is provided value is float in range -20.0 .. 20.0
-spec is_pitch(pitch()) -> boolean().
is_pitch('undefined') ->
    true;
is_pitch(Value) when Value > 20.0 ->
    false;
is_pitch(Value) when Value < -20.0 ->
    false;
is_pitch(_) ->
    true.

%% Checking is provided value is float in range -96.0 .. 16.0
-spec is_volumeGainDb(volumeGainDb()) -> boolean().
is_volumeGainDb('undefined') ->
    true;
is_volumeGainDb(Value) when Value > 16.0 ->
    false;
is_volumeGainDb(Value) when Value < -96.0 ->
    false;
is_volumeGainDb(_) ->
    true.

-spec checkRecord(synthesisInput() | voiceSelectionParams() | audioConfig()) -> {'ok'} | {'error', string()}.
checkRecord(#synthesisInput{text=Text, ssml=Ssml}) ->
    case {Text, Ssml} of
        {'undefined', 'undefined'} -> {'error', <<"Text or Ssml must be set">>};
        {'undefined', _} -> {'ok'};
        {_, 'undefined'} -> {'ok'};
        {_, _} -> {'error', <<"Text and Ssml value cannot be set both">>}
    end;
checkRecord(#voiceSelectionParams{languageCode=LanguageCode}) ->
    case LanguageCode of
        'undefined' -> {'error', <<"LanguageCode is mandatory">>};
        _ -> {'ok'}
    end;
checkRecord(#audioConfig{audioEncoding=AudioEncoding
                        ,speakingRate=SpeakingRate
                        ,pitch=Pitch
                        ,volumeGainDb=VolumeGainDb}) ->
    case {AudioEncoding, is_speakingRate(SpeakingRate), is_pitch(Pitch), is_volumeGainDb(VolumeGainDb)} of
        {'AUDIO_ENCODING_UNSPECIFIED', _, _, _} -> {'error', <<"Unsupported AudioEncoding value">>};
        {_, false, _, _} -> {'error', <<"Wrong SpeakingRate value">>};
        {_, _, false, _} -> {'error', <<"Wrong Pitch value">>};
        {_, _, _, false} -> {'error', <<"Wrong VolumeGainDb value">>};
        {_, true, true, true} -> {'ok'}
    end.

-spec getJson(synthesisInput() | voiceSelectionParams() | audioConfig()) -> {'ok', kz_json:object()} |
                                                                            {'error', string()}.
getJson(#synthesisInput{}=Record) ->
    case checkRecord(Record) of
        {'error', _Message} -> {'error', _Message};
        {'ok'} ->
            Proplist=props:filter_undefined(
                       [{<<"text">>, Record#synthesisInput.text}
                       ,{<<"ssml">>, Record#synthesisInput.ssml}
                       ]),
            Ret=kz_json:from_list(Proplist),
            Ret
    end;
getJson(#voiceSelectionParams{}=Record) ->
    case checkRecord(Record) of
        {'error', _Message} -> {'error', _Message};
        {'ok'} ->
            Proplist=props:filter_undefined(
                       [{<<"languageCode">>, Record#voiceSelectionParams.languageCode}
                       ,{<<"name">>, Record#voiceSelectionParams.name}
                       ,{<<"ssmlGender">>, Record#voiceSelectionParams.ssmlGender}
                       ]),
            kz_json:from_list(Proplist)
    end;
getJson(#audioConfig{}=Record) ->
    case checkRecord(Record) of
        {'error', _Message} -> {'error', _Message};
        {'ok'} ->
            Proplist=props:filter_undefined(
                       [{<<"audioEncoding">>, Record#audioConfig.audioEncoding}
                       ,{<<"speakingRate">>, Record#audioConfig.speakingRate}
                       ,{<<"pitch">>, Record#audioConfig.pitch}
                       ,{<<"volumeGainDb">>, Record#audioConfig.volumeGainDb}
                       ,{<<"sampleRateHertz">>, Record#audioConfig.sampleRateHertz}
                       ]),
            kz_json:from_list(Proplist)
    end.
