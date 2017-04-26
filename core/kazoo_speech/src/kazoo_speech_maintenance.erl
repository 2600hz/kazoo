-module(kazoo_speech_maintenance).

-export([set_tts_api_key/2
        ,set_tts_provider/1
        ,set_tts_language/1
        ]).

-include("kazoo_speech.hrl").

-spec set_tts_api_key(ne_binary(), ne_binary()) -> 'ok'.
set_tts_api_key(Provider, APIKey) ->
    (kazoo_tts:provider_module(Provider)):set_api_key(APIKey).

-spec set_tts_provider(ne_binary()) -> 'ok'.
set_tts_provider(Provider) ->
    case code:which(kazoo_tts:provider_module(Provider)) of
        'non_existing' -> io:format("No provider module for ~s~n", [Provider]);
        _Path ->
            kazoo_tts:set_default_provider(Provider),
            io:format("Updated TTS provider to ~s~n", [Provider])
    end.

-spec set_tts_language(ne_binary()) -> 'ok'.
set_tts_language(Language) ->
    kazoo_tts:set_default_language(Language),
    io:format("updated default language to ~s~n", [Language]).
