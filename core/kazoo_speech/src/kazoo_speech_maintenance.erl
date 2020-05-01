%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_speech_maintenance).

-export([set_asr_api_key/2
        ,set_asr_provider/1
        ,set_tts_api_key/2
        ,set_tts_provider/1
        ,set_tts_language/1
        ]).

-include("kazoo_speech.hrl").

-spec set_tts_api_key(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_tts_api_key(Provider, APIKey) ->
    (kazoo_tts:provider_module(Provider)):set_api_key(APIKey).

-spec set_tts_provider(kz_term:ne_binary()) -> 'ok'.
set_tts_provider(Provider) ->
    case code:which(kazoo_tts:provider_module(Provider)) of
        'non_existing' -> io:format("No provider module for ~s~n", [Provider]);
        _Path ->
            kazoo_tts:set_default_provider(Provider),
            io:format("Updated TTS provider to ~s~n", [Provider])
    end.

-spec set_tts_language(kz_term:ne_binary()) -> 'ok'.
set_tts_language(Language) ->
    kazoo_tts:set_default_language(Language),
    io:format("updated default language to ~s~n", [Language]).

-spec set_asr_api_key(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
set_asr_api_key(Provider, APIKey) ->
    (kazoo_asr:provider_module(Provider)):set_api_key(APIKey).

-spec set_asr_provider(kz_term:ne_binary()) -> 'ok'.
set_asr_provider(Provider) ->
    case code:which(kazoo_asr:provider_module(Provider)) of
        'non_existing' -> io:format("No provider module for ~s~n", [Provider]);
        _Path ->
            kazoo_asr:set_default_provider(Provider),
            io:format("Updated ASR provider to ~s~n", [Provider])
    end.
