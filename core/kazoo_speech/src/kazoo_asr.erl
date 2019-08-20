%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr).

-export([freeform/1, freeform/2, freeform/3, freeform/4
        ,commands/2, commands/3, commands/4, commands/5
        ]).

-include("kazoo_speech.hrl").

-spec default_provider() -> kz_term:ne_binary().
default_provider() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>).

-spec default_mime_type() -> kz_term:ne_binary().
default_mime_type() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_mime_type">>, <<"application/wav">>).

-spec default_locale() -> kz_term:ne_binary().
default_locale() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_locale">>, <<"en-us">>).

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------

-spec freeform(binary()) -> asr_resp().
freeform(Content) ->
    freeform(Content, default_mime_type()).

-spec freeform(binary(), kz_term:ne_binary()) -> asr_resp().
freeform(Content, ContentType) ->
    freeform(Content, ContentType, default_locale()).

-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> asr_resp().
freeform(Content, ContentType, Locale) ->
    freeform(Content, ContentType, Locale, []).

-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
freeform(Content, ContentType, Locale, Options) ->
    freeform(Content, ContentType, Locale, Options, default_provider()).

-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> asr_resp().
freeform(Content, ContentType, Locale, Options, ASRProvider) ->
    try (kz_term:to_atom(<<"kazoo_asr_", ASRProvider/binary>>, 'true')):freeform(Content, ContentType, Locale, Options)
    catch
        'error':'undef' ->
            lager:error("unknown ASR provider ~s", [ASRProvider]),
            {'error', 'unknown_provider'}
    end.

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries()) -> asr_resp().
commands(Bin, Commands) ->
    commands(Bin, Commands, default_mime_type()).

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> asr_resp().
commands(Bin, Commands, ContentType) ->
    commands(Bin, Commands, ContentType, default_locale()).

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary()) -> asr_resp().
commands(Bin, Commands, ContentType, Locale) ->
    commands(Bin, Commands, ContentType, Locale, []).

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
commands(Bin, Commands, ContentType, Locale, Options) ->
    commands(Bin, Commands, ContentType, Locale, Options, default_provider()).

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> asr_resp().
commands(Bin, Commands, ContentType, Locale, Options, ASRProvider) ->
    try (kz_term:to_atom(<<"kazoo_asr_", ASRProvider/binary>>, 'true')):commands(Bin, Commands, ContentType, Locale, Options)
    catch
        'error':'undef' ->
            lager:error("unknown ASR provider ~s", [ASRProvider]),
            {'error', 'unknown_provider'}
    end.
