-module(kazoo_asr).

-export([freeform/1, freeform/2, freeform/3, freeform/4
        ,commands/2, commands/3, commands/4, commands/5
        ]).

-include("kazoo_speech.hrl").

-spec default_provider() -> ne_binary().
default_provider() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, <<"ispeech">>).

-spec default_mime_type() -> ne_binary().
default_mime_type() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_mime_type">>, <<"application/wav">>).

-spec default_locale() -> ne_binary().
default_locale() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_locale">>, <<"en-us">>).

%%------------------------------------------------------------------------------
%% Transcribe the audio binary
%%------------------------------------------------------------------------------
-spec freeform(binary()) -> asr_resp().
-spec freeform(binary(), ne_binary()) -> asr_resp().
-spec freeform(binary(), ne_binary(), ne_binary()) -> asr_resp().
-spec freeform(binary(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
-spec freeform(binary(), ne_binary(), ne_binary(), kz_proplist(), ne_binary()) -> asr_resp().
freeform(Content) ->
    freeform(Content, default_mime_type()).
freeform(Content, ContentType) ->
    freeform(Content, ContentType, default_locale()).
freeform(Content, ContentType, Locale) ->
    freeform(Content, ContentType, Locale, []).
freeform(Content, ContentType, Locale, Options) ->
    freeform(Content, ContentType, Locale, Options, default_provider()).

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
-spec commands(ne_binary(), ne_binaries()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
-spec commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist(), ne_binary()) -> provider_return().
commands(Bin, Commands) ->
    commands(Bin, Commands, default_mime_type()).
commands(Bin, Commands, ContentType) ->
    commands(Bin, Commands, ContentType, default_locale()).
commands(Bin, Commands, ContentType, Locale) ->
    commands(Bin, Commands, ContentType, Locale, []).
commands(Bin, Commands, ContentType, Locale, Options) ->
    commands(Bin, Commands, ContentType, Locale, Options, default_provider()).

commands(Bin, Commands, ContentType, Locale, Options, ASRProvider) ->
    try (kz_term:to_atom(<<"kazoo_asr_", ASRProvider/binary>>, 'true')):commands(Bin, Commands, ContentType, Locale, Options)
    catch
        'error':'undef' ->
            lager:error("unknown ASR provider ~s", [ASRProvider]),
            {'error', 'unknown_provider'}
    end.
