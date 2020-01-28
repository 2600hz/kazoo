%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
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
        ,accepted_content_types/0
        ,default_provider/0, default_provider/1, set_default_provider/1
        ,preferred_content_type/0, preferred_content_type/1

        ,provider_module/1
        ]).

-include("kazoo_speech.hrl").

-define(DEFAULT_ASR_PROVIDER, <<"ispeech">>).
-define(DEFAULT_ASR_CONTENT_TYPE, <<"application/wav">>).
-define(DEFAULT_ASR_LOCALE, <<"en-us">>).
-define(ACCEPTED_CONTENT_TYPES, [<<"audio/mpeg">>, <<"audio/wav">>, <<"application/wav">>]).

%%%------------------------------------------------------------------------------
%%% @doc
%%% Return return configured or set the default ASR provider
%%% @end
%%%------------------------------------------------------------------------------
-spec default_provider() -> kz_term:ne_binary().
default_provider() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_provider">>, ?DEFAULT_ASR_PROVIDER).

-spec default_provider(kapps_call:call()) -> kz_term:ne_binary().
default_provider(Call) ->
    Default = default_provider(),
    kapps_account_config:get_global(kapps_call:account_id(Call)
                                   ,?MOD_CONFIG_CAT
                                   ,<<"asr_provider">>
                                   ,Default
                                   ).

-spec set_default_provider(kz_term:ne_binary()) -> 'ok'.
set_default_provider(Provider) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"asr_provider">>, Provider),
    'ok'.

%%%------------------------------------------------------------------------------
%%% @doc
%%% Return preferred content_type for ASR Provider
%%% @end
%%%------------------------------------------------------------------------------
-spec preferred_content_type() -> kz_term:ne_binary().
preferred_content_type() ->
    preferred_content_type(default_provider()).

-spec preferred_content_type(kz_term:ne_binary()) -> kz_term:ne_binary().
preferred_content_type(Provider) ->
    try (kz_term:to_atom(<<"kazoo_asr_", Provider/binary>>, 'true')):preferred_content_type()
    catch
        'error':'undef' ->
            lager:error("unknown provider ~s", [Provider]),
            ?DEFAULT_ASR_CONTENT_TYPE
    end.

%%%------------------------------------------------------------------------------
%%% @doc
%%% Return configured local for ASR
%%% @end
%%%------------------------------------------------------------------------------
-spec default_locale() -> kz_term:ne_binary().
default_locale() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_locale">>, ?DEFAULT_ASR_LOCALE).

%%%------------------------------------------------------------------------------
%%% @doc
%%% Return list of accepted content types for passthrough or conversion
%%% @end
%%%------------------------------------------------------------------------------
-spec accepted_content_types() -> kz_term:ne_binaries().
accepted_content_types() ->
    ?ACCEPTED_CONTENT_TYPES.

%%%------------------------------------------------------------------------------
%%% @doc
%%% Transcribe the audio binary
%%% @end
%%%------------------------------------------------------------------------------
-spec freeform(binary()) -> asr_resp().
freeform(Content) ->
    freeform(Content, preferred_content_type()).

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

%%%------------------------------------------------------------------------------
%%% @doc
%%% Transcribe the audio binary
%%% @end
%%%------------------------------------------------------------------------------
-spec commands(kz_term:ne_binary(), kz_term:ne_binaries()) -> asr_resp().
commands(Bin, Commands) ->
    commands(Bin, Commands, preferred_content_type()).

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

%%%------------------------------------------------------------------------------
%%% @doc
%%% Functions called from maintanance module
%%% @end
%%%------------------------------------------------------------------------------
-spec provider_module(kz_term:ne_binary()) -> atom().
provider_module(Provider) ->
    kz_term:to_atom(<<"kazoo_asr_", Provider/binary>>, 'true').
