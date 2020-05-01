%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_tts).

-export([create/1
        ,create/2
        ,create/3
        ,create/4
        ,create/5
        ]).

-export([decode/4]).

-export([cache_time_ms/0
        ,default_provider/0, default_provider/1, set_default_provider/1
        ,default_language/0, set_default_language/1
        ,default_voice/0, set_default_voice/1

        ,provider_module/1
        ]).

-include("kazoo_speech.hrl").

-spec create(kz_term:ne_binary()) -> create_resp().
create(Text) ->
    create(Text, <<"female/en-us">>).

-spec create(kz_term:ne_binary(), kz_term:ne_binary()) -> create_resp().
create(Text, Voice) ->
    create(Text, Voice, <<"wav">>).

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> create_resp().
create(Text, Voice, Format) ->
    create(Text, Voice, Format, []).

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    create(default_provider(), Text, Voice, Format, Options).

-spec create(kz_term:api_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create('undefined', Text, Voice, Format, Options) ->
    create(Text, Voice, Format, Options);
create(<<"flite">>, _Text, _Voice, _Format, _Options) ->
    {'error', 'tts_provider_failure', <<"flite is not available to create TTS media">>};
create(Provider, Text, Voice, Format, Options) ->
    lager:debug("using provider ~s to create tts", [Provider]),
    Module = provider_module(Provider),
    Module:create(Text, Voice, Format, Options).

-spec decode(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), any()) -> decode_resp().
decode(Provider, Contents, Metadata, ProviderData) ->
    lager:debug("using provider ~s to decode response", [Provider]),
    Module = provider_module(Provider),
    case kz_module:is_exported(Module, 'decode', 3) of
        'true' -> Module:decode(Contents, Metadata, ProviderData);
        'false' -> {Contents, Metadata}
    end.

-spec provider_module(kz_term:ne_binary()) -> atom().
provider_module(Provider) ->
    kz_term:to_atom(<<"kazoo_tts_", Provider/binary>>, 'true').

-spec cache_time_ms() -> pos_integer().
cache_time_ms() ->
    kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_cache">>, ?MILLISECONDS_IN_HOUR).

-spec default_provider() -> kz_term:ne_binary().
default_provider() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>).

-spec default_provider(kapps_call:call()) -> kz_term:ne_binary().
default_provider(Call) ->
    Default = default_provider(),
    kapps_account_config:get_global(kapps_call:account_id(Call)
                                   ,?MOD_CONFIG_CAT
                                   ,<<"tts_provider">>
                                   ,Default
                                   ).

-spec set_default_provider(kz_term:ne_binary()) -> 'ok'.
set_default_provider(Provider) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_provider">>, Provider),
    'ok'.

-spec default_language() -> kz_term:ne_binary().
default_language() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_language">>, <<"en-us">>).

-spec set_default_language(kz_term:ne_binary()) -> 'ok'.
set_default_language(Language) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_language">>, Language),
    'ok'.

-spec default_voice() -> kz_term:ne_binary().
default_voice() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_voice">>, <<"male">>).

-spec set_default_voice(kz_term:ne_binary()) -> 'ok'.
set_default_voice(Voice) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_voice">>, Voice),
    'ok'.
