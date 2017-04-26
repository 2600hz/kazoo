-module(kazoo_tts).

-export([create/1
        ,create/2
        ,create/3
        ,create/4
        ,create/5
        ]).

-export([cache_time_ms/0
        ,default_provider/0, default_provider/1, set_default_provider/1
        ,default_language/0, set_default_language/1
        ,default_voice/0, set_default_voice/1

        ,provider_module/1
        ]).

-include("kazoo_speech.hrl").

-spec create(ne_binary()) -> create_resp().
create(Text) ->
    create(Text, <<"female/en-us">>).

-spec create(ne_binary(), ne_binary()) -> create_resp().
create(Text, Voice) ->
    create(Text, Voice, <<"wav">>).

-spec create(ne_binary(), ne_binary(), ne_binary()) -> create_resp().
create(Text, Voice, Format) ->
    create(Text, Voice, Format, []).

-spec create(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    create(default_provider(), Text, Voice, Format, Options).

-spec create(api_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().
create('undefined', Text, Voice, Format, Options) ->
    create(Text, Voice, Format, Options);
create(<<"flite">>, _Text, _Voice, _Format, _Options) ->
    {'error', 'tts_provider_failure', <<"flite is not available to create TTS media">>};
create(Provider, Text, Voice, Format, Options) ->
    (provider_module(Provider)):create(Text, Voice, Format, Options).

-spec provider_module(ne_binary()) -> atom().
provider_module(Provider) ->
    kz_term:to_atom(<<"kazoo_tts_", Provider/binary>>, 'true').

-spec cache_time_ms() -> pos_integer().
cache_time_ms() ->
    kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_cache">>, ?MILLISECONDS_IN_HOUR).

-spec default_provider() -> ne_binary().
-spec default_provider(kapps_call:call()) -> ne_binary().
default_provider() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_provider">>, <<"flite">>).

default_provider(Call) ->
    Default = default_provider(),
    kapps_account_config:get_global(Call, ?MOD_CONFIG_CAT, <<"tts_provider">>, Default).

-spec set_default_provider(ne_binary()) -> 'ok'.
set_default_provider(Provider) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_provider">>, Provider),
    'ok'.

-spec default_language() -> ne_binary().
default_language() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_language">>, <<"en-us">>).

-spec set_default_language(ne_binary()) -> 'ok'.
set_default_language(Language) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_language">>, Language),
    'ok'.

-spec default_voice() -> ne_binary().
default_voice() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"tts_voice">>, <<"male">>).

-spec set_default_voice(ne_binary()) -> 'ok'.
set_default_voice(Voice) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_voice">>, Voice),
    'ok'.
