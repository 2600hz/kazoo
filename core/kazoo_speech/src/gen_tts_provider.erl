-module(gen_tts_provider).

-include("kazoo_speech.hrl").

-callback create(ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> create_resp().

-callback set_api_key(ne_binary()) -> 'ok'.
