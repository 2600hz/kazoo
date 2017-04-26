-module(gen_asr_provider).

-include("kazoo_speech.hrl").

-callback freeform(binary(), ne_binary(), ne_binary(), kz_proplist()) -> asr_resp().
-callback commands(ne_binary(), ne_binaries(), ne_binary(), ne_binary(), kz_proplist()) -> provider_return().
