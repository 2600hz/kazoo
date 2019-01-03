%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_asr_provider).

-include("kazoo_speech.hrl").

-callback freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
-callback commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> provider_return().
