%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tts_provider).

-include("kazoo_speech.hrl").

-callback create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().

-callback set_api_key(kz_term:ne_binary()) -> 'ok'.
