%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tts_provider).

-include("kazoo_speech.hrl").

-callback create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().

-callback set_api_key(kz_term:ne_binary()) -> 'ok'.

-callback decode(kz_term:ne_binary(), kz_json:object(), any()) -> decode_resp().

-optional_callbacks([decode/3]).
