%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_tts_provider).

-include("kazoo_speech.hrl").

-callback create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().

-callback set_api_key(kz_term:ne_binary()) -> 'ok'.

-callback decode(kz_term:ne_binary(), kz_json:object(), any()) -> decode_resp().

-optional_callbacks([decode/3]).
