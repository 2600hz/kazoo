%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_asr_provider).

-include("kazoo_speech.hrl").

-callback preferred_content_type() -> kz_term:ne_binary().
-callback accepted_content_types() -> kz_term:ne_binaries().
-callback freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
-callback commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> provider_return().
