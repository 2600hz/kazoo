%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Helpers for mod_flite
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_flite).

-include("ecallmgr.hrl").

-export([call_command/3
        ,voice/1
        ]).

%%------------------------------------------------------------------------------
%% @doc TTS command helpers
%% @end
%%------------------------------------------------------------------------------
-spec call_command(atom(), kz_term:ne_binary(), kz_json:object()) ->
          {kz_term:ne_binary(), kz_term:ne_binary()}.
call_command(Node, UUID, JObj) ->
    _ = ecallmgr_fs_command:set(Node, UUID
                               ,[{<<"tts_engine">>, <<"flite">>}
                                ,{<<"tts_voice">>, voice(JObj)}
                                ]),
    {<<"speak">>, kz_json:get_value(<<"Text">>, JObj)}.

-spec voice(kz_term:api_binary() | kz_json:object()) -> kz_term:ne_binary().
voice('undefined') -> <<"slt">>;
voice(<<"male">>) -> <<"rms">>;
voice(<<"female">>) -> <<"slt">>;
voice(JObj) ->
    case kz_json:is_json_object(JObj) of
        'true' -> voice(kz_json:get_value(<<"Voice">>, JObj));
        'false' -> voice(<<"female">>)
    end.
