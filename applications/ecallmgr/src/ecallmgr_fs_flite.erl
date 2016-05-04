%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Helpers for mod_flite
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_flite).

-include("ecallmgr.hrl").

-export([call_command/3
         ,voice/1
        ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TTS command helpers
%% @end
%%--------------------------------------------------------------------
-spec call_command(atom(), ne_binary(), kz_json:object()) ->
                       {ne_binary(), ne_binary()}.
call_command(Node, UUID, JObj) ->
    _ = ecallmgr_fs_command:set(Node, UUID
                          ,[{<<"tts_engine">>, <<"flite">>}
                            ,{<<"tts_voice">>, voice(JObj)}
                           ]),
    {<<"speak">>, kz_json:get_value(<<"Text">>, JObj)}.

-spec voice(maybe(binary()) | kz_json:object()) -> ne_binary().
voice('undefined') -> <<"slt">>;
voice(<<"male">>) -> <<"rms">>;
voice(<<"female">>) -> <<"slt">>;
voice(JObj) ->
    case kz_json:is_json_object(JObj) of
        'true' -> voice(kz_json:get_value(<<"Voice">>, JObj));
        'false' -> voice(<<"female">>)
    end.
