%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Text To Speech
%%% Data = {
%%%   "text":"text to say"
%%%   ,"voice":"female"
%%%   ,"language":"en-US"
%%%   ,"terminators":["1", "3", "5"]
%%%   ,"engine":"flite"
%%% }
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_tts).

-export([handle/2]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()}.
handle(Data, Call) ->
    TTS = kz_json:get_value(<<"text">>, Data),
    lager:debug("tts: '~s'", [TTS]),

    TTSCommand = kapps_call_command:tts_command(TTS
                                               ,kz_json:get_value(<<"voice">>, Data)
                                               ,kz_json:get_value(<<"language">>, Data)
                                               ,kz_json:get_value(<<"terminators">>, Data)
                                               ,kz_json:get_value(<<"engine">>, Data)
                                               ,Call
                                               ),
    kapps_call_command:send_command(kz_json:set_value(<<"Insert-At">>, <<"now">>, TTSCommand)
                                   ,Call
                                   ),
    {'continue', Call}.
