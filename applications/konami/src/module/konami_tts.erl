%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Text To Speech
%%% Data = {
%%%   "text":"text to say"
%%%   ,"voice":"female"
%%%   ,"language":"en-US"
%%%   ,"terminators":["1", "3", "5"]
%%%   ,"engine":"flite"
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_tts).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    TTS = wh_json:get_value(<<"text">>, Data),
    lager:debug("tts: '~s'", [TTS]),

    TTSCommand = whapps_call_command:tts_command(TTS
                                                 ,wh_json:get_value(<<"voice">>, Data)
                                                 ,wh_json:get_value(<<"language">>, Data)
                                                 ,wh_json:get_value(<<"terminators">>, Data)
                                                 ,wh_json:get_value(<<"engine">>, Data)
                                                 ,Call
                                                ),
    whapps_call_command:send_command(
      wh_json:set_value(<<"Insert-At">>, <<"now">>, TTSCommand)
      ,Call
     ),
    {'continue', Call}.
