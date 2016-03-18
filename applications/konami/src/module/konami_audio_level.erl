%%%-------------------------------------------------------------------
%%% @author Vladimir Ralev
%%% @copyright (C) 2016, Vladimir Ralev
%%% @doc
%%% Allow to change volume for channels, mute and unmute.
%%% Data = {
%%%   "action":"start" // or "stop"
%%%   ,"mode":"read" // or "write" depends on direction
%%%   ,"level":"-4" // -4 is mute, 0 is unmute, other values are not studied
%%% }
%%% @end
%%% Created : 17. Mar 2016 6:25 AM
%%%-------------------------------------------------------------------
-module(konami_audio_level).
-author("vladimirralev").

-export([handle/2
]).

-include("konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    Action = wh_json:get_value(<<"action">>, Data),
    Mode = wh_json:get_value(<<"mode">>, Data),
    Level = wh_json:get_value(<<"level">>, Data),
    whapps_call_command:audio_level(Call, Mode, Action, Level),
    {'continue', Call}.
