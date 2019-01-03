%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @author Vladimir Ralev
%%% @doc Allow to change volume for channels, mute and unmute.
%%% Data = {
%%%   "action":"start" // or "stop"
%%%   ,"mode":"read" // or "write" depends on direction
%%%   ,"level":"-4" // -4 is mute, 0 is unmute, other values are not studied
%%% }
%%% Created : 17. Mar 2016 6:25 AM
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_audio_level).
-author("vladimirralev").

-export([handle/2
        ]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()}.
handle(Data, Call) ->
    Action = kz_json:get_value(<<"action">>, Data),
    Mode = kz_json:get_value(<<"mode">>, Data),
    Level = kz_json:get_value(<<"level">>, Data),
    kapps_call_command:audio_level(Call, Mode, Action, Level),
    {'continue', Call}.
