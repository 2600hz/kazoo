%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016, Vladimir Ralev
%%% @doc Allow to change volume for channels, mute and unmute.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>`start' or `stop'.</dd>
%%%
%%%   <dt>`mode'</dt>
%%%   <dd>`read' or `write' (depends on direction).</dd>
%%%
%%%   <dt>`level'</dt>
%%%   <dd>`-4' is mute, `0' is unmute.</dd>
%%% </dl>
%%%
%%% @author Vladimir Ralev
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_audio_level).

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
