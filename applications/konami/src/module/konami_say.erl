%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Say something
%%% Data = {
%%%   "text":"text to say"
%%%   ,"type":"name_spelled"
%%%   ,"method":"pronounced"
%%%   ,"language":"en"
%%% }
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_say).

-export([handle/2]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()}.
handle(Data, Call) ->
    Say = kz_json:get_value(<<"text">>, Data),
    lager:debug("saying '~s'", [Say]),

    SayCommand = kapps_call_command:say_command(Say
                                               ,kz_json:get_value(<<"type">>, Data)
                                               ,kz_json:get_value(<<"method">>, Data)
                                               ,kz_json:get_value(<<"language">>, Data)
                                               ,Call
                                               ),
    kapps_call_command:send_command(kz_json:set_value(<<"Insert-At">>, <<"now">>, SayCommand)
                                   ,Call
                                   ),
    {'continue', Call}.
