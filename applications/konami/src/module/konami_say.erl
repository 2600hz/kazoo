%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Say something
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_say).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) -> {'continue', whapps_call:call()}.
handle(Data, Call) ->
    Say = wh_json:get_value(<<"text">>, Data),
    lager:debug("saying '~s'", [Say]),
    whapps_call_command:b_say(Say, Call),
    {'continue', Call}.
