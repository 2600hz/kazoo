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

handle(Data, Call) ->
    Say = wh_json:get_value(<<"text">>, Data),
    lager:debug("saying '~s'", [Say]),
    whapps_call_command:say(Say, Call).
