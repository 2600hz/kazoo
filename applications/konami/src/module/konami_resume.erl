%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Reconnect the two legs of the call, if possible
%%% Data = {
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_resume).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(_Data, Call) ->
    lager:debug("reconnecting ~s and ~s", [whapps_call:call_id(Call)
                                           ,whapps_call:other_leg_call_id(Call)
                                          ]),
    whapps_call_command:pickup(whapps_call:other_leg_call_id(Call), <<"now">>, Call),
    {'continue', Call}.
