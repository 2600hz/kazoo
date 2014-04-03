%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Put the caller (by default) on hold
%%% Data = {
%%%   "moh":"media_id"
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_hold).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    MOH = wh_json:get_value(<<"moh">>, Data),

    HoldCommand = whapps_call_command:hold_command(MOH, Call),
    whapps_call_command:send_command(
      wh_json:set_value(<<"Insert-At">>, <<"now">>, HoldCommand)
      ,Call
     ),
    {'continue', Call}.
