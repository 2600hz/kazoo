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

    RequestingLeg = wh_json:get_value(<<"dtmf_leg">>, Data),

    HoldLeg =
        case whapps_call:call_id(Call) of
            RequestingLeg -> whapps_call:other_leg_call_id(Call);
            CallId -> CallId
        end,

    HoldCommand = whapps_call_command:hold_command(MOH, HoldLeg),

    lager:debug("leg ~s is putting ~s on hold", [RequestingLeg, HoldLeg]),

    whapps_call_command:send_command(
      wh_json:set_value(<<"Insert-At">>, <<"now">>, HoldCommand)
      ,Call
     ),
    {'continue', Call}.
