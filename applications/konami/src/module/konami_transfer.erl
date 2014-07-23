%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Transfers caller to the extension extracted in the regex
%%% Data = {}
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_transfer).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(_Data, Call) ->
    lager:debug("putting b-leg ~s on hold", [whapps_call:other_leg_call_id(Call)]),
    lager:debug("data: ~p", [_Data]),
    whapps_call_command:hold(whapps_call:other_leg_call_id(Call)).
