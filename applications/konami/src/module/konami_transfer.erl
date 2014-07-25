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
handle(Data, Call) ->
    lager:debug("data: ~p", [_Data]),

    lager:debug("first, unbridge and put other leg on hold"),
    konami_hold:handle(Data, Call),

    [Extension|_] = wh_json:get_value(<<"captures">>, Data),
    lager:debug("ok, now we need to originate to the requested number ~s", [Extension]),

    %% originate to Extension
    %% enter FSM for attended_transfer state
    ok.
