%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_disa).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:debug("starting DISA handler"),

    Pin = wh_json:get_value(<<"pin">>, Data),
    Retries = wh_json:get_integer_value(<<"retries">>, Data, 3),

    try_collect_pin(Call, Pin, Retries).

try_collect_pin(Call, <<>>, _) ->
    lager:debug("no pin set on DISA object, permitting"),
    allow_dial(Call);
try_collect_pin(Call, _, 0) ->
    lager:debug("retries for DISA pin exceeded"),
    whapps_call_command:hangup(Call);
try_collect_pin(Call, Pin, Retries) ->
    play_enter_pin(Call),
    case whapps_call_command:collect_digits(6, Call) of
        {ok, Pin} ->
            lager:debug("pin matches, permitting"),
            allow_dial(Call);
        {ok, _Digits} ->
            lager:debug("caller entered ~s for pin", [_Digits]),
            play_invalid_pin(Call),
            try_collect_pin(Call, Pin, Retries-1)
    end.

allow_dial(Call) ->
    play_dialtone(Call),
    {ok, Digits} = whapps_call_command:collect_digits(15, Call),
    lager:debug("caller is trying to call ~s", [Digits]),

    case cf_util:lookup_callflow(Digits, whapps_call:account_id(Call)) of
        {ok, Flow, _} ->
            lager:debug("callflow ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)]),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow), Call);
        _ ->
            lager:debug("failed to find a callflow to satisfy ~s", [Digits]),
            cf_exe:continue(Call)
    end.

play_enter_pin(Call) ->
    whapps_call_command:b_play(<<"local_stream://en/us/callie/ivr/ivr-please_enter_pin_followed_by_pound.wav">>, Call).

play_invalid_pin(Call) ->
    whapps_call_command:b_play(<<"local_stream://en/us/callie/ivr/ivr-pin_or_extension_is-invalid.wav">>, Call).

play_dialtone(Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                              ,{<<"Duration-ON">>, <<"10000">>}
                              ,{<<"Duration-OFF">>, <<"0">>}
                              ]),
    whapps_call_command:tones([Tone], Call).
