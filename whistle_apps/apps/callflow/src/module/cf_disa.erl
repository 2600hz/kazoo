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

    case try_collect_pin(Call, Pin, Retries) of
        allow -> allow_dial(Call, Retries);
        _ -> cf_exe:hangup(Call)
    end.

try_collect_pin(_Call, <<>>, _) ->
    lager:debug("no pin set on DISA object, permitting"),
    allow;
try_collect_pin(_Call, _, 0) ->
    lager:debug("retries for DISA pin exceeded"),
    fail;
try_collect_pin(Call, Pin, Retries) ->
    play_enter_pin(Call),
    case whapps_call_command:collect_digits(6, Call) of
        {ok, Pin} ->
            lager:debug("pin matches, permitting"),
            allow;
        {ok, _Digits} ->
            lager:debug("caller entered ~s for pin", [_Digits]),
            play_invalid_pin(Call),
            try_collect_pin(Call, Pin, Retries-1)
    end.

allow_dial(Call, 0) ->
    lager:debug("retries exceeded for finding a callflow"),
    cf_exe:continue(Call);
allow_dial(Call, Retries) ->
    _ = play_dialtone(Call),
    {ok, Digits} = whapps_call_command:collect_digits(15, Call),
    lager:debug("caller is trying to call ~s", [Digits]),

    case cf_util:lookup_callflow(Digits, whapps_call:account_id(Call)) of
        {ok, Flow, _} ->
            lager:debug("callflow ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)]),
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow), Call);
        _ ->
            lager:debug("failed to find a callflow to satisfy ~s", [Digits]),
            _ = play_invalid_ext(Call),
            allow_dial(Call, Retries-1)
    end.

play_enter_pin(Call) ->
    whapps_call_command:b_play(<<"local_stream://en/us/callie/ivr/ivr-please_enter_pin_followed_by_pound.wav">>, Call).

play_invalid_pin(Call) ->
    whapps_call_command:b_play(<<"local_stream://en/us/callie/ivr/ivr-pin_or_extension_is-invalid.wav">>, Call).

play_invalid_ext(Call) ->
    whapps_call_command:b_play(<<"local_stream://en/us/callie/ivr/ivr-you_have_dialed_an_invalid_extension.wav">>, Call).

play_dialtone(Call) ->
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                              ,{<<"Duration-ON">>, <<"10000">>}
                              ,{<<"Duration-OFF">>, <<"0">>}
                              ]),
    whapps_call_command:tones([Tone], Call).
