%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(milliwatt_tone).

-export([exec/1]).

-include("milliwatt.hrl").

-define(FREQUENCIES, [<<"2600">>]).
-define(DURATION, 30000).

-spec exec(kapps_call:call()) -> 'ok'.
exec(Call) ->
    Tone = get_tone(),
    Duration = kz_json:get_integer_value(<<"Duration-ON">>, Tone, ?DURATION),
    lager:info("milliwatt execute action tone"),
    kapps_call_command:answer(Call),
    timer:sleep(500),
    kapps_call_command:tones([Tone], Call),
    timer:sleep(Duration),
    kapps_call_command:hangup(Call).

-spec get_tone() -> kz_json:object().
get_tone() ->
    JObj = ?TONE,
    Hz = kz_json:get_list_value(<<"frequencies">>, JObj, ?FREQUENCIES),
    Duration = kz_json:get_value(<<"duration">>, JObj, ?DURATION),
    kz_json:from_list(
      [{<<"Frequencies">>, Hz}
      ,{<<"Duration-ON">>, kz_util:to_binary(Duration)}
      ,{<<"Duration-OFF">>, <<"1000">>}
      ]
     ).
