-module(milliwatt_tone).

-export([exec/1]).

-include("milliwatt.hrl").

-define(FREQUENCIES, [<<"2600">>]).
-define(DURATION, [<<"5000">>]).

exec(Call) ->
	Tone = get_tone(),
	lager:info("milliwatt execute action tone", []),
	whapps_call_command:answer(Call),
	whapps_call_command:tones([Tone], Call),
	whapps_call_command:queued_hangup(Call).

-spec get_tone() -> wh_json:json();
get_tone() ->
	JObj = whapps_config:get_non_empty(<<"milliwatt">>, <<"tone">>),
	Hz = wh_json:get_value(<<"frequencies">>, JObj, ?FREQUENCIES),
	Duration = wh_json:get_value(<<"duration">>, JObj, ?DURATION),
	wh_json:from_list([{<<"Frequencies">>, Hz}
                      ,{<<"Duration-ON">>, Duration}
                      ,{<<"Duration-OFF">>, <<"100">>}
                      ]).