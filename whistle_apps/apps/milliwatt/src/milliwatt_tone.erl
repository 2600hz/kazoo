-module(milliwatt_tone).

-export([exec/1]).

-include("milliwatt.hrl").

exec(Call) ->
	lager:info("milliwatt execute action tone", []),
	whapps_call_command:answer(Call),
	whapps_call_command:tones([wh_json:new()], Call),
	whapps_call_command:hangup(Call).