%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(milliwatt_tone).

-export([exec/1]).

-include("milliwatt.hrl").

-define(FREQUENCIES, [<<"2600">>]).
-define(FREQ_ON, 5 * ?MILLISECONDS_IN_SECOND).
-define(DURATION, 30 * ?MILLISECONDS_IN_SECOND).

-spec exec(kapps_call:call()) -> 'ok'.
exec(Call) ->
    ToneConfig = get_tone(),
    Duration = kapps_config:get_integer(?CONFIG_CAT, [<<"tone">>, <<"duration">>], ?DURATION),

    lager:info("milliwatt execute action tone"),
    kapps_call_command:answer(Call),
    timer:sleep(500),
    kapps_call_command:tones([ToneConfig], Call),
    timer:sleep(Duration),
    kapps_call_command:hangup(Call).

-spec get_tone() -> kz_json:object().
get_tone() ->
    Hz = kapps_config:get_ne_binaries(?CONFIG_CAT, [<<"tone">>, <<"frequencies">>], ?FREQUENCIES),
    FrequencyOn = kapps_config:get_integer(?CONFIG_CAT, [<<"tone">>, <<"frequency_on">>], ?FREQ_ON),
    FrequencyOff = kapps_config:get_integer(?CONFIG_CAT, [<<"tone">>, <<"frequency_off">>], 1000),

    kz_json:from_list(
      [{<<"Frequencies">>, Hz}
      ,{<<"Duration-ON">>, FrequencyOn}
      ,{<<"Duration-OFF">>, FrequencyOff}
      ]
     ).
