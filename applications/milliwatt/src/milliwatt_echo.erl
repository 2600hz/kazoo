%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(milliwatt_echo).

-export([exec/1]).

-include("milliwatt.hrl").

-define(DURATION, 10000).

-spec exec(kapps_call:call()) -> 'ok'.
exec(Call) ->
    lager:info("milliwatt execute action echo", []),
    kapps_call_command:answer(Call),
    kapps_call_command:echo(Call),
    timer:sleep(get_duration()),
    kapps_call_command:hangup(Call).

-spec get_duration() -> integer().
get_duration() ->
    JObj = kapps_config:get_json(?CONFIG_CAT, <<"echo">>),
    kz_json:get_integer_value(<<"duration">>, JObj, ?DURATION).
