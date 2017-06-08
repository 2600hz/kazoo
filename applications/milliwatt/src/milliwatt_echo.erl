%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
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
