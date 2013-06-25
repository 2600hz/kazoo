%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
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

exec(Call) ->
	lager:info("milliwatt execute action echo", []),
	whapps_call_command:answer(Call),
	whapps_call_command:echo(Call),
	timer:sleep(get_duration()),
	whapps_call_command:hangup(Call).

-spec get_duration() -> integer().
get_duration() ->
	JObj = whapps_config:get_non_empty(<<"milliwatt">>, <<"echo">>),
	wh_json:get_integer_value(<<"duration">>, JObj, ?DURATION).