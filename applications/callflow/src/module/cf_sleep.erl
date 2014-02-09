%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% data:{
%%%   "duration":2 // how long to delay for
%%%   "unit":"s" // optional unit of time, defaults to seconds
%%% }
%%%
%%% unit can be one of: "ms", "s", "m", "h"
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_sleep).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DurationMS = get_duration_ms(Data),
    lager:debug("sleeping for ~b ms", [DurationMS]),
    case whapps_call_command:wait_for_hangup(DurationMS) of
        {'ok', 'channel_hungup'} -> cf_exe:stop(Call);
        {'error', 'timeout'} -> cf_exe:continue(Call)
    end.

-spec get_duration_ms(wh_json:object()) -> non_neg_integer().
get_duration_ms(Data) ->
    Duration = wh_json:get_integer_value(<<"duration">>, Data, 0),
    Unit = wh_json:get_value(<<"unit">>, Data, <<"s">>),
    duration_to_ms(Duration, Unit).

-spec duration_to_ms(integer(), ne_binary()) -> non_neg_integer().
duration_to_ms(Duration, <<"ms">>) ->
    constrain_duration(Duration);
duration_to_ms(Duration, <<"s">>) ->
    constrain_duration(Duration * 1000);
duration_to_ms(Duration, <<"m">>) ->
    constrain_duration(Duration * ?MILLISECONDS_IN_MINUTE);
duration_to_ms(Duration, <<"h">>) ->
    constrain_duration(Duration * ?MILLISECONDS_IN_HOUR);
duration_to_ms(Duration, _Unit) ->
    lager:debug("unknown unit: ~p", [_Unit]),
    duration_to_ms(Duration, <<"s">>).

-spec constrain_duration(integer()) -> integer().
constrain_duration(DurationMS) when DurationMS < 0 ->
    0;
constrain_duration(DurationMS) when DurationMS > ?MILLISECONDS_IN_DAY ->
    ?MILLISECONDS_IN_DAY;
constrain_duration(DurationMS) ->
    DurationMS.





