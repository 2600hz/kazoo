%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Sleeping before hanging up.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`duration'</dt>
%%%   <dd>How long to delay for. Default is zero.</dd>
%%%
%%%   <dt>`unit'</dt>
%%%   <dd><strong>Optional: </strong>Unit of time, defaults to `s' (seconds).</dd>
%%% </dl>
%%%
%%% Unit can be one of: `ms', `s', `m', `h'.
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_sleep).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DurationMS = get_duration_ms(Data),
    lager:info("sleeping for ~b ms", [DurationMS]),
    case kapps_call_command:wait_for_hangup(DurationMS) of
        {'ok', 'channel_hungup'} -> cf_exe:stop(Call);
        {'error', 'timeout'} -> cf_exe:continue(Call)
    end.

-spec get_duration_ms(kz_json:object()) -> non_neg_integer().
get_duration_ms(Data) ->
    Duration = kz_json:get_integer_value(<<"duration">>, Data, 0),
    Unit = kz_json:get_ne_binary_value(<<"unit">>, Data, <<"s">>),
    duration_to_ms(Duration, Unit).

-spec duration_to_ms(integer(), kz_term:ne_binary()) -> non_neg_integer().
duration_to_ms(Duration, <<"ms">>) ->
    constrain_duration(Duration);
duration_to_ms(Duration, <<"s">>) ->
    constrain_duration(Duration * ?MILLISECONDS_IN_SECOND);
duration_to_ms(Duration, <<"m">>) ->
    constrain_duration(Duration * ?MILLISECONDS_IN_MINUTE);
duration_to_ms(Duration, <<"h">>) ->
    constrain_duration(Duration * ?MILLISECONDS_IN_HOUR);
duration_to_ms(Duration, _Unit) ->
    lager:debug("unknown unit: ~p", [_Unit]),
    duration_to_ms(Duration, <<"s">>).

-spec constrain_duration(integer()) -> integer().
constrain_duration(DurationMS) when DurationMS < 0 -> 0;
constrain_duration(DurationMS) when DurationMS > ?MILLISECONDS_IN_DAY ->
    ?MILLISECONDS_IN_DAY;
constrain_duration(DurationMS) ->
    DurationMS.
