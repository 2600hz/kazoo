%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Answers to the call and switches media off.
%%% Could be useful for external calls/conferences recording over inbound leg.
%%%
%%%
%%% @author Kirill Sysoev github.com/onnet
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_dead_air).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    _ = kapps_call_command:b_answer(Call),
    _ = kapps_call_command:audio_level(Call, <<"write">>, <<"start">>, <<"-4">>),
    _ = kapps_call_command:wait_for_hangup(),
    cf_exe:stop(Call).
