%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc sends a noop
%%%
%%% "data":{}
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_noop).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    NoopId = kapps_call_command:noop(Call),
    lager:info("noop ~s sent", [NoopId]),
    case kapps_call_command:wait_for_noop(Call, NoopId) of
        {'error', _E} ->
            lager:info("noop failed to be received: ~p", [_E]),
            cf_exe:stop(Call);
        {'ok', _} ->
            lager:info("noop received"),
            cf_exe:continue(Call)
    end.
