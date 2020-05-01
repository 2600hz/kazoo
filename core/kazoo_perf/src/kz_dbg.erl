%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_dbg).

-export([debug_call/3]).

-spec debug_call(atom(), atom(), [any()]) -> any().
debug_call(M, F, Args) ->
    dbg:start(),

    dbg:tracer(),

    dbg:tpl(M, [{'_', [], [$_]}]),
    dbg:p(all, c),

    Result = apply(M, F, Args),

    dbg:stop_clear(),
    dbg:stop(),
    Result.
