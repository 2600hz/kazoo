%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
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
