%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc Modules to count usage of some specific case like `M:F/A' calls.
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(code_usage).

-export([process/0, process/1
        ,tabulate/0, tabulate/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").

-define(DEFAULT_TOP, 25).

-spec tabulate() -> 'ok'.
tabulate() ->
    print_usage(process(), ?DEFAULT_TOP).

-spec tabulate(atom() | pos_integer()) -> 'ok'.
tabulate(App) when is_atom(App) ->
    tabulate(App, ?DEFAULT_TOP);
tabulate(Top) when is_integer(Top)
                   andalso Top > 0 ->
    print_usage(process(), Top).

-spec tabulate(atom(), pos_integer()) -> 'ok'.
tabulate(App, Top) when is_atom(App)
                        andalso is_integer(Top)
                        andalso Top > 0 ->
    print_usage(process(App), Top).

-define(PRINT(Args), io:format("~6.6s | ~s~n", Args)).

print_usage(Dict, Top) ->
    Sorted = sort_and_truncate_dict(Dict, Top),
    ?PRINT(["Count", "M:F/A"]),
    [?PRINT([integer_to_list(Count), MFA])
     || {MFA, Count} <- Sorted
    ],
    'ok'.

sort_and_truncate_dict(Dict, Top) ->
    Sorted = lists:keysort(2, dict:to_list(Dict)),
    {TopL, _} = lists:split(Top, lists:reverse(Sorted)),
    TopL.

-define(OPTIONS, [{'expression', fun count_mfa/2}
                 ,{'module', fun print_dot/2}
                 ,{'accumulator', dict:new()}
                 ]).

-spec process() -> dict:dict().
process() ->
    io:format("process mfa counts: "),
    Usage = kazoo_ast:walk_project(?OPTIONS),
    io:format(" done~n", []),
    Usage.

-spec process(atom()) -> dict:dict().
process(App) ->
    io:format("process mfa counts in ~s: ", [App]),
    Usage = kazoo_ast:walk_app(App, ?OPTIONS),
    io:format(" done~n", []),
    Usage.

print_dot(_M, Acc) ->
    io:format(".", []),
    Acc.

count_mfa(?MOD_FUN_ARGS(M, F, Args), Acc) ->
    A = length(Args),
    count_mfa(M, F, A, Acc);
count_mfa(?MFA(M, F, A), Acc) ->
    count_mfa(M, F, A, Acc);
count_mfa(_Expr, Acc) -> Acc.

count_mfa(M, F, A, Dict) ->
    MFA = io_lib:format("~s:~s/~p", [M, F, A]),
    dict:update_counter(MFA, 1, Dict).
