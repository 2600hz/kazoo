%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_process_tests).

-include_lib("eunit/include/eunit.hrl").

spawns_test_() ->
    [?_assert(is_pid(kz_process:spawn(fun() -> io:format("x") end)))
    ,?_assert(is_pid(kz_process:spawn(fun(X) -> io:format("~p", [X]) end, ['x'])))
    ,?_assert(is_pid(kz_process:spawn_link(fun() -> io:format("x") end)))
    ,?_assert(is_pid(kz_process:spawn_link(fun(X) -> io:format("~p",[X]) end, ['x'])))
    ,?_assertMatch({_,_}, kz_process:spawn_monitor(fun(X) -> io:format("~p",[X]) end, ['x']))
    ].

runs_in_test_() ->
    [?_assertEqual('timeout', kz_process:runs_in(1, fun timer:sleep/1, [10]))
    ,?_assertEqual({'ok','ok'}, kz_process:runs_in(10, fun timer:sleep/1, [1]))
    ,?_assertEqual('timeout', kz_process:runs_in(1.0, fun timer:sleep/1, [10]))
    ,?_assertEqual({'ok','ok'}, kz_process:runs_in(10.0, fun timer:sleep/1, [1]))
    ].
