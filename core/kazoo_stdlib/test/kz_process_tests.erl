%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_process_tests).

-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    {'setup'
    ,fun() -> 'ok' end
    ,[spawns()
     ,runs_in()
     ]
    }.


spawns() ->
    [?_assert(is_pid(kz_process:spawn(fun() -> 'true' = kz_term:is_true('true') end)))
    ,?_assert(is_pid(kz_process:spawn(fun(X) -> 'false' = kz_term:is_true(X) end, ['x'])))
    ,?_assert(is_pid(kz_process:spawn_link(fun() -> 'true' = kz_term:is_true('true') end)))
    ,?_assert(is_pid(kz_process:spawn_link(fun(X) -> 'false' = kz_term:is_true(X) end, ['x'])))
    ,?_assertMatch({_,_}, kz_process:spawn_monitor(fun(X) -> 'false' = kz_term:is_true(X) end, ['x']))
    ].

runs_in() ->
    F = fun timer:sleep/1,
    [?_assertEqual('timeout', kz_process:runs_in(1, F, [10]))
    ,?_assertEqual({'ok','ok'}, kz_process:runs_in(100, F, [1]))
    ,?_assertEqual('timeout', kz_process:runs_in(1.0, F, [10]))
    ,?_assertEqual({'ok','ok'}, kz_process:runs_in(100.0, F, [1]))
    ].
