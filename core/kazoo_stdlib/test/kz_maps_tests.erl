%%%-----------------------------------------------------------------------------
%%% @Copyright (C) 2010-2016, 2600Hz
%%% @doc Test utilities for manipulating maps
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_maps_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M1, #{a => #{b => #{c1 => 5, c2 => 6}}}).
-define(M2, #{a => #{b => #{c1 => 6, c3 => 7}}}).

merge_right_test_() ->
    M3 = #{a => #{b => #{c1 => 6, c2 => 6, c3 => 7}}},
    M4 = kz_maps:merge(?M1, ?M2),
    [?_assertEqual(M3, M4)].

merge_left_test() ->
    M3 = #{a => #{b => #{c1 => 5, c2 => 6, c3 => 7}}},

    M4 = kz_maps:merge(fun kz_maps:merge_left/2, ?M1, ?M2),
    [?_assertEqual(M3, M4)].

-ifdef(PERF).
-define(REPEAT, 1000000).
horse_merge_right() ->
    horse:repeat(?REPEAT
                ,kz_maps:merge(fun kz_maps:merge_right/2, ?M1, ?M2)
                ).
-endif.
