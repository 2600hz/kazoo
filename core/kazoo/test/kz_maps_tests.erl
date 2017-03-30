%%%-------------------------------------------------------------------
%%% @Copyright (C) 2010-2016, 2600Hz
%%% @doc
%%% Test utilities for manipulating maps
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_maps_tests).

-include_lib("eunit/include/eunit.hrl").

-define(M1, #{a => #{b => #{c1 => 5, c2 => 6}}}).
-define(M2, #{a => #{b => #{c1 => 6, c3 => 7}}}).

merge_test_() ->
    M3 = #{a => #{b => #{c1 => 6, c2 => 6, c3 => 7}}},
    M4 = kz_maps:merge(?M1, ?M2),
    [?_assertEqual(M3, M4)].

merge_right_test() ->
    M3 = #{a => #{b => #{c1 => 6, c2 => 6, c3 => 7}}},

    M4 = kz_maps:merge_r(?M1, ?M2),
    [?_assertEqual(M3, M4)].

-ifdef(PERF).
-define(REPEAT, 1000000).
horse_merge_r() ->
    horse:repeat(?REPEAT
                ,kz_maps:merge_r(?M1, ?M2)
                ).

horse_merge() ->
    horse:repeat(?REPEAT
                ,kz_maps:merge(?M1, ?M2)
                ).

%% on my machine:
%% Running horse on application kazoo
%% kz_maps_tests:merge_r in 0.703022s
%% kz_maps_tests:merge in 3.408125s

-endif.
