%%%-------------------------------------------------------------------
%%% @Copyright (C) 2010-2016, 2600Hz
%%% @doc
%%% Test utilities for manipulating maps
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_maps_tests).

-include_lib("eunit/include/eunit.hrl").

merge_test_() ->
    M1 = #{a => #{b => #{c1 => 5, c2 => 6}}},
    M2 = #{a => #{b => #{c1 => 6, c3 => 7}}},
    M3 = #{a => #{b => #{c1 => 6, c2 => 6, c3 => 7}}},
    M4 = kz_maps:merge(M1, M2),
    [?_assertEqual(M3, M4)].

