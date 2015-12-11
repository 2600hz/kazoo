%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(knm_move_number_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/knm.hrl").

release_number_test_() ->
    Tests = [move_number/1
            ],
    lists:foldl(fun(F, Acc) ->
                        F(Acc)
                end, [], Tests
               ).

move_number(Tests) ->
    Tests.
