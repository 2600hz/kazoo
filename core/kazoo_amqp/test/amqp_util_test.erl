%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
%%% @doc
%%% Utilities to facilitate AMQP interaction
%%% @end
%%% @contributions
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(amqp_util_test).

-include_lib("eunit/include/eunit.hrl").

encode_key_test() ->
    ?assertEqual(<<"#">>, amqp_util:encode(<<"#">>)),
    ?assertEqual(<<"*">>, amqp_util:encode(<<"*">>)),
    ?assertEqual(<<"key">>, amqp_util:encode(<<"key">>)),
    ?assertEqual(<<"routing%2Ekey">>, amqp_util:encode(<<"routing.key">>)),
    ?assertEqual(<<"long%2Erouting%2Ekey">>, amqp_util:encode(<<"long.routing.key">>)),
    ?assertEqual(<<"test%26%2E192%2E+168%2E+5%2E+5%23">>, amqp_util:encode(<<"test&.192. 168. 5. 5#">>)),
    'ok'.

trim_test_() ->
    Min = 0,
    Max = 255,

    Vals = [{Min-100, Min}
            ,{Min-1, Min}
            ,{Min, Min}
            ,{Min+1, Min+1}
            ,{Max-1, Max-1}
            ,{Max, Max}
            ,{Max+1, Max}
            ,{Max+100, Max}
           ],
    [check_trim(Min, Max, Test) || Test <- Vals].

check_trim(Min, Max, {N, T}) ->
    ?_assertEqual(T, amqp_util:trim(Min, Max, N)).
