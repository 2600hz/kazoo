%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Utilities to facilitate AMQP interaction
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Edouard Swiac
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_util_tests).

-include_lib("eunit/include/eunit.hrl").

encode_key_test_() ->
    [?_assertEqual(<<"#">>, kz_amqp_util:encode(<<"#">>))
    ,?_assertEqual(<<"*">>, kz_amqp_util:encode(<<"*">>))
    ,?_assertEqual(<<"key">>, kz_amqp_util:encode(<<"key">>))
    ,?_assertEqual(<<"routing%2Ekey">>, kz_amqp_util:encode(<<"routing.key">>))
    ,?_assertEqual(<<"long%2Erouting%2Ekey">>, kz_amqp_util:encode(<<"long.routing.key">>))
    ,?_assertEqual(<<"test%26%2E192%2E+168%2E+5%2E+5%23">>, kz_amqp_util:encode(<<"test&.192. 168. 5. 5#">>))
    ].

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
    ?_assertEqual(T, kz_amqp_util:trim(Min, Max, N)).
