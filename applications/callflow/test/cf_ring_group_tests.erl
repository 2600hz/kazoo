%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author SIPLABS LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_ring_group_tests).

-include_lib("eunit/include/eunit.hrl").

weighted_random_sort_test_() ->
    EndpointsInt = [{1, <<"ep1">>}
                   ,{2, <<"ep2">>}
                   ,{3, <<"ep3">>}
                   ],
    Endpoints = cf_ring_group:weighted_random_sort(EndpointsInt),

    ?debugFmt("~ninit: ~p~nafter: ~p~n", [EndpointsInt, Endpoints]),

    [?_assertEqual(length(EndpointsInt), length(Endpoints))
     |
     [?_assertEqual('true', lists:member(X, EndpointsInt))
      || X <- Endpoints
     ]
    ].
