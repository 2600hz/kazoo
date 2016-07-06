%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_ring_group_test).

-include_lib("eunit/include/eunit.hrl").

weighted_random_sort_test_() ->
    EndpointsInt = [
                    {1, <<"1">>}
                   ,{2, <<"2">>}
                   ,{3, <<"3">>}
                   ],
    Endpoints = cf_ring_group:weighted_random_sort(EndpointsInt),
    [?_assertEqual(length(EndpointsInt), length(Endpoints))]
        ++ [?_assertEqual('true', lists:keymember(X, 2, EndpointsInt))
            || X <- Endpoints
           ].
