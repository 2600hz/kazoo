%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_ring_group_test).

-include_lib("eunit/include/eunit.hrl").

weighted_random_sort_test() ->
    EndpointsInt = [
                    {1, <<"1">>}
                    ,{2, <<"2">>}
                    ,{3, <<"3">>}
                   ],
    Endpoints = cf_ring_group:weighted_random_sort(EndpointsInt),
    ?assertEqual(length(EndpointsInt), length(Endpoints)),
    [?assertEqual(true, lists:keymember(X, 2, EndpointsInt)) || X <- Endpoints].
