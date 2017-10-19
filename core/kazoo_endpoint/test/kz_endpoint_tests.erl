%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_endpoint_tests).

-include_lib("eunit/include/eunit.hrl").


attributes_keys_unique_test_() ->
    Keys = kz_endpoint:attributes_keys(),
    [?_assertEqual(length(Keys), length(lists:usort(Keys)))
    ].
