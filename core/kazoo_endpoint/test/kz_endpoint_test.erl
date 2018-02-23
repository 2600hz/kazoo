%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
%%% @doc
%%%
%%%
%%% @author Pierre Fenoll
%%% @end
%%%-------------------------------------------------------------------
-module(kz_endpoint_test).

-include_lib("eunit/include/eunit.hrl").


attributes_keys_unique_test_() ->
    Keys = kz_endpoint:attributes_keys(),
    [?_assertEqual(length(Keys), length(lists:usort(Keys)))
    ].
