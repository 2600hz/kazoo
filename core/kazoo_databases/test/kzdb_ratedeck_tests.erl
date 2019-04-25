%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzdb_ratedeck_tests).

-include_lib("eunit/include/eunit.hrl").

prefix_keys_test_() ->
    [?_assertEqual([1], kzdb_ratedeck:prefix_keys(<<"1">>))
    ,?_assertEqual([12, 1], kzdb_ratedeck:prefix_keys(<<"12">>))
    ,?_assertEqual([123, 12, 1], kzdb_ratedeck:prefix_keys(<<"123">>))
    ,?_assertEqual([123, 12, 1], kzdb_ratedeck:prefix_keys(<<"**123">>))
    ].
