%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
