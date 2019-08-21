%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(api_resource_tests).

-include_lib("eunit/include/eunit.hrl").

get_range_test_() ->
    FullBinary = <<"abcdefg">>,
    [?_assertEqual({<<"a">>, 0, 0, 1, 7}, api_resource:get_range(FullBinary, <<"bytes=0-0">>))
    ,?_assertEqual({<<"bcd">>, 1, 3, 3, 7}, api_resource:get_range(FullBinary, <<"bytes=1-3">>))
    ,?_assertEqual({<<"g">>, 6, 6, 1, 7}, api_resource:get_range(FullBinary, <<"bytes=6-6">>))
    ,?_assertEqual({FullBinary, 0, 6, 7, 7}, api_resource:get_range(FullBinary, <<"bytes=0-6">>))
     %% Invalid should give full size
    ,?_assertEqual({FullBinary, 0, 6, 7, 7}, api_resource:get_range(FullBinary, <<"bytes=0-9">>))
    ,?_assertEqual({FullBinary, 0, 6, 7, 7}, api_resource:get_range(FullBinary, <<>>))
    ].
