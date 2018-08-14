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
