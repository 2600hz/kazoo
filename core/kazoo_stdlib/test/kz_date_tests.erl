%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_date_tests).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

pad_month_test_() ->
    [?_assertEqual(<<"10">>, kz_date:pad_month(10))
    ,?_assertEqual(<<"10">>, kz_date:pad_month(<<"10">>))
    ,?_assertEqual(<<"03">>, kz_date:pad_month(3))
    ,?_assertEqual(<<"03">>, kz_date:pad_month(<<"3">>))
    ,?_assertEqual(<<"03">>, kz_date:pad_month(<<"03">>))
    ].

iso8601_basic_test_() ->
    Tests = [{{2015,4,7}, <<"20150407">>}
            ,{{{2015,4,7},{0,0,0}}, <<"20150407">>}
            ,{{{2015,4,7},{1,3,2}}, <<"20150407">>}
            ,{{{2015,12,12},{12,13,12}}, <<"20151212">>}
            ,{63595733389, <<"20150408">>}
            ],
    [?_assertEqual(Expected, kz_date:to_iso8601(Date))
     || {Date, Expected} <- Tests
    ].

iso8601_extended_test_() ->
    Tests = [{{2015,4,7}, <<"2015-04-07">>}
            ,{{{2015,4,7},{0,0,0}}, <<"2015-04-07">>}
            ,{{{2015,4,7},{1,3,2}}, <<"2015-04-07">>}
            ,{{{2015,12,12},{12,13,12}}, <<"2015-12-12">>}
            ,{63595733389, <<"2015-04-08">>}
            ],
    [?_assertEqual(Expected, kz_date:to_iso8601_extended(Date))
     || {Date, Expected} <- Tests
    ].
