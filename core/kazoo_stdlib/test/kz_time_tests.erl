%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_time_tests).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

%% PROPER TESTING
-ifdef(PROPER).

prop_pretty_print_elapsed_s() ->
    ?FORALL({D, H, M, S}
           ,{non_neg_integer(), range(0,23), range(0, 59), range(0,59)}
           ,begin
                Seconds = (D * ?SECONDS_IN_DAY) + (H * ?SECONDS_IN_HOUR) + (M * ?SECONDS_IN_MINUTE) + S,
                Expected = lists:foldl(fun({0, "s"}, "") -> ["s", <<"0">>];
                                          ({0, _}, Acc) -> Acc;
                                          ({N, Unit}, Acc) -> [Unit, kz_term:to_binary(N) | Acc]
                                       end
                                      ,[]
                                      ,[{D, "d"}
                                       ,{H, "h"}
                                       ,{M, "m"}
                                       ,{S, "s"}
                                       ]),
                Result = kz_time:pretty_print_elapsed_s(Seconds),
                Result =:= iolist_to_binary(lists:reverse(Expected))
            end).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 20000,
      [?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

-endif.

to_x_test_() ->
    Unix = kz_time:current_unix_tstamp(),
    UnixMs = Unix * 1000,
    Greg = kz_time:now_s(),

    [?_assert(Unix < Greg)
    ,?_assert(1 >= abs(Greg - kz_time:unix_seconds_to_gregorian_seconds(Unix)))
    ,?_assert(1 >= abs(Greg - kz_time:unix_timestamp_to_gregorian_seconds(UnixMs)))
    ].

pretty_print_datetime_test_() ->
    TS = 63652662294,
    [?_assertEqual(<<"2017-01-26_15-04-54">>, kz_time:pretty_print_datetime(TS))
    ].

weekday_test_() ->
    Days = [<<"Mon">>, <<"Tue">>, <<"Wed">>, <<"Thu">>, <<"Fri">>, <<"Sat">>, <<"Sun">>],
    [?_assertEqual(lists:nth(I,Days), kz_time:weekday(I))
     || I <- lists:seq(1, 7)
    ].

month_test_() ->
    Months = [<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>
             ,<<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>],
    [?_assertEqual(lists:nth(I,Months), kz_time:month(I))
     || I <- lists:seq(1, 12)
    ].

greg_secs_to_unix_secs_test() ->
    GregSecs = kz_time:now_s(),
    ?assertEqual(GregSecs - ?UNIX_EPOCH_IN_GREGORIAN, kz_time:gregorian_seconds_to_unix_seconds(GregSecs)).

unix_secs_to_greg_secs_test() ->
    UnixSecs = 1000000000,
    ?assertEqual(UnixSecs + ?UNIX_EPOCH_IN_GREGORIAN, kz_time:unix_seconds_to_gregorian_seconds(UnixSecs)).

microsecs_to_secs_test() ->
    Microsecs = 1310157838405890,
    Secs = 1310157838,
    ?assertEqual(Secs, kz_time:microseconds_to_seconds(Microsecs)).

elapsed_test_() ->
    Start = {1401,998570,817606},
    Now = {1401,998594,798064},
    [?_assertEqual(23980458, kz_time:elapsed_us(Start, Now))
    ,?_assertEqual(23980, kz_time:elapsed_ms(Start, Now))
    ,?_assertEqual(23, kz_time:elapsed_s(Start, Now))
    ,?_assertEqual(<<"0s">>, kz_time:pretty_print_elapsed_s(0))
    ].

more_elapsed_test_() ->
    StartDateTime = {{2014,6,5},{20,7,7}},
    StartTimestamp = calendar:datetime_to_gregorian_seconds(StartDateTime),
    NowDateTime = {{2014,6,5},{20,7,9}},
    NowTimestamp = calendar:datetime_to_gregorian_seconds(NowDateTime),

    {Mega, Sec, Micro} = StartTS = os:timestamp(),
    FutureTS = {Mega, Sec + 10, Micro},

    TS = 63652663232,
    [?_assertEqual(2, kz_time:elapsed_s(StartTimestamp, NowTimestamp))
    ,?_assertEqual(2000, kz_time:elapsed_ms(StartTimestamp, NowTimestamp))
    ,?_assertEqual(2000000, kz_time:elapsed_us(StartTimestamp, NowTimestamp))
    ,?_assertEqual(<<"2017-1-26">>, kz_time:format_date(TS))
    ,?_assertEqual(<<"15:20:32">>, kz_time:format_time(TS))
    ,?_assertEqual(<<"2017-1-26 15:20:32">>, kz_time:format_datetime(TS))

    ,?_assertEqual(10, kz_time:elapsed_s(StartTS, FutureTS))
    ,?_assertEqual(10, kz_time:elapsed_s(kz_time:now_s(StartTS), FutureTS))
    ,?_assertEqual(10, kz_time:elapsed_s(StartTS, kz_time:now_s(FutureTS)))

    ,?_assertEqual(10 * ?MILLISECONDS_IN_SECOND, kz_time:elapsed_ms(StartTS, FutureTS))
    ,?_assertEqual(10 * ?MILLISECONDS_IN_SECOND, kz_time:elapsed_ms(kz_time:now_ms(StartTS), FutureTS))
    ,?_assertEqual(10 * ?MILLISECONDS_IN_SECOND, kz_time:elapsed_ms(StartTS, kz_time:now_ms(FutureTS)))

    ,?_assertEqual(10 * ?MICROSECONDS_IN_SECOND, kz_time:elapsed_us(StartTS, FutureTS))
    ,?_assertEqual(10 * ?MICROSECONDS_IN_SECOND, kz_time:elapsed_us(kz_time:now_us(StartTS), FutureTS))
    ,?_assertEqual(10 * ?MICROSECONDS_IN_SECOND, kz_time:elapsed_us(StartTS, kz_time:now_us(FutureTS)))
    ].

unitfy_and_timeout_test_() ->
    {Mega, Sec, Micro} = Start = os:timestamp(),
    Future = {Mega, Sec + 10, Micro},

    [?_assertEqual("", kz_time:unitfy_seconds(0))
    ,?_assertEqual(infinity, kz_time:decr_timeout(infinity, Start))
    ,?_assertEqual(infinity, kz_time:decr_timeout(infinity, Start, Future))
    ,?_assertEqual(0, kz_time:decr_timeout(10, Start, Future))
    ,?_assertEqual(0, kz_time:decr_timeout_elapsed(30, 42))
    ,?_assertEqual(12, kz_time:decr_timeout_elapsed(42, 30))
    ,?_assertEqual(10, kz_time:milliseconds_to_seconds(10*1000))
    ].

rfc1036_test_() ->
    Tests = [{{{2015,4,7},{1,3,2}}, <<"Tue, 07 Apr 2015 01:03:02 GMT">>}
            ,{{{2015,12,12},{12,13,12}}, <<"Sat, 12 Dec 2015 12:13:12 GMT">>}
            ,{63595733389, <<"Wed, 08 Apr 2015 17:29:49 GMT">>}
            ],
    [?_assertEqual(Expected, kz_time:rfc1036(Date))
     || {Date, Expected} <- Tests
    ].

iso8601_test_() ->
    Tests = [{{2015,4,7}, <<"2015-04-07">>}
            ,{{{2015,4,7},{0,0,0}}, <<"2015-04-07T00:00:00Z">>}
            ,{{{2015,4,7},{1,3,2}}, <<"2015-04-07T01:03:02Z">>}
            ,{{{2015,12,12},{12,13,12}}, <<"2015-12-12T12:13:12Z">>}
            ,{63595733389, <<"2015-04-08T17:29:49Z">>}
            ],
    [?_assertEqual(Expected, kz_time:iso8601(Date))
     || {Date, Expected} <- Tests
    ].

iso8601_offset_test_() ->
    Tests = [{{{2015,4,7},{0,0,0}}, <<"2015-04-07T00:00:00Z">>, <<"UTC">>}
            ,{{{2015,4,7},{0,0,0}}, <<"2015-04-06T17:00:00-07:00">>, <<"America/Los_Angeles">>}
            ,{{{2015,4,7},{1,3,2}}, <<"2015-04-07T04:33:02+03:30">>, <<"+03:30">>}
            ,{{{2015,12,12},{12,13,12}}, <<"2015-12-12T15:13:12+03:00">>, 10800}
            ,{63595733389, <<"2015-04-08T12:29:49-05:00">>, -18000}
            ],
    [?_assertEqual(Expected, kz_time:iso8601(Date, Offset))
     || {Date, Expected, Offset} <- Tests
    ].

from_iso8601_test_() ->
    Tests = [{<<"2015-04-07T00:00:00">>, {{2015,4,7},{0,0,0}}}
            ,{<<"20150407T000000Z">>, {{2015,4,7},{0,0,0}}}
            ,{<<"2015-04-07T04:33:02+03:30">>, {{2015,4,7},{1,3,2}}}
            ,{<<"2015-04-07T04:33:02-05:00">>, {{2015,4,7},{9,33,2}}}
            ,{<<"2015-04-07T04:33:02-0500">>, {{2015,4,7},{9,33,2}}}
            ,{<<"20150407T043302-05">>, {{2015,4,7},{9,33,2}}}
            ],

    ThrowsTests = [{<<"2015/04/07T00.00.00">>, 'invalid_date'}
                  ,{<<"2015-04-07T04.33.02-05:00">>, 'invalid_time'}
                  ,{<<"2015-04-07T04:33:02-5:0">>, 'invalid_offset'}
                  ],
    [{"parsing " ++ kz_term:to_list(Date)
     ,[?_assertEqual(Expected, kz_time:from_iso8601(Date))]
     }
     || {Date, Expected} <- Tests
    ] ++ [{"parser should throws for " ++ kz_term:to_list(Date)
          ,[?_assertThrow({'error', Expected}, kz_time:from_iso8601(Date))]
          }
          || {Date, Expected} <- ThrowsTests
         ].

iso8601_time_test_() ->
    Tests = [{{{2015,4,7},{0,0,0}}, <<"00:00:00">>}
            ,{{{2015,4,7},{1,3,2}}, <<"01:03:02">>}
            ,{{{2015,12,12},{12,13,12}}, <<"12:13:12">>}
            ,{63595733389, <<"17:29:49">>}
            ],
    [?_assertEqual(Expected, kz_time:iso8601_time(Date))
     || {Date, Expected} <- Tests
    ].

to_gregorian_seconds_test_() ->
    Datetime = {{2017,04,01}, {12,0,0}},
    LASeconds = kz_time:to_gregorian_seconds(Datetime, <<"America/Los_Angeles">>),
    NYSeconds = kz_time:to_gregorian_seconds(Datetime, <<"America/New_York">>),
    [?_assertEqual(63658292400, LASeconds)
    ,?_assertEqual(63658281600, NYSeconds)
    ].

tstamps_test_() ->
    Current = kz_time:current_tstamp(),
    Now = kz_time:now_s(),
    [?_assert(1 >= abs(Now-Current))].

-ifdef(PERF).
-define(REPEAT, 1000000).

%% Most recent run on my box:
%% kz_time_tests:now_s in 0.117557s
%% kz_time_tests:current_tstamp_s in 0.565916s
%% kz_time_tests:mono_now_s in 0.107770s
%% kz_time_tests:erlang_timestamp in 0.104192s
%% kz_time_tests:os_timestamp in 0.054200s

horse_now_s() ->
    horse:repeat(?REPEAT, kz_time:now_s()).

horse_current_tstamp_s() ->
    horse:repeat(?REPEAT, kz_time:current_tstamp()).

horse_mono_now_s() ->
    horse:repeat(?REPEAT, mono_now_s()).

mono_now_s() ->
    erlang:monotonic_time('seconds') + ?UNIX_EPOCH_IN_GREGORIAN.

horse_erlang_timestamp() ->
    horse:repeat(?REPEAT, erlang:timestamp()).

horse_os_timestamp() ->
    horse:repeat(?REPEAT, os:timestamp()).

-endif.
