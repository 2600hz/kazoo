%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_time_test).

-include_lib("kazoo/include/kz_types.hrl").

-ifdef(PROPER).
- include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").

%% PROPER TESTING
-ifdef(PROPER).

prop_pretty_print_elapsed_s() ->
    ?FORALL({D, H, M, S}
            ,{non_neg_integer(), range(0,23), range(0, 59), range(0,59)}
            ,begin
                 Seconds = (D * ?SECONDS_IN_DAY) + (H * ?SECONDS_IN_HOUR) + (M * ?SECONDS_IN_MINUTE) + S,
                 Expected = lists:foldl(fun({0, "s"}, "") ->
                                                ["s", <<"0">>];
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
     {'timeout', 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}]))
      ]}}.

-endif.

greg_secs_to_unix_secs_test() ->
    GregSecs = kz_time:current_tstamp(),
    ?assertEqual(GregSecs - ?UNIX_EPOCH_IN_GREGORIAN, kz_time:gregorian_seconds_to_unix_seconds(GregSecs)).

unix_secs_to_greg_secs_test() ->
    UnixSecs = 1000000000,
    ?assertEqual(UnixSecs + ?UNIX_EPOCH_IN_GREGORIAN, kz_time:unix_seconds_to_gregorian_seconds(UnixSecs)).

microsecs_to_secs_test() ->
    Microsecs = 1310157838405890,
    Secs = 1310157838,
    ?assertEqual(Secs, kz_time:microseconds_to_seconds(Microsecs)).

elapsed_test() ->
    Start = {1401,998570,817606},
    Now = {1401,998594,798064},

    ?assertEqual(kz_time:elapsed_us(Start, Now), 23980458),
    ?assertEqual(kz_time:elapsed_ms(Start, Now), 23980),
    ?assertEqual(kz_time:elapsed_s(Start, Now), 23),

    StartDateTime = {{2014,6,5},{20,7,7}},
    StartTimestamp = calendar:datetime_to_gregorian_seconds(StartDateTime),

    NowDateTime = {{2014,6,5},{20,7,9}},
    NowTimestamp = calendar:datetime_to_gregorian_seconds(NowDateTime),

    ?assertEqual(kz_time:elapsed_s(StartTimestamp, NowTimestamp), 2),
    ?assertEqual(kz_time:elapsed_ms(StartTimestamp, NowTimestamp), 2000),
    ?assertEqual(kz_time:elapsed_us(StartTimestamp, NowTimestamp), 2000000).

rfc1036_test() ->
    Tests = [{ {{2015,4,7},{1,3,2}}, <<"Tue, 07 Apr 2015 01:03:02 GMT">>}
             ,{ {{2015,12,12},{12,13,12}}, <<"Sat, 12 Dec 2015 12:13:12 GMT">>}
             ,{ 63595733389, <<"Wed, 08 Apr 2015 17:29:49 GMT">>}
            ],
    lists:foreach(fun({Date, Expected}) ->
                          ?assertEqual(Expected, kz_time:rfc1036(Date))
                  end, Tests).

iso8601_test() ->
    Tests = [{ {{2015,4,7},{1,3,2}}, <<"2015-04-07">>}
             ,{ {{2015,12,12},{12,13,12}}, <<"2015-12-12">>}
             ,{ 63595733389, <<"2015-04-08">>}
            ],
    lists:foreach(fun({Date, Expected}) ->
                          ?assertEqual(Expected, kz_time:iso8601(Date))
                  end, Tests).
