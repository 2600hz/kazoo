%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

%% Found during PropEr testing
kz_dates_test_() ->
    %% {FunkyDate, NormalizedDate}
    Tests = [{{1600,  1,  65}, {1600,  3,  5}}
            ,{{1601,  1,  65}, {1601,  3,  6}}
            ,{{   0, 12,  32}, {   1,  1,  1}}
            ,{{2601,  0,   2}, {2600, 12,  2}}
            ,{{2600,  2,   0}, {2600,  1, 31}}
            ,{{2008, 37,   2}, {2011,  1,  2}}
            ,{{2009,  1, 732}, {2011,  1,  2}}
            ],
    [?_assertEqual(Normalized, kz_date:normalize(Date))
     || {Date, Normalized} <- Tests
    ].

%% Found during PropEr testing
iso_week_test_() ->
    Roundtrips = [{0,53}, {8,53}],
    [?_assert(are_matched_weeks(ISOWeek, kz_date:to_iso_week(kz_date:from_iso_week(ISOWeek))))
     || ISOWeek <- Roundtrips
    ].

%% week 53 in a year is equivalent to week 1 in the next year.
%% See 1/1/1 for instance {0,53} or {1,1}
are_matched_weeks({Y1, 53}, {Y2, 1}) -> Y2 =:= Y1+1;
are_matched_weeks({Y1, 1}, {Y2, 53}) -> Y1 =:= Y2+1;
are_matched_weeks(ISO1, ISO2) -> ISO1 =:= ISO2.

-ifdef(PROPER).
proper_test_() ->
    {"Runs " ?MODULE_STRING " PropEr tests"
    ,[{atom_to_list(F)
      ,fun () ->
               ?assert(proper:quickcheck(?MODULE:F(), [{'to_file', 'user'}
                                                      ,{'numtests', 500}
                                                      ]))
       end
      }
      || {F, 0} <- ?MODULE:module_info('exports'),
         F > 'prop_',
         F < 'prop`'
     ]
    }.

prop_normalize() ->
    ?FORALL({GregorianDate, MonthsBack}
           ,{safe_date(), range(1,13)}
           ,begin
                Date = funky_date(GregorianDate, MonthsBack),
                NormalizedDate = kz_date:normalize(Date),

                ?WHENFAIL(io:format("failed to normalize ~w to ~w: got ~w(~w)~n"
                                   ,[Date, GregorianDate, NormalizedDate, MonthsBack]
                                   )
                         ,GregorianDate =:= NormalizedDate
                         )
            end
           ).

safe_date() ->
    ?LET({Y, M}
        ,{kz_time:year(), kz_time:month()}
        ,{Y, M, range(1, kz_date:days_in_month(Y, M))}
        ).

funky_date(Date, 0) -> Date;
funky_date({0, 1, D}, _MonthsBack) -> {0, 1, D};
funky_date({0, M, D}, MonthsBack) ->
    funky_date({0, M-1, D+kz_date:days_in_month(0, M-1)}, MonthsBack-1);
funky_date({Y, 1, D}, MonthsBack) ->
    funky_date({Y-1, 12, D+kz_date:days_in_month(Y-1, 12)}, MonthsBack-1);
funky_date({Y, M, D}, MonthsBack) ->
    funky_date({Y, M-1, D+kz_date:days_in_month(Y, M-1)}, MonthsBack-1).

prop_iso_week() ->
    ?FORALL(ISOWeek
           ,{kz_time:year(), kz_time:weeknum()}
           ,begin
                RoundTrip = kz_date:to_iso_week(kz_date:from_iso_week(ISOWeek)),
                ?WHENFAIL(io:format("failed to convert to/from iso week for ~p <=> ~p~n"
                                   ,[ISOWeek, RoundTrip]
                                   )
                         ,are_matched_weeks(ISOWeek, RoundTrip)
                         )
            end
           ).

prop_iso8601() ->
    ?FORALL({Date, ToFun}
           ,{safe_date(), oneof([to_iso8601, to_iso8601_extended])}
           ,?WHENFAIL(io:format("failed to roundtrip ~p with ~p~n", [Date, ToFun])
                     ,Date =:= kz_date:from_iso8601(kz_date:ToFun(Date))
                     )
           ).

prop_relative_diff() ->
    ?FORALL({GregorianSeconds, Offset}
           ,{range(31640000,82048118400) %% 1/1/1 00:00:00 - 2600/1/1 00:00:00
            ,range(1,31540000) %% seconds in year
            }
           ,begin
                Now = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
                Future = calendar:gregorian_seconds_to_datetime(GregorianSeconds + Offset),
                lists:all(fun({Diff, A, B}) ->
                                  case kz_date:relative_difference(A, B) of
                                      Diff -> 'true';
                                      _Result ->
                                          io:format("failed to calc relative diff of ~p and ~p~n"
                                                    "expected ~p got ~p~n"
                                                   ,[A, B, Diff, _Result]
                                                   ),
                                          'false'
                                  end
                          end
                         ,[{'future', Now, Future}
                          ,{'past', Future, Now}
                          ,{'equal', Now, Now}
                          ,{'equal', Future, Future}
                          ]
                         )
            end
           ).

-endif.
