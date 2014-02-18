-module(erlydtl_dateformat_tests).

-export([run_tests/0]).

run_tests() ->
   io:format("Running date format tests...~n"),
   Failures = test_group_runner([
      {
         "date 1",
         {1979, 7, 8}, % just a date
         [{"a", "a.m."}, {"A", "AM"}, {"c", "1979-07-08T00:00:00"},
          {"d", "08"}, {"D", "Sun"}, {"f", "12"}, {"F", "July"},
          {"g", "12"}, {"G", "0"},
          {"h", "12"}, {"H", "00"}, {"i", "00"},
          {"j", "8"}, {"l", "Sunday"}, {"L", "False"},
          {"m", "07"}, {"M", "Jul"}, {"b", "jul"},
          {"n", "7"}, {"N", "July"}, {"P", "midnight"},
          {"s", "00"}, {"S", "th"}, {"t", "31"},
          {"w", "0"}, {"W", "27"}, {"y", "79"}, {"Y", "1979"}, {"z", "189"},
          {"jS F Y H:i", "8th July 1979 00:00"},
          {"jS \\o\\f F", "8th of July"},
          % We expect these to come back verbatim
          {"x", "x"}, {"C", "C"}, {";", ";"}, {"%", "%"}

          % TODO : timzeone related tests.
          %{"r", "Sun, 8 Jul 1979 00:00:00 +0000"},
          %{"O", "0000"},
          %{"T", "CET"},
          %{"U", "300531600"},
          %{"Z", "3600"}
         ]
      },
      {
         "datetime 1",
         {{1979, 7, 8}, {22, 7, 12}}, % date/time tuple
         [{"a", "p.m."}, {"A", "PM"}, {"c", "1979-07-08T22:07:12"},
          {"d", "08"}, {"D", "Sun"}, {"f", "10:07"}, {"F", "July"},
          {"g", "10"}, {"G", "22"},
          {"h", "10"}, {"H", "22"}, {"i", "07"},
          {"j", "8"}, {"l", "Sunday"}, {"L", "False"},
          {"m", "07"}, {"M", "Jul"}, {"b", "jul"},
          {"n", "7"}, {"N", "July"}, {"P", "10:07 p.m."},
          {"s", "12"}, {"S", "th"}, {"t", "31"},
          {"w", "0"}, {"W", "27"}, {"y", "79"}, {"Y", "1979"}, {"z", "189"},
          {"jS F Y H:i", "8th July 1979 22:07"},
          {"jS \\o\\f F", "8th of July"},
          % We expect these to come back verbatim
          {"x", "x"}, {"C", "C"}, {";", ";"}, {"%", "%"}
          % TODO : timzeone related tests.
          %{"r", "Sun, 8 Jul 1979 22:07:12 +0000"},
          %{"O", "0000"},
          %{"T", "CET"},
          %{"U", "300531600"},
          %{"Z", "3600"}
         ]
      },
      {
         "datetime 2",
         {{2008, 12, 25}, {7, 0, 9}}, % date/time tuple
         [{"a", "a.m."}, {"A", "AM"}, {"c", "2008-12-25T07:00:09"},
          {"d", "25"}, {"D", "Thu"}, {"f", "7"}, {"F", "December"},
          {"g", "7"}, {"G", "7"},
          {"h", "07"}, {"H", "07"}, {"i", "00"},
          {"j", "25"}, {"l", "Thursday"}, {"L", "True"},
          {"m", "12"}, {"M", "Dec"}, {"b", "dec"},
          {"n", "12"}, {"N", "Dec."}, {"P", "7 a.m."},
          {"s", "09"}, {"S", "th"}, {"t", "31"},
          {"w", "4"}, {"W", "52"}, {"y", "08"}, {"Y", "2008"}, {"z", "360"},
          {"jS F Y H:i", "25th December 2008 07:00"},
          {"jS \\o\\f F", "25th of December"},
          % We expect these to come back verbatim
          {"x", "x"}, {"C", "C"}, {";", ";"}, {"%", "%"}
          % TODO : timzeone related tests.
          %{"r", "Thu, 25 Dec 2008 07:00:09 +0000"},
          %{"O", "0000"},
          %{"T", "CET"},
          %{"U", "300531600"},
          %{"Z", "3600"}
         ]
      },
      {
         "datetime 3",
         {{2004, 2, 29}, {12, 0, 59}}, % date/time tuple
         [{"a", "p.m."}, {"A", "PM"}, {"c", "2004-02-29T12:00:59"},
          {"d", "29"}, {"D", "Sun"}, {"f", "12"}, {"F", "February"},
          {"g", "12"}, {"G", "12"},
          {"h", "12"}, {"H", "12"}, {"i", "00"},
          {"j", "29"}, {"l", "Sunday"}, {"L", "True"},
          {"m", "02"}, {"M", "Feb"}, {"b", "feb"},
          {"n", "2"}, {"N", "Feb."}, {"P", "noon"},
          {"s", "59"}, {"S", "th"}, {"t", "29"},
          {"w", "0"}, {"W", "9"}, {"y", "04"}, {"Y", "2004"}, {"z", "58"},
          {"jS F Y H:i", "29th February 2004 12:00"},
          {"jS \\o\\f F", "29th of February"},
          % We expect these to come back verbatim
          {"x", "x"}, {"C", "C"}, {";", ";"}, {"%", "%"}
          % TODO : timzeone related tests.
          %{"r", "Sun, 29 Feb 2004 12:00:59 +0000"},
          %{"O", "0000"},
          %{"T", "CET"},
          %{"U", "300531600"},
          %{"Z", "3600"}
         ]
      },
      {
         "datetime 4",
         {{2004, 2, 29}, {12, 0, 09.256687}}, % date/time tuple
         [{"a", "p.m."}, {"A", "PM"}, {"c", "2004-02-29T12:00:09"},
          {"d", "29"}, {"D", "Sun"}, {"f", "12"}, {"F", "February"},
          {"g", "12"}, {"G", "12"},
          {"h", "12"}, {"H", "12"}, {"i", "00"},
          {"j", "29"}, {"l", "Sunday"}, {"L", "True"},
          {"m", "02"}, {"M", "Feb"}, {"b", "feb"},
          {"n", "2"}, {"N", "Feb."}, {"P", "noon"},
          {"s", "09"}, {"S", "th"}, {"t", "29"},
          {"w", "0"}, {"W", "9"}, {"y", "04"}, {"Y", "2004"}, {"z", "58"},
          {"jS F Y H:i", "29th February 2004 12:00"},
          {"jS \\o\\f F", "29th of February"},
          % We expect these to come back verbatim
          {"x", "x"}, {"C", "C"}, {";", ";"}, {"%", "%"}
          % TODO : timzeone related tests.
          %{"r", "Sun, 29 Feb 2004 12:00:59 +0000"},
          %{"O", "0000"},
          %{"T", "CET"},
          %{"U", "300531600"},
          %{"Z", "3600"}
         ]
      },
      % Weeknum tests.  Largely based on examples from :
      %   http://en.wikipedia.org/wiki/ISO_week_date
      { "weeknum 1.1",  {2005,  1,  1}, [{"W", "53"}] },
      { "weeknum 1.2",  {2005,  1,  2}, [{"W", "53"}] },
      { "weeknum 1.3",  {2005, 12, 31}, [{"W", "52"}] },
      { "weeknum 1.4",  {2007,  1,  1}, [{"W", "1"}]  },
      { "weeknum 1.5",  {2007, 12, 30}, [{"W", "52"}] },
      { "weeknum 1.6",  {2007, 12, 31}, [{"W", "1"}]  },
      { "weeknum 1.6",  {2008,  1,  1}, [{"W", "1"}]  },
      { "weeknum 1.7",  {2008, 12, 29}, [{"W", "1"}]  },
      { "weeknum 1.8",  {2008, 12, 31}, [{"W", "1"}]  },
      { "weeknum 1.9",  {2009,  1,  1}, [{"W", "1"}]  },
      { "weeknum 1.10", {2009, 12, 31}, [{"W", "53"}] },
      { "weeknum 1.11", {2010,  1,  3}, [{"W", "53"}] },
      % Examples where the ISO year is three days into
      % the next Gregorian year
      { "weeknum 2.1",  {2009, 12, 31}, [{"W", "53"}] },
      { "weeknum 2.2",  {2010,  1,  1}, [{"W", "53"}] },
      { "weeknum 2.3",  {2010,  1,  2}, [{"W", "53"}] },
      { "weeknum 2.4",  {2010,  1,  3}, [{"W", "53"}] },
      { "weeknum 2.5",  {2010,  1,  5}, [{"W", "1"}] },
      % Example where the ISO year is three days into
      % the previous Gregorian year
      { "weeknum 3.1",  {2008, 12, 28}, [{"W", "52"}] },
      { "weeknum 3.2",  {2008, 12, 29}, [{"W", "1"}] },
      { "weeknum 3.3",  {2008, 12, 30}, [{"W", "1"}] },
      { "weeknum 3.4",  {2008, 12, 31}, [{"W", "1"}] },
      { "weeknum 3.5",  {2009,  1,  1}, [{"W", "1"}] },
      % freeform tests
      { "weeknum 4.1",  {2008,  2, 28}, [{"W", "9"}] },
      { "weeknum 4.2",  {1975,  7, 24}, [{"W","30"}] },

      % Yearweek tests.  Largely based on examples from :
      %   http://en.wikipedia.org/wiki/ISO_week_date
      { "weeknum_year 1.1",  {2005,  1,  1}, [{"o", "2004"}] },
      { "weeknum_year 1.2",  {2005,  1,  2}, [{"o", "2004"}] },
      { "weeknum_year 1.3",  {2005, 12, 31}, [{"o", "2005"}] },
      { "weeknum_year 1.4",  {2007,  1,  1}, [{"o", "2007"}]  },
      { "weeknum_year 1.5",  {2007, 12, 30}, [{"o", "2007"}] },
      { "weeknum_year 1.6",  {2007, 12, 31}, [{"o", "2008"}]  },
      { "weeknum_year 1.6",  {2008,  1,  1}, [{"o", "2008"}]  },
      { "weeknum_year 1.7",  {2008, 12, 29}, [{"o", "2009"}]  },
      { "weeknum_year 1.8",  {2008, 12, 31}, [{"o", "2009"}]  },
      { "weeknum_year 1.9",  {2009,  1,  1}, [{"o", "2009"}]  },
      { "weeknum_year 1.10", {2009, 12, 31}, [{"o", "2009"}] },
      { "weeknum_year 1.11", {2010,  1,  3}, [{"o", "2009"}] },
      % Examples where the ISO year is three days into
      % the next Gregorian year
      { "weeknum_year 2.1",  {2009, 12, 31}, [{"o", "2009"}] },
      { "weeknum_year 2.2",  {2010,  1,  1}, [{"o", "2009"}] },
      { "weeknum_year 2.3",  {2010,  1,  2}, [{"o", "2009"}] },
      { "weeknum_year 2.4",  {2010,  1,  3}, [{"o", "2009"}] },
      { "weeknum_year 2.5",  {2010,  1,  5}, [{"o", "2010"}] },
      % Example where the ISO year is three days into
      % the previous Gregorian year
      { "weeknum_year 3.1",  {2008, 12, 28}, [{"o", "2008"}] },
      { "weeknum_year 3.2",  {2008, 12, 29}, [{"o", "2009"}] },
      { "weeknum_year 3.3",  {2008, 12, 30}, [{"o", "2009"}] },
      { "weeknum_year 3.4",  {2008, 12, 31}, [{"o", "2009"}] },
      { "weeknum_year 3.5",  {2009,  1,  1}, [{"o", "2009"}] },
      % freeform tests
      { "weeknum_year 4.1",  {2008,  2, 28}, [{"o", "2008"}] },
      { "weeknum_year 4.2",  {1975,  7, 24}, [{"o", "1975"}] },

      % Ordinal suffix tests.
      { "Ordinal suffix 1", {1984,1,1},  [{"S", "st"}] },
      { "Ordinal suffix 2", {1984,2,2},  [{"S", "nd"}] },
      { "Ordinal suffix 3", {1984,3,3},  [{"S", "rd"}] },
      { "Ordinal suffix 4", {1984,4,4},  [{"S", "th"}] },
      { "Ordinal suffix 5", {1984,6,5},  [{"S", "th"}] },
      { "Ordinal suffix 7", {1984,2,9},  [{"S", "th"}] },
      { "Ordinal suffix 8", {1984,9,9},  [{"S", "th"}] },
      { "Ordinal suffix 9", {1984,11,10}, [{"S", "th"}] },
      { "Ordinal suffix 10", {1984,12,11}, [{"S", "th"}] },
      { "Ordinal suffix 11", {1984,8,12}, [{"S", "th"}] },
      { "Ordinal suffix 12", {1984,1,19}, [{"S", "th"}] },
      { "Ordinal suffix 13", {1984,2,20}, [{"S", "th"}] },
      { "Ordinal suffix 14", {1984,2,21}, [{"S", "st"}] },
      { "Ordinal suffix 15", {1984,7,22}, [{"S", "nd"}] },
      { "Ordinal suffix 16", {1984,6,23}, [{"S", "rd"}] },
      { "Ordinal suffix 17", {1984,5,24}, [{"S", "th"}] },
      { "Ordinal suffix 18", {1984,1,29}, [{"S", "th"}] },
      { "Ordinal suffix 19", {1984,3,30}, [{"S", "th"}] },
      { "Ordinal suffix 20", {1984,1,31}, [{"S", "st"}] },
      { "Ordinal suffix 21", {1984,1,310}, [{"S", "th"}] },
      { "Ordinal suffix 22", {1984,1,121}, [{"S", "st"}] }
   ]),

    if Failures == 0 ->
            io:format("All Date format tests PASS~n~n");
       true ->
            io:format("Date format failures: ~p~n~n", [Failures]),
            throw(failed)
    end.

test_group_runner([]) -> 0;
test_group_runner([{Info, DateParam, Tests} | Rest]) ->
   io:format(" Test ~p -> ", [Info]),
   PassCount = test_runner(DateParam, Tests),
   case PassCount =:= length(Tests) of
       true ->
           io:format("Passed ~p/~p~n", [PassCount, length(Tests)]);
       _ ->
           io:format("~nFailed ~p/~p~n", [length(Tests) - PassCount, length(Tests)])
   end,
   test_group_runner(Rest) + length(Tests) - PassCount.

test_runner(DateParam, Tests) ->
    test_runner(DateParam, Tests, 1, 0).
test_runner(_DateParam, [], _TestNum, PassCount) ->
    PassCount;
test_runner(DateParam, [{Input, Expect} | Rest], TestNum, PassCount) ->
    Text = "'" ++ Input ++ "' -> '" ++ Expect ++ "'",
    IsPass = is(TestNum, Text, erlydtl_dateformat:format(DateParam, Input), Expect),
    test_runner(DateParam, Rest, TestNum + 1, PassCount + IsPass).
    
is(_TestNum, _Text, Input1, Input2) when Input1 =:= Input2 ->
    1;
is(TestNum, Text, Input1, Input2) -> 
    io:format("~nnot ok ~p - ~s~n     got : ~p~n expected : ~p", [
       TestNum, Text, Input1, Input2]),
    0.
