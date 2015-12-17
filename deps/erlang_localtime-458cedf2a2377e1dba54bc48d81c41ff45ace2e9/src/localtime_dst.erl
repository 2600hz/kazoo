%% @author  Dmitry S. Melnikov (dmitryme@gmail.com)
%% @copyright 2010 Dmitry S. Melnikov

-module(localtime_dst).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-export(
   [
      check/2
   ]).

-compile([export_all]).


% check(DateTime, TimeZone) -> is_in_dst | is_not_in_dst | ambiguous_time | time_not_exists
%  DateTime = DateTime()
%  TimeZone = tuple()
check({Date = {Year, _, _},Time}, {_, _, _, _Shift, DstShift, DstStartRule, DstStartTime, DstEndRule, DstEndTime}) ->
   DstStartDay = get_dst_day_of_year(DstStartRule, Year),
   DstEndDay = get_dst_day_of_year(DstEndRule, Year),
   CurrDay = get_day_of_year(Date),
   case is_dst_date(DstStartDay, DstEndDay, CurrDay) of
      equal_to_start ->
         is_dst_start_time(time_to_minutes(Time), time_to_minutes(DstStartTime), DstShift);
      equal_to_end ->
         is_dst_end_time(time_to_minutes(Time), time_to_minutes(DstEndTime), DstShift);
      Res ->
         Res
   end.

is_dst_start_time(CurrTime, DstStartTime, _DstShift) when CurrTime < DstStartTime ->
   is_not_in_dst;
is_dst_start_time(CurrTime, DstStartTime, DstShift) when CurrTime >= (DstStartTime + DstShift) ->
   is_in_dst;
is_dst_start_time(_CurrTime, _DstStartTime, _DstShift) ->
   time_not_exists.

is_dst_end_time(CurrTime, DstEndTime, DstShift) when CurrTime < (DstEndTime - DstShift) ->
   is_in_dst;
is_dst_end_time(CurrTime, DstEndTime, _DstShift) when CurrTime >= DstEndTime ->
   is_not_in_dst;
is_dst_end_time(_CurrTime, _DstStartTime, _DstShift) ->
   ambiguous_time.

is_dst_date(DstStartDay, _DstEndDay, CurrDay) when (CurrDay == DstStartDay) -> equal_to_start;
is_dst_date(_DstStartDay, DstEndDay, CurrDay) when (CurrDay == DstEndDay) -> equal_to_end;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
   when (DstStartDay < DstEndDay) andalso ((CurrDay > DstStartDay) and (CurrDay < DstEndDay)) ->
      is_in_dst;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
   when (DstStartDay < DstEndDay) andalso ((CurrDay < DstStartDay) or (CurrDay > DstEndDay)) ->
      is_not_in_dst;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
   when (DstStartDay > DstEndDay) andalso ((CurrDay < DstStartDay) and (CurrDay > DstEndDay)) ->
      is_not_in_dst;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
   when (DstStartDay > DstEndDay) andalso ((CurrDay > DstStartDay) or (CurrDay < DstEndDay)) ->
      is_in_dst.

get_dst_day_of_year({WeekDay,DayOfWeek,Month}, Year) when (WeekDay == last) or (WeekDay == 5) ->
   IntMonth = month_to_int(Month),
   IntDayOfWeek = day_to_int(DayOfWeek),
   get_last_dst(IntDayOfWeek, IntMonth, Year);
get_dst_day_of_year({WeekDay,DayOfWeek,Month}, Year) when (WeekDay > 0) and (WeekDay =< 4) ->
   IntMonth = month_to_int(Month),
   IntDayOfWeek = day_to_int(DayOfWeek),
   DstDays = get_day_of_year({Year, IntMonth, 1}),
   DstDayOfWeek = calendar:day_of_the_week({Year, IntMonth, 1}),
   case (DstDayOfWeek =:= IntDayOfWeek) and (WeekDay =:= 1) of
      true ->
         DstDays;
      false ->
         AdjustedDstDays =
         case IntDayOfWeek >= DstDayOfWeek of
            true ->
               DstDays + (IntDayOfWeek - DstDayOfWeek);
            false ->
               DstDays + (7 - DstDayOfWeek) + IntDayOfWeek
         end,
         AdjustedDstDays + (WeekDay - 1) * 7
      end;
get_dst_day_of_year(_, _) ->
   throw({error, wrong_week_day}).

get_last_dst(IntDayOfWeek, IntMonth, Year) ->
   MonthLastDays = calendar:date_to_gregorian_days(Year, IntMonth, 1) + calendar:last_day_of_the_month(Year, IntMonth),
   MonthLastDate = calendar:gregorian_days_to_date(MonthLastDays),
   MonthLastDayOfWeek = calendar:day_of_the_week(MonthLastDate),
   case MonthLastDayOfWeek > IntDayOfWeek of
      true ->
         MonthLastDays - (MonthLastDayOfWeek - IntDayOfWeek) - calendar:date_to_gregorian_days({Year - 1, 12, 31});
      false ->
         MonthLastDays - MonthLastDayOfWeek - (7 - IntDayOfWeek) - calendar:date_to_gregorian_days({Year - 1, 12, 31})
   end.

get_day_of_year(Date = {Year, _Month, _Day}) ->
   calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days({Year - 1, 12, 31}).

month_to_int(jan) -> 1;
month_to_int(feb) -> 2;
month_to_int(mar) -> 3;
month_to_int(apr) -> 4;
month_to_int(may) -> 5;
month_to_int(jun) -> 6;
month_to_int(jul) -> 7;
month_to_int(aug) -> 8;
month_to_int(sep) -> 9;
month_to_int(oct) -> 10;
month_to_int(nov) -> 11;
month_to_int(dec) -> 12.

day_to_int(mon) -> 1;
day_to_int(tue) -> 2;
day_to_int(wed) -> 3;
day_to_int(thu) -> 4;
day_to_int(fri) -> 5;
day_to_int(sat) -> 6;
day_to_int(sun) -> 7.

time_to_minutes({Hours, Minutes}) ->
   Hours * 60 + Minutes;
time_to_minutes({Hours, Minutes, _Seconds}) ->
   Hours * 60 + Minutes.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_day_of_year_test() ->
   ?assertEqual(62, get_dst_day_of_year({1,wed,mar}, 2010)).

check_test() ->
   Tz = {"Europe/Moscow",{"MSK","MSK"},{"MSD","MSD"},180,60,{last,sun,mar},{2,0},{last,sun,oct},{3,0}},
   ?assertEqual(is_not_in_dst, localtime_dst:check({{2010, 1, 1}, {10, 10, 0}}, Tz)),
   ?assertEqual(is_in_dst, check({{2010, 7, 8}, {10, 10, 0}}, Tz)),
   ?assertEqual(is_not_in_dst, check({{2010, 3, 28}, {1, 59, 0}}, Tz)),
   ?assertEqual(time_not_exists, check({{2010, 3, 28}, {2, 00, 0}}, Tz)),
   ?assertEqual(time_not_exists, check({{2010, 3, 28}, {2, 15, 0}}, Tz)),
   ?assertEqual(time_not_exists, check({{2010, 3, 28}, {2, 30, 0}}, Tz)),
   ?assertEqual(time_not_exists, check({{2010, 3, 28}, {2, 59, 0}}, Tz)),
   ?assertEqual(is_in_dst, check({{2010, 3, 28}, {3, 00, 0}}, Tz)),

   ?assertEqual(is_in_dst, check({{2010, 10, 31}, {1, 59, 0}}, Tz)),
   ?assertEqual(ambiguous_time, check({{2010, 10, 31}, {2, 00, 0}}, Tz)),
   ?assertEqual(ambiguous_time, check({{2010, 10, 31}, {2, 10, 0}}, Tz)),
   ?assertEqual(ambiguous_time, check({{2010, 10, 31}, {2, 30, 0}}, Tz)),
   ?assertEqual(ambiguous_time, check({{2010, 10, 31}, {2, 59, 0}}, Tz)),
   ?assertEqual(is_not_in_dst, check({{2010, 10, 31}, {3, 00, 0}}, Tz)),

   %% DST starts at hour 24; DST ends at hour 0:
   TzGaza = {"Asia/Gaza",{"EET","EET"},{"EEST","EEST"},120,60,{last,thu,mar},{24,0},{4,fri,sep},{0,0}},
   ?assertEqual(is_not_in_dst,   check({{2014, 3, 27}, {23, 59, 59}}, TzGaza)),
   %% Currently ST->DT transitions in the last hour of the day are not handled correctly.
   %?assertEqual(time_not_exists, check({{2014, 3, 28}, { 0, 00, 00}}, TzGaza)),
   %?assertEqual(time_not_exists, check({{2014, 3, 28}, { 0, 59, 59}}, TzGaza)),
   ?assertEqual(is_in_dst,       check({{2014, 3, 28}, { 0, 59, 59}}, TzGaza)), % WRONG
   ?assertEqual(is_in_dst,       check({{2014, 3, 28}, { 1, 00, 00}}, TzGaza)),
   ?assertEqual(is_in_dst,       check({{2014, 9, 25}, {22, 59, 59}}, TzGaza)),
   %% Currently DT->ST transitions in the first hour of the day are not handled correctly.
   %?assertEqual(ambiguous_time,  check({{2014, 9, 25}, {23, 00, 00}}, TzGaza)),
   %?assertEqual(ambiguous_time,  check({{2014, 9, 25}, {23, 59, 59}}, TzGaza)),
   ?assertEqual(is_in_dst,       check({{2014, 9, 25}, {23, 59, 59}}, TzGaza)), % WRONG
   ?assertEqual(is_not_in_dst,   check({{2014, 9, 26}, { 0, 00, 00}}, TzGaza)),

   %% DST starts at hour 0; DST ends at hour 0.
   TzDamascus = {"Asia/Damascus",{"EET","EET"},{"EEST","EEST"},120,60,{last,fri,mar},{0,0},{last,fri,oct},{0,0}},
   ?assertEqual(is_not_in_dst,   check({{2014,  3, 27}, {23, 59, 59}}, TzDamascus)),
   ?assertEqual(time_not_exists, check({{2014,  3, 28}, { 0, 00, 00}}, TzDamascus)),
   ?assertEqual(time_not_exists, check({{2014,  3, 28}, { 0, 59, 59}}, TzDamascus)),
   ?assertEqual(is_in_dst,       check({{2014,  3, 28}, { 1, 00, 00}}, TzDamascus)),
   ?assertEqual(is_in_dst,       check({{2014, 10, 30}, {22, 59, 59}}, TzDamascus)),
   %% Currently DT->ST transitions in the first hour of the day are not handled correctly.
   %?assertEqual(ambiguous_time,  check({{2014, 10, 30}, {23, 00, 00}}, TzDamascus)),
   %?assertEqual(ambiguous_time,  check({{2014, 10, 30}, {23, 59, 59}}, TzDamascus)),
   ?assertEqual(is_not_in_dst,   check({{2014, 10, 31}, { 0, 00, 00}}, TzDamascus)),

   %% DST ends before starts (southern hemisphere):
   TzMontevideo = {"America/Montevideo",{"UYT","UYT"},{"UYST","UYST"},-180,60,{1,sun,oct},{2,0},{2,sun,mar},{2,0}},
   ?assertEqual(is_in_dst,       check({{2014,  3, 09}, { 0, 59, 59}}, TzMontevideo)),
   ?assertEqual(ambiguous_time,  check({{2014,  3, 09}, { 1, 00, 00}}, TzMontevideo)),
   ?assertEqual(ambiguous_time,  check({{2014,  3, 09}, { 1, 59, 59}}, TzMontevideo)),
   ?assertEqual(is_not_in_dst,   check({{2014,  3, 09}, { 2, 00, 00}}, TzMontevideo)),
   ?assertEqual(is_not_in_dst,   check({{2014, 10, 05}, { 1, 59, 59}}, TzMontevideo)),
   ?assertEqual(time_not_exists, check({{2014, 10, 05}, { 2, 00, 00}}, TzMontevideo)),
   ?assertEqual(time_not_exists, check({{2014, 10, 05}, { 2, 59, 59}}, TzMontevideo)),
   ?assertEqual(is_in_dst,       check({{2014, 10, 05}, { 3, 00, 00}}, TzMontevideo)),

   true.

-endif.
