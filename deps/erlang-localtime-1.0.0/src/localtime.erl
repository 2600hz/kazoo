%% @author  Dmitry S. Melnikov (dmitryme@gmail.com)
%% @copyright 2010 Dmitry S. Melnikov

-module(localtime).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-include("tz_database.hrl").
-include("tz_index.hrl").

-export(
  [
     utc_to_local/2
     ,local_to_utc/2
     ,local_to_local/3
     ,local_to_local_dst/3
     ,tz_name/2
     ,tz_shift/2
     ,tz_shift/3
  ]).

% utc_to_local(UtcDateTime, Timezone) -> LocalDateTime | [LocalDateTime, DstLocalDateTime] | {error, ErrDescr}
%  UtcDateTime = DateTime()
%  Timezone = String()
%  LocalDateTime = DateTime()
%  DstLocalDateTime = DateTime()
%  ErrDescr = atom(), unknown_tz
utc_to_local(UtcDateTime, Timezone) ->
   case lists:keyfind(get_timezone(Timezone), 1, ?tz_database) of
      false ->
         {error, unknown_tz};
      {_Tz, _, _, Shift, _DstShift, undef, _DstStartTime, undef, _DstEndTime} ->
         adjust_datetime(UtcDateTime, Shift);
      TzRule = {_, _, _, Shift, DstShift, _, _, _, _} ->
         LocalDateTime = adjust_datetime(UtcDateTime, Shift),
         case localtime_dst:check(LocalDateTime, TzRule) of
            Res when (Res == is_in_dst) or (Res == time_not_exists) ->
               adjust_datetime(LocalDateTime, DstShift);
            is_not_in_dst ->
               LocalDateTime;
            ambiguous_time ->
               [LocalDateTime, adjust_datetime(LocalDateTime, DstShift)]
         end
   end.

% local_to_utc(LocalDateTime, Timezone) -> UtcDateTime | [UtcDateTime, DstUtcDateTime] | time_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  Timezone = String()
%  UtcDateTime = DateTime()
%  DstUtcDateTime = DateTime()
%  ErrDescr = atom(), unknown_tz
local_to_utc(LocalDateTime, "UTC") ->
    LocalDateTime;
local_to_utc(LocalDateTime, Timezone) when is_binary(Timezone) ->
    local_to_utc(LocalDateTime, binary_to_list(Timezone));
local_to_utc(LocalDateTime, Timezone) ->
   case lists:keyfind(get_timezone(Timezone), 1, ?tz_database) of
      false ->
         {error, unknown_tz};
      {_Tz, _, _, Shift, _DstShift, undef, _DstStartTime, undef, _DstEndTime} ->
         adjust_datetime(LocalDateTime, invert_shift(Shift));
      TzRule = {_, _, _, Shift, DstShift, _, _, _, _} ->
         UtcDateTime = adjust_datetime(LocalDateTime, invert_shift(Shift)),
         case localtime_dst:check(LocalDateTime, TzRule) of
            is_in_dst ->
               adjust_datetime(UtcDateTime, invert_shift(DstShift));
            is_not_in_dst ->
               UtcDateTime;
            ambiguous_time ->
               [UtcDateTime, adjust_datetime(UtcDateTime, invert_shift(DstShift))];
            time_not_exists ->
               time_not_exists
         end
   end.

% local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) -> LocalDateTime | ambiguous | time_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  TimezoneFrom = String()
%  TimezoneTo = String()
%  ErrDescr = atom(), unknown_tz
local_to_local(LocalDateTime, TzFrom, TzTo) when is_binary(TzFrom) ->
    local_to_local(LocalDateTime, binary_to_list(TzFrom), TzTo);
local_to_local(LocalDateTime, TzFrom, TzTo) when is_binary(TzTo) ->
    local_to_local(LocalDateTime, TzFrom, binary_to_list(TzTo));
local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) ->
   case local_to_utc(LocalDateTime, TimezoneFrom) of
      UtcDateTime = {{_,_,_},{_,_,_}} ->
         LocalDateTime2 = utc_to_local(UtcDateTime, TimezoneTo);
      [UtcDateTime, {{_,_,_},{_,_,_}}] ->
         LocalDateTime2 = utc_to_local(UtcDateTime, TimezoneTo);
      Res ->
         LocalDateTime2 = Res
   end,
   case LocalDateTime2 of
      [DateTimeToReturn, {{_,_,_},{_,_,_}}] ->
         DateTimeToReturn;
      Other ->
         Other
   end.

% local_to_local_dst(LocalDateTime, TimezoneFrom, TimezoneTo) -> LocalDateTime | ambiguous | time_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  TimezoneFrom = String()
%  TimezoneTo = String()
%  ErrDescr = atom(), unknown_tz
local_to_local_dst(LocalDateTime, TzFrom, TzTo) when is_binary(TzFrom) ->
    local_to_local_dst(LocalDateTime, binary_to_list(TzFrom), TzTo);
local_to_local_dst(LocalDateTime, TzFrom, TzTo) when is_binary(TzTo) ->
    local_to_local_dst(LocalDateTime, TzFrom, binary_to_list(TzTo));
local_to_local_dst(LocalDateTime, TimezoneFrom, TimezoneTo) ->
   case local_to_utc(LocalDateTime, TimezoneFrom) of
      UtcDateTime = {{_,_,_},{_,_,_}} ->
         LocalDateTime2 = utc_to_local(UtcDateTime, TimezoneTo);
      [{{_,_,_},{_,_,_}}, UtcDateTime] ->
         LocalDateTime2 = utc_to_local(UtcDateTime, TimezoneTo);
      Res ->
         LocalDateTime2 = Res
   end,
   case LocalDateTime2 of
      [{{_,_,_},{_,_,_}}, DateTimeToReturn] ->
         DateTimeToReturn;
      Other ->
         Other
   end.

% tz_name(DateTime(), Timezone) -> {Abbr, Name} | {{StdAbbr, StdName}, {DstAbbr, DstName}} | unable_to_detect | {error, ErrDesc}
%  Timezone = String()
%  Abbr = String()
%  Name = String()
%  StdAbbr = String()
%  StdName = String()
%  DstAbbr = String()
%  DstName = String()
%  ErrDesc = atom(), unknown_tz
tz_name(_UtcDateTime, "UTC") ->
   {"UTC", "UTC"};
tz_name(LocalDateTime, Timezone) when is_binary(Timezone) ->
    tz_name(LocalDateTime, binary_to_list(Timezone));
tz_name(LocalDateTime, Timezone) ->
   case lists:keyfind(get_timezone(Timezone), 1, ?tz_database) of
      false ->
         {error, unknown_tz};
      {_Tz, StdName, undef, _Shift, _DstShift, undef, _DstStartTime, undef, _DstEndTime} ->
         StdName;
      TzRule = {_, StdName, DstName, _Shift, _DstShift, _, _, _, _} ->
         case localtime_dst:check(LocalDateTime, TzRule) of
            is_in_dst ->
               DstName;
            is_not_in_dst ->
               StdName;
            ambiguous_time ->
               {StdName, DstName};
            time_not_exists ->
               unable_to_detect
         end
   end.

% tz_shift(LocalDateTime, Timezone) ->  Shift | {Shift, DstSift} | unable_to_detect | {error, ErrDesc}
%  returns time shift from GMT
%  LocalDateTime = DateTime()
%  Timezone = String()
%  Shift = DstShift = {Sign, Hours, Minutes}
%  Sign = term(), '+', '-'
%  Hours = Minutes = Integer(),
%  {Shift, DstShift} - returns, when shift is ambiguous
%  ErrDesc = atom(), unknown_tz
tz_shift(_UtcDateTime, "UTC") ->
   0;
tz_shift(LocalDateTime, Timezone) when is_binary(Timezone) ->
    tz_shift(LocalDateTime, binary_to_list(Timezone));
tz_shift(LocalDateTime, Timezone) ->
   case lists:keyfind(get_timezone(Timezone), 1, ?tz_database) of
      false ->
         {error, unknown_tz};
      {_Tz, _StdName, undef, Shift, _DstShift, undef, _DstStartTime, undef, _DstEndTime} ->
         fmt_min(Shift);
      TzRule = {_, _StdName, _DstName, Shift, DstShift, _, _, _, _} ->
         case localtime_dst:check(LocalDateTime, TzRule) of
            is_in_dst ->
               fmt_min(Shift + DstShift);
            is_not_in_dst ->
               fmt_min(Shift);
            ambiguous_time ->
               {fmt_min(Shift), fmt_min(Shift + DstShift)};
            time_not_exists ->
               unable_to_detect
         end
   end.

% the same as tz_shift/2, but calculates time difference between two local timezones
tz_shift(LocalDateTime, TimezoneFrom, TimezoneTo) when is_binary(TimezoneFrom) ->
    tz_shift(LocalDateTime, binary_to_list(TimezoneFrom), TimezoneTo);
tz_shift(LocalDateTime, TimezoneFrom, TimezoneTo) when is_binary(TimezoneTo) ->
    tz_shift(LocalDateTime, TimezoneFrom, binary_to_list(TimezoneTo));
tz_shift(LocalDateTime, TimezoneFrom, TimezoneTo) ->
   F = fun() ->
      FromShift = fmt_shift(tz_shift(LocalDateTime, TimezoneFrom)),
      DateTimeTo = localtime:local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo),
      ToShift = fmt_shift(tz_shift(DateTimeTo, TimezoneTo)),
      fmt_min(ToShift-FromShift)
   end,
   try F()
   catch
      _:Err ->
         Err
   end.

% =======================================================================
% privates
% =======================================================================

adjust_datetime(DateTime, Minutes) ->
   Seconds = calendar:datetime_to_gregorian_seconds(DateTime) + Minutes * 60,
   calendar:gregorian_seconds_to_datetime(Seconds).

invert_shift(Minutes) ->
   -Minutes.

fmt_min(Shift) when Shift < 0 ->
   {'-', abs(Shift) div 60, abs(Shift) rem 60};
fmt_min(Shift) ->
   {'+', Shift div 60, Shift rem 60}.

fmt_shift({'+', H, M}) ->
   H * 60 + M;
fmt_shift({'-', H, M}) ->
   -(H * 60 + M);
fmt_shift(Any) ->
   throw(Any).

tr_char(String, From, To) ->
   case string:chr(String, From) of
      0 -> String; % Optimize for String does not contain From.
      _ -> tr_char(String, From, To, [])
   end.
tr_char([], _From, _To, Acc) ->
   lists:reverse(Acc);
tr_char([H|T], From, To, Acc) ->
   case H of
      From -> tr_char(T, From, To, [To|Acc]);
      _ -> tr_char(T, From, To, [H|Acc])
   end.

-define(SPACE_CHAR, 32).
get_timezone(TimeZone) when is_binary(TimeZone) ->
    get_timezone(binary_to_list(TimeZone));
get_timezone(TimeZone) ->
   TimeZoneNoSpaces = tr_char(TimeZone, ?SPACE_CHAR, $_),
   case dict:find(TimeZoneNoSpaces, ?tz_index)  of
      error ->
         TimeZoneNoSpaces;
      {ok, [TZName | _]} ->
            TZName
   end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tr_char_test() ->
   ?assertEqual("ABCDE", tr_char("ABCDE", ?SPACE_CHAR, $_)),
   ?assertEqual("AB_DE", tr_char("AB DE", ?SPACE_CHAR, $_)),
   ?assertEqual("A_C_E", tr_char("A C E", ?SPACE_CHAR, $_)).

get_timezone_test() ->
   ?assertEqual("America/Los_Angeles", get_timezone("America/Los Angeles")).

tz_shift_test() ->
   ?assertEqual({'+',3,0}, tz_shift({{2014,1,1},{12,0,0}}, "America/Los_Angeles", "America/New_York")).

-endif. % TEST
