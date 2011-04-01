%% Copyright (C) 07/01/2010 Dmitry S. Melnikov (dmitryme@gmail.com)
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-module(localtime).

-author("Dmitry Melnikov <dmitryme@gmail.com>").

-include("../include/tz_database.hrl").

-export(
  [
     utc_to_local/2
     ,local_to_utc/2
     ,local_to_local/3
     ,tz_name/2
  ]).

% utc_to_local(UtcDateTime, Timezone) -> LocalDateTime | {error, ErrDescr}
%  UtcDateTime = DateTime()
%  Timezone = String()
%  LocalDateTime = DateTime()
%  ErrDescr = unknown_tz
utc_to_local(UtcDateTime, Timezone) ->
   case lists:keyfind(Timezone, 1, ?tz_database) of
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
               RecheckIt = adjust_datetime(LocalDateTime, DstShift),
               case localtime_dst:check(RecheckIt, TzRule) of
                  ambiguous_time ->
                     RecheckIt;
                  _ ->
                     LocalDateTime
               end
         end
   end.

% local_to_utc(LocalDateTime, Timezone) -> UtcDateTime | tim_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  Timezone = String()
%  UtcDateTime = DateTime()
%  ErrDescr = unknown_tz
local_to_utc(LocalDateTime, Timezone) ->
   case lists:keyfind(Timezone, 1, ?tz_database) of
      false ->
         {error, unknown_tz};
      {_Tz, _, _, Shift, _DstShift, undef, _DstStartTime, undef, _DstEndTime} ->
         adjust_datetime(LocalDateTime, invert_shift(Shift));
      TzRule = {_, _, _, Shift, DstShift, _, _, _, _} ->
         UtcDateTime = adjust_datetime(LocalDateTime, invert_shift(Shift)),
         case localtime_dst:check(LocalDateTime, TzRule) of
            is_in_dst ->
               adjust_datetime(UtcDateTime, invert_shift(DstShift));
            Res when (Res == is_not_in_dst) or (Res == ambiguous_time) ->
               UtcDateTime;
            time_not_exists ->
               time_not_exists
         end
   end.

% local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) -> LocalDateTime | tim_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  TimezoneFrom = String()
%  TimezoneTo = String()
%  ErrDescr = unknown_tz
local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) ->
   case local_to_utc(LocalDateTime, TimezoneFrom) of
      Date = {{_,_,_},{_,_,_}} ->
         utc_to_local(Date, TimezoneTo);
      Res ->
         Res
   end.

% tz_name(DateTime(), Timezone) -> {Abbr, Name} | {{StdAbbr, StdName}, {DstAbbr, DstName}} | unable_to_detect
%  Timezone = String()
%  Abbr = String()
%  Name = String()
%  StdAbbr = String()
%  StdName = String()
%  DstAbbr = String()
%  DstName = String()
tz_name(_UtcDateTime, "UTC") ->
   {"UTC", "UTC"};
tz_name(LocalDateTime, Timezone) ->
   case lists:keyfind(Timezone, 1, ?tz_database) of
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

adjust_datetime(DateTime, {Hours, Minutes}) ->
   Seconds = calendar:datetime_to_gregorian_seconds(DateTime) + Hours * 3600,
   case Hours < 0 of
      true ->
         calendar:gregorian_seconds_to_datetime(Seconds - Minutes * 60);
      false ->
         calendar:gregorian_seconds_to_datetime(Seconds + Minutes * 60)
   end.

invert_shift({Hours, Minutes}) ->
   {-Hours, Minutes}.
