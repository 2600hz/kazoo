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

-include("tz_database.hrl").
-include("tz_index.hrl").

-export(
  [
     utc_to_local/2
     ,local_to_utc/2
     ,local_to_local/3
     ,tz_name/2
     ,tz_shift/2
     ,tz_shift/3
  ]).

% utc_to_local(UtcDateTime, Timezone) -> LocalDateTime | {error, ErrDescr}
%  UtcDateTime = DateTime()
%  Timezone = String()
%  LocalDateTime = DateTime()
%  ErrDescr = atom(), unknown_tz
-spec utc_to_local/2 :: (calendar:t_datetime(), binary() | nonempty_string()) -> calendar:t_datetime() | {'error', 'unknown_tz'}.
utc_to_local(UtcDateTime, "UTC") ->
    UtcDateTime;
utc_to_local(UtcDateTime, TZ) when is_list(TZ) ->
    case get_timezone(TZ) of
        {tzrule, TzRule} -> process_tz_rule(UtcDateTime, TzRule);
        {error, _}=E -> E
    end;
utc_to_local(UtcDateTime, TZ) when is_binary(TZ) ->
    utc_to_local(UtcDateTime, binary_to_list(TZ)).

process_tz_rule(UtcDateTime, {_, _, _, Shift, DstShift, _, _, _, _}=TzRule) ->
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
    end.

% local_to_utc(LocalDateTime, Timezone) -> UtcDateTime | tim_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  Timezone = String()
%  UtcDateTime = DateTime()
%  ErrDescr = atom(), unknown_tz
-spec local_to_utc/2 :: (calendar:t_datetime(), binary() | nonempty_string()) ->
                                calendar:t_datetime() |
                                {'error', 'unknown_tz' | 'time_not_exists'}.
local_to_utc(LocalDateTime, "UTC") ->
    LocalDateTime;
local_to_utc(LocalDateTime, Timezone) when is_list(Timezone) ->
    case get_timezone(Timezone) of
        {tzrule, {_, _, _, Shift, DstShift, _, _, _, _}=TzRule} ->
            UtcDateTime = adjust_datetime(LocalDateTime, invert_shift(Shift)),
            case localtime_dst:check(LocalDateTime, TzRule) of
                is_in_dst ->
                    adjust_datetime(UtcDateTime, invert_shift(DstShift));
                Res when (Res == is_not_in_dst) or (Res == ambiguous_time) ->
                    UtcDateTime;
                time_not_exists ->
                    {error, time_not_exists}
            end;
        E -> E
    end;
local_to_utc(LocalDateTime, Timezone) when is_binary(Timezone) ->
    local_to_utc(LocalDateTime, binary_to_list(Timezone)).

% local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) -> LocalDateTime | tim_not_exists | {error, ErrDescr}
%  LocalDateTime = DateTime()
%  TimezoneFrom = String()
%  TimezoneTo = String()
%  ErrDescr = atom(), unknown_tz
-spec local_to_local/3 :: (calendar:t_datetime(), binary() | string(), binary() | string()) -> calendar:t_datetime().
local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) when is_list(TimezoneFrom), is_list(TimezoneTo) ->
    case local_to_utc(LocalDateTime, TimezoneFrom) of
        Date = {{_,_,_},{_,_,_}} ->
            utc_to_local(Date, TimezoneTo);
        Res ->
            Res
    end;
local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) when is_binary(TimezoneFrom) ->
    local_to_local(LocalDateTime, binary_to_list(TimezoneFrom), TimezoneTo);
local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo) when is_binary(TimezoneTo) ->
    local_to_local(LocalDateTime, TimezoneFrom, binary_to_list(TimezoneTo)).

% tz_name(DateTime(), Timezone) -> {Abbr, Name} | {{StdAbbr, StdName}, {DstAbbr, DstName}} | unable_to_detect | {error, ErrDesc}
%  Timezone = String()
%  Abbr = String()
%  Name = String()
%  StdAbbr = String()
%  StdName = String()
%  DstAbbr = String()
%  DstName = String()
%  ErrDesc = atom(), unknown_tz
-spec tz_name/2 :: (calendar:t_datetime(), string()) -> {string(), string()} | 'undef' |
                                                        {{string(), string()} | 'undef', {string(), string()} | 'undef'} |
                                                        'unable_to_detect' | {'error', 'unknown_tz'} |
                                                        {'shift', integer()}.
tz_name(_UtcDateTime, "UTC") -> {"UTC", "UTC"};
tz_name(LocalDateTime, Timezone) ->
    case get_timezone(Timezone) of
        {tzrule, {_, StdName, DstName, _Shift, _DstShift, _, _, _, _}=TzRule} ->
            case localtime_dst:check(LocalDateTime, TzRule) of
                is_in_dst ->
                    DstName;
                is_not_in_dst ->
                    StdName;
                ambiguous_time ->
                    {StdName, DstName};
                time_not_exists ->
                    unable_to_detect
            end;
        E -> E
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
-spec tz_shift/2 :: (calendar:t_datetime(), nonempty_string() | binary()) -> 0 | fmt_min_ret() | 'unable_to_detect' | {fmt_min_ret(), fmt_min_ret()} | {'error', 'unknown_tz'}.
tz_shift(DT, TZ) when is_binary(TZ) ->
    tz_shift(DT, binary_to_list(TZ));
tz_shift(_UtcDateTime, "UTC") ->
   0;
tz_shift(LocalDateTime, Timezone) ->
    case get_timezone(Timezone) of
        {tzrule, {_, _StdName, _DstName, Shift, DstShift, _, _, _, _}=TzRule} ->
            case localtime_dst:check(LocalDateTime, TzRule) of
                is_in_dst ->
                    fmt_min(Shift + DstShift);
                is_not_in_dst ->
                    fmt_min(Shift);
                ambiguous_time ->
                    {fmt_min(Shift), fmt_min(Shift + DstShift)};
                time_not_exists ->
                    unable_to_detect
            end;
        {error, unknown_tz}=E -> E
    end.

% the same as tz_shift/2, but calculates time difference between two local timezones
-spec tz_shift/3 :: (calendar:t_datetime(), nonempty_string() | binary(), nonempty_string() | binary()) -> fmt_min_ret().
tz_shift(DT, TzFrom, TzTo) when is_binary(TzFrom) ->
    tz_shift(DT, binary_to_list(TzFrom), TzTo);
tz_shift(DT, TzFrom, TzTo) when is_binary(TzTo) ->
    tz_shift(DT, TzFrom, binary_to_list(TzTo));
tz_shift(LocalDateTime, TimezoneFrom, TimezoneTo) ->
    FromShift = fmt_shift(tz_shift(LocalDateTime, TimezoneFrom)),
    DateTimeTo = localtime:local_to_local(LocalDateTime, TimezoneFrom, TimezoneTo),
    ToShift = fmt_shift(tz_shift(DateTimeTo, TimezoneTo)),
    fmt_min(ToShift-FromShift).

% =======================================================================
% privates
% =======================================================================

-spec adjust_datetime/2 :: (calendar:t_datetime(), non_neg_integer()) -> calendar:t_datetime().
adjust_datetime(DateTime, Minutes) ->
   Seconds = calendar:datetime_to_gregorian_seconds(DateTime) + Minutes * 60,
   calendar:gregorian_seconds_to_datetime(Seconds).

-spec invert_shift/1 :: (integer()) -> integer().
invert_shift(Minutes) ->
   -Minutes.

-type fmt_min_ret() :: {'-' | '+', non_neg_integer(), non_neg_integer()}.
-spec fmt_min/1 :: (integer()) -> fmt_min_ret().
fmt_min(Shift) when Shift < 0 ->
   {'-', abs(Shift) div 60, abs(Shift) rem 60};
fmt_min(Shift) ->
   {'+', Shift div 60, Shift rem 60}.

-spec fmt_shift/1 :: ({'+' | '-', non_neg_integer(), non_neg_integer()} | 0) -> integer().
fmt_shift({'+', H, M}) ->
   H * 60 + M;
fmt_shift({'-', H, M}) ->
   -(H * 60 + M);
fmt_shift(0) ->
    0.

-spec get_timezone/1 :: (nonempty_string() | binary()) -> {'error', 'unknown_tz'} |
                                                          {'tzrule', tz_db_row()}.
get_timezone(TZ) when is_binary(TZ) ->
    get_timezone(binary_to_list(TZ));
get_timezone(TZ) ->
    Timezone = re:replace(TZ, "_", " ", [{return, list}, global]),
    case lists:keyfind(get_timezone_from_index(Timezone), 1, ?tz_database) of
        false -> {error, unknown_tz};
        {_,_,_,_,_,_,_,_,_}=TzRule -> {tzrule, TzRule}
    end.

-spec get_timezone_from_index/1 :: (string() | binary()) -> string().
get_timezone_from_index(TZ) when is_binary(TZ) ->
    get_timezone(binary_to_list(TZ));
get_timezone_from_index(TimeZone) ->
    case lists:keyfind(TimeZone, 1, ?tz_index)  of
        false ->
            TimeZone;
        {_, [TZName | _]} ->
            TZName
    end.
