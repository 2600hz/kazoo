%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Represent a date, and perform various manipulations on it.
%%% @author Mark Magnusson
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_date).

-include("kazoo_stdlib/include/kz_types.hrl").

%% Date object functions
-export([from_gregorian_seconds/2
        ,from_iso_week/1
        ,to_iso_week/1

        ,find_next_weekday/2
        ,previous_day/1
        ,normalize/1
        ,relative_difference/2

        ,from_iso8601/1
        ,to_iso8601/1
        ,to_iso8601_extended/1
        ]).

%% Utility Functions
-export([ordinal_to_position/1
        ,wday_to_dow/1
        ,dow_to_wday/1
        ,days_in_month/2

        ,pad_month/1
        ,pad_day/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Convert a Gregorian seconds integer to kz_date taking into consideration timezone
%% @end
%%------------------------------------------------------------------------------
-spec from_gregorian_seconds(kz_time:gregorian_seconds(), kz_term:ne_binary()) ->
          kz_time:date().
from_gregorian_seconds(Seconds, <<TZ/binary>>) when is_integer(Seconds) ->
    {{_,_,_}, {_,_,_}} = DateTime = calendar:gregorian_seconds_to_datetime(Seconds),
    TZList = kz_term:to_list(TZ),
    {{_,_,_}=Date, {_,_,_}} = localtime:utc_to_local(DateTime, TZList),
    Date.

%%------------------------------------------------------------------------------
%% @doc Calculates the date of a given ISO 8601 week
%% @end
%%------------------------------------------------------------------------------
-spec from_iso_week(kz_time:iso_week()) -> kz_time:date().
from_iso_week({Year, Week}) ->
    Jan4 = calendar:date_to_gregorian_days(Year, 1, 4),
    Jan4DOW = calendar:day_of_the_week(Year, 1, 4),
    Days =
        %% days to the ISO 8601 first week for the year
        (Jan4 - weekday_distance(Jan4DOW, 1))
        +
        %% plus the number of days not including the first week
        (Week - 1) * 7,
    calendar:gregorian_days_to_date(Days).

-spec weekday_distance(kz_time:daynum(), kz_time:daynum()) -> kz_time:daynum().
weekday_distance(D0, D1) when D0 =< 7, D1 =< 7 ->
    case D0 - D1 of
        Days when Days =< 7 -> Days;
        Days -> Days + 7
    end.

-spec to_iso_week(kz_time:date()) -> kz_time:iso_week().
to_iso_week({_Year, _Month, _Day}=Date) ->
    calendar:iso_week_number(Date).

%%------------------------------------------------------------------------------
%% @doc Normalizes dates, for example corrects for months that are given
%% with more days then they have (i.e.: {2011, 1, 36} -> {2011, 2, 5}).
%% I have been referring to this as 'spanning a month/year border'
%% @end
%%------------------------------------------------------------------------------
-spec normalize(kz_time:date()) -> kz_time:date().
normalize({Y, 0, D}) ->
    normalize({Y - 1, 12, D});
normalize({Y, M, 0}) ->
    {Y1, M1, _} = normalize({Y, M - 1, 1}),
    D0 = calendar:last_day_of_the_month(Y1, M1),
    normalize({Y1, M1, D0});
normalize({Y, M, D}) when M > 12, M div 12 > 0 ->
    normalize({Y + (M div 12), M rem 12, D});
normalize({Y, M, D}=Date) ->
    case D - days_in_month(Y, M) of
        D1 when D1 > 0 -> normalize({Y, M + 1, D1});
        _ -> Date
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate when the second date is in time, relative to the first
%% calendar:time_difference/2 is obsolete. Convert to Gregorian seconds instead
%% @end
%%------------------------------------------------------------------------------
-spec relative_difference(kz_time:datetime(), kz_time:datetime()) -> 'future' | 'equal' | 'past'.
relative_difference(Date1, Date2) ->
    case {calendar:datetime_to_gregorian_seconds(Date1)
         ,calendar:datetime_to_gregorian_seconds(Date2)
         }
    of
        {Present, Present} -> 'equal';
        {Past, Future} when Future > Past -> 'future';
        {Future, Past} when Future > Past -> 'past'
    end.

%%------------------------------------------------------------------------------
%% @doc Calculates the date of the next occurrence of a weekday from the given
%% start date.
%%
%% <div class="notice">It is possible for this function to cross month/year boundaries.</div>
%% @end
%%------------------------------------------------------------------------------
-spec find_next_weekday(kz_time:date(), kz_term:ne_binary()) -> kz_time:date().
find_next_weekday({Y, M, D}, Weekday) ->
    RefDOW = wday_to_dow(Weekday),
    case calendar:day_of_the_week({Y, M, D}) of
        %% Today is the DOW we wanted, calculate for next week
        RefDOW ->
            normalize({Y, M, D + 7});
        %% If the DOW has not occurred this week yet
        DOW when RefDOW > DOW ->
            normalize({Y, M, D + (RefDOW - DOW)});
        %% If the DOW occurrence has already happened, calculate
        %%   for the next week using the current DOW as a reference
        DOW ->
            normalize({Y, M, D + ( 7 - DOW ) + RefDOW})
    end.

%%------------------------------------------------------------------------------
%% @doc Calculates the date of the previous day, while also handling the situation
%% where that day falls on the previous month or previous year.
%% @end
%%------------------------------------------------------------------------------
-spec previous_day(kz_time:date()) -> kz_time:date().
previous_day({Y, 1, 1}) ->
    {Y-1, 12, days_in_month(Y-1, 12)};

previous_day({Y, M, 1}) ->
    {Y, M-1, days_in_month(Y, M-1)};

previous_day({Y, M, D}) ->
    {Y, M, D-1}.

-spec from_iso8601(binary()) -> kz_time:date() | 'error'.
from_iso8601(<<Year:4/binary, Month:2/binary, Day:2/binary>>) ->
    {kz_term:to_integer(Year), kz_term:to_integer(Month), kz_term:to_integer(Day)};

from_iso8601(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>) ->
    {kz_term:to_integer(Year), kz_term:to_integer(Month), kz_term:to_integer(Day)};

from_iso8601(_NotValid) ->
    'error'.

-spec to_iso8601(calendar:date() | calendar:datetime() | kz_time:gregorian_seconds()) -> kz_term:ne_binary().
to_iso8601({Year, Month, Day}) ->
    Y = kz_binary:pad_left(kz_term:to_binary(Year), 4, <<"0">>),
    M = kz_binary:pad_left(kz_term:to_binary(Month), 2, <<"0">>),
    D = kz_binary:pad_left(kz_term:to_binary(Day), 2, <<"0">>),

    <<Y/binary, M/binary, D/binary>>;

to_iso8601({{_Y,_M,_D}=Date, _}) ->
    to_iso8601(Date);

to_iso8601(Timestamp) ->
    to_iso8601(calendar:gregorian_seconds_to_datetime(Timestamp)).

-spec to_iso8601_extended(calendar:date() | calendar:datetime() | kz_time:gregorian_seconds()) -> kz_term:ne_binary().
to_iso8601_extended({Year, Month, Day}) ->
    Y = kz_binary:pad_left(kz_term:to_binary(Year), 4, <<"0">>),
    M = kz_binary:pad_left(kz_term:to_binary(Month), 2, <<"0">>),
    D = kz_binary:pad_left(kz_term:to_binary(Day), 2, <<"0">>),

    <<Y/binary, "-", M/binary, "-", D/binary>>;

to_iso8601_extended({{_Y,_M,_D}=Date, _}) ->
    to_iso8601_extended(Date);

to_iso8601_extended(Timestamp) ->
    to_iso8601_extended(calendar:gregorian_seconds_to_datetime(Timestamp)).

%%------------------------------------------------------------------------------
%% @doc Convert the ordinal words to cardinal numbers representing
%% the position
%% @end
%%------------------------------------------------------------------------------
-spec ordinal_to_position(kz_term:ne_binary()) -> 0..4.
ordinal_to_position(<<"first">>) -> 0;
ordinal_to_position(<<"second">>) -> 1;
ordinal_to_position(<<"third">>) -> 2;
ordinal_to_position(<<"fourth">>) -> 3;
ordinal_to_position(<<"fifth">>) -> 4.

%%------------------------------------------------------------------------------
%% @doc Map the days of the week to cardinal numbers representing the
%% position, in accordance with ISO 8601
%% @end
%%------------------------------------------------------------------------------
-spec wday_to_dow(kz_term:ne_binary()) -> kz_time:daynum().
wday_to_dow(<<"monday">>) -> 1;
wday_to_dow(<<"tuesday">>) -> 2;
wday_to_dow(<<"wednesday">>) -> 3;
wday_to_dow(<<"wensday">>) -> 3;
wday_to_dow(<<"thursday">>) -> 4;
wday_to_dow(<<"friday">>) -> 5;
wday_to_dow(<<"saturday">>) -> 6;
wday_to_dow(<<"sunday">>) -> 7.

%%------------------------------------------------------------------------------
%% @doc Map the position of a week day to its textual representation.
%% @end
%%------------------------------------------------------------------------------
-spec dow_to_wday(kz_time:daynum()) -> kz_term:ne_binary().
dow_to_wday(1) -> <<"monday">>;
dow_to_wday(2) -> <<"tuesday">>;
dow_to_wday(3) -> <<"wednesday">>;
dow_to_wday(4) -> <<"thursday">>;
dow_to_wday(5) -> <<"friday">>;
dow_to_wday(6) -> <<"saturday">>;
dow_to_wday(7) -> <<"sunday">>.

-spec days_in_month(kz_time:year(), 0..13) -> 28..31.
days_in_month(_Year,  1) -> 31;
days_in_month(_Year,  3) -> 31;
days_in_month(_Year,  5) -> 31;
days_in_month(_Year,  7) -> 31;
days_in_month(_Year,  8) -> 31;
days_in_month(_Year, 10) -> 31;
days_in_month(_Year, 12) -> 31;
days_in_month(_Year,  4) -> 30;
days_in_month(_Year,  6) -> 30;
days_in_month(_Year,  9) -> 30;
days_in_month(_Year, 11) -> 30;
days_in_month(Year,   0) -> days_in_month(Year-1, 12);
days_in_month(Year,  13) -> days_in_month(Year+1, 1);
days_in_month(Year,   2) ->
    case calendar:is_leap_year(Year) of
        'true' -> 29;
        'false' -> 28
    end.

-spec pad_month(kz_time:month() | kz_term:ne_binary()) -> kz_term:ne_binary().
pad_month(Month) ->
    kz_binary:pad_left(kz_term:to_binary(Month), 2, <<"0">>).

-spec pad_day(kz_time:day() | kz_term:ne_binary()) -> kz_term:ne_binary().
pad_day(Day) ->
    kz_binary:pad_left(kz_term:to_binary(Day), 2, <<"0">>).
