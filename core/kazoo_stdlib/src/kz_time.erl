%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Various utilities to work with time.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_time).

-export([current_tstamp/0, current_unix_tstamp/0
        ,gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1
        ,unix_timestamp_to_gregorian_seconds/1
        ,to_gregorian_seconds/2
        ,pretty_print_datetime/1, pretty_print_datetime/2
        ,rfc1036/1, rfc1036/2
        ,rfc2822/1, rfc2822/2
        ,iso8601/1, iso8601/2
        ,iso8601_time/1
        ,from_iso8601/1
        ,is_iso8601/1
        ,trim_iso8601_ms/1
        ,maybe_add_iso8601_ms_suffix/1
        ,pretty_print_elapsed_s/1
        ,decr_timeout/2, decr_timeout/3
        ,microseconds_to_seconds/1
        ,milliseconds_to_seconds/1
        ,elapsed_s/1, elapsed_ms/1, elapsed_us/1
        ,elapsed_s/2, elapsed_ms/2, elapsed_us/2
        ,now/0, now_s/0, now_ms/0, now_us/0
        ,now_s/1, now_ms/1, now_us/1
        ,format_date/0, format_date/1
        ,format_time/0, format_time/1
        ,format_datetime/0, format_datetime/1

        ,weekday/1, month/1
        ,unitfy_seconds/1
        ,adjust_utc_timestamp/2, adjust_utc_datetime/2
        ,tz_name/2

        ,start_time/0
        ]).

-ifdef(TEST).
-export([decr_timeout_elapsed/2]).
-endif.

-compile({'no_auto_import', [now/0]}).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type now() :: erlang:timestamp().
%% {MegaSecs :: integer() >= 0, Secs :: integer() >= 0, MicroSecs :: integer() >= 0}
-type year() :: non_neg_integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type daynum() :: 1..7.
-type weeknum() :: 1..53.
-type date() :: calendar:date(). %%{year(), month(), day()}.
-type time() :: calendar:time(). %%{hour(), minute(), second()}.
-type datetime() :: calendar:datetime(). %%{date(), time()}.
-type iso_week() :: {year(), weeknum()}.
-type gregorian_seconds() :: pos_integer().
-type unix_seconds() :: pos_integer().
-type api_seconds() :: 'undefined' | gregorian_seconds().
-type ordinal() :: kz_term:ne_binary(). % "every" | "first" | "second" | "third" | "fourth" | "fifth" | "last".
-type kazoo_month() :: {year(), month()}.

%% {'start_time', erlang:monotonic_time()}
-type start_time() :: {'start_time', integer()}.

-export_type([api_seconds/0
             ,date/0
             ,datetime/0
             ,day/0
             ,daynum/0
             ,gregorian_seconds/0
             ,hour/0
             ,iso_week/0
             ,kazoo_month/0
             ,minute/0
             ,month/0
             ,now/0
             ,ordinal/0
             ,second/0
             ,start_time/0
             ,time/0
             ,unix_seconds/0
             ,weeknum/0
             ,year/0
             ]).

%% returns current seconds
-spec current_tstamp() -> gregorian_seconds().
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-spec current_unix_tstamp() -> unix_seconds().
current_unix_tstamp() ->
    erlang:system_time('seconds').

-spec gregorian_seconds_to_unix_seconds(integer() | string() | binary()) -> integer().
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    kz_term:to_integer(GregorianSeconds) - ?UNIX_EPOCH_IN_GREGORIAN.

-spec unix_seconds_to_gregorian_seconds(integer() | string() | binary()) -> integer().
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    kz_term:to_integer(UnixSeconds) + ?UNIX_EPOCH_IN_GREGORIAN.

-spec unix_timestamp_to_gregorian_seconds(integer() | string() | binary()) -> integer().
unix_timestamp_to_gregorian_seconds(UnixTimestamp) ->
    ?UNIX_EPOCH_IN_GREGORIAN + (kz_term:to_integer(UnixTimestamp) div ?MILLISECONDS_IN_SECOND).

-spec to_gregorian_seconds(datetime(), kz_term:ne_binary()) -> gregorian_seconds().
-ifdef(TEST).
to_gregorian_seconds({{_,_,_},{_,_,_}}=Datetime, ?NE_BINARY=FromTimezone) ->
    calendar:datetime_to_gregorian_seconds(
      localtime:local_to_local(Datetime, binary_to_list(FromTimezone), "Etc/UTC")).
-else.
to_gregorian_seconds({{_,_,_},{_,_,_}}=Datetime, ?NE_BINARY=FromTimezone) ->
    {{_,_,_}, {_,_,_}} = UTC = localtime:local_to_local(Datetime, binary_to_list(FromTimezone), "Etc/UTC"),
    calendar:datetime_to_gregorian_seconds(UTC).
-endif.

-spec pretty_print_datetime(datetime() | integer()) -> kz_term:ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w:~2..0w:~2..0w"
                                  ,[Y, Mo, D, H, Mi, S]
                                  )).

-spec pretty_print_datetime(datetime() | gregorian_seconds(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
pretty_print_datetime(DateTimeOrTS, 'undefined') ->
    pretty_print_datetime(DateTimeOrTS);
pretty_print_datetime(DateTimeOrTS, Timezone) ->
    Prettified = pretty_print_datetime(
                   adjust_utc_timestamp(DateTimeOrTS, Timezone)
                  ),
    TzName = tz_name(DateTimeOrTS, Timezone),
    <<Prettified/binary, "_", TzName/binary>>.

-spec rfc1036(calendar:datetime() | gregorian_seconds()) -> kz_term:ne_binary().
rfc1036(DateTime) ->
    rfc1036(DateTime, <<"GMT">>).

-spec rfc1036(calendar:datetime() | gregorian_seconds(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
rfc1036(Thing, 'undefined') ->
    rfc1036(Thing, <<"GMT">>);
rfc1036({Date = {Y, Mo, D}, {H, Mi, S}}, TZ) ->
    Wday = calendar:day_of_the_week(Date),
    <<(weekday(Wday))/binary, ", ",
      (kz_binary:pad_left(kz_term:to_binary(D), 2, <<"0">>))/binary, " ",
      (month(Mo))/binary, " ",
      (kz_term:to_binary(Y))/binary, " ",
      (kz_binary:pad_left(kz_term:to_binary(H), 2, <<"0">>))/binary, ":",
      (kz_binary:pad_left(kz_term:to_binary(Mi), 2, <<"0">>))/binary, ":",
      (kz_binary:pad_left(kz_term:to_binary(S), 2, <<"0">>))/binary,
      " ", TZ/binary
    >>;
rfc1036(Timestamp, TZ) when is_integer(Timestamp) ->
    rfc1036(calendar:gregorian_seconds_to_datetime(Timestamp), TZ).

-spec rfc2822(calendar:datetime() | gregorian_seconds()) -> kz_term:ne_binary().
rfc2822(Timestamp) ->
    rfc2822(Timestamp, <<"GMT">>).

-spec rfc2822(calendar:datetime() | gregorian_seconds(), kz_term:ne_binary()) -> kz_term:ne_binary().
rfc2822(Timestamp, TZ) when is_integer(Timestamp) ->
    rfc2822(calendar:gregorian_seconds_to_datetime(Timestamp), TZ);
rfc2822(Datetime = {Date={Y, Mo, D}, {H, Mi, S}}, TZ) ->
    Wday = calendar:day_of_the_week(Date),
    <<(weekday(Wday))/binary, ", ",
      (kz_binary:pad_left(kz_term:to_binary(D), 2, <<"0">>))/binary, " ",
      (month(Mo))/binary, " ",
      (kz_term:to_binary(Y))/binary, " ",
      (kz_binary:pad_left(kz_term:to_binary(H), 2, <<"0">>))/binary, ":",
      (kz_binary:pad_left(kz_term:to_binary(Mi), 2, <<"0">>))/binary, ":",
      (kz_binary:pad_left(kz_term:to_binary(S), 2, <<"0">>))/binary,
      " ", (tz_offset(Datetime, TZ))/binary
    >>.

-spec tz_offset(calendar:datetime(), kz_term:ne_binary()) -> kz_term:ne_binary().
tz_offset(Datetime, <<FromTz/binary>>) ->
    case localtime:tz_shift(Datetime, kz_term:to_list(FromTz), "UTC") of
        0 -> <<"+0000">>;
        {'error', 'unknown_tz'} -> <<"+0000">>;
        {Sign, H, M} ->
            list_to_binary([kz_term:to_binary(Sign)
                           ,kz_binary:pad_left(kz_term:to_binary(H), 2, <<"0">>)
                           ,kz_binary:pad_left(kz_term:to_binary(M), 2, <<"0">>)
                           ])
    end.

%%------------------------------------------------------------------------------
%% @doc Format time part of ISO 8601.
%% @end
%%------------------------------------------------------------------------------
-spec iso8601_time(calendar:time() | calendar:datetime() | gregorian_seconds()) -> kz_term:ne_binary().
iso8601_time({Hours, Mins, Secs}) ->
    H = kz_binary:pad_left(kz_term:to_binary(Hours), 2, <<"0">>),
    M = kz_binary:pad_left(kz_term:to_binary(Mins), 2, <<"0">>),
    S = kz_binary:pad_left(kz_term:to_binary(Secs), 2, <<"0">>),

    <<H/binary, ":", M/binary, ":", S/binary>>;
iso8601_time({{_,_,_}, {_H, _M, _S}=Time}) ->
    iso8601_time(Time);
iso8601_time(Timestamp) when is_integer(Timestamp) ->
    iso8601_time(calendar:gregorian_seconds_to_datetime(Timestamp)).

%%------------------------------------------------------------------------------
%% @doc Format date or datetime or Gregorian timestamp (all in UTC)
%% according to ISO 8601.
%%
%% It assumes the input is in UTC.
%% @throws {error, invalid_offset | unknown_tz}
%% @end
%%------------------------------------------------------------------------------
-spec iso8601(calendar:datetime() | date() | gregorian_seconds()) -> kz_term:ne_binary().
iso8601({_Y,_M,_D}=Date) ->
    kz_date:to_iso8601_extended(Date);
iso8601(Timestamp) ->
    iso8601(Timestamp, <<"UTC">>).

%%------------------------------------------------------------------------------
%% @doc Format Gregorian timestamp or datetime (all un UTC) to the local Timezone
%% according to ISO 8601.
%%
%% Timezone can be:
%%
%% * An offset in seconds ({@link integer()}), negative or positive.
%% * A {@link kz_term:ne_binary()} in format of `<<"+HH:MM">>' or `<<"-HH:MM">>'.
%% * The name of the timezone {@link kz_term:ne_binary()} like `America/Los_Angeles'.
%% @throws {error, invalid_offset | unknown_tz}
%% @end
%%------------------------------------------------------------------------------
-spec iso8601(datetime() | gregorian_seconds(), kz_term:api_ne_binary() | integer()) -> kz_term:ne_binary().
iso8601(Thing, 'undefined') ->
    iso8601(Thing, <<"UTC">>);
iso8601({{_, _, _}, {_, _, _}}=Datetime, Timezone) ->
    iso8601(calendar:datetime_to_gregorian_seconds(Datetime), Timezone);
iso8601(Timestamp, Timezone) when is_integer(Timestamp) ->
    AdjustedTimestamp = adjust_utc_timestamp(Timestamp, Timezone),
    format_iso8601(AdjustedTimestamp
                  ,format_iso8601_offset(AdjustedTimestamp - Timestamp)
                  ).

%%------------------------------------------------------------------------------
%% @doc Format timezone offset for ISO 8601.
%% @end
%%------------------------------------------------------------------------------
-spec format_iso8601_offset(integer()) -> kz_term:ne_binary().
format_iso8601_offset(Offset) when Offset == 0 ->
    <<"Z">>;
format_iso8601_offset(Offset) when Offset > 0 ->
    {Hours, Minutes, _Seconds} = calendar:seconds_to_time(Offset),
    format_iso8601_offset(<<"+">>, Hours, Minutes);
format_iso8601_offset(Offset) when Offset < 0 ->
    {Hours, Minutes, _Seconds} = calendar:seconds_to_time(-1 * Offset),
    format_iso8601_offset(<<"-">>, Hours, Minutes).

-spec format_iso8601_offset(kz_term:ne_binary(), hour(), minute()) -> kz_term:ne_binary().
format_iso8601_offset(Sign, Hour0, Minute0) ->
    Hour1 = kz_binary:pad_left(kz_term:to_binary(Hour0), 2, <<"0">>),
    Minute1 = kz_binary:pad_left(kz_term:to_binary(Minute0), 2, <<"0">>),
    <<Sign/binary, Hour1/binary, ":", Minute1/binary>>.

%%------------------------------------------------------------------------------
%% @doc Format Gregorian timestamp or datetime to ISO 8601.
%% @end
%%------------------------------------------------------------------------------
-spec format_iso8601(gregorian_seconds() | datetime(), kz_term:ne_binary()) -> kz_term:ne_binary().
format_iso8601({{_Y,_Mo,_D}=Date, {_H, _Mi, _S}=Time}, TimeOffset) ->
    <<(kz_date:to_iso8601_extended(Date))/binary, "T", (iso8601_time(Time))/binary, TimeOffset/binary>>;
format_iso8601(Timestamp, TimeOffset) ->
    format_iso8601(calendar:gregorian_seconds_to_datetime(Timestamp), TimeOffset).

%%------------------------------------------------------------------------------
%% @doc Parse time part of ISO 8601.
%% @throws {error, invalid_time}
%% @end
%%------------------------------------------------------------------------------
-spec from_iso8601_time(binary()) -> {time(), integer()}.
%% HH:MM:SS
from_iso8601_time(<<Hour:2/binary, ":", Minute:2/binary, ":", Second:2/binary>>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<>>);
%% HHMMSS
from_iso8601_time(<<Hour:2/binary, Minute:2/binary, Second:2/binary>>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<>>);

%% HH:MM:SSZ
from_iso8601_time(<<Hour:2/binary, ":", Minute:2/binary, ":", Second:2/binary, "Z">>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<"Z">>);
%% HHMMSSZ
from_iso8601_time(<<Hour:2/binary, Minute:2/binary, Second:2/binary, "Z">>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<"Z">>);

%% HH:MM:SS+
from_iso8601_time(<<Hour:2/binary, ":", Minute:2/binary, ":", Second:2/binary, "+", TzOffset/binary>>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<"+", TzOffset/binary>>);
%% HH:MM:SS-
from_iso8601_time(<<Hour:2/binary, ":", Minute:2/binary, ":", Second:2/binary, "-", TzOffset/binary>>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<"-", TzOffset/binary>>);

%% HHMMSS+
from_iso8601_time(<<Hour:2/binary, Minute:2/binary, Second:2/binary, "+", TzOffset/binary>>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<"+", TzOffset/binary>>);
%% HHMMSS-
from_iso8601_time(<<Hour:2/binary, Minute:2/binary, Second:2/binary, "-", TzOffset/binary>>) ->
    iso8601_offset(from_binary_to_time(Hour, Minute, Second), <<"-", TzOffset/binary>>);

from_iso8601_time(<<>>) ->
    iso8601_offset({0, 0, 0}, <<"Z">>);
from_iso8601_time(_NotValid) ->
    throw({'error', 'invalid_time'}).

-spec from_binary_to_time(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> time().
from_binary_to_time(Hour, Minute, Second) ->
    {cast_integer(Hour, 'invalid_time'), cast_integer(Minute, 'invalid_time'), cast_integer(Second, 'invalid_time')}.

%%------------------------------------------------------------------------------
%% @doc Parse timezone offset part of ISO 8601.
%%
%% Assumes UTC if timezone offset part is missing.
%% @throws {error, invalid_offset}
%% @end
%%------------------------------------------------------------------------------
-spec iso8601_offset(time(), binary()) -> {time(), integer()}.
iso8601_offset(Time, <<>>) ->
    {Time, 0};
%% Z
iso8601_offset(Time, <<"Z">>) ->
    {Time, 0};
%% +HH:MM
iso8601_offset(Time, <<"+", Hour:2/binary, ":", Minute:2/binary>>) ->
    Offset = {cast_integer(Hour, 'invalid_offset'), cast_integer(Minute, 'invalid_offset'), 0},
    {Time, -1 * calendar:time_to_seconds(Offset)};
%% -HH:MM
iso8601_offset(Time, <<"-", Hour:2/binary, ":", Minute:2/binary>>) ->
    Offset = {cast_integer(Hour, 'invalid_offset'), cast_integer(Minute, 'invalid_offset'), 0},
    {Time, calendar:time_to_seconds(Offset)};
%% +HHMM
iso8601_offset(Time, <<"+", Hour:2/binary, Minute:2/binary>>) ->
    Offset = {cast_integer(Hour, 'invalid_offset'), cast_integer(Minute, 'invalid_offset'), 0},
    {Time, -1 * calendar:time_to_seconds(Offset)};
%% -HHMM
iso8601_offset(Time, <<"-", Hour:2/binary, Minute:2/binary>>) ->
    Offset = {cast_integer(Hour, 'invalid_offset'), cast_integer(Minute, 'invalid_offset'), 0},
    {Time, calendar:time_to_seconds(Offset)};
%% +HH
iso8601_offset(Time, <<"+", Hour:2/binary>>) ->
    Offset = {cast_integer(Hour, 'invalid_offset'), 0, 0},
    {Time, -1 * calendar:time_to_seconds(Offset)};
%% -HH
iso8601_offset(Time, <<"-", Hour:2/binary>>) ->
    Offset = {cast_integer(Hour, 'invalid_offset'), 0, 0},
    {Time, calendar:time_to_seconds(Offset)};
iso8601_offset(_Time, _NotValid) ->
    throw({'error', 'invalid_offset'}).

%%------------------------------------------------------------------------------
%% @doc Parse date part of ISO 8601.
%% @throws {error, invalid_date}
%% @end
%%------------------------------------------------------------------------------
-spec from_iso8601_date(kz_term:ne_binary()) -> date().
from_iso8601_date(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>) ->
    from_binary_to_date(Year, Month, Day);
from_iso8601_date(<<Year:4/binary, Month:2/binary, Day:2/binary>>) ->
    from_binary_to_date(Year, Month, Day);
from_iso8601_date(_NotValid) ->
    throw({'error', 'invalid_date'}).

-spec from_binary_to_date(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> date().
from_binary_to_date(Year, Month, Day) ->
    {cast_integer(Year, 'invalid_date'), cast_integer(Month, 'invalid_date'), cast_integer(Day, 'invalid_date')}.

%%------------------------------------------------------------------------------
%% @doc Parse date time formatted in ISO 8601 and convert it to UTC if the input
%% is in another timezone.
%%
%% This is very raw parser for ISO 8601, and only supports the date format and
%% combined(date and time) format. In combined format separators are optional.
%%
%% UTC timezone will be used if time offset part is missing!
%% @throws {error, invalid_offset | invalid_time | invalid_date}
%% @end
%%------------------------------------------------------------------------------
-spec from_iso8601(kz_term:ne_binary()) -> datetime().
from_iso8601(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, "T", Time/binary>>) ->
    from_iso8601(from_binary_to_date(Year, Month, Day), from_iso8601_time(Time));
from_iso8601(<<Year:4/binary, Month:2/binary, Day:2/binary, "T", Time/binary>>) ->
    from_iso8601(from_binary_to_date(Year, Month, Day), from_iso8601_time(Time));
from_iso8601(MaybeDate) ->
    from_iso8601_date(MaybeDate).

%%------------------------------------------------------------------------------
%% @doc Adjust date and time of parsed ISO 8601 by timezone offset.
%% @end
%%------------------------------------------------------------------------------
-spec from_iso8601(date(), {time(), integer()}) -> datetime().
from_iso8601(Date, {Time, Offset}) -> adjust_utc_datetime({Date, Time}, Offset).

%%------------------------------------------------------------------------------
%% @doc Return 'true' if the input is an ISO 8601 datetime binary.
%% @end
%%------------------------------------------------------------------------------
-spec is_iso8601(any()) -> boolean().
is_iso8601(MaybeISO8601) ->
    try from_iso8601(trim_iso8601_ms(MaybeISO8601)) of
        _ -> 'true'
    catch
        'throw':{'error', _} -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Remove the milliseconds suffix from ISO8601 datetimes of the formats:
%%
%% `2019-10-09T18:36:13.000Z'
%% `20191009T183613.000Z'
%% @end
%%------------------------------------------------------------------------------
-spec trim_iso8601_ms(kz_term:ne_binary()) -> kz_term:ne_binary().
trim_iso8601_ms(<<NonMS:15/binary, ".", _:3/binary, TzOffset/binary>>) ->
    <<NonMS/binary, TzOffset/binary>>;
trim_iso8601_ms(<<NonMS:19/binary, ".", _:3/binary, TzOffset/binary>>) ->
    <<NonMS/binary, TzOffset/binary>>;
trim_iso8601_ms(ISO8601) -> ISO8601.

%%------------------------------------------------------------------------------
%% @doc Add the milliseconds suffix to ISO8601 datetimes of the formats:
%%
%% `2019-10-09T18:36:13Z'
%% `20191009T183613Z'
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_iso8601_ms_suffix(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_add_iso8601_ms_suffix(<<_:19/binary, ".", _:3/binary, _/binary>>=ISO8601) ->
    ISO8601;
maybe_add_iso8601_ms_suffix(<<_:15/binary, ".", _:3/binary, _/binary>>=ISO8601) ->
    ISO8601;
maybe_add_iso8601_ms_suffix(<<Date:10/binary, "T", Time:8/binary, TzOffset/binary>>) ->
    <<Date/binary, "T", Time/binary, ".000", TzOffset/binary>>;
maybe_add_iso8601_ms_suffix(<<Date:8/binary, "T", Time:6/binary, TzOffset/binary>>) ->
    <<Date/binary, "T", Time/binary, ".000", TzOffset/binary>>.

%%------------------------------------------------------------------------------
%% @doc Apply the adjustment to the UTC Timestamp.
%%
%% To convert an UTC timezone to a local timezone by means of adding an offset
%% or specifying the timezone name.
%%
%% Adjustment can be:
%%
%% * An offset in seconds ({@link integer()}), negative or positive.
%% * A {@link kz_term:ne_binary()} in format of `<<"+HH:MM">>' or `<<"-HH:MM">>'.
%% * The name of the timezone {@link kz_term:ne_binary()} like `America/Los_Angeles'.
%% @throws {error, invalid_offset | unknown_tz}
%% @end
%%------------------------------------------------------------------------------
-spec adjust_utc_timestamp(datetime() | gregorian_seconds(), integer() | kz_term:api_ne_binary() | integer()) -> gregorian_seconds().
adjust_utc_timestamp({{_, _, _}, {_, _, _}}=DateTime, 'undefined') ->
    calendar:datetime_to_gregorian_seconds(DateTime);
adjust_utc_timestamp(Timestamp, 'undefined') -> Timestamp;
adjust_utc_timestamp({{_, _, _}, {_, _, _}}=DateTime, Timezone) ->
    adjust_utc_timestamp(calendar:datetime_to_gregorian_seconds(DateTime), Timezone);
adjust_utc_timestamp(Timestamp, <<"UTC">>) -> Timestamp;
adjust_utc_timestamp(Timestamp, <<"Etc/UTC">>) -> Timestamp;
adjust_utc_timestamp(Timestamp, <<"GMT">>) -> Timestamp;
adjust_utc_timestamp(Timestamp, <<"+00:00">>) -> Timestamp;
adjust_utc_timestamp(Timestamp, 0) -> Timestamp;
adjust_utc_timestamp(Timestamp, Offset) when is_integer(Offset) ->
    Timestamp + Offset;
adjust_utc_timestamp(Timestamp, <<"+", Hour:2/binary, ":", Minute:2/binary>>) ->
    OffsetSeconds = calendar:time_to_seconds({cast_integer(Hour, 'invalid_offset'), cast_integer(Minute, 'invalid_offset'), 0}),
    Timestamp + OffsetSeconds;
adjust_utc_timestamp(Timestamp, <<"-", Hour:2/binary, ":", Minute:2/binary>>) ->
    OffsetSeconds = calendar:time_to_seconds({cast_integer(Hour, 'invalid_offset'), cast_integer(Minute, 'invalid_offset'), 0}),
    Timestamp - OffsetSeconds;
adjust_utc_timestamp(_Timestamp, <<"+", _/binary>>) ->
    throw({'error', 'invalid_offset'});
adjust_utc_timestamp(_Timestamp, <<"-", _/binary>>) ->
    throw({'error', 'invalid_offset'});
adjust_utc_timestamp(Timestamp, <<Timezone/binary>>) when is_integer(Timestamp) ->
    try localtime:utc_to_local(calendar:gregorian_seconds_to_datetime(Timestamp), kz_term:to_list(Timezone)) of
        {{_, _, _}, {_, _, _}} = Datetime ->
            calendar:datetime_to_gregorian_seconds(Datetime);
        [LocalDatetime, _LocalDstDatetime] ->
            calendar:datetime_to_gregorian_seconds(LocalDatetime);
        {'error', 'unknown_tz'}=Error ->
            throw(Error)
    catch
        _:_ ->
            throw({'error', 'unknown_tz'})
    end.

%%------------------------------------------------------------------------------
%% @doc Apply the adjustment to the Datetime.
%% @throws {error, invalid_offset | unknown_tz}
%% @end
%%------------------------------------------------------------------------------
-spec adjust_utc_datetime(datetime() | gregorian_seconds(), integer() | kz_term:api_ne_binary()) -> datetime().
adjust_utc_datetime(Timestamp, 'undefined') when is_integer(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp);
adjust_utc_datetime(Datetime, 'undefined') ->
    Datetime;
adjust_utc_datetime(DateTimeOrTS, Adjustment) ->
    Adjusted = adjust_utc_timestamp(DateTimeOrTS, Adjustment),
    calendar:gregorian_seconds_to_datetime(Adjusted).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec tz_name(datetime() | gregorian_seconds(), kz_term:api_ne_binary()) -> kz_term:ne_binary().
tz_name(_, 'undefined') ->
    <<"UTC">>;
tz_name({{_, _, _}, {_, _, _}}=DateTime, Timezone) ->
    try localtime:tz_name(DateTime, kz_term:to_list(Timezone)) of
        'unable_to_detect' -> Timezone;
        {'error', 'unknown_tz'} -> Timezone;
        {{StdAbbr, _}, {_, _}} -> kz_term:to_binary(StdAbbr);
        {Abbr, _} -> kz_term:to_binary(Abbr)
    catch _:_ -> Timezone
    end;
tz_name(Timestamp, Timezone) when is_integer(Timestamp) ->
    tz_name(calendar:gregorian_seconds_to_datetime(Timestamp), Timezone).

%%------------------------------------------------------------------------------
%% @doc
%% @throws {error, any()}
%% @end
%%------------------------------------------------------------------------------
-spec cast_integer(any(), atom()) -> integer().
cast_integer(Value, Exception) ->
    try kz_term:to_integer(Value)
    catch
        'error':'badarg' ->
            throw({'error', Exception})
    end.

%% borrowed from cow_date.erl
-spec weekday(daynum()) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(month()) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

-spec pretty_print_elapsed_s(non_neg_integer()) -> kz_term:ne_binary().
pretty_print_elapsed_s(0) -> <<"0s">>;
pretty_print_elapsed_s(Seconds) when is_integer(Seconds), Seconds > 0 ->
    iolist_to_binary(unitfy_seconds(Seconds)).

-spec unitfy_seconds(non_neg_integer()) -> iolist().
unitfy_seconds(0) -> "";
unitfy_seconds(Seconds) when Seconds < ?SECONDS_IN_MINUTE ->
    [kz_term:to_binary(Seconds), "s"];
unitfy_seconds(Seconds) when Seconds < ?SECONDS_IN_HOUR ->
    M = Seconds div ?SECONDS_IN_MINUTE,
    [kz_term:to_binary(M), "m", unitfy_seconds(Seconds - (M * ?SECONDS_IN_MINUTE))];
unitfy_seconds(Seconds) when Seconds < ?SECONDS_IN_DAY ->
    H = Seconds div ?SECONDS_IN_HOUR,
    [kz_term:to_binary(H), "h", unitfy_seconds(Seconds - (H * ?SECONDS_IN_HOUR))];
unitfy_seconds(Seconds) ->
    D = Seconds div ?SECONDS_IN_DAY,
    [kz_term:to_binary(D), "d", unitfy_seconds(Seconds - (D * ?SECONDS_IN_DAY))].

-spec decr_timeout(timeout(), now() | gregorian_seconds() | start_time()) -> timeout().
decr_timeout('infinity', _) -> 'infinity';
decr_timeout(Timeout, {_Mega, _S, _Micro}=Start) when is_integer(Timeout) ->
    decr_timeout(Timeout, Start, now());
decr_timeout(Timeout, StartS) when is_integer(Timeout),
                                   is_integer(StartS),
                                   ?UNIX_EPOCH_IN_GREGORIAN < StartS ->
    decr_timeout(Timeout, StartS, now_s());
decr_timeout(Timeout, {'start_time', _}=StartTime) ->
    decr_timeout(Timeout, StartTime, start_time()).

-spec decr_timeout(timeout(), now() | gregorian_seconds() | start_time(), now() | gregorian_seconds() | start_time()) -> timeout().
decr_timeout('infinity', _Start, _Future) -> 'infinity';
decr_timeout(Timeout, Start, {_Mega, _S, _Micro}=Now) ->
    decr_timeout_elapsed(Timeout, elapsed_s(Start, Now));
decr_timeout(Timeout, StartS, Now) when is_integer(Timeout),
                                        is_integer(StartS),
                                        ?UNIX_EPOCH_IN_GREGORIAN < StartS,
                                        is_integer(Now),
                                        ?UNIX_EPOCH_IN_GREGORIAN < Now ->
    decr_timeout_elapsed(Timeout, elapsed_s(StartS, Now));
decr_timeout(Timeout, {'start_time', _}=Start, {'start_time', _}=End) ->
    decr_timeout_elapsed(Timeout, elapsed_s(Start, End)).

-spec decr_timeout_elapsed(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
decr_timeout_elapsed(Timeout, Elapsed) ->
    Diff = Timeout - Elapsed,
    case Diff < 0 of
        'true' -> 0;
        'false' -> Diff
    end.

-spec microseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
microseconds_to_seconds(Microseconds) -> kz_term:to_integer(Microseconds) div ?MICROSECONDS_IN_SECOND.

-spec milliseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
milliseconds_to_seconds(Milliseconds) -> kz_term:to_integer(Milliseconds) div ?MILLISECONDS_IN_SECOND.

-spec elapsed_s(now() | pos_integer() | start_time()) -> non_neg_integer().
elapsed_s({_,_,_}=Start) -> elapsed_s(Start, now());
elapsed_s(Start) when is_integer(Start) -> elapsed_s(Start, now_s());
elapsed_s({'start_time', _}=Start) -> elapsed_s(Start, start_time()).

-spec elapsed_ms(now() | pos_integer() | start_time()) -> non_neg_integer().
elapsed_ms({_,_,_}=Start) -> elapsed_ms(Start, now());
elapsed_ms(Start) when is_integer(Start) -> elapsed_ms(Start, now_ms());
elapsed_ms({'start_time', _}=Start) -> elapsed_ms(Start, start_time()).

-spec elapsed_us(now() | pos_integer() | start_time()) -> non_neg_integer().
elapsed_us({_,_,_}=Start) -> elapsed_us(Start, now());
elapsed_us(Start) when is_integer(Start) -> elapsed_us(Start, now_us());
elapsed_us({'start_time', _}=Start) -> elapsed_us(Start, start_time()).

-spec elapsed_s(now() | pos_integer() | start_time(), now() | pos_integer() | start_time()) -> non_neg_integer().
elapsed_s({'start_time', Start}, {'start_time', End}) ->
    erlang:convert_time_unit(End-Start, 'native', 'second');
elapsed_s({_,_,_}=Start, {_,_,_}=Now) ->
    timer:now_diff(Now, Start) div ?MICROSECONDS_IN_SECOND;
elapsed_s({_,_,_}=Start, Now) -> elapsed_s(now_s(Start), Now);
elapsed_s(Start, {_,_,_}=Now) -> elapsed_s(Start, now_s(Now));
elapsed_s(Start, Now)
  when is_integer(Start),
       is_integer(Now) ->
    Now - Start.

-spec elapsed_ms(now() | pos_integer() | start_time(), now() | pos_integer() | start_time()) -> non_neg_integer().
elapsed_ms({'start_time', Start}, {'start_time', End}) ->
    erlang:convert_time_unit(End-Start, 'native', 'millisecond');
elapsed_ms({_,_,_}=Start, {_,_,_}=Now) ->
    timer:now_diff(Now, Start) div ?MILLISECONDS_IN_SECOND;
elapsed_ms({_,_,_}=Start, Now) -> elapsed_ms(now_ms(Start), Now);
elapsed_ms(Start, {_,_,_}=Now) -> elapsed_ms(Start, now_ms(Now));
elapsed_ms(Start, Now)
  when is_integer(Start),
       is_integer(Now),
       Start > (?UNIX_EPOCH_IN_GREGORIAN * ?MILLISECONDS_IN_SECOND),
       Now > (?UNIX_EPOCH_IN_GREGORIAN * ?MILLISECONDS_IN_SECOND) ->
    Now - Start;
elapsed_ms(Start, Now)
  when is_integer(Start),
       is_integer(Now) ->
    (Now - Start) * ?MILLISECONDS_IN_SECOND.

-spec elapsed_us(now() | pos_integer() | start_time(), now() | pos_integer() | start_time()) -> non_neg_integer().
elapsed_us({'start_time', Start}, {'start_time', End}) ->
    erlang:convert_time_unit(End-Start, 'native', 'microsecond');
elapsed_us({_,_,_}=Start, {_,_,_}=Now) ->
    timer:now_diff(Now, Start);
elapsed_us({_,_,_}=Start, Now) -> elapsed_us(now_us(Start), Now);
elapsed_us(Start, {_,_,_}=Now) -> elapsed_us(Start, now_us(Now));
elapsed_us(Start, Now)
  when is_integer(Start),
       is_integer(Now),
       Start > (?UNIX_EPOCH_IN_GREGORIAN * ?MICROSECONDS_IN_SECOND),
       Now > (?UNIX_EPOCH_IN_GREGORIAN * ?MICROSECONDS_IN_SECOND) ->
    Now - Start;
elapsed_us(Start, Now)
  when is_integer(Start),
       is_integer(Now) ->
    (Now - Start) * ?MICROSECONDS_IN_SECOND.

-spec now() -> now().
now() -> os:timestamp().

-spec now_s() -> gregorian_seconds().
now_s() ->  erlang:system_time('seconds') + ?UNIX_EPOCH_IN_GREGORIAN.

-spec now_ms() -> pos_integer().
now_ms() -> erlang:system_time('milli_seconds') + (?UNIX_EPOCH_IN_GREGORIAN * ?MILLISECONDS_IN_SECOND).

-spec now_us() -> pos_integer().
now_us() -> erlang:system_time('micro_seconds') + (?UNIX_EPOCH_IN_GREGORIAN * ?MICROSECONDS_IN_SECOND).

-spec now_us(now()) -> pos_integer().
now_us({MegaSecs, Secs, MicroSecs}) ->
    unix_us_to_gregorian_us((MegaSecs*?MICROSECONDS_IN_SECOND + Secs)*?MICROSECONDS_IN_SECOND + MicroSecs).

-spec now_ms(now()) -> pos_integer().
now_ms({_,_,_}=Now) ->
    now_us(Now) div ?MILLISECONDS_IN_SECOND.

-spec now_s(now()) -> gregorian_seconds().
now_s({_,_,_}=Now) ->
    now_us(Now) div ?MICROSECONDS_IN_SECOND.

unix_us_to_gregorian_us(UnixUS) ->
    UnixUS + (?UNIX_EPOCH_IN_GREGORIAN * ?MICROSECONDS_IN_SECOND).

-spec format_date() -> binary().
format_date() ->
    format_date(now_s()).

-spec format_date(gregorian_seconds()) -> binary().
format_date(Timestamp) when is_integer(Timestamp) ->
    {{Y,M,D}, _ } = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([kz_term:to_binary(Y), "-", kz_term:to_binary(M), "-", kz_term:to_binary(D)]).

-spec format_time() -> binary().
format_time() ->
    format_time(now_s()).

-spec format_time(gregorian_seconds()) -> binary().
format_time(Timestamp) when is_integer(Timestamp) ->
    { _, {H,I,S}} = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([kz_term:to_binary(H), ":", kz_term:to_binary(I), ":", kz_term:to_binary(S)]).

-spec format_datetime() -> binary().
format_datetime() ->
    format_datetime(now_s()).

-spec format_datetime(gregorian_seconds()) -> binary().
format_datetime(Timestamp) when is_integer(Timestamp) ->
    list_to_binary([format_date(Timestamp), " ", format_time(Timestamp)]).

-spec start_time() -> start_time().
start_time() ->
    {'start_time', erlang:monotonic_time()}.
