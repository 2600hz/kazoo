%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Various utilities to work with time.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_time).

-export([current_tstamp/0, current_unix_tstamp/0
        ,gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1
        ,unix_timestamp_to_gregorian_seconds/1
        ,to_gregorian_seconds/2
        ,pretty_print_datetime/1
        ,rfc1036/1, rfc1036/2
        ,iso8601/1, iso8601_time/1
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
-type iso_week() :: calendar:yearweeknum(). %%{year(), weeknum()}.
-type gregorian_seconds() :: pos_integer().
-type unix_seconds() :: pos_integer().
-type api_seconds() :: 'undefined' | gregorian_seconds().
-type ordinal() :: kz_term:ne_binary(). % "every" | "first" | "second" | "third" | "fourth" | "fifth" | "last".

-export_type([api_seconds/0
             ,date/0
             ,datetime/0
             ,day/0
             ,daynum/0
             ,gregorian_seconds/0
             ,hour/0
             ,iso_week/0
             ,minute/0
             ,month/0
             ,now/0
             ,ordinal/0
             ,second/0
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

-spec to_gregorian_seconds(datetime(), kz_term:api_ne_binary()) -> gregorian_seconds().
-ifdef(TEST).
to_gregorian_seconds(Datetime, 'undefined') ->
    to_gregorian_seconds(Datetime, <<"America/Los_Angeles">>);
to_gregorian_seconds({{_,_,_},{_,_,_}}=Datetime, ?NE_BINARY=FromTimezone) ->
    calendar:datetime_to_gregorian_seconds(
      localtime:local_to_local(Datetime, binary_to_list(FromTimezone), "Etc/UTC")).
-else.
to_gregorian_seconds(Datetime, 'undefined') ->
    to_gregorian_seconds(Datetime, kzd_accounts:default_timezone());
to_gregorian_seconds({{_,_,_},{_,_,_}}=Datetime, ?NE_BINARY=FromTimezone) ->
    calendar:datetime_to_gregorian_seconds(
      localtime:local_to_local(Datetime, binary_to_list(FromTimezone), "Etc/UTC")
     ).
-endif.

-spec pretty_print_datetime(datetime() | integer()) -> kz_term:ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w-~2..0w-~2..0w"
                                  ,[Y, Mo, D, H, Mi, S]
                                  )).

-spec rfc1036(calendar:datetime() | gregorian_seconds()) -> kz_term:ne_binary().
rfc1036(DateTime) ->
    rfc1036(DateTime, <<"GMT">>).

-spec rfc1036(calendar:datetime() | gregorian_seconds(), kz_term:ne_binary()) -> kz_term:ne_binary().
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

-spec iso8601(calendar:datetime() | gregorian_seconds()) -> kz_term:ne_binary().
iso8601({_Y,_M,_D}=Date) ->
    kz_date:to_iso8601_extended(Date);
iso8601({{_Y,_M,_D}=Date, {0, 0, 0}}) ->
    kz_date:to_iso8601_extended(Date);
iso8601({{_Y,_Mo,_D}=Date, {_H, _Mi, _S}=Time}) ->
    <<(kz_date:to_iso8601_extended(Date))/binary, "T", (iso8601_time(Time))/binary>>;
iso8601(Timestamp) when is_integer(Timestamp) ->
    iso8601(calendar:gregorian_seconds_to_datetime(Timestamp)).

%% borrowed from cow_date.erl
-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
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
pretty_print_elapsed_s(Seconds) ->
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

-spec decr_timeout(timeout(), now() | gregorian_seconds()) -> timeout().
decr_timeout('infinity', _) -> 'infinity';
decr_timeout(Timeout, {_Mega, _S, _Micro}=Start) when is_integer(Timeout) ->
    decr_timeout(Timeout, Start, now());
decr_timeout(Timeout, StartS) when is_integer(Timeout),
                                   is_integer(StartS),
                                   ?UNIX_EPOCH_IN_GREGORIAN < StartS ->
    decr_timeout(Timeout, StartS, now_s()).

-spec decr_timeout(timeout(), now() | gregorian_seconds(), now() | gregorian_seconds()) -> timeout().
decr_timeout('infinity', _Start, _Future) -> 'infinity';
decr_timeout(Timeout, Start, {_Mega, _S, _Micro}=Now) ->
    decr_timeout_elapsed(Timeout, elapsed_s(Start, Now));
decr_timeout(Timeout, StartS, Now) when is_integer(Timeout),
                                        is_integer(StartS),
                                        ?UNIX_EPOCH_IN_GREGORIAN < StartS,
                                        is_integer(Now),
                                        ?UNIX_EPOCH_IN_GREGORIAN < Now ->
    decr_timeout_elapsed(Timeout, elapsed_s(StartS, Now)).

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

-spec elapsed_s(now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start) -> elapsed_s(Start, now());
elapsed_s(Start) when is_integer(Start) -> elapsed_s(Start, now_s()).

-spec elapsed_ms(now() | pos_integer()) -> pos_integer().
elapsed_ms({_,_,_}=Start) -> elapsed_ms(Start, now());
elapsed_ms(Start) when is_integer(Start) -> elapsed_ms(Start, now_ms()).

-spec elapsed_us(now() | pos_integer()) -> pos_integer().
elapsed_us({_,_,_}=Start) -> elapsed_us(Start, now());
elapsed_us(Start) when is_integer(Start) -> elapsed_us(Start, now_us()).

-spec elapsed_s(now() | pos_integer(), now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start, {_,_,_}=Now) ->
    timer:now_diff(Now, Start) div ?MICROSECONDS_IN_SECOND;
elapsed_s({_,_,_}=Start, Now) -> elapsed_s(now_s(Start), Now);
elapsed_s(Start, {_,_,_}=Now) -> elapsed_s(Start, now_s(Now));
elapsed_s(Start, Now)
  when is_integer(Start),
       is_integer(Now) ->
    Now - Start.

-spec elapsed_ms(now() | pos_integer(), now() | pos_integer()) -> pos_integer().
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

-spec elapsed_us(now() | pos_integer(), now() | pos_integer()) -> pos_integer().
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
