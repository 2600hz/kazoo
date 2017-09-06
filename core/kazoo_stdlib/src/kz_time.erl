%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_time).

-export([current_tstamp/0, current_unix_tstamp/0
        ,gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1
        ,unix_timestamp_to_gregorian_seconds/1
        ,to_gregorian_seconds/2
        ,pretty_print_datetime/1
        ,rfc1036/1, rfc1036/2
        ,iso8601/1, iso8601_time/1
        ,pretty_print_elapsed_s/1
        ,decr_timeout/2
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

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% returns current seconds
-spec current_tstamp() -> gregorian_seconds().
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-spec current_unix_tstamp() -> unix_seconds().
current_unix_tstamp() ->
    gregorian_seconds_to_unix_seconds(current_tstamp()).

-spec gregorian_seconds_to_unix_seconds(integer() | string() | binary()) -> integer().
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    kz_term:to_integer(GregorianSeconds) - ?UNIX_EPOCH_IN_GREGORIAN.

-spec unix_seconds_to_gregorian_seconds(integer() | string() | binary()) -> integer().
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    kz_term:to_integer(UnixSeconds) + ?UNIX_EPOCH_IN_GREGORIAN.

-spec unix_timestamp_to_gregorian_seconds(integer() | string() | binary()) -> integer().
unix_timestamp_to_gregorian_seconds(UnixTimestamp) ->
    ?UNIX_EPOCH_IN_GREGORIAN + (kz_term:to_integer(UnixTimestamp) div 1000).

-spec to_gregorian_seconds(kz_datetime(), api_ne_binary()) -> gregorian_seconds().
-ifdef(TEST).
to_gregorian_seconds(Datetime, 'undefined') ->
    to_gregorian_seconds(Datetime, <<"America/Los_Angeles">>);
to_gregorian_seconds({{_,_,_},{_,_,_}}=Datetime, ?NE_BINARY=FromTimezone) ->
    calendar:datetime_to_gregorian_seconds(
      localtime:local_to_local(Datetime, binary_to_list(FromTimezone), "Etc/UTC")).
-else.
to_gregorian_seconds(Datetime, 'undefined') ->
    to_gregorian_seconds(Datetime, kz_account:default_timezone());
to_gregorian_seconds({{_,_,_},{_,_,_}}=Datetime, ?NE_BINARY=FromTimezone) ->
    calendar:datetime_to_gregorian_seconds(
      localtime:local_to_local(Datetime, binary_to_list(FromTimezone), "Etc/UTC")
     ).
-endif.

-spec pretty_print_datetime(kz_datetime() | integer()) -> ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w-~2..0w-~2..0w"
                                  ,[Y, Mo, D, H, Mi, S]
                                  )).

-spec rfc1036(calendar:datetime() | gregorian_seconds()) -> ne_binary().
-spec rfc1036(calendar:datetime() | gregorian_seconds(), ne_binary()) -> ne_binary().
rfc1036(DateTime) ->
    rfc1036(DateTime, <<"GMT">>).

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

-spec iso8601_time(calendar:time() | calendar:datetime() | gregorian_seconds()) -> ne_binary().
iso8601_time({Hours, Mins, Secs}) ->
    H = kz_binary:pad_left(kz_term:to_binary(Hours), 2, <<"0">>),
    M = kz_binary:pad_left(kz_term:to_binary(Mins), 2, <<"0">>),
    S = kz_binary:pad_left(kz_term:to_binary(Secs), 2, <<"0">>),

    <<H/binary, ":", M/binary, ":", S/binary>>;
iso8601_time({{_,_,_}, {_H, _M, _S}=Time}) ->
    iso8601_time(Time);
iso8601_time(Timestamp) ->
    iso8601_time(calendar:gregorian_seconds_to_datetime(Timestamp)).

-spec iso8601(calendar:datetime() | gregorian_seconds()) -> ne_binary().
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

-spec pretty_print_elapsed_s(non_neg_integer()) -> ne_binary().
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

-spec decr_timeout(kz_timeout(), non_neg_integer() | kz_now()) -> kz_timeout().
decr_timeout('infinity', _) -> 'infinity';
decr_timeout(Timeout, Elapsed) when is_integer(Elapsed) ->
    Diff = Timeout - Elapsed,
    case Diff < 0 of
        'true' -> 0;
        'false' -> Diff
    end;
decr_timeout(Timeout, Start) ->
    decr_timeout(Timeout, elapsed_ms(Start)).

-spec microseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
-spec milliseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
microseconds_to_seconds(Microseconds) -> kz_term:to_integer(Microseconds) div 1000000.
milliseconds_to_seconds(Milliseconds) -> kz_term:to_integer(Milliseconds) div ?MILLISECONDS_IN_SECOND.

-spec elapsed_s(kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_ms(kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_us(kz_now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start) -> elapsed_s(Start, os:timestamp());
elapsed_s(Start) when is_integer(Start) -> elapsed_s(Start, current_tstamp()).

elapsed_ms({_,_,_}=Start) -> elapsed_ms(Start, os:timestamp());
elapsed_ms(Start) when is_integer(Start) -> elapsed_ms(Start, current_tstamp()).

elapsed_us({_,_,_}=Start) -> elapsed_us(Start, os:timestamp());
elapsed_us(Start) when is_integer(Start) -> elapsed_us(Start, current_tstamp()).

-spec elapsed_s(kz_now() | pos_integer(), kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_ms(kz_now() | pos_integer(), kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_us(kz_now() | pos_integer(), kz_now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start, {_,_,_}=Now) -> timer:now_diff(Now, Start) div 1000000;
elapsed_s({_,_,_}=Start, Now) -> elapsed_s(now_s(Start), Now);
elapsed_s(Start, {_,_,_}=Now) -> elapsed_s(Start, now_s(Now));
elapsed_s(Start, Now) when is_integer(Start), is_integer(Now) -> Now - Start.

elapsed_ms({_,_,_}=Start, {_,_,_}=Now) -> timer:now_diff(Now, Start) div ?MILLISECONDS_IN_SECOND;
elapsed_ms({_,_,_}=Start, Now) -> elapsed_ms(now_s(Start), Now);
elapsed_ms(Start, {_,_,_}=Now) -> elapsed_ms(Start, now_s(Now));
elapsed_ms(Start, Now)
  when is_integer(Start),
       is_integer(Now) ->
    (Now - Start) * ?MILLISECONDS_IN_SECOND.

elapsed_us({_,_,_}=Start, {_,_,_}=Now) -> timer:now_diff(Now, Start);
elapsed_us({_,_,_}=Start, Now) -> elapsed_us(now_s(Start), Now);
elapsed_us(Start, {_,_,_}=Now) -> elapsed_us(Start, now_s(Now));
elapsed_us(Start, Now) when is_integer(Start), is_integer(Now) -> (Now - Start) * 1000000.

-spec now() -> kz_now().
now() -> erlang:timestamp().

-spec now_s() -> gregorian_seconds().
-spec now_ms() -> pos_integer().
-spec now_us() -> pos_integer().

now_s() ->  erlang:system_time('seconds').
now_ms() -> erlang:system_time('milli_seconds').
now_us() -> erlang:system_time('micro_seconds').

-spec now_s(kz_now()) -> gregorian_seconds().
-spec now_ms(kz_now()) -> pos_integer().
-spec now_us(kz_now()) -> pos_integer().
now_us({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.
now_ms({_,_,_}=Now) -> now_us(Now) div ?MILLISECONDS_IN_SECOND.
now_s({_,_,_}=Now) -> unix_seconds_to_gregorian_seconds(now_us(Now) div 1000000).

-spec format_date() -> binary().
-spec format_date(gregorian_seconds()) -> binary().
-spec format_time() -> binary().
-spec format_time(gregorian_seconds()) -> binary().
-spec format_datetime() -> binary().
-spec format_datetime(gregorian_seconds()) -> binary().

format_date() ->
    format_date(current_tstamp()).

format_date(Timestamp) ->
    {{Y,M,D}, _ } = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([kz_term:to_binary(Y), "-", kz_term:to_binary(M), "-", kz_term:to_binary(D)]).

format_time() ->
    format_time(current_tstamp()).

format_time(Timestamp) ->
    { _, {H,I,S}} = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([kz_term:to_binary(H), ":", kz_term:to_binary(I), ":", kz_term:to_binary(S)]).

format_datetime() ->
    format_datetime(current_tstamp()).

format_datetime(Timestamp) ->
    list_to_binary([format_date(Timestamp), " ", format_time(Timestamp)]).

