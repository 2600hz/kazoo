%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, 2600Hz
%%% @doc
%%%
%%% Unit tests for shifting time around to various timezones
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(t_localtime).

-export([localtime_test/0]).

-include_lib("eunit/include/eunit.hrl").

-define(TZ_NAMES, ["Africa/Abidjan"
                   ,"Africa/Cairo"
                   ,"Africa/Timbuktu"
                   ,"America/Anchorage"
                   ,"America/Denver"
                   ,"America/Cayenne"
                   ,"America/Chicago"
                   ,"America/Costa Rica"
                   ,"America/Godthab"
                   ,<<"America/Los Angeles">>
                   ,"America/New York"
                   ,"Antarctica/DumontDUrville"
                   ,"Arctic/Longyearbyen"
                   ,"Asia/Aden"
                   ,"Asia/Anadyr"
                  ]).

localtime_test() ->
    UtcNow = calendar:universal_time(),
    test_utc(UtcNow).

test_utc(UtcNow) ->
    ?assertEqual(UtcNow, localtime:utc_to_local(UtcNow, "UTC")),
    ?assertEqual(UtcNow, localtime:local_to_utc(UtcNow, "UTC")),

    ?assertEqual({'error', 'unknown_tz'}, localtime:utc_to_local(UtcNow, "Mars/Heinlein")),

    [test_utc(UtcNow, Tz) || Tz <- ?TZ_NAMES],
    test_usa(UtcNow).
test_utc(UtcNow, Tz) ->
    LocalNow = localtime:utc_to_local(UtcNow, Tz),
    ?assertEqual(localtime:local_to_utc(LocalNow, Tz), UtcNow).

test_usa(UtcNow) ->
    EastNow = localtime:utc_to_local(UtcNow, "America/New York"),
    MidNow = localtime:utc_to_local(UtcNow, "America/Chicago"),
    MntNow = localtime:utc_to_local(UtcNow, "America/Denver"),
    WestNow = localtime:utc_to_local(UtcNow, "America/Los Angeles"),

    EastSec = calendar:datetime_to_gregorian_seconds(EastNow),
    MidSec = calendar:datetime_to_gregorian_seconds(MidNow),
    MntSec = calendar:datetime_to_gregorian_seconds(MntNow),
    WestSec = calendar:datetime_to_gregorian_seconds(WestNow),

    ?assertEqual(EastSec, MidSec + 3600),
    ?assertEqual(MidSec, MntSec + 3600),
    ?assertEqual(MntSec, WestSec + 3600),

    ?assertEqual(EastNow, localtime:local_to_local(WestNow, "America/Los Angeles", "America/New York")).
