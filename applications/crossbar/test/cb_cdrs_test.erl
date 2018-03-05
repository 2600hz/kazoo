%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2018, Voxter Communications Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Ben Bradford
%%%-------------------------------------------------------------------
-module(cb_cdrs_test).
-include_lib("eunit/include/eunit.hrl").

%% Test handle_utc_time_offset function with different server
%% time zones and offsets
handle_utc_time_offset_test_() ->
    %% Simulate a time stamp from a CDR
    %% (generated in freeswitch depending on system TZ)
    LocalTimestamp = 63687252084,

    %% Depending on the systems timezone, Calculate the UTC time from the LocalTimestamp
    LocalDateTime = calendar:gregorian_seconds_to_datetime(LocalTimestamp),
    UTCDateTimeList = calendar:local_time_to_universal_time_dst(LocalDateTime),
    UTCTimestamp = calendar:datetime_to_gregorian_seconds(lists:last(UTCDateTimeList)),

    [{"Verify the Atom 'undefined' returns unaltered timestamp"
     ,?_assertEqual(LocalTimestamp, cb_cdrs:handle_utc_time_offset(LocalTimestamp, 'undefined'))
     }
    ,{"Verify the Atom 'true' returns unaltered timestamp"
     ,?_assertEqual(LocalTimestamp, cb_cdrs:handle_utc_time_offset(LocalTimestamp, 'true'))
     }
    ,{"Verify the binary <<\"abc\">> returns unaltered timestamp as its NAN"
     ,?_assertEqual(LocalTimestamp, cb_cdrs:handle_utc_time_offset(LocalTimestamp, <<"bla">>))
     }
    ,{"Verify with offset binary <<\"0\">> returns UTC timestamp"
     ,?_assertEqual(UTCTimestamp, cb_cdrs:handle_utc_time_offset(LocalTimestamp, <<"0">>))
     }
    ,{"Verify with offset binary <<\"-14400\">> returns the UTC -4H"
     ,?_assertEqual((UTCTimestamp - 14400), cb_cdrs:handle_utc_time_offset(LocalTimestamp, <<"-14400">>))
     }
    ,{"Verify with offset binary <<\"14400\">> returns the UTC +4H"
     ,?_assertEqual((UTCTimestamp + 14400), cb_cdrs:handle_utc_time_offset(LocalTimestamp, <<"14400">>))
     }
    ].


