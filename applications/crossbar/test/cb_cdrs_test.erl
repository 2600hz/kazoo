%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, Voxter Communications Inc
%%% @doc
%%% @author Ben Bradford
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_cdrs_test).
-include_lib("eunit/include/eunit.hrl").

%% Test handle_utc_time_offset function with different server
%% time zones and offsets
handle_utc_time_offset_test_() ->
    Timestamp = 63687252084,
    [{"Verify the Atom 'undefined' returns unaltered timestamp"
     ,?_assertEqual(Timestamp, cb_cdrs:handle_utc_time_offset(Timestamp, 'undefined'))
     }
    ,{"Verify the binary <<\"abc\">> triggers a crash as its NAN"
     ,?_assertError('badarg', cb_cdrs:handle_utc_time_offset(Timestamp, <<"bla">>))
     }
    ,{"Verify with offset binary <<\"0\">> returns UTC timestamp"
     ,?_assertEqual(Timestamp, cb_cdrs:handle_utc_time_offset(Timestamp, <<"0">>))
     }
    ].

handle_utc_time_offset_1_test_() ->
    %% {CDR Time Stamp, Resulting Timestamp, UTC offset}
    Timestamp = 63687252084,
    Tests = [{Timestamp, Timestamp - 14400, <<"-14400">>}
            ,{Timestamp, Timestamp + 14400, <<"14400">>}
            ],
    [?_assertEqual(Result, cb_cdrs:handle_utc_time_offset(UTC, Offset)) || {UTC, Result, Offset} <- Tests].
