%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_win_tests).

-include_lib("eunit/include/eunit.hrl").

start_recording_test_() ->
    [account_settings()
    ,endpoint_settings()
    ].

account_settings() ->
    [account_inbound_onnet()
    ,account_inbound_offnet()
    ,account_outbound_onnet()
    ,account_outbound_offnet()
    ].

endpoint_settings() ->
    [endpoint_inbound_onnet()
    ,endpoint_inbound_offnet()
    ,endpoint_outbound_onnet()
    ,endpoint_outbound_offnet()
    ].

account_inbound_onnet() ->
    ok.
