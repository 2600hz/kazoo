%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_yodb_tests).

-include_lib("eunit/include/eunit.hrl").

get_yodb_test_() ->
    {Year, _, _} = erlang:date(),
    YearBin = kz_term:to_binary(Year),
    OSTimestamp = os:timestamp(),

    AccountRaw = <<"d680c11889f6247fa12925350d4a6bc8">>,
    AccountEnc = <<"account%2Fd6%2F80%2Fc11889f6247fa12925350d4a6bc8">>,
    AccountUn = <<"account/d6/80/c11889f6247fa12925350d4a6bc8">>,

    DB = <<"account%2Fd6%2F80%2Fc11889f6247fa12925350d4a6bc8-",(YearBin):4/binary>>,

    [?_assertEqual(DB, kazoo_yodb:get_yodb(AccountRaw))
    ,?_assertEqual(DB, kazoo_yodb:get_yodb(AccountEnc))
    ,?_assertEqual(DB, kazoo_yodb:get_yodb(AccountUn))
    ,?_assertEqual(DB, kazoo_yodb:get_yodb(AccountRaw, Year))
    ,?_assertEqual(DB, kazoo_yodb:get_yodb(AccountRaw, YearBin))
    ,?_assertEqual(DB, kazoo_yodb:get_yodb(AccountRaw, OSTimestamp))
    ].
