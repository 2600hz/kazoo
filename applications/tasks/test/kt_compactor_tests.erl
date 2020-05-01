%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_compactor_tests).

-include_lib("eunit/include/eunit.hrl").

sort_by_disk_size_test_() ->
    TestF = fun kt_compactor:sort_by_disk_size/1,

    Db1 = new_db_disk_and_data(11111, 55555),
    Db2 = new_db_disk_and_data(22222, 44444),
    Db3 = new_db_disk_and_data(33333, 33333),
    Db4 = new_db_disk_and_data(44444, 22222),
    Db5 = new_db_disk_and_data(55555, 11111),
    Undefined = new_db_disk_and_data('undefined'),
    NotFound = new_db_disk_and_data('not_found'),

    Expected1 = [Db3, Db2, Db1],
    Expected2 = [Db5, Db4, Db3, Db2, Db1],
    Expected3 = [Db4, Db2, Undefined],
    Expected4 = [Db3, Db1, NotFound],

    [{"only sort by disk_size and ignore data_size"
     ,?_assertEqual(Expected1, TestF(kz_term:shuffle_list(Expected1)))
     }
    ,{"only sort by disk_size and ignore data_size"
     ,?_assertEqual(Expected2, TestF(kz_term:shuffle_list(Expected2)))
     }
    ,{"sort undefined disk_and_data values too"
     ,?_assertEqual(Expected3, TestF(kz_term:shuffle_list(Expected3)))
     }
    ,{"sort not_found disk_and_data values too"
     ,?_assertEqual(Expected4, TestF(kz_term:shuffle_list(Expected4)))
     }
    ].

build_compaction_callid_test_() ->
    TestF = fun() -> kt_compactor:build_compaction_callid(<<"testing">>) end,

    {Year, Month, _} = erlang:date(),
    YearBin = integer_to_binary(Year),
    MonthBin = kz_binary:pad_left(integer_to_binary(Month), 2, <<"0">>),

    [{"Prefix with YYYYMM-"
     ,?_assertMatch(<<YearBin:4/binary, MonthBin:2/binary, "-", _/binary>>, TestF())
     }
    ,{"Suffix with random hex"
     ,?_assertMatch(<<_Prefix:7/binary, "testing_", _Suffix:8/binary>>, TestF())
     }
    ,{"Return unique callid every time"
     ,?_assertNotEqual(TestF(), TestF())
     }
    ].

%% =======================================================================================
%% Helpers
%% =======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
new_db_disk_and_data(UndefinedOrNotFound) ->
    {kz_binary:rand_hex(4), UndefinedOrNotFound}.

new_db_disk_and_data(DiskSize, DataSize) ->
    {kz_binary:rand_hex(4), {DiskSize, DataSize}}.
