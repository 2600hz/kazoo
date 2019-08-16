%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_perf_tests).

-include_lib("eunit/include/eunit.hrl").

metrics_test_() ->
    {A, C, Z} = {<<"my_account">>, <<"my_cluster">>, <<"my_zone">>},
    %% We only care about if this crashes
    [?_assertEqual(no_return, kazoo_perf_maintenance:graphite_metrics(A, C, Z))
    ,?_assertEqual(no_return, kazoo_perf_maintenance:json_metrics())
    ].
