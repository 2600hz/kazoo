-module(kazoo_perf_maintenance_tests).

-include_lib("eunit/include/eunit.hrl").

metrics_test_() ->
    %% {A, C, Z} = {<<"my_account">>, <<"my_cluster">>, <<"my_zone">>},
    %% %% We only care about if this crashes
    %% [{"Testing Graphite metrics"
    %%  %% ,{timeout, 2*60, ?_assertEqual(no_return, kazoo_perf_maintenance:graphite_metrics(A, C, Z))}
    %%  ,?_assertEqual(no_return, kazoo_perf_maintenance:graphite_metrics(A, C, Z))
    %%  }
    %% ,{"Testing JSON metrics"
    %%  ,?_assertEqual(no_return, kazoo_perf_maintenance:json_metrics())
    %%  }
    %% ].
    [].
