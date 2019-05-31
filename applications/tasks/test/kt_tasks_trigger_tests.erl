-module(kt_tasks_trigger_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATE, {2600, 10, 10}).

next_minute_test_() ->
    [?_assertEqual(60, kz_tasks_trigger:seconds_until_next_minute({?DATE, {1, 2, 0}}))
    ,?_assertEqual(59, kz_tasks_trigger:seconds_until_next_minute({?DATE, {1, 2, 1}}))
    ,?_assertEqual(30, kz_tasks_trigger:seconds_until_next_minute({?DATE, {1, 2, 30}}))
    ,?_assertEqual(2, kz_tasks_trigger:seconds_until_next_minute({?DATE, {1, 2, 58}}))
    ,?_assertEqual(1, kz_tasks_trigger:seconds_until_next_minute({?DATE, {1, 2, 59}}))
    ,?_assertEqual(0, kz_tasks_trigger:seconds_until_next_minute({?DATE, {1, 2, 60}}))
    ].

next_hour_test_() ->
    [?_assertEqual(3600, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 0, 0}}))
    ,?_assertEqual(3599, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 0, 1}}))
    ,?_assertEqual(3540, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 1, 0}}))
    ,?_assertEqual(3539, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 1, 1}}))

    ,?_assertEqual(1800, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 30, 0}}))
    ,?_assertEqual(1770, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 30, 30}}))
    ,?_assertEqual(1741, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 30, 59}}))

    ,?_assertEqual(60, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 59, 0}}))
    ,?_assertEqual(59, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 59, 1}}))
    ,?_assertEqual(1, kz_tasks_trigger:seconds_until_next_hour({?DATE, {1, 59, 59}}))
    ].

next_day_test_() ->
    [?_assertEqual(86400, kz_tasks_trigger:seconds_until_next_day({?DATE, {0, 0, 0}}))

    ,?_assertEqual(61, kz_tasks_trigger:seconds_until_next_day({?DATE, {23, 58, 59}}))
    ,?_assertEqual(60, kz_tasks_trigger:seconds_until_next_day({?DATE, {23, 59, 0}}))

    ,?_assertEqual(3, kz_tasks_trigger:seconds_until_next_day({?DATE, {23, 59, 57}}))
    ,?_assertEqual(2, kz_tasks_trigger:seconds_until_next_day({?DATE, {23, 59, 58}}))
    ,?_assertEqual(1, kz_tasks_trigger:seconds_until_next_day({?DATE, {23, 59, 59}}))
    ].
