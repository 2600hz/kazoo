%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_cron_task_tests).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(AMQP_TESTS).

-export([simple_task/0]).
-export([dying_task/1]).



oneshot_anon_test_() ->
    Schedule = {'oneshot', 500},
    Fun = fun(T) -> timer:sleep(T) end,
    {'ok', Pid} = amqp_cron_task:start_link(Schedule, {Fun, [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    [?_assertMatch({'waiting', 500, _}, amqp_cron_task:status(Pid))
    ,?_assertMatch('ok', timer:sleep(550))
    ,?_assertMatch({'running', 500, _}, amqp_cron_task:status(Pid)),
    ,?_assertEqual('true', is_process_alive(TaskPid)),
    ,?_assertEqual('ok', timer:sleep(550))
    ,?_assertMatch({'done', 500, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ].

oneshot_millis_test_() ->
    Schedule = {'oneshot', 500},
    {'ok', Pid} = amqp_cron_task:start_link(Schedule, {'timer', 'sleep', [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    [?_assertMatch({'waiting', 500, _}, amqp_cron_task:status(Pid))
    ,?_assertMatch('ok', timer:sleep(550))
    ,?_assertMatch({'running', 500, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('true', is_process_alive(TaskPid))
    ,?_assertEqual('ok', timer:sleep(550))
    ,?_assertMatch({'done', 500, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ].

oneshot_datetime_test_() ->
    DateTime = advance_seconds(calendar:universal_time(), 2),
    Schedule = {'oneshot', DateTime},
    {'ok', Pid} = amqp_cron_task:start_link(Schedule, {'timer', 'sleep', [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    [?_assertMatch({'waiting', DateTime, _}, amqp_cron_task:status(Pid))
    ,?_assertMatch('ok', timer:sleep(2100))
    ,?_assertMatch({'running', DateTime, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('true', is_process_alive(TaskPid))
    ,?_assertEqual('ok', timer:sleep(550))
    ,?_assertMatch({'done', DateTime, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ].

oneshot_in_the_past_test_() ->
    DateTime = {{1970, 1, 1}, {1, 1, 1}},
    Schedule = {'oneshot', DateTime},
    {'ok', Pid} = amqp_cron_task:start_link(Schedule, {'timer', 'sleep', [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    [?_assertEqual('ok', timer:sleep(500))
    ,?_assertMatch({'error', _, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ].

nominal_sleeper_workflow_test_() ->
    Schedule = {'sleeper', 1000},
    {'ok', Pid} = amqp_cron_task:start_link(
                    Schedule,
                    {'timer', 'sleep', [1000]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    [?_assertMatch({'running', 1000, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('ok', timer:sleep(1500))
    ,?_assertMatch({'waiting', 1000, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('ok', timer:sleep(1000))
    ,?_assertMatch({'running', 1000, _}, amqp_cron_task:status(Pid))
    ,?_assertEqual('true', is_process_alive(TaskPid))
    ,?_assertEqual('ok', amqp_cron_task:stop(Pid))
    ,?_assertEqual('ok', timer:sleep(100))
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ,?_assertException('exit',
                       {'noproc',{'gen_server','call',[Pid, 'status']}},
                       amqp_cron_task:status(Pid))
    ].

nominal_cron_workflow_test_() ->
    {'timeout', 90,
     fun() ->
             Schedule = {'cron', {'all', 'all', 'all', 'all', 'all'}},
             {'ok', Pid} = amqp_cron_task:start_link(
                             Schedule,
                             {'timer', 'sleep', [5000]}),
             Current = calendar:universal_time(),
             Next = next_valid_datetime(Schedule, Current),
             WaitFor = time_to_wait_millis(Current, Next),
             [?_assertMatch({'waiting', Next, _}, amqp_cron_task:status(Pid))
             ,?_assertEqual('ok', timer:sleep(WaitFor + 2000))
             ,?_assertMatch({'running', Next, _}, amqp_cron_task:status(Pid))
             ,?_assertEqual('ok', timer:sleep(4000))
             ,?_assertMatch({'waiting', Next1, _}, begin
                                                       Next1 = next_valid_datetime(Schedule, Next),
                                                       amqp_cron_task:status(Pid)
                                                   end)
             ,?_assertEqual('ok', amqp_cron_task:stop(Pid))
             ,?_assertException('exit',
                                {'normal',{'gen_server','call',[Pid, 'status']}},
                                amqp_cron_task:status(Pid))
             ]
     end}.

invalid_range_test_() ->
    [?_assertException('throw', {'error', {'out_of_range', {'min', 2}, {'value', 1}}},
                       extract_integers([], 2, 10, [1]))
    ,?_assertException('throw', {'error', {'out_of_range', {'max', 2}, {'value', 3}}},
                       extract_integers([], 1, 2, [3]))
    ].

extract_integers_test_() ->
    [?_assertException('error', 'function_clause', extract_integers([], 5, 4))
    ,?_assertException('error', {'case_clause', 'bad'}, extract_integers(['bad'], 0, 5))
    ,?_assertEqual([1,2,3,4,5], extract_integers([{'range', 1, 5}], 0, 10))
    ,?_assertEqual([1,2,3,4,5], extract_integers([{1, 5}], 0, 10))
    ,?_assertEqual([1,2,3,4,5], extract_integers([{'list', [1,2,3,4,5]}], 0, 10))
    ,?_assertEqual([1,2,3,4,5], extract_integers([[1,2,3,4,5]], 0, 10))
    ,?_assertEqual([5], extract_integers([{'list', [5]}], 0, 10))
    ,?_assertEqual([5], extract_integers([5], 0, 10))
    ].

next_valid_datetime_cron_test_() ->
                                                % roll year
    [?_assertEqual({{2013, 1, 1}, {0, 0, 0}},
                   next_valid_datetime({'cron', {'all', 'all', 'all', 'all', 'all'}},
                                       {{2012, 12, 31}, {23, 59, 48}}))
                                                % last second of minute (we skip a second)
    ,?_assertEqual({{2012, 1, 1}, {0, 1, 0}},
                   next_valid_datetime({'cron', {'all', 'all', 'all', 'all', 'all'}},
                                       {{2012, 1, 1}, {0, 0, 59}}))
                                                % 12th month rolls year
    ,?_assertEqual({{2013, 2, 1}, {0, 0, 0}},
                   next_valid_datetime({'cron', {'all', 'all', 'all',
                                                 [{'list', [2]}], 'all'}},
                                       {{2012, 12, 1}, {0, 0, 0}}))
                                                % normal month advance
    ,?_assertEqual({{2012, 12, 1}, {0, 0, 0}},
                   next_valid_datetime(
                     {'cron', {'all', 'all', 'all', [{'list', [12]}], 'all'}},
                     {{2012, 4, 1}, {0, 0, 0}}))
                                                % day of month (no day of week)
    ,?_assertEqual({{2012, 1, 13}, {0, 0, 0}},
                   next_valid_datetime(
                     {'cron', {'all', 'all', [{'list', [13]}], 'all', 'all'}},
                     {{2012, 1, 5}, {0, 0, 0}}))
                                                % day of week (no day of month)
    ,?_assertEqual({{2012, 2, 10}, {0, 0, 0}},
                   next_valid_datetime(
                     {'cron', {'all', 'all', 'all', 'all', [{'list', [5]}]}}, % 5 is Friday
                     {{2012, 2, 7}, {0, 0, 0}}))
                                                % day of week and day of month (day of month comes first and wins)
    ,?_assertEqual({{2012, 2, 8}, {0, 0, 0}},
                   next_valid_datetime(
                     {'cron', {'all', 'all', [{'list', [8]}], 'all', [{'list', [5]}]}},
                     {{2012, 2, 7}, {0, 0, 0}}))
                                                % day of week and day of month (day of week comes first and wins)
    ,?_assertEqual({{2012, 2, 10}, {0, 0, 0}},
                   next_valid_datetime(
                     {'cron', {'all', 'all', [{'list', [12]}], 'all', [{'list', [5]}]}},
                     {{2012, 2, 7}, {0, 0, 0}}))
                                                % hour advance
    ,?_assertEqual({{2012, 1, 1}, {22, 0, 0}},
                   next_valid_datetime(
                     {'cron', {'all', [{'list', [22]}], 'all', 'all', 'all'}},
                     {{2012, 1, 1}, {0, 0, 0}}))
                                                % minute advance
    ,?_assertEqual({{2012, 1, 1}, {0, 59, 0}},
                   next_valid_datetime(
                     {'cron', {[{'list', [59]}], 'all', 'all', 'all', 'all'}},
                     {{2012, 1, 1}, {0, 0, 0}}))
    ].

time_to_wait_millis_test() ->
    ?assertEqual(60000, time_to_wait_millis(
                          {{2012, 1, 1}, {0, 0, 0}},
                          {{2012, 1, 1}, {0, 1, 0}})).

-endif.
