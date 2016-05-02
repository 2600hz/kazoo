%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(amqp_cron_task_tests).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(AMQP_TESTS).

-export([simple_task/0]).
-export([dying_task/1]).



oneshot_anon_test() ->
    Schedule = {oneshot, 500},
    Fun = fun(T) -> timer:sleep(T) end,
    {ok, Pid} = amqp_cron_task:start_link(Schedule, {Fun, [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    ?assertMatch({waiting, 500, _}, amqp_cron_task:status(Pid)),
    timer:sleep(550),
    ?assertMatch({running, 500, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(true, is_process_alive(TaskPid)),
    timer:sleep(550),
    ?assertMatch({done, 500, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(false, is_process_alive(TaskPid)).

oneshot_millis_test() ->
    Schedule = {oneshot, 500},
    {ok, Pid} = amqp_cron_task:start_link(Schedule, {timer, sleep, [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    ?assertMatch({waiting, 500, _}, amqp_cron_task:status(Pid)),
    timer:sleep(550),
    ?assertMatch({running, 500, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(true, is_process_alive(TaskPid)),
    timer:sleep(550),
    ?assertMatch({done, 500, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(false, is_process_alive(TaskPid)).

oneshot_datetime_test() ->
    DateTime = advance_seconds(calendar:universal_time(), 2),
    Schedule = {oneshot, DateTime},
    {ok, Pid} = amqp_cron_task:start_link(Schedule, {timer, sleep, [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    ?assertMatch({waiting, DateTime, _}, amqp_cron_task:status(Pid)),
    timer:sleep(2100),
    ?assertMatch({running, DateTime, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(true, is_process_alive(TaskPid)),
    timer:sleep(550),
    ?assertMatch({done, DateTime, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(false, is_process_alive(TaskPid)).

oneshot_in_the_past_test() ->
    DateTime = {{1970, 1, 1}, {1, 1, 1}},
    Schedule = {oneshot, DateTime},
    {ok, Pid} = amqp_cron_task:start_link(Schedule, {timer, sleep, [500]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    timer:sleep(500),
    ?assertMatch({error, _, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(false, is_process_alive(TaskPid)).

nominal_sleeper_workflow_test() ->
    Schedule = {sleeper, 1000},
    {ok, Pid} = amqp_cron_task:start_link(
		  Schedule,
		  {timer, sleep, [1000]}),
    {_, _, TaskPid} = amqp_cron_task:status(Pid),
    ?assertMatch({running, 1000, _}, amqp_cron_task:status(Pid)),
    timer:sleep(1500),
    ?assertMatch({waiting, 1000, _}, amqp_cron_task:status(Pid)),
    timer:sleep(1000),
    ?assertMatch({running, 1000, _}, amqp_cron_task:status(Pid)),
    ?assertEqual(true, is_process_alive(TaskPid)),
    ?assertEqual(ok, amqp_cron_task:stop(Pid)),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(TaskPid)),
    ?assertException(exit,
		     {noproc,{gen_server,call,[Pid, status]}},
		     amqp_cron_task:status(Pid)).

nominal_cron_workflow_test_() ->
    {timeout, 90,
     fun() ->
	     Schedule = {cron, {all, all, all, all, all}},
	     {ok, Pid} = amqp_cron_task:start_link(
			   Schedule,
			   {timer, sleep, [5000]}),
	     Current = calendar:universal_time(),
	     Next = next_valid_datetime(Schedule, Current),
	     WaitFor = time_to_wait_millis(Current, Next),
	     ?assertMatch({waiting, Next, _}, amqp_cron_task:status(Pid)),
	     timer:sleep(WaitFor + 2000),
	     ?assertMatch({running, Next, _}, amqp_cron_task:status(Pid)),
	     timer:sleep(4000),
	     Next1 = next_valid_datetime(Schedule, Next),
	     ?assertMatch({waiting, Next1, _}, amqp_cron_task:status(Pid)),
	     ?assertEqual(ok, amqp_cron_task:stop(Pid)),
	     ?assertException(exit,
			      {normal,{gen_server,call,[Pid, status]}},
			      amqp_cron_task:status(Pid))
     end}.

invalid_range_test() ->
    ?assertException(throw, {error, {out_of_range, {min, 2}, {value, 1}}},
		     extract_integers([], 2, 10, [1])),
    ?assertException(throw, {error, {out_of_range, {max, 2}, {value, 3}}},
		     extract_integers([], 1, 2, [3])).

extract_integers_test() ->
    ?assertException(error, function_clause, extract_integers([], 5, 4)),
    ?assertException(error, {case_clause, bad}, extract_integers([bad], 0, 5)),
    ?assertEqual([1,2,3,4,5], extract_integers([{range, 1, 5}], 0, 10)),
    ?assertEqual([1,2,3,4,5], extract_integers([{1, 5}], 0, 10)),
    ?assertEqual([1,2,3,4,5], extract_integers([{list, [1,2,3,4,5]}], 0, 10)),
    ?assertEqual([1,2,3,4,5], extract_integers([[1,2,3,4,5]], 0, 10)),
    ?assertEqual([5], extract_integers([{list, [5]}], 0, 10)),
    ?assertEqual([5], extract_integers([5], 0, 10)).

next_valid_datetime_cron_test() ->
    % roll year
    ?assertEqual({{2013, 1, 1}, {0, 0, 0}},
		 next_valid_datetime({cron, {all, all, all, all, all}},
				     {{2012, 12, 31}, {23, 59, 48}})),
    % last second of minute (we skip a second)
    ?assertEqual({{2012, 1, 1}, {0, 1, 0}},
		 next_valid_datetime({cron, {all, all, all, all, all}},
				     {{2012, 1, 1}, {0, 0, 59}})),
    % 12th month rolls year
     ?assertEqual({{2013, 2, 1}, {0, 0, 0}},
		 next_valid_datetime({cron, {all, all, all,
					     [{list, [2]}], all}},
				     {{2012, 12, 1}, {0, 0, 0}})),
    % normal month advance
    ?assertEqual({{2012, 12, 1}, {0, 0, 0}},
		 next_valid_datetime(
		   {cron, {all, all, all, [{list, [12]}], all}},
		   {{2012, 4, 1}, {0, 0, 0}})),
    % day of month (no day of week)
    ?assertEqual({{2012, 1, 13}, {0, 0, 0}},
		 next_valid_datetime(
		   {cron, {all, all, [{list, [13]}], all, all}},
		   {{2012, 1, 5}, {0, 0, 0}})),
    % day of week (no day of month)
    ?assertEqual({{2012, 2, 10}, {0, 0, 0}},
		 next_valid_datetime(
		   {cron, {all, all, all, all, [{list, [5]}]}}, % 5 is Friday
		   {{2012, 2, 7}, {0, 0, 0}})),
    % day of week and day of month (day of month comes first and wins)
    ?assertEqual({{2012, 2, 8}, {0, 0, 0}},
		 next_valid_datetime(
		   {cron, {all, all, [{list, [8]}], all, [{list, [5]}]}},
		   {{2012, 2, 7}, {0, 0, 0}})),
    % day of week and day of month (day of week comes first and wins)
    ?assertEqual({{2012, 2, 10}, {0, 0, 0}},
		 next_valid_datetime(
		   {cron, {all, all, [{list, [12]}], all, [{list, [5]}]}},
		   {{2012, 2, 7}, {0, 0, 0}})),
    % hour advance
    ?assertEqual({{2012, 1, 1}, {22, 0, 0}},
		 next_valid_datetime(
		   {cron, {all, [{list, [22]}], all, all, all}},
		   {{2012, 1, 1}, {0, 0, 0}})),
    % minute advance
    ?assertEqual({{2012, 1, 1}, {0, 59, 0}},
		 next_valid_datetime(
		   {cron, {[{list, [59]}], all, all, all, all}},
		   {{2012, 1, 1}, {0, 0, 0}})).

time_to_wait_millis_test() ->
    ?assertEqual(60000, time_to_wait_millis(
			  {{2012, 1, 1}, {0, 0, 0}},
			  {{2012, 1, 1}, {0, 1, 0}})).

-endif.
