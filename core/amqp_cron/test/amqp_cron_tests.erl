%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_cron_tests).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(AMQP_TESTS).

-export([simple_task/0]).
-export([dying_task/1]).



simple_task() ->
    receive
        'go' ->
            'ok'
    after
        1000 ->
            'ok'
    end.

dying_task(TimeToLiveMillis) ->
    timer:sleep(TimeToLiveMillis),
    throw('time_to_go').

all_test_() ->
    {'foreach',
     fun() ->
             amqp_cron:start_link([node()]),
             Tasks = amqp_cron:task_list(),
             lists:foreach(fun({_, Pid, _, _}) ->
                                   'ok' = amqp_cron:cancel_task(Pid)
                           end, Tasks)
     end,
     [fun test_single_node_task/0
     ,fun test_dying_task/0
     ,fun test_done_task_removal/0
     ,fun test_single_named_task_with_atom_name/0
     ,fun test_single_named_task_with_atom_name_undefined/0
     ,fun test_single_named_task_with_binary_name/0
     ]}.

test_single_node_task_() ->
    Schedule = {'sleeper', 100},
    Exec = {'amqp_cron', 'simple_task', []},
    {'ok', SchedulerPid} = amqp_cron:schedule_task(Schedule, Exec),
    {'running', _, TaskPid} = amqp_cron:task_status(SchedulerPid),
    TaskPid ! 'go',
    [?_assertMatch({'waiting', _, TaskPid}, amqp_cron:task_status(SchedulerPid))
    ,?_assertEqual([{'undefined', SchedulerPid, Schedule, Exec}],
                   amqp_cron:task_list())
    ,?_assertEqual('true', is_process_alive(TaskPid))
    ,?_assertEqual('ok', amqp_cron:cancel_task(SchedulerPid))
    ,?_assertEqual([], amqp_cron:task_list())
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ].

test_dying_task_() ->
    Schedule = {'sleeper', 100000},
    {'ok', SchedulerPid} = amqp_cron:schedule_task(
                             Schedule, {'amqp_cron', 'dying_task', [100]}),
    [?_assertMatch({'running', _, _TPid}, amqp_cron:task_status(SchedulerPid))
    ,?_assertEqual('ok', timer:sleep(200))
    ,?_assertMatch({'waiting', _, _TPid}, amqp_cron:task_status(SchedulerPid))
    ].

test_done_task_removal_() ->
    Schedule = {'oneshot', 1},
    Exec = {'timer', 'sleep', [1]},
    {'ok', Pid} = amqp_cron:schedule_task(Schedule, Exec),
    timer:sleep(5),
    [?_assertMatch([_], amqp_cron:task_list())
    ,?_assertMatch({'done', _, _}, amqp_cron:task_status(Pid))
    ,?_assertEqual('ok', amqp_cron:remove_done_tasks())
    ,?_assertEqual([], amqp_cron:task_list())
    ].

test_single_named_task_with_atom_name() ->
    test_single_named_task('test_task').

test_single_named_task_with_binary_name() ->
    test_single_named_task(<<"test task">>).

test_single_named_task_with_atom_name_undefined() ->
    ?assertError('function_clause', amqp_cron:schedule_task('undefined', 'ok', 'ok')).

test_single_named_task_(Name) ->
    Schedule = {'sleeper', 100},
    Exec = {'amqp_cron', 'simple_task', []},
    {'ok', SchedulerPid} = amqp_cron:schedule_task(Name, Schedule, Exec),
    {'error', 'already_exists'} = amqp_cron:schedule_task(Name, Schedule, Exec),
    {'running', _, TaskPid} = amqp_cron:task_status(Name),
    TaskPid ! 'go',
    [?_assertMatch({'waiting', _, TaskPid}, amqp_cron:task_status(Name))
    ,?_assertEqual([{Name, SchedulerPid, Schedule, Exec}],
                   amqp_cron:task_list())
    ,?_assertEqual('true', is_process_alive(TaskPid))
    ,?_assertEqual('ok', amqp_cron:cancel_task(Name))
    ,?_assertEqual([], amqp_cron:task_list())
    ,?_assertEqual('false', is_process_alive(TaskPid))
    ].

-endif.
