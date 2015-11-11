%%==============================================================================
%% Copyright 2012 Jeremy Raymond
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @author Jeremy Raymond <jeraymond@gmail.com>
%%% @copyright (C) 2012, Jeremy Raymond
%%% @doc
%%% The leader_cron module provides a distrubuted task scheduler for
%%% executing tasks periodically. The connected nodes elect a leader
%%% to manage task scheduling and execution. Should the current leader
%%% become unavailable a new leader node is elected who resumes task
%%% execution responsibilities.
%%%
%%% There are several different ways to specify the schedule for a task.
%%% See {@link leader_cron_task} for details.
%%%
%%% Each node that is part of the scheduling cluster must be working
%%% with the same list of nodes as given to {@link start_link/1}. If
%%% the node list needs to change <code>leader_cron</code> must be
%%% stopped on all nodes. Once stopped everywhere restart
%%% <code>leader_cron</code> with the new node list. Rolling updates
%%% currently are not supported.
%%%
%%% @see leader_cron_task
%%%
%%% @end
%%% Created : 31 Jan 2012 by Jeremy Raymond <jeraymond@gmail.com>
%%%-------------------------------------------------------------------
-module(leader_cron).

-behaviour(gen_leader).

%% API
-export([start_link/1,
	 status/0,
	 schedule_task/2,
	 schedule_task/3,
	 cancel_task/1,
	 task_status/1,
	 task_list/0,
	 remove_done_tasks/0
	]).

%% gen_leader callbacks
-export([init/1,
         handle_cast/3,
         handle_call/4,
         handle_info/2,
         handle_leader_call/4,
         handle_leader_cast/3,
         handle_DOWN/3,
         elected/3,
         surrendered/3,
         from_leader/3,
         code_change/4,
         terminate/2]).

-define(SERVER, ?MODULE).

-type name() :: atom() | binary().
%% Name for a named task.

-type ident() :: name() | pid().
%% The task pid() or name.

-type task() :: {ident(),
                 leader_cron_task:schedule(),
                 leader_cron_task:execargs()}.
%% Task definition.

-record(state, {tasks = [], is_leader = false}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a linked process to manage scheduled tasks in coordination
%% with the given nodes. The current node must be part of the node
%% list. Each leader_cron node must be working with the same list of
%% nodes to coordinate correctly.
%%
%% @end
%%--------------------------------------------------------------------

-spec start_link(Nodes) -> {ok, pid()} | {error, Reason} when
      Nodes :: [node()],
      Reason :: term().

start_link(Nodes) ->
    Opts = [],
    amqp_leader:start_link(?SERVER, Nodes, Opts, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Gets the status of this scheduler.
%%
%% @end
%%--------------------------------------------------------------------

-spec status() -> Status when
      Status :: {[term()]}.

status() ->
    amqp_leader_proc:call(?SERVER, status).

%%--------------------------------------------------------------------
%% @doc
%% Schedules a task. See {@link leader_cron_task} for scheduling
%% details.
%%
%% @end
%%--------------------------------------------------------------------

-spec schedule_task(Schedule, Exec) -> {ok, pid()} | {error, term()} when
      Schedule :: leader_cron_task:schedule(),
      Exec :: leader_cron_task:execargs().

schedule_task(Schedule, Exec) ->
    amqp_leader_proc:leader_call(?SERVER, {schedule, {undefined, Schedule, Exec}}).

%%--------------------------------------------------------------------
%% @doc
%% Schedules a named task. There cannot be more than one task with
%% a given name at any one time. See {@link leader_cron_task} for
%% scheduling details.
%%
%% The name 'undefined' is reserved for all unnamed tasks and cannot
%% be used.
%%
%% @end
%%--------------------------------------------------------------------

-spec schedule_task(ident(), Schedule, Exec) ->
			   {ok, pid()} | {error, term()} when
      Schedule :: leader_cron_task:schedule(),
      Exec :: leader_cron_task:execargs().

schedule_task(Name, Schedule, Exec) when
      is_binary(Name); is_atom(Name), Name /= undefined ->
    amqp_leader_proc:leader_call(?SERVER, {schedule, {Name, Schedule, Exec}}).

%%--------------------------------------------------------------------
%% @doc
%% Cancels a task.
%%
%% @end
%%--------------------------------------------------------------------

-spec cancel_task(ident()) -> ok | {error, Reason} when
      Reason :: term().

cancel_task(Ident) ->
    amqp_leader_proc:leader_call(?SERVER, {cancel, Ident}).

%%--------------------------------------------------------------------
%% @doc
%% Gets the status of a task.
%%
%% @end
%%--------------------------------------------------------------------

-spec task_status(ident()) -> {Status, ScheduleTime, TaskPid} when
      Status :: leader_cron_task:status(),
      ScheduleTime :: leader_cron_task:datetime(),
      TaskPid :: pid().

task_status(Ident) ->
    amqp_leader_proc:leader_call(?SERVER, {task_status, Ident}).

%%--------------------------------------------------------------------
%% @doc
%% Gets the list of tasks.
%%
%% @end
%%--------------------------------------------------------------------

-spec task_list() -> [task()].

task_list() ->
    amqp_leader_proc:leader_call(?SERVER, task_list).

%%--------------------------------------------------------------------
%% @doc
%% Remove tasks with a status of done.
%%
%% @end
%%--------------------------------------------------------------------

-spec remove_done_tasks() -> ok.

remove_done_tasks() ->
    amqp_leader_proc:leader_call(?SERVER, remove_done_tasks).

%%%===================================================================
%%% gen_leader callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
elected(State, _Election, undefined) ->
    Sync = State#state.tasks,
    State1 = case State#state.is_leader of
		 false ->
		     start_tasks(State);
		 true ->
		     State
	     end,
    State2 = State1#state{is_leader = true},
    {ok, Sync, State2};
elected(State, _Election, _Node) ->
    Sync = State#state.tasks,
    State1 = case State#state.is_leader of
		 false ->
		     start_tasks(State);
		 true ->
		     State
	     end,
    State2 = State1#state{is_leader = true},
    {reply, Sync, State2}.

%% @private
surrendered(State, Sync, _Election) ->
    State1 = stop_tasks(State),
    State2 = save_tasks(State1, Sync),
    State3 = State2#state{is_leader = false},
    {ok, State3}.

%% @private
handle_leader_call({cancel, Name}, From, State, Election) when
      is_binary(Name); is_atom(Name) ->
    case pid_for_name(Name, State#state.tasks) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        Pid ->
            handle_leader_call({cancel, Pid}, From, State, Election)
    end;
handle_leader_call({cancel, Pid}, _From, State, Election) ->
    Tasks = State#state.tasks,
    {Reply, State1} = case lists:keyfind(Pid, 2, Tasks) of
			  false ->
			      {{error, no_such_pid}, State};
			  {_, Pid, _, _} ->
			      ok = leader_cron_task:stop(Pid),
			      Tasks1 = lists:keydelete(Pid, 2, Tasks),
			      send_tasks(Tasks1, Election),
			      {ok, State#state{tasks = Tasks1}}
		      end,
    {reply, Reply, State1};
handle_leader_call({schedule, {Name, Schedule, Exec}}, _From, State, Election) ->
    case not (Name == undefined)
	andalso lists:keymember(Name, 1, State#state.tasks) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            case leader_cron_task:start_link(Schedule, Exec) of
                {ok, Pid} ->
                    Task = {Name, Pid, Schedule, Exec},
                    TaskList = [Task|State#state.tasks],
                    State1 = State#state{tasks = TaskList},
                    ok = send_tasks(TaskList, Election),
                    {reply, {ok, Pid}, State1};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;
handle_leader_call({task_status, Name}, From, State, Election) when
      is_binary(Name); is_atom(Name) ->
    case pid_for_name(Name, State#state.tasks) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        Pid ->
            handle_leader_call({task_status, Pid}, From, State, Election)
    end;
handle_leader_call({task_status, Pid}, _From, State, _Election) ->
    Status = leader_cron_task:status(Pid),
    {reply, Status, State};
handle_leader_call(task_list, _From, State, _Election) ->
    Tasks = State#state.tasks,
    {reply, Tasks, State};
handle_leader_call(remove_done_tasks, _From, State, Election) ->
    Tasks = State#state.tasks,
    Tasks1 = lists:foldl(fun remove_task_if_done/2, [], Tasks),
    State1 = State#state{tasks = Tasks1},
    ok = send_tasks(Tasks1, Election),
    {reply, ok, State1}.

%% @private
handle_leader_cast(_Request, State, _Election) ->
    {noreply, State}.

%% @private
from_leader({tasks, Tasks}, State, _Election) ->
    State1 = save_tasks(State, Tasks),
    {ok, State1}.

%% @private
handle_DOWN(_Node, State, _Election) ->
    {ok, State}.

%% @private
handle_call(status, _From, State, Election) ->
    Reply = [{leader, amqp_leader_proc:leader_node(Election)},
	     {alive, amqp_leader_proc:alive(Election)},
	     {down, amqp_leader_proc:down(Election)},
	     {candidates, amqp_leader_proc:candidates(Election)},
	     {workers, amqp_leader_proc:workers(Election)},
	     {me, node()}
	    ],
    {reply, Reply, State};
handle_call(_Request, _From, State, _Election) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State, _Election) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Election, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

save_tasks(State, Tasks) ->
    State#state{tasks = Tasks}.

-spec send_tasks(Tasks, Election) -> ok when
      Tasks :: [task()],
      Election :: term().

send_tasks(Tasks, Election) ->
    case amqp_leader_proc:alive(Election) -- [node()] of
	[] ->
	    ok;
	Alive ->
	    Election = amqp_leader_proc:broadcast({from_leader, {tasks, Tasks}},
					    Alive,
					    Election),
	    ok
    end.

-spec stop_tasks(State :: #state{}) -> #state{}.

stop_tasks(State) ->
    Tasks = State#state.tasks,
    Tasks1 = lists:foldl(fun({Name, Pid, Schedule, Exec}, Acc) ->
				 ok = leader_cron_task:stop(Pid),
				 [{Name, undefined, Schedule, Exec}|Acc]
			 end, [], Tasks),
    State#state{tasks = Tasks1}.

-spec start_tasks(#state{}) -> #state{}.

start_tasks(State) ->
    TaskList = State#state.tasks,
    TaskList1 = lists:foldl(
	     fun(Task, Acc) ->
                    {Name, _, Schedule, Exec} = Task,
                    case leader_cron_task:start_link(Schedule, Exec) of
                        {ok, Pid} ->
                            [{Name, Pid, Schedule, Exec}|Acc];
                        {error, Reason} ->
                            Format = "Could not start task ~p ~p, name: ~p",
                            Message = io_lib:format(Format,
						    [Exec, Reason, Name]),
                            error_logger:error_report(Message),
                            [{Name, undefined, Schedule, Exec}|Acc]
                    end
	     end, [], TaskList),
    State#state{tasks = TaskList1}.

remove_task_if_done(Task, Acc) ->
    {_, Pid, _, _} = Task,
    case leader_cron_task:status(Pid) of
	{done, _, _} ->
	    ok = leader_cron_task:stop(Pid),
	    Acc;
	_ ->
	    [Task|Acc]
    end.

pid_for_name(Name, Tasks) ->
    case lists:keyfind(Name, 1, Tasks) of
        false ->
            {error, no_such_name};
        {_, Pid, _, _} ->
            Pid
    end.

%%%===================================================================
%%% Unit Tests
%%%===================================================================

-ifdef(TEST).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

simple_task() ->
    receive
	go ->
	    ok
    after
	1000 ->
	    ok
    end.

dying_task(TimeToLiveMillis) ->
    timer:sleep(TimeToLiveMillis),
    throw(time_to_go).

all_test_() ->
    {foreach,
     fun() ->
         leader_cron:start_link([node()]),
	     Tasks = leader_cron:task_list(),
	     lists:foreach(fun({_, Pid, _, _}) ->
				   ok = leader_cron:cancel_task(Pid)
			   end, Tasks)
     end,
     [
      fun test_single_node_task/0,
      fun test_dying_task/0,
      fun test_done_task_removal/0,
      fun test_single_named_task_with_atom_name/0,
      fun test_single_named_task_with_atom_name_undefined/0,
      fun test_single_named_task_with_binary_name/0
     ]}.

test_single_node_task() ->
    Schedule = {sleeper, 100},
    Exec = {leader_cron, simple_task, []},
    {ok, SchedulerPid} = leader_cron:schedule_task(Schedule, Exec),
    {running, _, TaskPid} = leader_cron:task_status(SchedulerPid),
    TaskPid ! go,
    ?assertMatch({waiting, _, TaskPid}, leader_cron:task_status(SchedulerPid)),
    ?assertEqual([{undefined, SchedulerPid, Schedule, Exec}],
		 leader_cron:task_list()),
    ?assertEqual(true, is_process_alive(TaskPid)),
    ?assertEqual(ok, leader_cron:cancel_task(SchedulerPid)),
    ?assertEqual([], leader_cron:task_list()),
    ?assertEqual(false, is_process_alive(TaskPid)).

test_dying_task() ->
    Schedule = {sleeper, 100000},
    {ok, SchedulerPid} = leader_cron:schedule_task(
			   Schedule, {leader_cron, dying_task, [100]}),
    ?assertMatch({running, _, _TPid}, leader_cron:task_status(SchedulerPid)),
    timer:sleep(200),
    ?assertMatch({waiting, _, _TPid}, leader_cron:task_status(SchedulerPid)).

test_done_task_removal() ->
    Schedule = {oneshot, 1},
    Exec = {timer, sleep, [1]},
    {ok, Pid} = leader_cron:schedule_task(Schedule, Exec),
    timer:sleep(5),
    ?assertMatch([_], leader_cron:task_list()),
    ?assertMatch({done, _, _}, leader_cron:task_status(Pid)),
    ?assertEqual(ok, leader_cron:remove_done_tasks()),
    ?assertEqual([], leader_cron:task_list()).

test_single_named_task_with_atom_name() ->
    test_single_named_task(test_task).

test_single_named_task_with_binary_name() ->
    test_single_named_task(<<"test task">>).

test_single_named_task_with_atom_name_undefined() ->
    ?assertError(function_clause, leader_cron:schedule_task(undefined, ok, ok)).

test_single_named_task(Name) ->
    Schedule = {sleeper, 100},
    Exec = {leader_cron, simple_task, []},
    {ok, SchedulerPid} = leader_cron:schedule_task(Name, Schedule, Exec),
    {error, already_exists} = leader_cron:schedule_task(Name, Schedule, Exec),
    {running, _, TaskPid} = leader_cron:task_status(Name),
    TaskPid ! go,
    ?assertMatch({waiting, _, TaskPid}, leader_cron:task_status(Name)),
    ?assertEqual([{Name, SchedulerPid, Schedule, Exec}],
		 leader_cron:task_list()),
    ?assertEqual(true, is_process_alive(TaskPid)),
    ?assertEqual(ok, leader_cron:cancel_task(Name)),
    ?assertEqual([], leader_cron:task_list()),
    ?assertEqual(false, is_process_alive(TaskPid)).

-endif.
