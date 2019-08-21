%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-, Jeremy Raymond
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Jeremy Raymond <jeraymond@gmail.com>
%%%
%%% @doc The amqp_cron module provides a distributed task scheduler for
%%% executing tasks periodically. The connected nodes elect a leader
%%% to manage task scheduling and execution. Should the current leader
%%% become unavailable a new leader node is elected who resumes task
%%% execution responsibilities.
%%%
%%% There are several different ways to specify the schedule for a task.
%%% See {@link amqp_cron_task} for details.
%%%
%%% Each node that is part of the scheduling cluster must be working
%%% with the same list of nodes as given to {@link start_link/1}. If
%%% the node list needs to change <code>amqp_cron</code> must be
%%% stopped on all nodes. Once stopped everywhere restart
%%% <code>amqp_cron</code> with the new node list. Rolling updates
%%% currently are not supported.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_cron).

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

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-type name() :: atom() | binary().
%% Name for a named task.

-type ident() :: name() | pid().
%% The task pid() or name.

-type task() :: {name()
                ,pid()
                ,amqp_cron_task:schedule()
                ,amqp_cron_task:execargs()
                }.
-type tasks() :: [task()].

-record(state, {tasks = [] :: tasks()
               ,is_leader = 'false' :: boolean()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates a linked process to manage scheduled tasks in coordination
%% with the given nodes. The current node must be part of the node
%% list. Each amqp_cron node must be working with the same list of
%% nodes to coordinate correctly.
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_link([node()]) -> kz_types:startlink_ret().
start_link(Nodes) ->
    Opts = [],
    amqp_leader:start_link(?SERVER, Nodes, Opts, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc Gets the status of this scheduler.
%% @end
%%------------------------------------------------------------------------------
-spec status() -> Status when
      Status :: {[any()]}.
status() ->
    amqp_leader_proc:call(?SERVER, 'status').

%%------------------------------------------------------------------------------
%% @doc
%% Schedules a task. See {@link amqp_cron_task} for scheduling
%% details.
%%
%% @end
%%------------------------------------------------------------------------------
-spec schedule_task(Schedule, Exec) -> {'ok', pid()} | {'error', any()} when
      Schedule :: amqp_cron_task:schedule(),
      Exec :: amqp_cron_task:execargs().
schedule_task(Schedule, Exec) ->
    amqp_leader_proc:leader_call(?SERVER, {'schedule', {'undefined', Schedule, Exec}}).

%%------------------------------------------------------------------------------
%% @doc Schedules a named task. There cannot be more than one task with
%% a given name at any one time. See {@link amqp_cron_task} for
%% scheduling details.
%%
%% The name `undefined' is reserved for all unnamed tasks and cannot
%% be used.
%%
%% @end
%%------------------------------------------------------------------------------
-spec schedule_task(ident(), Schedule, Exec) ->
                           {'ok', pid()} | {'error', any()} when
      Schedule :: amqp_cron_task:schedule(),
      Exec :: amqp_cron_task:execargs().
schedule_task(Name, Schedule, Exec) when
      is_binary(Name); is_atom(Name), Name =/= 'undefined' ->
    amqp_leader_proc:leader_call(?SERVER, {'schedule', {Name, Schedule, Exec}}).

%%------------------------------------------------------------------------------
%% @doc Cancels a task.
%% @end
%%------------------------------------------------------------------------------
-spec cancel_task(ident()) -> 'ok' | {'error', Reason} when
      Reason :: any().
cancel_task(Ident) ->
    amqp_leader_proc:leader_call(?SERVER, {'cancel', Ident}).

%%------------------------------------------------------------------------------
%% @doc Gets the status of a task.
%% @end
%%------------------------------------------------------------------------------
-spec task_status(ident()) -> {Status, ScheduleTime, TaskPid} when
      Status :: amqp_cron_task:status(),
      ScheduleTime :: amqp_cron_task:datetime(),
      TaskPid :: pid().
task_status(Ident) ->
    amqp_leader_proc:leader_call(?SERVER, {'task_status', Ident}).

%%------------------------------------------------------------------------------
%% @doc Gets the list of tasks.
%% @end
%%------------------------------------------------------------------------------
-spec task_list() -> [task()].
task_list() ->
    amqp_leader_proc:leader_call(?SERVER, 'task_list').

%%------------------------------------------------------------------------------
%% @doc Remove tasks with a status of done.
%% @end
%%------------------------------------------------------------------------------
-spec remove_done_tasks() -> 'ok'.
remove_done_tasks() ->
    amqp_leader_proc:leader_call(?SERVER, 'remove_done_tasks').

%%%=============================================================================
%%% gen_leader callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

-spec elected(state(), any(), any()) -> {'ok', state()} | {'reply', any(), state()}.
elected(State, _Election, 'undefined') ->
    State1 = case State#state.is_leader of
                 'false' -> start_tasks(State);
                 'true' -> State
             end,
    State2 = State1#state{is_leader = 'true'},
    {'ok', State2#state.tasks, State2};
elected(State, _Election, _Node) ->
    State1 = case State#state.is_leader of
                 'false' -> start_tasks(State);
                 'true' -> State
             end,
    State2 = State1#state{is_leader = 'true'},
    {'reply', State2#state.tasks, State2}.

-spec surrendered(state(), any(), any()) -> {'ok', state()}.
surrendered(State, Sync, _Election) ->
    State1 = stop_tasks(State),
    State2 = save_tasks(State1, Sync),
    State3 = State2#state{is_leader = 'false'},
    {'ok', State3}.

-spec handle_leader_call(any(), kz_term:pid_ref(), state(), any()) -> kz_types:handle_call_ret_state(state()).
handle_leader_call({'cancel', Name}, From, State, Election) when
      is_binary(Name); is_atom(Name) ->
    case pid_for_name(Name, State#state.tasks) of
        {'error', _Reason} = Error ->
            {'reply', Error, State};
        Pid ->
            handle_leader_call({'cancel', Pid}, From, State, Election)
    end;
handle_leader_call({'cancel', Pid}, _From, State, Election) ->
    Tasks = State#state.tasks,
    {Reply, State1} =
        case lists:keyfind(Pid, 2, Tasks) of
            'false' ->
                {{'error', 'no_such_pid'}, State};
            {_, Pid, _, _} ->
                'ok' = amqp_cron_task:stop(Pid),
                Tasks1 = lists:keydelete(Pid, 2, Tasks),
                send_tasks(Tasks1, Election),
                {'ok', State#state{tasks = Tasks1}}
        end,
    {'reply', Reply, State1};
handle_leader_call({'schedule', {Name, Schedule, Exec}}, _From, State, Election) ->
    case (Name =/= 'undefined')
        andalso lists:keymember(Name, 1, State#state.tasks) of
        'true' ->
            {'reply', {'error', 'already_exists'}, State};
        'false' ->
            case amqp_cron_task:start_link(Schedule, Exec) of
                {'ok', Pid} = Ok ->
                    Task = {Name, Pid, Schedule, Exec},
                    TaskList = [Task|State#state.tasks],
                    State1 = State#state{tasks = TaskList},
                    'ok' = send_tasks(TaskList, Election),
                    {'reply', Ok, State1};
                {'error', _Reason} = Error ->
                    {'reply', Error, State}
            end
    end;
handle_leader_call({'task_status', Name}, From, State, Election) when
      is_binary(Name); is_atom(Name) ->
    case pid_for_name(Name, State#state.tasks) of
        {'error', _Reason} = Error ->
            {'reply', Error, State};
        Pid ->
            handle_leader_call({'task_status', Pid}, From, State, Election)
    end;
handle_leader_call({'task_status', Pid}, _From, State, _Election) ->
    Status = amqp_cron_task:status(Pid),
    {'reply', Status, State};
handle_leader_call('task_list', _From, State, _Election) ->
    Tasks = State#state.tasks,
    {'reply', Tasks, State};
handle_leader_call('remove_done_tasks', _From, State, Election) ->
    Tasks = State#state.tasks,
    Tasks1 = lists:foldl(fun remove_task_if_done/2, [], Tasks),
    State1 = State#state{'tasks' = Tasks1},
    'ok' = send_tasks(Tasks1, Election),
    {'reply', 'ok', State1}.

-spec handle_leader_cast(any(), state(), any()) -> kz_types:handle_cast_ret_state(state()).
handle_leader_cast(_Request, State, _Election) ->
    {'noreply', State}.

-spec from_leader(any(), state(), any()) -> {'ok', state()}.
from_leader({'tasks', Tasks}, State, _Election) ->
    State1 = save_tasks(State, Tasks),
    {'ok', State1}.

-spec handle_DOWN(node(), state(), any()) -> {'ok', state()}.
handle_DOWN(_Node, State, _Election) ->
    {'ok', State}.

-spec handle_call(any(), kz_term:pid_ref(), state(), any()) -> kz_types:handle_call_ret_state(state()).
handle_call('status', _From, State, Election) ->
    Reply = [{'leader', amqp_leader_proc:leader_node(Election)}
            ,{'alive', amqp_leader_proc:alive(Election)}
            ,{'down', amqp_leader_proc:down(Election)}
            ,{'candidates', amqp_leader_proc:candidates(Election)}
            ,{'workers', amqp_leader_proc:workers(Election)}
            ,{'me', node()}
            ],
    {'reply', Reply, State};
handle_call(_Request, _From, State, _Election) ->
    {'reply', 'ok', State}.

-spec handle_cast(any(), state(), any()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State, _Election) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

-spec code_change(any(), state(), any(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Election, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
save_tasks(State, Tasks) ->
    State#state{tasks = Tasks}.

-spec send_tasks(Tasks, Election) -> 'ok' when
      Tasks :: [task()],
      Election :: any().
send_tasks(Tasks, Election) ->
    case amqp_leader_proc:alive(Election) -- [node()] of
        [] -> 'ok';
        Alive ->
            Election = amqp_leader_proc:broadcast({'from_leader', {'tasks', Tasks}}
                                                 ,Alive
                                                 ,Election
                                                 ),
            'ok'
    end.

-spec stop_tasks(State :: state()) -> state().
stop_tasks(State) ->
    Tasks = State#state.tasks,
    Tasks1 = lists:foldl(
               fun({Name, Pid, Schedule, Exec}, Acc) when node(Pid) =:= node() ->
                       'ok' = amqp_cron_task:stop(Pid),
                       [{Name, 'undefined', Schedule, Exec}|Acc];
                  (Task, Acc) ->
                       [Task | Acc]
               end, [], Tasks),
    State#state{tasks = Tasks1}.

-spec start_tasks(state()) -> state().
start_tasks(State) ->
    TaskList = State#state.tasks,
    TaskList1 = lists:foldl(
                  fun(Task, Acc) ->
                          {Name, _, Schedule, Exec} = Task,
                          case amqp_cron_task:start_link(Schedule, Exec) of
                              {'ok', Pid} ->
                                  [{Name, Pid, Schedule, Exec}|Acc];
                              {'error', Reason} ->
                                  Format = "Could not start task ~p ~p, name: ~p",
                                  Message = io_lib:format(Format,
                                                          [Exec, Reason, Name]),
                                  error_logger:error_report(Message),
                                  [{Name, 'undefined', Schedule, Exec}|Acc]
                          end
                  end, [], TaskList),
    State#state{tasks = TaskList1}.

remove_task_if_done(Task, Acc) ->
    {_, Pid, _, _} = Task,
    case amqp_cron_task:status(Pid) of
        {'done', _, _} ->
            'ok' = amqp_cron_task:stop(Pid),
            Acc;
        _ ->
            [Task|Acc]
    end.

-spec pid_for_name(binary() | atom(), tasks()) -> pid() |
                                                  {error, no_such_name}.
pid_for_name(Name, Tasks) ->
    case lists:keyfind(Name, 1, Tasks) of
        'false' ->
            {'error', 'no_such_name'};
        {_, Pid, _, _} ->
            Pid
    end.
