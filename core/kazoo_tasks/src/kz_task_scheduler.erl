%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Schedule one-off tasks only once per cluster
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_scheduler).

%% API
-export([start_link/1
        ,status/0
        ,schedule_task/2
        ,cancel_task/1
        ,task_status/1
        ,list_tasks/0
        ,remove_done_tasks/0
	]).

%% gen_leader callbacks
-export([init/1
        ,handle_cast/3
        ,handle_call/4
        ,handle_info/2
        ,handle_leader_call/4
        ,handle_leader_cast/3
        ,handle_DOWN/3
        ,elected/3
        ,surrendered/3
        ,from_leader/3
        ,code_change/4
        ,terminate/2
        ]).

-include("kz_tasks.hrl").

-define(SERVER, ?MODULE).

-define(A_TASK_ID, kz_util:rand_hex_binary(16)).
-type task_id() :: ne_binary().

-type task_name() :: ne_binary().

-type task_data() :: kz_json:object().

-record(task, { pid = 'undefined' :: api_pid()
              , id :: task_id()
              , name :: task_name()
              , data = kz_json:new() :: task_data()
              }).
-opaque task() :: #task{}.
-opaque tasks() :: [task()].

-record(state, { is_leader = 'false' :: boolean()
               , tasks = [] :: tasks()
               }).
-type state() :: #state{}.

-export_type([task_name/0
             ,task_id/0
             ,task/0, tasks/0
             ]).

-type ident() :: task_name() | pid().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a linked process to manage scheduled tasks in coordination
%% with the given nodes. The current node must be part of the node
%% list. Each amqp_cron node must be working with the same list of
%% nodes to coordinate correctly.
%% @end
%%--------------------------------------------------------------------
-spec start_link([node()]) -> startlink_ret().
start_link(Nodes) ->
    Opts = [],
    amqp_leader:start_link(?SERVER, Nodes, Opts, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec status() -> kz_proplist().
status() ->
    amqp_leader_proc:call(?SERVER, 'status').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Schedules a named task. There cannot be more than one task with
%% a given name at any one time.
%% @end
%%--------------------------------------------------------------------
-spec schedule_task(task_name(), task_data()) -> {'ok', pid()} |
                                                 {'error', any()}.
schedule_task(Name=?NE_BINARY, TaskData) ->
    TaskName = task_name(Name),
    case kz_util:is_empty(TaskName) of
        'true' -> {'error', 'bad_name'};
        'false' ->
            Task = #task{ id = ?A_TASK_ID
                        , name = TaskName
                        , data = TaskData
                        },
            amqp_leader_proc:leader_call(?SERVER, {'schedule', {Name, Task}})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cancel_task(ident()) -> 'ok' |
                              {'error', any()}.
cancel_task(Ident) ->
    amqp_leader_proc:leader_call(?SERVER, {'cancel', Ident}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec task_status(ident()) -> {kz_task_worker:status(), pid()}.
task_status(Ident) ->
    amqp_leader_proc:leader_call(?SERVER, {'task_status', Ident}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list_tasks() -> tasks().
list_tasks() ->
    amqp_leader_proc:leader_call(?SERVER, 'list_tasks').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_done_tasks() -> 'ok'.
remove_done_tasks() ->
    amqp_leader_proc:leader_call(?SERVER, 'remove_tasks_done').


%%%===================================================================
%%% gen_leader callbacks
%%%===================================================================

init([]) ->
    {'ok', #state{}}.


elected(State, _Election, 'undefined') ->
    State1 =
        case State#state.is_leader of
            'false' -> start_tasks(State);
            'true' -> State
        end,
    State2 = State1#state{is_leader = 'true'},
    {'ok', State2#state.tasks, State2};

elected(State, _Election, _Node) ->
    State1 =
        case State#state.is_leader of
            'false' -> start_tasks(State);
            'true' -> State
        end,
    State2 = State1#state{is_leader = 'true'},
    {'reply', State2#state.tasks, State2}.


surrendered(State, Sync, _Election) ->
    State1 = stop_tasks(State),
    State2 = State1#state{tasks = Sync},
    State3 = State2#state{is_leader = 'false'},
    {'ok', State3}.


handle_leader_call({'cancel', Name=?NE_BINARY}, From, State, Election) ->
    case pid_for_name(Name, State#state.tasks) of
        {'error', Reason} ->
            {'reply', {'error', Reason}, State};
        Pid ->
            handle_leader_call({'cancel', Pid}, From, State, Election)
    end;

handle_leader_call({'cancel', Pid}, _From, State, Election)
  when is_pid(Pid) ->
    Tasks = State#state.tasks,
    {Reply, State1} =
        case lists:keyfind(Pid, #task.pid, Tasks) of
            'false' ->
                {{'error', 'no_such_pid'}, State};
            #task{pid = Pid} ->
                'ok' = kz_task_worker:stop(Pid),
                Tasks1 = lists:keydelete(Pid, #task.pid, Tasks),
                send_tasks(Tasks1, Election),
                {'ok', State#state{tasks = Tasks1}}
        end,
    {'reply', Reply, State1};

handle_leader_call({'schedule', {Name=?NE_BINARY, TaskData}}, _From, State, Election) ->
    case lists:keymember(Name, #task.name, State#state.tasks) of
        'true' ->
            {'reply', {'error', 'already_exists'}, State};
        'false' ->
            case kz_task_worker:start_link(TaskData) of
                {'ok', Pid} ->
                    Task = #task{ name = Name
                                , pid = Pid
                                , data = TaskData
                                },
                    TaskList = [Task | State#state.tasks],
                    State1 = State#state{tasks = TaskList},
                    'ok' = send_tasks(TaskList, Election),
                    {'reply', {'ok', Pid}, State1};
                {'error', Reason} ->
                    {'reply', {'error', Reason}, State}
            end
    end;

handle_leader_call({'task_status', Name=?NE_BINARY}, From, State, Election) ->
    case pid_for_name(Name, State#state.tasks) of
        {'error', Reason} ->
            {'reply', {'error', Reason}, State};
        Pid ->
            handle_leader_call({'task_status', Pid}, From, State, Election)
    end;

handle_leader_call({'task_status', Pid}, _From, State, _Election)
  when is_pid(Pid) ->
    Status = kz_task_worker:status(Pid),
    {'reply', Status, State};

handle_leader_call('list_tasks', _From, State, _Election) ->
    Tasks = State#state.tasks,
    {'reply', Tasks, State};

handle_leader_call('remove_tasks_done', _From, State, Election) ->
    Tasks = State#state.tasks,
    Tasks1 = lists:foldl(fun remove_task_if_done/2, [], Tasks),
    State1 = State#state{tasks = Tasks1},
    'ok' = send_tasks(Tasks1, Election),
    {'reply', 'ok', State1}.


handle_leader_cast(_Request, State, _Election) ->
    {'noreply', State}.


from_leader({'tasks', Tasks}, State, _Election) ->
    State1 = State#state{tasks = Tasks},
    {'ok', State1}.


handle_DOWN(_Node, State, _Election) ->
    {'ok', State}.


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
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.


handle_cast(_Msg, State, _Election) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.


handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.


terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]),
    'ok'.


code_change(_OldVsn, State, _Election, _Extra) ->
    {'ok', State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec task_name(ne_binary()) -> binary().
task_name(Name) ->
    Stripped = kz_util:strip_binary(Name),
    kz_util:to_lower_binary(Stripped).


-spec send_tasks(tasks(), any()) -> 'ok'.
send_tasks(Tasks, Election) ->
    case amqp_leader_proc:alive(Election) -- [node()] of
	[] -> 'ok';
	Alive ->
	    Election =
                amqp_leader_proc:broadcast({'from_leader', {'tasks', Tasks}}
                                          ,Alive
                                          ,Election),
	    'ok'
    end.

-spec stop_tasks(state()) -> state().
stop_tasks(State) ->
    Tasks =
        [ case Task of
              #task{ pid = Pid} when node(Pid) == node() ->
                  'ok' = kz_task_worker:stop(Pid),
                  Task#task{pid = 'undefined'};
              _ ->
                  Task
          end
          || Task <- State#state.tasks
        ],
    State#state{tasks = Tasks}.

-spec start_tasks(state()) -> state().
start_tasks(State) ->
    Tasks =
        [case kz_task_worker:start_link(TaskData) of
             {'ok', Pid} -> Task#task{pid = Pid};
             {'error', _Reason} ->
                 lager:error("could not start task ~p: ~p ( ~p )", [Name, _Reason, TaskData]),
                 error_logger:error_report(
                   io_lib:format("could not start task ~p: ~p ( ~p )"
                                ,[Name, _Reason, TaskData])
                  ),
                 Task#task{pid = 'undefined'}
         end
        || #task{ name = Name
                , data = TaskData
                }=Task <- State#state.tasks],
    State#state{tasks = Tasks}.

remove_task_if_done(Task=#task{pid = Pid}, Acc) ->
    case kz_task_worker:status(Pid) of
	{'done', _, _} ->
	    'ok' = kz_task_worker:stop(Pid),
	    Acc;
	_ ->
	    [Task | Acc]
    end.

pid_for_name(Name, Tasks) ->
    case lists:keyfind(Name, #task.name, Tasks) of
        'false' ->
            {'error', 'no_such_name'};
        #task{pid = Pid} ->
            Pid
    end.
