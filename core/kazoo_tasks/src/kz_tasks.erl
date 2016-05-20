%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Schedule one-off tasks only once per cluster
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_tasks).
-behaviour(gen_server).

%% Public API
-export([start_link/0]).
-export([new/3
        ,start/1
        ,read/1
        ,all/0
        ,remove/1
	]).

%% API use by workers
-export([worker_finished/1
        ,worker_failed/2
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_cast/2
        ,handle_call/3
        ,handle_info/2
        ,code_change/3
        ,terminate/2
        ]).

-include("tasks.hrl").

-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-define(TASK_ID_SIZE, 15).
-define(A_TASK_ID, kz_util:rand_hex_binary(?TASK_ID_SIZE)).
-type task_id() :: <<_:(8*2*?TASK_ID_SIZE)>>.

-opaque task() :: #{ worker_pid => api_pid()
                   , id => task_id()
                   , m => module()
                   , f => atom()
                   , a => list()
                   , submitted => gregorian_seconds() %% Times of state activation
                   , started => api_seconds()
                   , finished => api_seconds()
                   , failed => api_binary() %% Error that occured during processing
                   }.
-opaque tasks() :: [task()].

-export_type([task_id/0
             ,task/0, tasks/0
             ]).


-record(state, { tasks = [] :: tasks()
               }).
-type state() :: #state{}.

-define(REPLY_FOUND(State, Task), {'reply', {'ok', Task}, State}).
-define(REPLY_NOT_FOUND(State), {'reply', {'error', 'not_found'}, State}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}} ->
            'true' = link(Pid),
            {'ok', Pid};
        Other -> Other
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec all() -> kz_json:objects().
all() ->
    case gen_server:call(?SERVER, 'get_tasks') of
        [] -> [];
        Tasks ->
            Sorted = lists:sort(fun compare_tasks/2, Tasks),
            [task_to_public_json(Task) || Task <- Sorted]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(task_id()) -> {'ok', kz_json:object()} |
                          {'error', 'not_found' | 'already_started'}.
start(TaskId) ->
    case gen_server:call(?SERVER, {'start_task', TaskId}) of
        {'ok', Task} -> {'ok', task_to_public_json(Task)};
        {'error', _R}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(module(), atom(), list()) -> {'ok', task_id()} |
                                       {'error', any()}.
new(M, F, A)
  when is_atom(M),
       is_atom(F),
       is_list(A) ->
    Arity = length(A),
    case check_MFa(M, F, Arity) of
        {'error', _R}=E ->
            lager:debug("checking ~s:~s/~p failed: ~p", [M, F, Arity, _R]),
            E;
        'ok' ->
            Task = #{ worker_pid => 'undefined'
                    , id => ?A_TASK_ID
                    , m => M
                    , f => F
                    , a => A
                    , submitted => kz_util:current_tstamp()
                    , started => 'undefined'
                    , finished => 'undefined'
                    , failed => 'undefined'
                    },
            gen_server:call(?SERVER, {'add_task', Task})
    end;
new(M, _, _) when not is_atom(M) ->
    {'error', {'bad_module', M}};
new(_, F, _) when not is_atom(F) ->
    {'error', {'bad_function', F}};
new(_, _, A) when not is_list(A) ->
    {'error', {'bad_list', A}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read(task_id()) -> {'ok', kz_json:object()} |
                         {'error', 'not_found'}.
read(TaskId=?NE_BINARY) ->
    case gen_server:call(?SERVER, {'get_task_by_id', TaskId}) of
        {'ok', Task} -> {'ok', task_to_public_json(Task)};
        {'error', _R}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove(task_id()) -> {'ok', kz_json:object()} |
                           {'error', 'not_found' | 'task_running'}.
remove(TaskId=?NE_BINARY) ->
    case gen_server:call(?SERVER, {'remove_task', TaskId}) of
        {'ok', Task} -> {'ok', task_to_public_json(Task)};
        {'error', _R}=E -> E
    end.

%%%===================================================================
%%% Worker API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_finished(task_id()) -> 'ok'.
worker_finished(TaskId=?NE_BINARY) ->
    gen_server:cast(?SERVER, {'set_terminated', TaskId, 'false', 'undefined'}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_failed(task_id(), binary()) -> 'ok'.
worker_failed(TaskId=?NE_BINARY, Reason)
  when is_binary(Reason) ->
    gen_server:cast(?SERVER, {'set_terminated', TaskId, 'true', Reason}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call('get_tasks', _From, State) ->
    Tasks = State#state.tasks,
    {'reply', Tasks, State};

handle_call({'add_task', Task=#{id := TaskId}}, _From, State) ->
    Tasks = [Task | State#state.tasks],
    State1 = State#state{tasks = Tasks},
    {'reply', {'ok', TaskId}, State1};

handle_call({'start_task', TaskId}, _From, State) ->
    case task_by_id(TaskId, State) of
        [] -> ?REPLY_NOT_FOUND(State);
        [#{started := Started}]
          when Started /= 'undefined' ->
            {'reply', {'error', 'alread_started'}, State};
        [#{m := M, f := F, a := A} = Task] ->
            case kz_task_worker:start_link(TaskId, M, F, A) of
                {'ok', Pid} ->
                    Task1 = Task#{ started => kz_util:current_tstamp()
                                 , worker_pid => Pid
                                 },
                    State1 = add_task(Task1, remove_task(TaskId, State)),
                    ?REPLY_FOUND(State1, Task1);
                {'error', _R}=E ->
                    {'reply', E, State}
            end
    end;

handle_call({'get_task_by_id', TaskId}, _From, State) ->
    case task_by_id(TaskId, State) of
        [Task] -> ?REPLY_FOUND(State, Task);
        [] -> ?REPLY_NOT_FOUND(State)
    end;

handle_call({'remove_task', TaskId}, _From, State) ->
    case task_by_id(TaskId, State) of
        [] -> ?REPLY_NOT_FOUND(State);
        [Task] ->
            case is_processing(Task) of
                'true' -> {'reply', {'error', 'task_running'}, State};
                'false' ->
                    State1 = remove_task(TaskId, State),
                    ?REPLY_FOUND(State1, Task)
            end
    end;

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'set_terminated', TaskId, IsFailed, MaybeReason}, State) ->
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{ finished => kz_util:current_tstamp()
                 },
    Task2 =
        case IsFailed of
            'true' -> Task1#{failed => MaybeReason};
            'false' -> Task1
        end,
    State1 = add_task(Task2, remove_task(TaskId, State)),
    {'noreply', State1};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec task_by_id(task_id(), state()) -> [] | [task()].
task_by_id(TaskId, State) ->
    [T || T=#{id := Id} <- State#state.tasks,
          TaskId == Id
    ].

-spec task_to_public_json(task()) -> kz_json:object().
task_to_public_json(#{id := TaskId
                     ,m := M
                     ,f := F
                     ,a := A
                     ,submitted := Submitted
                     ,started := Started
                     ,finished := Finished
                     ,failed := MaybeError
                     } = Task) ->
    IsProcessed =
        case is_started(Task) of
            'false' -> 'undefined';
            'true' -> is_processed(Task)
        end,
    IsSuccess =
        case IsProcessed of
            'undefined' -> 'undefined';
            'false' -> 'undefined';
            'true' -> is_success(Task)
        end,
    kz_json:from_list(
      props:filter_undefined(
        [{<<"id">>, TaskId}
        ,{<<"M">>, M}
        ,{<<"F">>, F}
        ,{<<"A">>, A}
        ,{<<"submitted_at">>, Submitted}
        ,{<<"started_at">>, Started}
        ,{<<"ended_at">>, Finished}
        ,{<<"error">>, MaybeError}
        ,{<<"is_terminated">>, IsProcessed}
        ,{<<"is_success">>, IsSuccess}
        ,{<<"ran_for">>, time_ran(Task)}
        ])).

-spec compare_tasks(task(), task()) -> boolean().
compare_tasks(#{submitted := A}, #{submitted := B}) ->
    A =< B.

-spec remove_task(task_id(), state()) -> state().
remove_task(TaskId, State) ->
    NewTasks =
        [T || T=#{id := Id} <- State#state.tasks,
              TaskId /= Id
        ],
    State#state{tasks = NewTasks}.

-spec add_task(task(), state()) -> state().
add_task(Task, State) ->
    Tasks = [Task | State#state.tasks],
    State#state{tasks = Tasks}.

%%--------------------------------------------------------------------
%% @private
%% @doc Whether a task has been started (can have finished or failed by now).
%%--------------------------------------------------------------------
-spec is_started(task()) -> boolean().
is_started(#{started := Started})
  when Started /= 'undefined' ->
    'true';
is_started(_Task) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc Whether task has been started and is still running.
%% @end
%%--------------------------------------------------------------------
-spec is_processing(task()) -> boolean().
is_processing(#{ started := Started
               , finished := Finished
               })
  when Started  /= 'undefined',
       Finished == 'undefined' ->
    'true';
is_processing(_Task) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc Whether a started task succeeded or failed (ie. has terminated).
%%--------------------------------------------------------------------
-spec is_processed(task()) -> boolean().
is_processed(#{finished := Finished})
  when Finished /= 'undefined' ->
    'true';
is_processed(_Task) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc Whether a started task ended with no error.
%%--------------------------------------------------------------------
-spec is_success(task()) -> boolean().
is_success(#{ finished := Finished
            , failed := Failed
            })
  when Finished /= 'undefined',
       Failed   == 'undefined' ->
    'true';
is_success(_Task) ->
    'false'.

-spec time_ran(task()) -> api_non_neg_integer().
time_ran(#{ started := Started
          , finished := Finished
          } = Task) ->
    case is_processed(Task) of
        'true' -> kz_util:elapsed_s(Started, Finished);
        'false' -> 'undefined'
    end.

-spec check_MFa(module(), atom(), non_neg_integer()) -> 'ok' |
                                                        {'error', any()}.
check_MFa(M, F, Arity) ->
    case kz_util:try_load_module(M) of
        'false' -> {'error', {'no_module', M}};
        M ->
            case erlang:function_exported(M, F, Arity) of
                'false' -> {'error', {'no_function', M, F, Arity}};
                'true' -> 'ok'
            end
    end.

%%% End of Module.
