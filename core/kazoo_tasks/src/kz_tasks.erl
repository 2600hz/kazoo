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
	]).

%% API use by workers
-export([worker_running/2
        ,worker_finished/1
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

-include("kz_tasks.hrl").

-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-define(TASK_ID_SIZE, 15).
-define(A_TASK_ID, kz_util:rand_hex_binary(?TASK_ID_SIZE)).
-type task_id() :: <<_:(8*2*?TASK_ID_SIZE)>>.

-opaque task() :: #{ pid => api_pid()
                   , id => task_id()
                   , m => module()
                   , f => atom()
                   , a => list()
                   , submitted => gregorian_seconds() %% Times of state activation
                   , running => api_seconds()
                   , finished => api_seconds()
                   , failed => api_binary() %% Error that occured during processing
                   }.
-opaque tasks() :: [task()].

-record(state, { tasks = [] :: tasks()
               }).
-type state() :: #state{}.

-export_type([task_id/0
             ,task/0, tasks/0
             ]).

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
-spec start(task_id()) -> 'ok' | {'error', 'not_found'}.
start(TaskId) ->
    gen_server:call(?SERVER, {'start_task', TaskId}).

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
    Task = #{ pid => 'undefined'
            , id => ?A_TASK_ID
            , m => M
            , f => F
            , a => A
            , submitted => kz_util:current_tstamp()
            , running => 'undefined'
            , finished => 'undefined'
            , failed => 'undefined'
            },
    gen_server:call(?SERVER, {'add_task', Task});
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

%%%===================================================================
%%% Worker API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_running(task_id(), pid()) -> 'ok'.
worker_running(TaskId=?NE_BINARY, Pid)
  when is_pid(Pid) ->
    gen_server:cast(?SERVER, {'set_running', TaskId, Pid}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_finished(task_id()) -> 'ok'.
worker_finished(TaskId=?NE_BINARY) ->
    gen_server:cast(?SERVER, {'set_finished', TaskId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_failed(task_id(), binary()) -> 'ok'.
worker_failed(TaskId=?NE_BINARY, Reason)
  when is_binary(Reason) ->
    gen_server:cast(?SERVER, {'set_failed', TaskId, Reason}).


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

handle_call({'add_task', Task}, _From, State) ->
    Tasks = [Task | State#state.tasks],
    State1 = State#state{tasks = Tasks},
    {'reply', {'ok', maps:get('id', Task)}, State1};

handle_call({'start_task', TaskId}, _From, State) ->
    case task_by_id(TaskId, State) of
        [] -> {'error', 'not_found'};
        [Task] ->
            #{m := M, f := F, a := A} = Task,
            case kz_task_worker:start_link(TaskId, M, F, A) of
                {'ok', _Pid} ->
                    lager:debug("started task ~s: ~p", [TaskId, _Pid]),
                    {'reply', 'ok', State};
                {'error', _R}=E ->
                    lager:debug("error starting task ~s: ~p", [TaskId, _R]),
                    {'reply', E, State}
            end
    end;

handle_call({'get_task_by_id', TaskId}, _From, State) ->
    Rep =
        case task_by_id(TaskId, State) of
            [Task] -> {'ok', Task};
            [] -> {'error', 'not_found'}
        end,
    {'reply', Rep, State};

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
handle_cast({'set_running', TaskId, Pid}, State) ->
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{ running => kz_util:current_tstamp()
                 , pid => Pid
                 },
    Tasks = (State#state.tasks -- [Task]) ++ [Task1],
    State1 = State#state{tasks = Tasks},
    {'noreply', State1};

handle_cast({'set_finished', TaskId}, State) ->
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{ finished => kz_util:current_tstamp()
                 },
    Tasks = (State#state.tasks -- [Task]) ++ [Task1],
    State1 = State#state{tasks = Tasks},
    {'noreply', State1};

handle_cast({'set_failed', TaskId, Reason}, State) ->
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{ failed => Reason
                 },
    Tasks = (State#state.tasks -- [Task]) ++ [Task1],
    State1 = State#state{tasks = Tasks},
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
    [T || T <- State#state.tasks, TaskId == maps:get('id', T)].

-spec task_to_public_json(task()) -> kz_json:object().
task_to_public_json(#{id := TaskId
                     ,m := M
                     ,f := F
                     ,a := A
                     ,submitted := Submitted
                     ,running := Running
                     ,finished := Finished
                     ,failed := Failed
                     }) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"id">>, TaskId}
        ,{<<"M">>, M}
        ,{<<"F">>, F}
        ,{<<"A">>, A}
        ,{<<"submitted">>, Submitted}
        ,{<<"running">>, Running}
        ,{<<"finished">>, Finished}
        ,{<<"failed">>, Failed}
        ])).

-spec compare_tasks(task(), task()) -> boolean().
compare_tasks(#{submitted := A}, #{submitted := B}) ->
    A =< B.

%%% End of Module.
