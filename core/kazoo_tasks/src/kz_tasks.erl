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
-export([available/0]).
-export([new/4
        ,start/1
        ,read/1
        ,all/0, all/1
        ,remove/1
	]).

%% API used by workers
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
                   , worker_node => ne_binary() | 'undefined'
                   , account_id => ne_binary()
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

-type api_category() :: ne_binary().

-record(state, { tasks = [] :: tasks()
               , apis = kz_json:new() :: kz_json:object()
               , apps = #{} :: #{api_category() => ne_binary()}
               , nodes = #{} :: #{api_category() => ne_binary()}
               , modules = #{} :: #{api_category() => module()}
               }).
-type state() :: #state{}.

-define(REPLY_FOUND(TaskJObj, State), {'reply', {'ok', TaskJObj}, State}).
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
-spec available() -> kz_json:object().
available() ->
    CollectUntil = {kz_util:to_atom(?APP_NAME), fun kapi_tasks:help_resp_v/1, 'true', 'true'},
    case kz_amqp_worker:call_collect(kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                    ,fun kapi_tasks:publish_help_req/1
                                    ,CollectUntil
                                    )
    of
        {'ok', JObjs} ->
            lager:debug("help_req got ~p replies", [length(JObjs)]),
            {Apps, Nodes, Modules, APIs} = parse_apis(JObjs),
            gen_server:call(?SERVER, {'replace_APIs', Apps, Nodes, Modules, APIs});
        {'timeout', []} ->
            lager:debug("no app replied to help_req"),
            kz_json:new();
        {'error', _Reason} ->
            lager:error("error in broadcasted help_req: ~p", [_Reason]),
            kz_json:new()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec all() -> kz_json:objects().
all() ->
    view([]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec all(ne_binary()) -> kz_json:objects().
all(AccountId=?NE_BINARY) ->
    view([{'key', AccountId, 'undefined'}
         ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(task_id()) -> {'ok', kz_json:object()} |
                          {'error', 'not_found' | 'already_started'}.
start(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'start_task', TaskId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary(), module(), atom(), list()) -> {'ok', kz_json:object()} |
                                                    {'error', any()}.
new(?MATCH_ACCOUNT_RAW(_)=AccountId, M, F, A)
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
                    , worker_node => 'undefined'
                    , account_id => AccountId
                    , id => ?A_TASK_ID
                    , m => M
                    , f => F
                    , a => A
                    , submitted => kz_util:current_tstamp()
                    , started => 'undefined'
                    , finished => 'undefined'
                    , failed => 'undefined'
                    },
            {'ok', _JObj} = Ok = save_task(Task),
            Ok
    end;
new(_, M, _, _) when not is_atom(M) ->
    {'error', {'bad_module', M}};
new(_, _, F, _) when not is_atom(F) ->
    {'error', {'bad_function', F}};
new(_, _, _, A) when not is_list(A) ->
    {'error', {'bad_list', A}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read(task_id()) -> {'ok', kz_json:object()} |
                         {'error', 'not_found'}.
read(TaskId=?NE_BINARY) ->
    case task_by_id(TaskId) of
        [Task] -> {'ok', to_public_json(Task)};
        [] -> {'error', 'not_found'}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove(task_id()) -> {'ok', kz_json:object()} |
                           {'error', 'not_found' | 'task_running'}.
remove(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'remove_task', TaskId}).

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
    lager:info("ensuring db ~s exists", [?KZ_TASKS_DB]),
    'true' = kz_datamgr:db_create(?KZ_TASKS_DB),
    kz_datamgr:revise_views_from_folder(?KZ_TASKS_DB, kz_util:to_atom(?APP_NAME)),
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
handle_call({'replace_APIs', Apps, Nodes, Modules, APIs}, _From, State) ->
    State1 = State#state{ apis = APIs
                        , apps = Apps
                        , nodes = Nodes
                        , modules = Modules
                        },
    {'reply', APIs, State1};

handle_call({'start_task', TaskId}, _From, State) ->
    case task_by_id(TaskId, State) of
        [] ->
            case task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] -> handle_call_start_task(Task, State)
            end;
        [#{started := Started}]
          when Started /= 'undefined' ->
            {'reply', {'error', 'already_started'}, State};
        [Task] ->
            handle_call_start_task(Task, State)
    end;

handle_call({'remove_task', TaskId}, _From, State) ->
    case task_by_id(TaskId, State) of
        [] ->
            case task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] ->
                    %%TODO: if app shutdown before task termination
                    %%this would be the place to attempt some cleanup
                    {'ok', _} = kz_datamgr:del_doc(?KZ_TASKS_DB, TaskId),
                    ?REPLY_FOUND(to_public_json(Task), State)
            end;
        [Task = #{worker_pid := Pid}] ->
            case is_processing(Task) of
                'true' -> {'reply', {'error', 'task_running'}, State};
                'false' ->
                    _ = kz_task_worker:stop(Pid),
                    State1 = remove_task(TaskId, State),
                    ?REPLY_FOUND(to_public_json(Task), State1)
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
    {'ok', _} = save_task(Task2),
    State1 = remove_task(TaskId, State),
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

-spec compare_tasks(kz_json:object(), kz_json:object()) -> boolean().
compare_tasks(JObjA, JObjB) ->
    kz_json:get_value(?PVT_SUBMITTED_AT, JObjA)
        =<
        kz_json:get_value(?PVT_SUBMITTED_AT, JObjB).

-spec save_task(task()) -> {'ok', kz_json:object()} |
                           {'error', any()}.
save_task(Task = #{id := _TaskId}) ->
    case kz_datamgr:save_doc(?KZ_TASKS_DB, to_json(Task)) of
        {'ok', Doc} -> {'ok', to_public_json(from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to save ~s in ~s: ~p", [_TaskId, ?KZ_TASKS_DB, _R]),
            E
    end.

-spec view(list()) -> kz_json:objects().
view(ViewOptions) ->
    case kz_datamgr:get_results(?KZ_TASKS_DB, ?KZ_TASKS_BY_ACCOUNT, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} ->
            Found = [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
            lists:sort(fun compare_tasks/2, Found);
        {'error', _R} ->
            lager:debug("error viewing tasks (~p): ~p", [ViewOptions, _R]),
            []
    end.

-spec task_by_id(task_id()) -> [task()].
task_by_id(TaskId) ->
    case kz_datamgr:open_cache_doc(?KZ_TASKS_DB, TaskId) of
        {'ok', JObj} -> [from_json(JObj)];
        {'error', _R} ->
            lager:error("failed to open ~s in ~s: ~p", [TaskId, ?KZ_TASKS_DB, _R]),
            []
    end.

-spec task_by_id(task_id(), state()) -> [task()].
task_by_id(TaskId, State) ->
    [T || T=#{id := Id} <- State#state.tasks,
          TaskId == Id
    ].

-spec handle_call_start_task(task(), state()) -> any().
handle_call_start_task(Task=#{ id := TaskId
                             , m := M
                             , f := F
                             , a := A
                             }, State) ->
    case kz_task_worker:start_link(TaskId, M, F, A) of
        {'ok', Pid} ->
            Task1 = Task#{ started => kz_util:current_tstamp()
                         , worker_pid => Pid
                         , worker_node => kz_util:to_binary(node())
                         },
            {'ok', JObj} = save_task(Task1),
            State1 = add_task(Task1, remove_task(TaskId, State)),
            ?REPLY_FOUND(JObj, State1);
        {'error', _R}=E ->
            lager:error("wroker failed starting ~s: ~p", [TaskId, _R]),
            {'reply', E, State}
    end.

-spec from_json(kz_json:object()) -> task().
from_json(Doc) ->
    #{ worker_pid => 'undefined'
     , worker_node => 'undefined'
     , account_id => kz_json:get_value(?PVT_ACCOUNT_ID, Doc)
     , id => kz_doc:id(Doc)
     , m => kz_util:to_atom(kz_json:get_value(?PVT_MODULE, Doc), 'true')
     , f => kz_util:to_atom(kz_json:get_value(?PVT_FUNCTION, Doc), 'true')
     , a => kz_json:get_list_value(?PVT_ARGUMENTS, Doc)
     , submitted => kz_json:get_value(?PVT_SUBMITTED_AT, Doc)
     , started => kz_json:get_value(?PVT_STARTED_AT, Doc)
     , finished => kz_json:get_value(?PVT_FINISHED_AT, Doc)
     , failed => kz_json:get_value(?PVT_FAILED_AT, Doc)
     }.

-spec to_json(task()) -> kz_json:object().
to_json(#{id := TaskId
         ,worker_node := Node
         ,account_id := AccountId
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
        [{<<"_id">>, TaskId}
        ,{?PVT_TYPE, ?KZ_TASKS_DOC_TYPE}
        ,{?PVT_WORKER_NODE, Node}
        ,{?PVT_ACCOUNT_ID, AccountId}
        ,{?PVT_MODULE, kz_util:to_binary(M)}
        ,{?PVT_FUNCTION, kz_util:to_binary(F)}
        ,{?PVT_ARGUMENTS, A}
        ,{?PVT_SUBMITTED_AT, Submitted}
        ,{?PVT_STARTED_AT, Started}
        ,{?PVT_FINISHED_AT, Finished}
        ,{?PVT_ERROR, MaybeError}
        ,{?PVT_IS_TERMINATED, IsProcessed}
        ,{?PVT_IS_SUCCESS, IsSuccess}
        ,{?PVT_RAN_FOR, time_ran(Task)}
        ])).

-spec to_public_json(task()) -> kz_json:object().
to_public_json(Task) ->
    Doc = to_json(Task),
    kz_json:from_list(
      props:filter_undefined(
        [{<<"id">>, kz_doc:id(Doc)}
        ,{<<"node">>, kz_json:get_value(?PVT_WORKER_NODE, Doc)}
        ,{<<"account_id">>, kz_json:get_value(?PVT_ACCOUNT_ID, Doc)}
        ,{<<"module">>, kz_json:get_value(?PVT_MODULE, Doc)}
        ,{<<"function">>, kz_json:get_value(?PVT_FUNCTION, Doc)}
        ,{<<"arguments">>, kz_json:get_value(?PVT_ARGUMENTS, Doc)}
        ,{<<"submitted_at">>, kz_json:get_value(?PVT_SUBMITTED_AT, Doc)}
        ,{<<"started_at">>, kz_json:get_value(?PVT_STARTED_AT, Doc)}
        ,{<<"ended_at">>, kz_json:get_value(?PVT_FINISHED_AT, Doc)}
        ,{<<"error">>, kz_json:get_value(?PVT_ERROR, Doc)}
        ,{<<"is_terminated">>, kz_json:get_value(?PVT_IS_TERMINATED, Doc)}
        ,{<<"is_success">>, kz_json:get_value(?PVT_IS_SUCCESS, Doc)}
        ,{<<"ran_for">>, kz_json:get_value(?PVT_RAN_FOR, Doc)}
        ])).

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

-type m_apis() :: {map(), map(), map(), kz_json:object()}.
-spec parse_apis(kz_json:objects()) -> m_apis().
parse_apis(JObjs) ->
    Acc0 = {#{}, #{}, #{}, kz_json:new()},
    lists:foldl(fun parse_apis_fold/2, Acc0, JObjs).

-spec parse_apis_fold(kz_json:object(), m_apis()) -> m_apis().
parse_apis_fold(JObj, {Apps, Nodes, Modules, APIs}) ->
    APICategory = kz_json:get_value(<<"Tasks-For">>, JObj),
    App = kz_json:get_value(<<"App-Name">>, JObj),
    Node = kz_json:get_value(<<"Tasks-Node">>, JObj),
    Module = kz_json:get_value(<<"Tasks-Module">>, JObj),
    TasksProvided = kz_json:get_value(<<"Tasks">>, JObj),
    { Apps#{APICategory => App}
    , Nodes#{APICategory => Node}
    , Modules#{APICategory => kz_util:to_atom(Module, 'true')}
    , kz_json:set_value(APICategory, TasksProvided, APIs)
    }.

%%% End of Module.
