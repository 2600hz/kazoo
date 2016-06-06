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
-export([help/0, help/1, help/2]).
-export([new/4
        ,start/1
        ,read/1
        ,all/0, all/1
        ,remove/1
	]).

-export([is_csv/1
        ,attachment_name/1
        ]).

%% API used by workers
-export([worker_finished/3
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
-include_lib("kazoo/src/kz_json.hrl").

-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-define(TASK_ID_SIZE, 15).
-define(A_TASK_ID, kz_util:rand_hex_binary(?TASK_ID_SIZE)).
-type task_id() :: <<_:(8*2*?TASK_ID_SIZE)>>.

-opaque task() :: #{ worker_pid => api_pid()
                   , worker_node => ne_binary() | 'undefined'
                   , account_id => ne_binary()
                   , id => task_id()
                   , category => ne_binary()
                   , action => ne_binary()
                   , submitted => gregorian_seconds() %% Times of state activation
                   , started => api_seconds()
                   , finished => api_seconds()
                   , total_rows => api_pos_integer() %% CSV rows
                   , total_rows_failed => api_non_neg_integer() %% Amount of CSV entries that crashed
                   , attachment_name => api_binary() %% Name of most up to date CSV/JSON
                   }.
-opaque tasks() :: [task()].

-type input() :: ne_binary() | kz_json:object().

-type help_error() :: {'error'
                      ,'no_categories' |
                       'unknown_category' |
                       'unknown_action'
                      }.

-export_type([task_id/0
             ,task/0, tasks/0
             ,input/0
             ,help_error/0
             ]).

-type api_category() :: ne_binary().

-record(state, { tasks = [] :: tasks()
               , apis = #{} :: #{api_category() => #{ne_binary() => kz_json:object()}}
               , apps = #{} :: #{api_category() => ne_binary()}
               , nodes = #{} :: #{api_category() => ne_binary()}
               , modules = #{} :: #{api_category() => module()}
               }).
-type state() :: #state{}.

-define(REPLY(State, Value), {'reply', Value, State}).
-define(REPLY_FOUND(State, TaskJObj), {'reply', {'ok', TaskJObj}, State}).
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
-spec help() -> {'ok', kz_json:object()} |
                help_error().
help() ->
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
-spec help(ne_binary()) -> {'ok', kz_json:object()} |
                           help_error().
help(Category=?NE_BINARY) ->
    gen_server:call(?SERVER, {'help', Category}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec help(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                        help_error().
help(Category=?NE_BINARY, Action=?NE_BINARY) ->
    gen_server:call(?SERVER, {'help', Category, Action}).

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
                          {'error'
                          ,'not_found' |
                           'already_started' |
                           'no_categories' |
                           any()
                          }.
start(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'start_task', TaskId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary(), ne_binary(), ne_binary(), input()) -> {'ok', kz_json:object()} |
                                                             help_error() |
                                                             {'error', kz_json:object()}.
new(?MATCH_ACCOUNT_RAW(_)=AccountId, Category=?NE_BINARY, Action=?NE_BINARY, Input) ->
    case help(Category, Action) of
        {'error', _R}=E ->
            lager:debug("adding task ~s ~s failed: ~p", [Category, Action, _R]),
            E;
        {'ok', API} ->
            lager:debug("task ~s ~s matched api ~s", [Category, Action, kz_json:encode(API)]),
            case find_input_errors(API, Input) of
                Errors when Errors =/= #{} ->
                    JObj = kz_json:from_list(
                             props:filter_empty(
                               maps:to_list(Errors))),
                    {'error', JObj};
                _ ->
                    gen_server:call(?SERVER, {'new', AccountId, Category, Action})
            end
    end.

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
-spec attachment_name(task_id()) -> ne_binary() | 'undefined'.
attachment_name(TaskId=?NE_BINARY) ->
    case task_by_id(TaskId) of
        [#{attachment_name := Name}]
        when Name =/= 'undefined' -> Name;
        _ -> 'undefined'
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
-spec worker_finished(task_id(), pos_integer(), non_neg_integer()) -> 'ok'.
worker_finished(TaskId=?NE_BINARY, TotalRows, TotalErrors)
  when is_integer(TotalRows), is_integer(TotalErrors) ->
    gen_server:cast(?SERVER, {'worker_terminated', TaskId, TotalRows, TotalErrors}).


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
    _ = process_flag('trap_exit', 'true'),
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
    JObj =
        kz_json:from_list(
          [ {Category, kz_json:from_list(maps:to_list(Actions))}
            || {Category, Actions} <- maps:to_list(APIs)
          ]),
    ?REPLY(State1, JObj);

handle_call({'help', _}, _From, State=#state{apis = APIs})
  when APIs == #{} ->
    ?REPLY(State, {'error', 'no_categories'});
handle_call({'help', _, _}, _From, State=#state{apis = APIs})
  when APIs == #{} ->
    ?REPLY(State, {'error', 'no_categories'});
%%FIXME: upgrade OTP http://stackoverflow.com/a/23107510/1418165
handle_call({'help', Category}, _From, State=#state{apis = Categories}) ->
    case maps:get(Category, Categories, 'undefined') of
        'undefined' -> ?REPLY(State, {'error', 'unknown_category'});
        Actions ->
            JObj = kz_json:from_list(maps:to_list(Actions)),
            ?REPLY_FOUND(State, JObj)
    end;
handle_call({'help', Category, Action}, _From, State=#state{apis = Categories}) ->
    case maps:get(Category, Categories, 'undefined') of
        'undefined' -> ?REPLY(State, {'error', 'unknown_category'});
        Actions ->
            case maps:get(Action, Actions, 'undefined') of
                'undefined' -> ?REPLY(State, {'error', 'unknown_action'});
                API -> ?REPLY_FOUND(State, API)
            end
    end;

handle_call({'new', AccountId, Category, Action}, _From, State) ->
    Task = #{ worker_pid => 'undefined'
            , worker_node => 'undefined'
            , account_id => AccountId
            , id => ?A_TASK_ID
            , category => Category
            , action => Action
            , submitted => kz_util:current_tstamp()
            , started => 'undefined'
            , finished => 'undefined'
            , total_rows => 'undefined'
            , total_rows_failed => 'undefined'
            },
    {'ok', _JObj} = Ok = save_task(Task),
    lager:debug("created task ~s", [kz_json:encode(_JObj)]),
    ?REPLY(State, Ok);

handle_call({'start_task', _TaskId}, _From, State=#state{apis = APIs})
  when APIs == #{} ->
    lager:error("no categories starting task ~s: app was probably just restarted, run discovery again"
               ,[_TaskId]),
    ?REPLY(State, {'error', 'no_categories'});
handle_call({'start_task', TaskId}, _From, State) ->
    %% Running tasks are stored in server State.
    %% They are then promptly removed.
    %% Rationale is to rely on the task document most.
    case task_by_id(TaskId, State) of
        [] ->
            case task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] -> handle_call_start_task(Task, State)
            end;
        [#{started := Started}]
          when Started /= 'undefined' ->
            ?REPLY(State, {'error', 'already_started'})
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
                    ?REPLY_FOUND(State, to_public_json(Task))
            end;
        [Task = #{worker_pid := Pid}] ->
            case is_processing(Task) of
                'true' -> ?REPLY(State, {'error', 'task_running'});
                'false' ->
                    _ = kz_task_worker:stop(Pid),
                    State1 = remove_task(TaskId, State),
                    ?REPLY_FOUND(State1, to_public_json(Task))
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
handle_cast({'worker_terminated', TaskId, TotalRows, TotalErrors}, State) ->
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{ finished => kz_util:current_tstamp()
                 , total_rows => TotalRows
                 , total_rows_failed => TotalErrors
                 },
    {'ok', _JObj} = save_task(Task1),
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

%% @private
-spec is_csv(binary()) -> boolean().
is_csv(<<>>) -> 'false';
is_csv(CSV) when is_binary(CSV) ->
    ThrowBad = fun (Row, -1) -> length(Row);
                   (Row, Acc) ->
                       case length(Row) of
                           Acc -> Acc;
                           _ -> throw('bad_csv')
                       end
               end,
    try
        ecsv:process_csv_binary_with(CSV, ThrowBad, -1),
        'true'
    catch
        'throw':'bad_csv' -> 'false'
    end.

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

-spec ensure_save_task(task()) -> {'ok', kz_json:object()} |
                                  {'error', any()}.
ensure_save_task(Task = #{id := _TaskId}) ->
    case kz_datamgr:ensure_saved(?KZ_TASKS_DB, to_json(Task)) of
        {'ok', Doc} -> {'ok', to_public_json(from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to ensure_save ~s in ~s: ~p", [_TaskId, ?KZ_TASKS_DB, _R]),
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

-spec handle_call_start_task(task(), state()) -> ?REPLY(state(), Response) when
      Response :: {'ok', kz_json:object()} |
                  {'error', any()}.
handle_call_start_task(Task=#{ id := TaskId
                             , account_id := AccountId
                             , category := Category
                             , action := Action
                             , attachment_name := AName
                             }
                      ,State=#state{ apis = APIs
                                   , nodes = Nodes
                                   , modules = Modules
                                   , apps = Apps
                                   }
                      ) ->
    lager:info("about to start task ~s: ~s ~s using ~s", [TaskId, Category, Action, AName]),
    API = maps:get(Action, maps:get(Category, APIs)),
    lager:debug("API ~s", [kz_json:encode(API)]),
    Node = maps:get(Category, Nodes),
    lager:debug("node ~p", [Node]),
    Module = maps:get(Category, Modules),
    lager:debug("module ~p", [Module]),
    lager:debug("app ~s", [maps:get(Category, Apps)]),
    Function = kz_util:to_atom(Action, 'true'),
    Fields = mandatory(API) ++ optional(API),
    ExtraArgs = [{'auth_account_id', AccountId}
                ],
    %% Task needs to run where App is started.
    try erlang:spawn_link(kz_util:to_atom(Node, 'true')
                         ,'kz_task_worker'
                         ,'start_link'
                         ,[TaskId, Module, Function, ExtraArgs, Fields, AName]
                         )
    of
        Pid ->
            Task1 = Task#{ started => kz_util:current_tstamp()
                         , worker_pid => Pid
                         , worker_node => Node %%FIXME: start worker on Node for real
                         },
            JObj =
                case save_task(Task1) of
                    {'ok', TaskJObj} -> TaskJObj;
                    {'error', 'conflict'} ->
                        lager:debug("FIXME using ensure_saved for task ~s", [TaskId]),
                        {'ok', TaskJObj} = ensure_save_task(Task1),
                        TaskJObj
                end,
            State1 = add_task(Task1, remove_task(TaskId, State)),
            ?REPLY_FOUND(State1, JObj)
    catch
        _E:_R ->
            lager:error("worker failed starting ~s: ~p", [TaskId, _R]),
            ?REPLY(State, {'error', _R})
    end.

-spec from_json(kz_json:object()) -> task().
from_json(Doc) ->
    #{ worker_pid => 'undefined'
     , worker_node => kz_json:get_value(?PVT_WORKER_NODE, Doc)
     , account_id => kz_json:get_value(?PVT_ACCOUNT_ID, Doc)
     , id => kz_doc:id(Doc)
     , category => kz_json:get_value(?PVT_CATEGORY, Doc)
     , action => kz_json:get_value(?PVT_ACTION, Doc)
     , submitted => kz_json:get_value(?PVT_SUBMITTED_AT, Doc)
     , started => kz_json:get_value(?PVT_STARTED_AT, Doc)
     , finished => kz_json:get_value(?PVT_FINISHED_AT, Doc)
     , total_rows => kz_json:get_value(?PVT_TOTAL_ROWS, Doc)
     , total_rows_failed => kz_json:get_value(?PVT_TOTAL_ROWS_FAILED, Doc)
     , attachment_name => kz_doc:latest_attachment_id(Doc)
     }.

-spec to_json(task()) -> kz_json:object().
to_json(#{id := TaskId
         ,worker_node := Node
         ,account_id := AccountId
         ,category := Category
         ,action := Action
         ,submitted := Submitted
         ,started := Started
         ,finished := Finished
         ,total_rows := TotalRows
         ,total_rows_failed := TotalErrors
         } = Task) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"_id">>, TaskId}
        ,{?PVT_TYPE, ?KZ_TASKS_DOC_TYPE}
        ,{?PVT_WORKER_NODE, Node}
        ,{?PVT_ACCOUNT_ID, AccountId}
        ,{?PVT_CATEGORY, Category}
        ,{?PVT_ACTION, Action}
        ,{?PVT_SUBMITTED_AT, Submitted}
        ,{?PVT_STARTED_AT, Started}
        ,{?PVT_FINISHED_AT, Finished}
        ,{?PVT_TOTAL_ROWS, TotalRows}
        ,{?PVT_TOTAL_ROWS_FAILED, TotalErrors}
        ,{?PVT_TOTAL_ROWS_SUCCEEDED, success_count(TotalRows, TotalErrors)}
        ,{?PVT_STATUS, status(Task)}
        ])).

-spec to_public_json(task()) -> kz_json:object().
to_public_json(Task) ->
    Doc = to_json(Task),
    JObj =
        kz_json:from_list(
          props:filter_undefined(
            [{<<"id">>, kz_doc:id(Doc)}
            ,{<<"node">>, kz_json:get_value(?PVT_WORKER_NODE, Doc)}
            ,{<<"account_id">>, kz_json:get_value(?PVT_ACCOUNT_ID, Doc)}
            ,{<<"category">>, kz_json:get_value(?PVT_CATEGORY, Doc)}
            ,{<<"action">>, kz_json:get_value(?PVT_ACTION, Doc)}
            ,{<<"submit_timestamp">>, kz_json:get_value(?PVT_SUBMITTED_AT, Doc)}
            ,{<<"start_timestamp">>, kz_json:get_value(?PVT_STARTED_AT, Doc)}
            ,{<<"end_timestamp">>, kz_json:get_value(?PVT_FINISHED_AT, Doc)}
            ,{<<"failure_count">>, kz_json:get_value(?PVT_TOTAL_ROWS_FAILED, Doc)}
            ,{<<"success_count">>, kz_json:get_value(?PVT_TOTAL_ROWS_SUCCEEDED, Doc)}
            ,{<<"status">>, kz_json:get_value(?PVT_STATUS, Doc)}
            ])),
    kz_json:set_value(<<"_read_only">>, JObj, kz_json:new()).

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

%% @private
-spec success_count(api_pos_integer(), api_non_neg_integer()) -> api_non_neg_integer().
success_count('undefined', _) -> 'undefined';
success_count(_, 'undefined') -> 'undefined';
success_count(TotalRows, TotalErrors) ->
    TotalRows - TotalErrors.

%% @private
-spec status(task()) -> api_binary().
status(#{started := 'undefined'}) ->
    ?STATUS_PENDING;
status(#{started := Started
        ,finished := 'undefined'
        })
  when Started /= 'undefined' ->
    ?STATUS_EXECUTING;
status(#{finished := Finished
        ,total_rows_failed := 0
        })
  when Finished /= 'undefined' ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := Total
        ,total_rows_failed := Total
        })
  when Finished /= 'undefined',
       is_integer(Total) ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows_failed := TotalErrors
        })
  when Finished /= 'undefined',
       TotalErrors /= 0 ->
    ?STATUS_PARTIAL;
status(_Task) ->
    lager:error("impossible task ~p", [_Task]),
    'undefined'.

-type m_apis() :: {map(), map(), map(), map()}.
-spec parse_apis(kz_json:objects()) -> m_apis().
parse_apis(JObjs) ->
    Acc0 = {#{}, #{}, #{}, #{}},
    lists:foldl(fun parse_apis_fold/2, Acc0, JObjs).

-spec parse_apis_fold(kz_json:object(), m_apis()) -> m_apis().
parse_apis_fold(JObj, {Apps, Nodes, Modules, APIs}) ->
    APICategory = kz_json:get_value(<<"Tasks-Category">>, JObj),
    App = kz_json:get_value(<<"App-Name">>, JObj),
    Module = kz_json:get_value(<<"Tasks-Module">>, JObj),
    TasksProvided = kz_json:get_value(<<"Tasks">>, JObj),
    lager:debug("verifying ~s (~s) tasks have unique fields", [APICategory, App]),
    _ = kz_json:map(fun verify_unicity_map/2, TasksProvided),
    { Apps#{APICategory => App}
      %%TODO: use a set of nodes
    , Nodes#{APICategory => kz_api:node(JObj)}
    , Modules#{APICategory => kz_util:to_atom(Module, 'true')}
    , APIs#{APICategory => maps:from_list(kz_json:to_proplist(TasksProvided))}
    }.

-spec verify_unicity_map(ne_binary(), kz_json:object()) -> any().
verify_unicity_map(_Action, API) ->
    Fields0 = mandatory(API) ++ optional(API),
    Fields = [kz_util:to_lower_binary(Field) || Field <- Fields0],
    case length(lists:usort(Fields)) == length(Fields)
        andalso Fields == Fields0
    of
        'true' -> 'ok';
        'false' ->
            lager:error("action '~s' has duplicate or uppercase fields", [_Action])
    end.

-spec mandatory(kz_json:object()) -> ne_binaries().
mandatory(APIJObj) ->
    kz_json:get_list_value(?API_MANDATORY, APIJObj, []).

-spec optional(kz_json:object()) -> ne_binaries().
optional(APIJObj) ->
    kz_json:get_list_value(?API_OPTIONAL, APIJObj, []).

-spec find_input_errors(kz_json:object(), input()) -> map().
find_input_errors(API, Input=?NE_BINARY) ->
    {Fields, InputData} = kz_csv:take_row(Input),
    Mandatory = mandatory(API),
    Errors = find_API_errors(API, Mandatory, Fields),
    %% Stop here if there is no Mandatory fields to check against.
    case Mandatory of
        [] -> Errors;
        _ ->
            IsMandatory = [lists:member(Field, Mandatory) || Field <- Fields],
            Unsets =
                fun (Row, Es) ->
                        case are_mandatories_unset(IsMandatory, Row) of
                            'false' -> Es;
                            'true' -> [list_to_binary(string:join(Row, ",")) | Es]
                        end
                end,
            case ecsv:process_csv_binary_with(InputData, Unsets, []) of
                {'ok', []} -> Errors;
                {'ok', MMVs} -> Errors#{<<"missing_mandatory_values">> => lists:reverse(MMVs)}
            end
    end;

find_input_errors(API, InputRecord) ->
    %%NOTE: assumes first record has all the fields that all the other records will ever need set
    Fields = kz_json:get_keys(hd(InputRecord)),
    Mandatory = mandatory(API),
    Errors = find_API_errors(API, Mandatory, Fields),
    %% Stop here if there is no Mandatory fields to check against.
    case Mandatory of
        [] -> Errors;
        _ ->
            CheckJObjValues =
                fun (JObj, Es) ->
                        IsUnset = ['undefined' == kz_json:get_ne_binary_value(Key, JObj)
                                   || Key <- kz_json:get_keys(JObj),
                                      lists:member(Key, Mandatory)
                                  ],
                        case lists:foldl(fun erlang:'or'/2, 'false', IsUnset) of
                            'false' -> Es;
                            'true' -> [JObj | Es]
                        end
                end,
            case lists:foldl(CheckJObjValues, [], InputRecord) of
                [] -> Errors;
                MMVs -> Errors#{<<"missing_mandatory_values">> => lists:reverse(MMVs)}
            end
    end.

find_API_errors(API, Mandatory, Fields) ->
    Routines =
        [fun (Errors) ->
                 case Mandatory -- Fields of
                     [] -> Errors;
                     Missing -> Errors#{<<"missing_mandatory_fields">> => Missing}
                  end
         end
        ,fun (Errors) ->
                 case Fields -- (Mandatory ++ optional(API)) of
                     [] -> Errors;
                     Unknown -> Errors#{<<"unknown_fields">> => Unknown}
                 end
         end],
    lists:foldl(fun (F, Errors) -> F(Errors) end, #{}, Routines).

-spec are_mandatories_unset(nonempty_list(boolean()), nonempty_list(ne_binary())) -> boolean().
are_mandatories_unset(IsMandatory, Row) ->
    MapF = fun (Mandatory, Value) ->
                   %% Note: ecsv's empty cell is "".
                   Mandatory andalso "" == Value
           end,
    RedF = fun erlang:'or'/2,
    lists:foldl(RedF, 'false', lists:zipwith(MapF, IsMandatory, Row)).

%%% End of Module.
