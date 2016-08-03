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

%%% Public API
-export([start_link/0]).
-export([help/0, help/1, help/2]).
-export([new/7
        ,start/1
        ,read/1
        ,all/0, all/1
        ,remove/1
        ]).

-export([handle_lookup_req/2
        ]).

%%% API used by workers
-export([worker_finished/4
        ,worker_error/1
        ,worker_pause/0
        ,worker_maybe_send_update/3
        ,get_output_header/2
        ,cleanup_task/3
        ]).

-export([mandatory/1
        ,optional/1
        ,input_mime/1
        ]).

%%% gen_server callbacks
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

-define(WAIT_AFTER_ROW,
        kapps_config:get_integer(?CONFIG_CAT, <<"wait_after_row_ms">>, 500)).
-define(PROGRESS_AFTER_PROCESSED,
        kapps_config:get_integer(?CONFIG_CAT, <<"send_progress_after_processed">>, 1000)).

-define(TASK_ID_SIZE, 15).
-define(A_TASK_ID, kz_util:rand_hex_binary(?TASK_ID_SIZE)).
-type task_id() :: <<_:(8*2*?TASK_ID_SIZE)>>.

-type task() :: #{ worker_pid => api_pid()
                 , worker_node => ne_binary() | 'undefined'
                 , account_id => ne_binary()
                 , auth_account_id => ne_binary()
                 , id => task_id()
                 , category => ne_binary()
                 , action => ne_binary()
                 , file_name => ne_binary() | 'undefined'
                 , created => gregorian_seconds() %% Time of task creation (PUT)
                 , started => api_seconds() %% Time of task start (PATCH)
                 , finished => api_seconds() %% Time of task finish (> started)
                 , total_rows => api_pos_integer() %% CSV rows (undefined for a noinput task)
                 , total_rows_failed => api_non_neg_integer() %% Rows that crashed or didn't return ok
                 , total_rows_succeeded => api_non_neg_integer() %% Rows that returned 'ok'
                 }.

-type input() :: ne_binary() | kz_json:objects() | 'undefined'.

-type help_error() :: {'error'
                      ,'no_categories' |
                       'unknown_category' |
                       'unknown_action'
                      }.

-export_type([task_id/0
             ,input/0
             ,help_error/0
             ]).

-type api_category() :: ne_binary().
-type map_apis() :: #{api_category() => #{ne_binary() => kz_json:object()}}.
-type map_apps() :: #{api_category() => ne_binary()}.
-type map_nodes() :: #{api_category() => ne_binary()}.
-type map_modules() :: #{api_category() => module()}.
-record(state, { tasks = [] :: [task()]
               , apis = #{} :: map_apis()
               , apps = #{} :: map_apps()
               , nodes = #{} :: map_nodes()
               , modules = #{} :: map_modules()
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
-spec help() -> kz_json:object().
help() ->
    JObjs = tasks_bindings:map(<<"tasks.help.*">>, []),
    parse_apis(JObjs).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec help(ne_binary()) -> {'ok', kz_json:object()} |
                           help_error().
help(Category=?NE_BINARY) ->
    JObjs = tasks_bindings:map(<<"tasks.help.", Category/binary>>, []),
    JObj = parse_apis(JObjs),
    case kz_json:is_empty(JObj) of
        'true' -> {'error', 'unknown_category'};
        'false' -> JObj
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec help(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                        help_error().
help(Category=?NE_BINARY, Action=?NE_BINARY) ->
    JObjs = tasks_bindings:map(<<"tasks.help.", Category/binary>>, [[Action]]),
    JObj = parse_apis(JObjs),
    case kz_json:is_empty(JObj) of
        'false' -> JObj;
        'true' ->
            case help(Category) of
                {'error', _}=Error -> Error;
                _ -> {'error', 'unknown_action'}
            end
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
    view([{'startkey', [AccountId]}
         ,{'endkey', [AccountId, kz_json:new()]}
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
-spec new(ne_binary(), ne_binary(), ne_binary(), ne_binary(), api_pos_integer(), input(), api_binary()) ->
                 {'ok', kz_json:object()} |
                 help_error() |
                 {'error', kz_json:object()}.
new(?MATCH_ACCOUNT_RAW(AuthAccountId), ?MATCH_ACCOUNT_RAW(AccountId)
   ,Category=?NE_BINARY, Action=?NE_BINARY, TotalRows, Input, CSVName)
  when is_integer(TotalRows), TotalRows > 0;
       TotalRows == 'undefined', Input == 'undefined' ->
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
                    InputName = case kz_util:is_empty(CSVName) of
                                    'true' -> 'undefined';
                                    'false' -> kz_util:to_binary(CSVName)
                                end,
                    Msg = {'new', AuthAccountId, AccountId, Category, Action, TotalRows, InputName},
                    gen_server:call(?SERVER, Msg)
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
-spec remove(task_id()) -> {'ok', kz_json:object()} |
                           {'error', 'not_found' | 'task_running'}.
remove(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'remove_task', TaskId}).


%%%===================================================================
%%% AMQP API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_lookup_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_lookup_req(JObj, _Props) ->
    'true' = kapi_tasks:lookup_req_v(JObj),
    Help =
        case {kapi_tasks:category(JObj), kapi_tasks:action(JObj)} of
            {'undefined', 'undefined'} -> help();
            {Category, 'undefined'} -> help(Category);
            {Category, Action} -> help(Category, Action)
        end,
    Resp = kz_json:from_list(
             [{<<"Help">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_lookup_resp(kz_api:server_id(JObj), Resp).


%%%===================================================================
%%% Worker API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_error(task_id()) -> 'ok'.
worker_error(TaskId=?NE_BINARY) ->
    gen_server:cast(?SERVER, {'worker_error', TaskId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_pause() -> 'ok'.
worker_pause() ->
    MS = ?WAIT_AFTER_ROW,
    lager:debug("taking a ~pms break before next row", [MS]),
    timer:sleep(MS).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_maybe_send_update(task_id(), pos_integer(), pos_integer()) -> 'ok'.
worker_maybe_send_update(TaskId, TotalSucceeded, TotalFailed) ->
    case (TotalFailed + TotalSucceeded) rem ?PROGRESS_AFTER_PROCESSED == 0 of
        'false' -> 'ok';
        'true' ->
            gen_server:cast(?SERVER, {'worker_update_processed', TaskId, TotalSucceeded, TotalFailed})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec worker_finished(task_id(), non_neg_integer(), non_neg_integer(), file:filename_all()) -> 'ok'.
worker_finished(TaskId=?NE_BINARY, TotalSucceeded, TotalFailed, Output=?NE_BINARY)
  when is_integer(TotalSucceeded), is_integer(TotalFailed) ->
    _ = gen_server:call(?SERVER, {'worker_finished', TaskId, TotalSucceeded, TotalFailed}),
    {'ok', CSVOut} = file:read_file(Output),
    case kz_datamgr:put_attachment(?KZ_TASKS_DB
                                  ,TaskId
                                  ,?KZ_TASKS_ATTACHMENT_NAME_OUT
                                  ,CSVOut
                                  ,[{'content_type', <<"text/csv">>}]
                                  )
    of
        {'ok', _TaskJObj} ->
            lager:debug("saved ~s", [?KZ_TASKS_ATTACHMENT_NAME_OUT]),
            kz_util:delete_file(Output),
            'ok';
        {'error', _R}=Error ->
            lager:error("failed saving ~s/~s: ~p"
                       ,[TaskId, ?KZ_TASKS_ATTACHMENT_NAME_OUT, _R]),
            Error
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_output_header(module(), atom()) -> kz_csv:row().
get_output_header(Module, Function) ->
    try Module:output_header(Function) of
        [_|_]=Header -> Header;
        _NotARow ->
            lager:debug("bad CSV output header ~p, using default", [_NotARow]),
            ?OUTPUT_CSV_HEADER_ROW
    catch
        _E:_R ->
            lager:debug("output_header not found for ~s:~s (~p:~p), using default"
                       ,[Module, Function, _E, _R]),
            ?OUTPUT_CSV_HEADER_ROW
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_task(module(), kz_json:object(), any()) -> 'ok'.
cleanup_task(Module, API, Data) ->
    lager:debug("cleaning up after task"),
    Action = kz_util:to_atom(kz_json:get_value(<<"action">>, API), 'true'),
    try
        Module:cleanup(Action, Data),
        lager:debug("cleanup completed")
    catch
        'error':'function_clause' ->
            lager:debug("skipped cleanup");
        _E:_R ->
            lager:debug("cleanup ~p: ~p", [_E, _R])
    end.


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
    kz_datamgr:revise_views_from_folder(?KZ_TASKS_DB, ?APP),
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
handle_call({'new', AuthAccountId, AccountId, Category, Action, TotalRows, InputName}, _From, State) ->
    lager:debug("creating ~s/~s task (~p)", [Category, Action, TotalRows]),
    lager:debug("using auth ~s and account ~s", [AuthAccountId, AccountId]),
    TaskId = ?A_TASK_ID,
    Task = #{ worker_pid => 'undefined'
            , worker_node => 'undefined'
            , account_id => AccountId
            , auth_account_id => AuthAccountId
            , id => TaskId
            , category => Category
            , action => Action
            , file_name => InputName
            , created => kz_util:current_tstamp()
            , started => 'undefined'
            , finished => 'undefined'
            , total_rows => TotalRows
            , total_rows_failed => 'undefined'
            , total_rows_succeeded => 'undefined'
            },
    {'ok', _JObj} = Ok = save_new_task(Task),
    lager:debug("task ~s created, rows: ~p", [TaskId, TotalRows]),
    ?REPLY(State, Ok);

handle_call({'start_task', _TaskId}, _From, State=#state{apis = APIs})
  when APIs == #{} ->
    lager:error("no categories starting task ~s: app was probably just restarted, run discovery again"
               ,[_TaskId]),
    ?REPLY(State, {'error', 'no_categories'});
handle_call({'start_task', TaskId}, _From, State) ->
    lager:debug("attempting to start ~s", [TaskId]),
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

%% This used to be cast but would race with worker process' EXIT signal.
handle_call({'worker_finished', TaskId, TotalSucceeded, TotalFailed}, _From, State) ->
    lager:debug("worker finished ~s: ~p/~p", [TaskId, TotalSucceeded, TotalFailed]),
    case task_by_id(TaskId, State) of
        [Task] ->
            Task1 = Task#{ finished => kz_util:current_tstamp()
                         , total_rows_failed => TotalFailed
                         , total_rows_succeeded => TotalSucceeded
                         },
            %% This MUST happen before put_attachment or conflicts won't be resolved.
            {'ok', _JObj} = update_task(Task1),
            State1 = remove_task(TaskId, State),
            ?REPLY(State1, 'ok');
        _ ->
            %% Assuming Task has already been saved.
            ?REPLY(State, 'ok')
    end;

handle_call({'remove_task', TaskId}, _From, State) ->
    lager:debug("attempting to remove ~s", [TaskId]),
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
        [Task = #{worker_pid := _Pid}] ->
            case is_processing(Task) of
                'true' -> ?REPLY(State, {'error', 'task_running'});
                'false' ->
                    %%FIXME: should attempt to kill worker process.
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
handle_cast({'worker_error', TaskId}, State) ->
    lager:debug("worker error ~s", [TaskId]),
    [Task=#{total_rows := TotalRows}] = task_by_id(TaskId, State),
    Task1 = Task#{ finished => kz_util:current_tstamp()
                 , total_rows_failed => TotalRows
                 , total_rows_succeeded => 0
                 },
    {'ok', _JObj} = update_task(Task1),
    State1 = remove_task(TaskId, State),
    {'noreply', State1};

handle_cast({'worker_update_processed', TaskId, TotalSucceeded, TotalFailed}, State) ->
    lager:debug("worker update ~s: ~p/~p", [TaskId, TotalSucceeded, TotalFailed]),
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{ total_rows_failed => TotalFailed
                 , total_rows_succeeded => TotalSucceeded
                 },
    {'ok', _JObj} = update_task(Task1),
    State1 = add_task(Task1, remove_task(TaskId, State)),
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
handle_info({'EXIT', Pid, _Reason}, State) ->
    case task_by_pid(Pid, State) of
        [] ->
            lager:debug("worker ~p finished: ~p", [Pid, _Reason]),
            {'noreply', State};
        [Task=#{id := TaskId}] ->
            lager:error("worker ~p died executing ~s: ~p", [Pid, TaskId, _Reason]),
            %% Note: this means output attachment was MAYBE NOT saved to task doc.
            %% Note: setting total_rows_failed to undefined here will change
            %%  status to ?STATUS_BAD but will not update total_rows_failed value in doc.
            Task1 = Task#{ finished => kz_util:current_tstamp()
                         , total_rows_failed := 'undefined'
                         },
            {'ok', _JObj} = update_task(Task1),
            State1 = remove_task(TaskId, State),
            {'noreply', State1}
    end;

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
    kz_doc:created(JObjA) =< kz_doc:created(JObjB).

-spec save_new_task(task()) -> {'ok', kz_json:object()} |
                               {'error', any()}.
save_new_task(Task = #{id := _TaskId}) ->
    case kz_datamgr:save_doc(?KZ_TASKS_DB, to_json(Task)) of
        {'ok', Doc} -> {'ok', to_public_json(from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to save ~s in ~s: ~p", [_TaskId, ?KZ_TASKS_DB, _R]),
            E
    end.

-spec update_task(task()) -> {'ok', kz_json:object()} |
                             {'error', any()}.
update_task(Task = #{id := TaskId}) ->
    Updates = kz_json:to_proplist(to_json(Task)),
    case kz_datamgr:update_doc(?KZ_TASKS_DB, TaskId, Updates) of
        {'ok', Doc} -> {'ok', to_public_json(from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to update ~s in ~s: ~p", [TaskId, ?KZ_TASKS_DB, _R]),
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

-spec task_by_pid(pid(), state()) -> [task()].
task_by_pid(Pid, State) ->
    [T || T=#{worker_pid := WPid} <- State#state.tasks,
          Pid == WPid
    ].

-spec handle_call_start_task(task(), state()) -> ?REPLY(state(), Response) when
      Response :: {'ok', kz_json:object()} |
                  {'error', any()}.
handle_call_start_task(#{ finished := Finished
                        }, State)
  when Finished /= 'undefined' ->
    ?REPLY(State, {'error', 'already_started'});
handle_call_start_task(Task=#{ id := TaskId
                             , account_id := AccountId
                             , auth_account_id := AuthAccountId
                             , category := Category
                             , action := Action
                             }
                      ,State=#state{ apis = APIs
                                   , nodes = Nodes
                                   , modules = Modules
                                   , apps = Apps
                                   }
                      ) ->
    lager:info("about to start task ~s: ~s ~s", [TaskId, Category, Action]),
    API = task_api(Category, Action, APIs),
    lager:debug("API ~s", [kz_json:encode(API)]),
    WorkerModule = worker_module(API),
    lager:debug("worker type: ~s", [WorkerModule]),
    Node = maps:get(Category, Nodes),
    Module = maps:get(Category, Modules),
    lager:debug("app ~s module ~s node ~s", [maps:get(Category, Apps), Module, Node]),
    Function = kz_util:to_atom(Action, 'true'),
    Fields = mandatory(API) ++ optional(API),
    ExtraArgs = [{'account_id', AccountId}
                ,{'auth_account_id', AuthAccountId}
                ],
    lager:debug("extra args: ~p", [ExtraArgs]),
    %% Task needs to run where App is started.
    try erlang:spawn_link(kz_util:to_atom(Node, 'true')
                         ,WorkerModule
                         ,'start'
                         ,[TaskId, API, Module, Function, ExtraArgs, Fields]
                         )
    of
        Pid ->
            Task1 = Task#{ started => kz_util:current_tstamp()
                         , worker_pid => Pid
                         , worker_node => Node
                         },
            {'ok', JObj} = update_task(Task1),
            State1 = add_task(Task1, State),
            ?REPLY_FOUND(State1, JObj)
    catch
        _E:_R ->
            lager:error("worker failed starting ~s: ~p", [TaskId, _R]),
            ?REPLY(State, {'error', _R})
    end.

-spec from_json(kz_json:object()) -> task().
from_json(Doc) ->
    TotalRows = kz_json:get_integer_value(?PVT_TOTAL_ROWS, Doc),
    #{ worker_pid => 'undefined'
     , worker_node => kz_json:get_value(?PVT_WORKER_NODE, Doc)
     , account_id => kz_json:get_value(?PVT_ACCOUNT_ID, Doc)
     , auth_account_id => kz_json:get_value(?PVT_AUTH_ACCOUNT_ID, Doc)
     , id => kz_doc:id(Doc)
     , category => kz_json:get_value(?PVT_CATEGORY, Doc)
     , action => kz_json:get_value(?PVT_ACTION, Doc)
     , file_name => kz_json:get_value(?PVT_FILENAME, Doc)
     , created => kz_doc:created(Doc)
     , started => kz_json:get_integer_value(?PVT_STARTED_AT, Doc)
     , finished => kz_json:get_integer_value(?PVT_FINISHED_AT, Doc)
     , total_rows => TotalRows
     , total_rows_failed => kz_json:get_integer_value(?PVT_TOTAL_ROWS_FAILED, Doc)
     , total_rows_succeeded => kz_json:get_integer_value(?PVT_TOTAL_ROWS_SUCCEEDED, Doc)
     }.

-spec to_json(task()) -> kz_json:object().
to_json(#{id := TaskId
         ,worker_node := Node
         ,account_id := AccountId
         ,auth_account_id := AuthAccountId
         ,category := Category
         ,action := Action
         ,file_name := InputName
         ,created := Created
         ,started := Started
         ,finished := Finished
         ,total_rows := TotalRows
         ,total_rows_failed := TotalFailed
         ,total_rows_succeeded := TotalSucceeded
         } = Task) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"_id">>, TaskId}
        ,{?PVT_TYPE, ?KZ_TASKS_DOC_TYPE}
        ,{?PVT_WORKER_NODE, Node}
        ,{?PVT_ACCOUNT_ID, AccountId}
        ,{?PVT_AUTH_ACCOUNT_ID, AuthAccountId}
        ,{?PVT_CATEGORY, Category}
        ,{?PVT_ACTION, Action}
        ,{?PVT_FILENAME, InputName}
        ,{?PVT_CREATED, Created}
        ,{?PVT_MODIFIED, kz_util:current_tstamp()}
        ,{?PVT_STARTED_AT, Started}
        ,{?PVT_FINISHED_AT, Finished}
        ,{?PVT_TOTAL_ROWS, TotalRows}
        ,{?PVT_TOTAL_ROWS_FAILED, TotalFailed}
        ,{?PVT_TOTAL_ROWS_SUCCEEDED, TotalSucceeded}
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
            ,{<<"auth_account_id">>, kz_json:get_value(?PVT_AUTH_ACCOUNT_ID, Doc)}
            ,{<<"category">>, kz_json:get_value(?PVT_CATEGORY, Doc)}
            ,{<<"action">>, kz_json:get_value(?PVT_ACTION, Doc)}
            ,{<<"created">>, kz_doc:created(Doc)}
            ,{<<"start_timestamp">>, kz_json:get_integer_value(?PVT_STARTED_AT, Doc)}
            ,{<<"end_timestamp">>, kz_json:get_integer_value(?PVT_FINISHED_AT, Doc)}
            ,{<<"total_count">>, kz_json:get_integer_value(?PVT_TOTAL_ROWS, Doc)}
            ,{<<"failure_count">>, kz_json:get_integer_value(?PVT_TOTAL_ROWS_FAILED, Doc)}
            ,{<<"success_count">>, kz_json:get_integer_value(?PVT_TOTAL_ROWS_SUCCEEDED, Doc)}
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

-spec task_api(ne_binary(), ne_binary(), kz_json:objects()) -> kz_json:object().
task_api(Category, Action, APIs) ->
    kz_json:set_values([{<<"category">>, Category}
                       ,{<<"action">>, Action}
                       ]
                      ,maps:get(Action, maps:get(Category, APIs))
                      ).

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
-spec status(task()) -> api_binary().
status(#{started := 'undefined'}) ->
    ?STATUS_PENDING;
status(#{started := Started
        ,finished := 'undefined'
        })
  when Started /= 'undefined' ->
    ?STATUS_EXECUTING;

%% For tasks with CSV input
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalRows
        })
  when Finished /= 'undefined',
       is_integer(TotalRows), TotalRows > 0 ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := TotalRows
        ,total_rows_succeeded := 0
        })
  when Finished /= 'undefined',
       is_integer(TotalRows), TotalRows > 0 ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows := TotalRows
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished /= 'undefined',
       is_integer(TotalRows), is_integer(TotalFailed), is_integer(TotalSucceeded),
       TotalRows > TotalFailed, TotalRows > TotalSucceeded ->
    ?STATUS_PARTIAL;

%% For noinput tasks
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := 0
        ,total_rows_succeeded := 0
        })
  when Finished /= 'undefined' ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := 0
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished /= 'undefined',
       is_integer(TotalSucceeded), TotalSucceeded > 0 ->
    ?STATUS_SUCCESS;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := 0
        })
  when Finished /= 'undefined',
       is_integer(TotalFailed), TotalFailed > 0 ->
    ?STATUS_FAILURE;
status(#{finished := Finished
        ,total_rows := 'undefined'
        ,total_rows_failed := TotalFailed
        ,total_rows_succeeded := TotalSucceeded
        })
  when Finished /= 'undefined',
       is_integer(TotalFailed), is_integer(TotalSucceeded) ->
    ?STATUS_PARTIAL;

status(_Task) ->
    lager:error("impossible task ~p", [_Task]),
    %% Probably due to worker killed (due to e.g. OOM).
    ?STATUS_BAD.

-spec parse_apis(kz_proplist()) -> kz_json:object().
parse_apis(JObjs) ->
    parse_apis(JObjs, kz_json:new()).

-spec parse_apis(kz_json:objects(), kz_json:object()) -> kz_json:object().
parse_apis([], Acc) -> Acc;
parse_apis([JObj|JObjs], Acc) ->
    [Category] = kz_json:get_keys(JObj),
    Actions = kz_json:get_value(Category, JObj),
    _ = lists:foreach(fun verify_unicity_map/1, kz_json:to_proplist(Actions)),
    parse_apis(JObjs
              ,kz_json:set_value(Category, Actions, Acc)
              ).

-spec verify_unicity_map({ne_binary(), kz_json:object()}) -> 'ok'.
verify_unicity_map({_Action, API}) ->
    Fields0 = mandatory(API) ++ optional(API),
    Fields = [kz_util:to_lower_binary(Field) || Field <- Fields0],
    case
        length(lists:usort(Fields)) == length(Fields)
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

-spec input_mime(kz_json:object()) -> api_binary().
input_mime(APIJObj) ->
    kz_json:get_ne_binary_value(?API_INPUT_MIME, APIJObj).


-spec find_input_errors(kz_json:object(), input()) -> map().
find_input_errors(API, 'undefined') ->
    find_API_errors(API, [], 'false');

find_input_errors(API, Input=?NE_BINARY) ->
    {Fields, InputData} = kz_csv:take_row(Input),
    Errors = find_API_errors(API, Fields, 'true'),
    %% Stop here if there is no Mandatory fields to check against.
    case mandatory(API) of
        [] -> Errors;
        Mandatory ->
            IsMandatory = [lists:member(Field, Mandatory) || Field <- Fields],
            Unsets =
                fun (Row, Es) ->
                        case are_mandatories_unset(IsMandatory, Row) of
                            'false' -> Es;
                            'true' -> [iolist_to_binary(kz_csv:row_to_iolist(Row)) | Es]
                        end
                end,
            case kz_csv:fold(InputData, Unsets, []) of
                [] -> Errors;
                MMVs -> Errors#{?KZ_TASKS_INPUT_ERROR_MMV => lists:reverse(MMVs)}
            end
    end;

find_input_errors(API, InputRecord=[_|_]) ->
    %%NOTE: assumes first record has all the fields that all the other records will ever need set,
    %%NOTE: assumes all records have all the same fields defined.
    Fields = kz_json:get_keys(hd(InputRecord)),
    find_API_errors(API, Fields, 'true').

-spec find_API_errors(kz_json:object(), ne_binaries(), boolean()) -> map().
find_API_errors(API, Fields, HasInputData) ->
    Mandatory = mandatory(API),
    Routines =
        [fun (Errors) ->
                 case Mandatory -- Fields of
                     [] -> Errors;
                     Missing -> Errors#{?KZ_TASKS_INPUT_ERROR_MMF => Missing}
                 end
         end
        ,fun (Errors) ->
                 case Fields -- (Mandatory ++ optional(API)) of
                     [] -> Errors;
                     Unknown -> Errors#{?KZ_TASKS_INPUT_ERROR_UF => Unknown}
                 end
         end
        ,fun (Errors) ->
                 MIME = input_mime(API),
                 APIRequiresInputData = 'undefined' /= MIME,
                 RequestedMIME = case MIME of 'undefined' -> <<"none">>; _ -> MIME end,
                 case APIRequiresInputData xor HasInputData of
                     'false' -> Errors;
                     'true' ->  Errors#{?KZ_TASKS_INPUT_ERROR_MIME => RequestedMIME}
                 end
         end
        ],
    lists:foldl(fun (F, Errors) -> F(Errors) end, #{}, Routines).

-spec are_mandatories_unset(nonempty_list(boolean()), nonempty_list(ne_binary())) -> boolean().
are_mandatories_unset(IsMandatory, Row) ->
    MapF = fun (Mandatory, Value) ->
                   Mandatory
                       andalso 'undefined' == Value
           end,
    RedF = fun erlang:'or'/2,
    lists:foldl(RedF, 'false', lists:zipwith(MapF, IsMandatory, Row)).

-spec worker_module(kz_json:object()) -> module().
worker_module(API) ->
    case input_mime(API) of
        'undefined' -> 'kz_task_noinput_worker';
        _TextCSV -> 'kz_task_worker'
    end.

%%% End of Module.
