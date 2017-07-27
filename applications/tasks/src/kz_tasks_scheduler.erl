%%%-------------------------------------------------------------------
%%% @copyright (C) 2016-2017, 2600Hz INC
%%% @doc
%%% Schedule one-off tasks only once per cluster
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_tasks_scheduler).
-behaviour(gen_server).

%%% Public API
-export([start_link/0]).
-export([start/1
        ,remove/1
        ]).

%%% For playfull debugging
-export([restart/1]).

%%% API used by workers
-export([worker_finished/5
        ,worker_error/1
        ,worker_pause/0
        ,worker_maybe_send_update/3
        ,get_output_header/1
        ,cleanup_task/2
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
-include_lib("kazoo_tasks/include/task_fields.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(SERVER, {'via', 'kz_globals', ?MODULE}).

-define(WAIT_AFTER_ROW,
        kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"wait_after_row_ms">>, 500)).
-define(PROGRESS_AFTER_PROCESSED,
        kapps_config:get_pos_integer(?CONFIG_CAT, <<"send_progress_after_processed">>, 1000)).
-define(PAUSE_BETWEEN_UPLOAD_ATTEMPTS,
        kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"pause_between_upload_output_attempts_s">>, 10)).
-define(UPLOAD_ATTEMPTS,
        kapps_config:get_pos_integer(?CONFIG_CAT, <<"attempt_upload_output_times">>, 5)).

-record(state, {tasks = [] :: [kz_tasks:task()]
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
-spec start(kz_tasks:id()) -> {ok, kz_json:object()} |
                              {error
                              ,not_found |
                               already_started |
                               any()
                              }.
start(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'start_task', TaskId}).

-spec restart(kz_tasks:id()) -> {'ok', kz_json:object()} |
                                {'error'
                                ,'not_found' |
                                 'already_started' |
                                 any()
                                }.
restart(TaskId = ?NE_BINARY) ->
    gen_server:call(?SERVER, {'restart_task', TaskId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove(kz_tasks:id()) -> {ok, kz_json:object()} |
                               {error, not_found | task_running}.
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
-spec worker_error(kz_tasks:id()) -> ok.
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
-spec worker_maybe_send_update(kz_tasks:id(), pos_integer(), pos_integer()) -> ok.
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
-spec worker_finished(kz_tasks:id(), non_neg_integer(), non_neg_integer(), ne_binary(), kz_tasks:columns()) -> ok.
worker_finished(TaskId=?NE_BINARY, TotalSucceeded, TotalFailed, CSVPath=?NE_BINARY, Columns)
  when is_integer(TotalSucceeded), is_integer(TotalFailed) ->
    _ = gen_server:call(?SERVER, {'worker_finished', TaskId, TotalSucceeded, TotalFailed}),
    _ = try_maybe_strip_columns(Columns, CSVPath),
    {'ok', CSV} = file:read_file(CSVPath),
    lager:debug("csv size is ~s", [kz_util:pretty_print_bytes(byte_size(CSV))]),
    Max = ?UPLOAD_ATTEMPTS,
    attempt_upload(TaskId, ?KZ_TASKS_ANAME_OUT, CSV, CSVPath, Max, Max).

try_maybe_strip_columns(Columns, CSVPath) ->
    try maybe_strip_columns(Columns, CSVPath)
    catch _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:warning("stripping empty columns failed: ~p:~p", [_E, _R]),
            kz_util:log_stacktrace(ST)
    end.

maybe_strip_columns(Columns, CSVPath) ->
    maybe_strip_columns(Columns, CSVPath, sets:size(Columns)).

maybe_strip_columns(_Columns, _CSVPath, 0) ->
    lager:info("no columns written, nothing to strip");
maybe_strip_columns(Columns, CSVPath, ColumnsWritten) ->
    lager:debug("attempting to strip empty columns, keeping only ~p", [ColumnsWritten]),
    {ok, Bin} = file:read_file(CSVPath),
    {FullHeader, CSV} = kz_csv:take_row(Bin),
    lager:debug("csv size is ~s", [kz_util:pretty_print_bytes(byte_size(CSV))]),
    true = ColumnsWritten < length(FullHeader),
    OutputPath = <<CSVPath/binary, "_reversed">>,
    Header = [Column || Column <- FullHeader,
                        sets:is_element(Column, Columns)
             ],
    strip_columns(FullHeader, Header, CSV, OutputPath),
    lager:debug("mv ~s ~s", [OutputPath, CSVPath]),
    ok = file:rename(OutputPath, CSVPath).

strip_columns(FullHeader, Header, CSV, OutputPath) ->
    case kz_csv:take_mapped_row(FullHeader, CSV) of
        eof ->
            ok = file:write_file(OutputPath, [kz_csv:row_to_iolist(Header), $\n], [append]),
            tac(OutputPath);
        {MappedRow, NewCSV} ->
            Stripped = maps:with(Header, MappedRow),
            Data = [kz_csv:mapped_row_to_iolist(Header, Stripped), $\n],
            ok = file:write_file(OutputPath, Data, [append]),
            strip_columns(FullHeader, Header, NewCSV, OutputPath)
    end.

tac(OutputPath) ->
    Exe = "tac",
    true = false =/= os:find_executable(Exe),
    Tmp = binary_to_list(OutputPath) ++ "_" ++ Exe,
    Cmd = Exe ++ " " ++ binary_to_list(OutputPath) ++ " > " ++ Tmp,
    lager:debug("executing ~s", [Cmd]),
    [] = os:cmd(Cmd),
    lager:debug("mv ~s ~s", [Tmp, OutputPath]),
    ok = file:rename(Tmp, OutputPath).

attempt_upload(_TaskId, _AName, _, _, 0, _) ->
    lager:error("failed saving ~s/~s: last failing attempt", [_TaskId, _AName]),
    {error, conflict};
attempt_upload(TaskId, AName, CSV, CSVPath, Retries, Max) ->
    lager:debug("attempt #~p to save ~s/~s", [Max-Retries+1, TaskId, AName]),
    Options = [{content_type, <<"text/csv">>}],
    case kz_datamgr:put_attachment(?KZ_TASKS_DB, TaskId, AName, CSV, Options) of
        {ok, _TaskJObj} ->
            lager:debug("saved ~s after ~p attempts", [AName, Max-Retries+1]),
            kz_util:delete_file(CSVPath);
        {error, _R} ->
            lager:debug("upload of ~s failed (~s), may retry soon", [TaskId, _R]),
            Pause = ?MILLISECONDS_IN_SECOND * ?PAUSE_BETWEEN_UPLOAD_ATTEMPTS,
            lager:debug("waiting ~pms before next upload attempt of ~s", [Pause, TaskId]),
            timer:sleep(Pause),
            attempt_upload(TaskId, AName, CSV, CSVPath, Retries-1, Max)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_output_header(kz_json:object()) -> kz_tasks:output_header().
get_output_header(API) ->
    Action = kz_json:get_value(<<"action">>, API),
    case tasks_bindings:apply(API, <<"output_header">>, [Action]) of
        [[_|_]=Header] -> Header;
        [{replace, [_|_]}=Header] -> Header;
        [{'EXIT', {_E, _R}}] ->
            lager:debug("output_header not found for ~s (~p), using default", [Action, _E]),
            [?OUTPUT_CSV_HEADER_ERROR];
        _NotARow ->
            lager:debug("bad CSV output header ~p, using default", [_NotARow]),
            [?OUTPUT_CSV_HEADER_ERROR]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cleanup_task(kz_json:object(), any()) -> 'ok'.
cleanup_task(API, Data) ->
    lager:debug("cleaning up after task"),
    Action = kz_json:get_value(<<"action">>, API),
    case tasks_bindings:apply(API, <<"cleanup">>, [Action, Data]) of
        [] -> lager:debug("skipped cleanup");
        [{'EXIT', {_E, _Rs}}] ->
            lager:debug("cleanup ~p: ~p", [_E, hd(_Rs)]);
        _ -> lager:debug("cleanup completed")
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
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
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'start_task', TaskId}, _From, State) ->
    lager:debug("attempting to start ~s", [TaskId]),
    %% Running tasks are stored in server State.
    %% They are then promptly removed.
    %% Rationale is to rely on the task document most.
    case task_by_id(TaskId, State) of
        [] ->
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] -> handle_call_start_task(Task, State)
            end;
        [#{started := Started}]
          when Started /= 'undefined' ->
            lager:info("task ~s exists already", [TaskId]),
            ?REPLY(State, {'error', 'already_started'})
    end;

handle_call({'restart_task', TaskId}, _From, State) ->
    lager:debug("attempting to restart ~s", [TaskId]),
    case task_by_id(TaskId, State) of
        [] ->
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] -> handle_call_start_task(Task#{finished=>'undefined'}, State)
            end;
        [#{started := Started}]
          when Started /= 'undefined' ->
            lager:info("task ~s exists already", [TaskId]),
            ?REPLY(State, {'error', 'already_started'})
    end;

%% This used to be cast but would race with worker process' EXIT signal.
handle_call({'worker_finished', TaskId, TotalSucceeded, TotalFailed}, _From, State) ->
    lager:debug("worker finished ~s: ~p/~p", [TaskId, TotalSucceeded, TotalFailed]),
    case task_by_id(TaskId, State) of
        [Task] ->
            Task1 = Task#{finished => kz_time:current_tstamp()
                         ,total_rows_failed => TotalFailed
                         ,total_rows_succeeded => TotalSucceeded
                         },
            log_elapsed_time(Task1),
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
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] ->
                    %%TODO: if app shutdown before task termination
                    %%this would be the place to attempt some cleanup
                    {'ok', _} = kz_datamgr:del_doc(?KZ_TASKS_DB, TaskId),
                    ?REPLY_FOUND(State, kz_tasks:to_public_json(Task))
            end;
        [Task = #{worker_pid := _Pid}] ->
            case kz_tasks:is_processing(Task) of
                'true' -> ?REPLY(State, {'error', 'task_running'});
                'false' ->
                    %%FIXME: should attempt to kill worker process.
                    State1 = remove_task(TaskId, State),
                    ?REPLY_FOUND(State1, kz_tasks:to_public_json(Task))
            end
    end;

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'worker_error', TaskId}, State) ->
    lager:debug("worker error ~s", [TaskId]),
    [Task=#{total_rows := TotalRows}] = task_by_id(TaskId, State),
    Task1 = Task#{finished => kz_time:current_tstamp()
                 ,total_rows_failed => TotalRows
                 ,total_rows_succeeded => 0
                 },
    {'ok', _JObj} = update_task(Task1),
    State1 = remove_task(TaskId, State),
    {'noreply', State1};

handle_cast({'worker_update_processed', TaskId, TotalSucceeded, TotalFailed}, State) ->
    lager:debug("worker update ~s: ~p/~p", [TaskId, TotalSucceeded, TotalFailed]),
    [Task] = task_by_id(TaskId, State),
    Task1 = Task#{total_rows_failed => TotalFailed
                 ,total_rows_succeeded => TotalSucceeded
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
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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
            Task1 = Task#{finished => kz_time:current_tstamp()
                         ,total_rows_failed := 'undefined'
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
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec task_by_id(kz_tasks:id(), state()) -> [kz_tasks:task()].
task_by_id(TaskId, State) ->
    [T || T=#{id := Id} <- State#state.tasks,
          TaskId == Id
    ].

-spec task_by_pid(pid(), state()) -> [kz_tasks:task()].
task_by_pid(Pid, State) ->
    [T || T=#{worker_pid := WPid} <- State#state.tasks,
          Pid == WPid
    ].

-spec log_elapsed_time(kz_tasks:task()) -> 'ok'.
log_elapsed_time(#{started := Start
                  ,finished := End
                  }) ->
    lager:debug("task ran for ~s", [kz_time:pretty_print_elapsed_s(End - Start)]).

-spec handle_call_start_task(kz_tasks:task(), state()) -> ?REPLY(state(), Response) when
      Response :: {'ok', kz_json:object()} |
                  {'error', any()}.
handle_call_start_task(#{finished := Finished
                        ,id := _Id
                        }, State)
  when Finished /= 'undefined' ->
    lager:info("task ~s in a finished state: ~p", [_Id, Finished]),
    ?REPLY(State, {'error', 'already_started'});
handle_call_start_task(Task=#{id := TaskId
                             ,account_id := AccountId
                             ,auth_account_id := AuthAccountId
                             ,category := Category
                             ,action := Action
                             }
                      ,State
                      ) ->
    lager:info("about to start task ~s: ~s ~s", [TaskId, Category, Action]),
    API = task_api(Category, Action),
    lager:debug("API ~s", [kz_json:encode(API)]),
    Worker = worker_module(API),
    lager:debug("worker type: ~s", [Worker]),
    ExtraArgs = #{account_id => AccountId
                 ,auth_account_id => AuthAccountId
                 },
    lager:debug("extra args: ~p", [ExtraArgs]),
    %% Task needs to run where App is started.
    try kz_util:spawn_link(fun Worker:start/3, [TaskId, API, ExtraArgs]) of
        Pid ->
            Task1 = Task#{started => kz_time:current_tstamp()
                         ,worker_pid => Pid
                         ,worker_node => kz_term:to_binary(node())
                         },
            {'ok', JObj} = update_task(Task1),
            State1 = add_task(Task1, State),
            ?REPLY_FOUND(State1, JObj)
    catch
        _E:_R ->
            lager:error("worker failed starting ~s: ~p", [TaskId, _R]),
            ?REPLY(State, {'error', _R})
    end.

-spec remove_task(kz_tasks:id(), state()) -> state().
remove_task(TaskId, State) ->
    NewTasks =
        [T || T=#{id := Id} <- State#state.tasks,
              TaskId /= Id
        ],
    State#state{tasks = NewTasks}.

-spec add_task(kz_tasks:task(), state()) -> state().
add_task(Task, State) ->
    Tasks = [Task | State#state.tasks],
    State#state{tasks = Tasks}.

-spec update_task(kz_tasks:task()) -> {'ok', kz_json:object()} |
                                      {'error', any()}.
update_task(Task = #{id := TaskId}) ->
    Updates = kz_json:to_proplist(kz_tasks:to_json(Task)),
    case kz_datamgr:update_doc(?KZ_TASKS_DB, TaskId, Updates) of
        {'ok', Doc} -> {'ok', kz_tasks:to_public_json(kz_tasks:from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to update ~s in ~s: ~p", [TaskId, ?KZ_TASKS_DB, _R]),
            E
    end.

-spec task_api(ne_binary(), ne_binary()) -> kz_json:object().
task_api(Category, Action) ->
    {'ok', JObj} = kz_tasks_help:help(Category, Action),
    kz_json:set_values([{<<"category">>, Category}
                       ,{<<"action">>, Action}
                       ]
                      ,JObj
                      ).

-spec worker_module(kz_json:object()) -> module().
worker_module(API) ->
    case kz_tasks:input_mime(API) of
        <<"none">> -> kz_task_worker_noinput;
        _TextCSV -> kz_task_worker
    end.

%%% End of Module.
