%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Schedule one-off tasks only once per cluster
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_tasks_scheduler).
-behaviour(gen_server).

%%% Public API
-export([start_link/0]).
-export([start/1
        ,stop/1
        ,remove/1
        ]).

%%% For playful debugging
-export([restart/1]).

%%% API used by workers
-export([worker_finished/4
        ,worker_error/1
        ,worker_pause/0
        ,worker_maybe_send_update/3
        ,get_output_header/1
        ,output_path/1
        ,finish_task/2
        ,cleanup_task/2
        ,attempt_upload/4, attempt_upload/6
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

-define(WAIT_AFTER_ROW
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"wait_after_row_ms">>, 500)
       ).
-define(PROGRESS_AFTER_PROCESSED
       ,kapps_config:get_pos_integer(?CONFIG_CAT, <<"send_progress_after_processed">>, 1000)
       ).
-define(PAUSE_BETWEEN_UPLOAD_ATTEMPTS
       ,kapps_config:get_non_neg_integer(?CONFIG_CAT, <<"pause_between_upload_output_attempts_s">>, 10)
       ).
-define(UPLOAD_ATTEMPTS
       ,kapps_config:get_pos_integer(?CONFIG_CAT, <<"attempt_upload_output_times">>, 5)
       ).

%% `last_worker_update':
%% Keeps track per `task_id' of the last counter at which a `worker update' has
%% been performed. Mainly used at `worker_update_processed' handle_cast.
-record(state, {tasks = #{} :: #{kz_tasks:id() => kz_tasks:task()}
               ,last_worker_update = #{} :: #{kz_tasks:id() => non_neg_integer()}
               }).
-type state() :: #state{}.

-define(STOP_REASON, 'task_stopped').

-define(REPLY(State, Value), {'reply', Value, State}).
-define(REPLY_FOUND(State, TaskJObj), {'reply', {'ok', TaskJObj}, State}).
-define(REPLY_NOT_FOUND(State), {'reply', {'error', 'not_found'}, State}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}} ->
            'true' = link(Pid),
            {'ok', Pid};
        Other -> Other
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(kz_tasks:id()) -> {'ok', kz_json:object()} |
                              {'error', 'not_found' | 'already_started' | any()}.
start(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'start_task', TaskId}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec stop(kz_tasks:id()) -> {'ok', kz_json:object()} |
                             {'error', 'not_found' | 'not_running'}.
stop(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'stop_task', TaskId}).


%% Not for public use
-spec restart(kz_tasks:id()) -> {'ok', kz_json:object()} |
                                {'error'
                                ,'not_found' |
                                 'already_started' |
                                 any()
                                }.
restart(TaskId = ?NE_BINARY) ->
    gen_server:call(?SERVER, {'restart_task', TaskId}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove(kz_tasks:id()) -> {'ok', kz_json:object()} |
                               {'error', 'not_found' | 'task_running'}.
remove(TaskId=?NE_BINARY) ->
    gen_server:call(?SERVER, {'remove_task', TaskId}).

%%%=============================================================================
%%% Worker API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec worker_error(kz_tasks:id()) -> 'ok'.
worker_error(TaskId=?NE_BINARY) ->
    gen_server:cast(?SERVER, {'worker_error', TaskId}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec worker_pause() -> 'ok'.
worker_pause() ->
    MS = ?WAIT_AFTER_ROW,
    lager:debug("taking a ~pms break before next row", [MS]),
    timer:sleep(MS).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec worker_maybe_send_update(kz_tasks:id(), non_neg_integer(), non_neg_integer()) -> 'ok'.
worker_maybe_send_update(TaskId, Succeeded, Failed) ->
    gen_server:cast(?SERVER, {'worker_update_processed', TaskId, Succeeded, Failed}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec worker_finished(kz_tasks:id(), non_neg_integer(), non_neg_integer(), kz_tasks:columns()) -> 'ok'.
worker_finished(TaskId=?NE_BINARY, TotalSucceeded, TotalFailed, Columns)
  when is_integer(TotalSucceeded), is_integer(TotalFailed) ->
    _ = gen_server:call(?SERVER, {'worker_finished', TaskId, TotalSucceeded, TotalFailed}),
    worker_finished(TaskId, Columns).

-spec worker_finished(kz_tasks:id(), kz_tasks:columns()) -> 'ok'.
worker_finished(TaskId, Columns) ->
    CSVPath = output_path(TaskId),
    _ = try_maybe_strip_columns(Columns, CSVPath),
    {'ok', CSV} = file:read_file(CSVPath),
    lager:debug("csv size is ~s", [kz_util:pretty_print_bytes(byte_size(CSV))]),
    Max = ?UPLOAD_ATTEMPTS,
    attempt_upload(TaskId, ?KZ_TASKS_ANAME_OUT, CSV, CSVPath, Max, Max).

try_to_salvage_output(TaskId=?NE_BINARY) ->
    lager:info("task ~s was stopped/killed, trying to upload output anyway", [TaskId]),
    worker_finished(TaskId, sets:new()).

try_maybe_strip_columns(Columns, CSVPath) ->
    try maybe_strip_columns(Columns, CSVPath)
    catch
        ?STACKTRACE(_E, _R, ST)
        lager:warning("stripping empty columns failed: ~p:~p", [_E, _R]),
        kz_log:log_stacktrace(ST)
        end.

maybe_strip_columns(Columns, CSVPath) ->
    maybe_strip_columns(Columns, CSVPath, sets:size(Columns)).

maybe_strip_columns(_Columns, _CSVPath, 0) ->
    lager:info("no columns written, nothing to strip");
maybe_strip_columns(Columns, CSVPath, ColumnsWritten) ->
    lager:debug("attempting to strip empty columns, keeping only ~p", [ColumnsWritten]),
    {'ok', Bin} = file:read_file(CSVPath),
    {FullHeader, CSV} = kz_csv:take_row(Bin),
    lager:debug("csv size is ~s", [kz_util:pretty_print_bytes(byte_size(CSV))]),
    case length(FullHeader) of
        ColumnsWritten -> 'ok';
        MightHaveEmpty when ColumnsWritten < MightHaveEmpty ->
            OutputPath = <<CSVPath/binary, "_reversed">>,
            Header = [Column || Column <- FullHeader,
                                sets:is_element(Column, Columns)
                     ],
            strip_columns(FullHeader, Header, CSV, OutputPath),
            lager:debug("mv ~s ~s", [OutputPath, CSVPath]),
            'ok' = file:rename(OutputPath, CSVPath);
        _DamnYouMoreColumn -> throw({'error', <<"more columns were written">>})
    end.

strip_columns(FullHeader, Header, CSV, OutputPath) ->
    case kz_csv:take_mapped_row(FullHeader, CSV) of
        'eof' ->
            'ok' = file:write_file(OutputPath, [kz_csv:row_to_iolist(Header), $\n], ['append']),
            tac(OutputPath);
        {MappedRow, NewCSV} ->
            Stripped = maps:with(Header, MappedRow),
            Data = [kz_csv:mapped_row_to_iolist(Header, Stripped), $\n],
            'ok' = file:write_file(OutputPath, Data, ['append']),
            strip_columns(FullHeader, Header, NewCSV, OutputPath)
    end.

tac(OutputPath) ->
    Exe = "tac",
    'true' = ('false' =/= os:find_executable(Exe)),
    Tmp = binary_to_list(OutputPath) ++ "_" ++ Exe,
    Cmd = Exe ++ " " ++ binary_to_list(OutputPath) ++ " > " ++ Tmp,
    lager:debug("executing ~s", [Cmd]),
    [] = os:cmd(Cmd),
    lager:debug("mv ~s ~s", [Tmp, OutputPath]),
    'ok' = file:rename(Tmp, OutputPath).

-spec attempt_upload(kz_term:ne_binary(), kz_term:ne_binary(), iodata(), file:filename_all()) ->
                            'ok' | {'error', 'conflict'}.
attempt_upload(TaskId, AName, CSV, CSVPath) ->
    Max = ?UPLOAD_ATTEMPTS,
    attempt_upload(TaskId, AName, CSV, CSVPath, Max, Max).

-spec attempt_upload(kz_term:ne_binary(), kz_term:ne_binary(), iodata(), file:filename_all(), non_neg_integer(), pos_integer()) ->
                            'ok' | {'error', 'conflict'}.
attempt_upload(_TaskId, _AName, _, _, 0, _) ->
    lager:error("failed saving ~s/~s: last failing attempt", [_TaskId, _AName]),
    {'error', 'conflict'};
attempt_upload(TaskId, AName, CSV, CSVPath, Retries, Max) ->
    lager:debug("attempt #~p to save ~s/~s", [Max-Retries+1, TaskId, AName]),
    Options = [{'content_type', <<"text/csv">>}],
    case kz_datamgr:put_attachment(?KZ_TASKS_DB, TaskId, AName, CSV, Options) of
        {'ok', _TaskJObj} ->
            lager:debug("saved ~s after ~p attempts", [AName, Max-Retries+1]),
            kz_util:delete_file(CSVPath);
        {'error', _R} ->
            lager:debug("upload of ~s failed (~s), may retry soon", [TaskId, _R]),
            Pause = ?MILLISECONDS_IN_SECOND * ?PAUSE_BETWEEN_UPLOAD_ATTEMPTS,
            lager:debug("waiting ~pms before next upload attempt of ~s", [Pause, TaskId]),
            timer:sleep(Pause),
            attempt_upload(TaskId, AName, CSV, CSVPath, Retries-1, Max)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_output_header(kz_json:object()) -> kz_tasks:output_header().
get_output_header(API) ->
    Action = kz_json:get_value(<<"action">>, API),
    case tasks_bindings:apply(API, <<"output_header">>, [Action]) of
        [[_|_]=Header] -> Header;
        [{'replace', [_|_]}=Header] -> Header;
        [{'EXIT', {_E, _R}}] ->
            lager:debug("output_header not found for ~s (~p), using default", [Action, _E]),
            [?OUTPUT_CSV_HEADER_ERROR];
        _NotARow ->
            lager:debug("bad CSV output header ~p, using default", [_NotARow]),
            [?OUTPUT_CSV_HEADER_ERROR]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec output_path(kz_tasks:id()) -> file:filename_all().
output_path(TaskId=?NE_BINARY) ->
    <<"/tmp/task_out.", TaskId/binary, ".csv">>.

%%------------------------------------------------------------------------------
%% @doc Calls the task's 'finish' function, if applicable
%%
%% Run after all rows are processed but before the task is considered complete.
%% @end
%%------------------------------------------------------------------------------
-spec finish_task(kz_json:object(), any()) -> 'ok'.
finish_task(API, Data) ->
    lager:debug("finishing up after task"),
    Action = kz_json:get_value(<<"action">>, API),
    case tasks_bindings:apply(API, <<"finish">>, [Action, Data]) of
        [] -> lager:debug("skipped finish");
        [{'EXIT', {_E, _Rs}}] ->
            lager:debug("finish ~p: ~p", [_E, hd(_Rs)]);
        _ -> lager:debug("finish completed")
    end.

%%------------------------------------------------------------------------------
%% @doc Calls the task's 'cleanup' function, once the task is considered complete
%% @end
%%------------------------------------------------------------------------------
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

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    _ = process_flag('trap_exit', 'true'),
    lager:info("ensuring db ~s exists", [?KZ_TASKS_DB]),
    {'ok', #state{}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'start_task', TaskId}, _From, State) ->
    lager:debug("attempting to start ~s", [TaskId]),
    %% Running tasks are stored in server State then promptly removed.
    %% So make sure to check State first then maybe DB.
    case task_by_id(TaskId, State) of
        'undefined' ->
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] -> handle_call_start_task(Task, State)
            end;
        Task ->
            'true' = kz_tasks:is_processing(Task),
            lager:info("task ~s already started and running", [TaskId]),
            ?REPLY(State, {'error', 'already_started'})
    end;

handle_call({'stop_task', TaskId}, _From, State) ->
    lager:debug("attempting to stop ~s", [TaskId]),
    case task_by_id(TaskId, State) of
        'undefined' ->
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [_] -> ?REPLY(State, {'error', 'not_running'})
            end;
        #{worker_pid := Pid}=Task ->
            case kz_tasks:is_processing(Task) of
                'false' ->
                    lager:info("task ~s is not running", [TaskId]),
                    ?REPLY(State, {'error', 'not_running'});
                'true' ->
                    lager:info("stopping ~s worker ~p", [TaskId, Pid]),
                    'true' = exit(Pid, ?STOP_REASON),
                    lager:info("removing ~s from state", [TaskId]),
                    Task1 = Task#{finished => kz_time:now_s()
                                 ,was_stopped => 'true'
                                 },
                    {'ok', JObj} = update_task(Task1),
                    State1 = remove_task(TaskId, State),
                    State2 = remove_last_worker_update(TaskId, State1),
                    kz_process:spawn(fun try_to_salvage_output/1, [TaskId]),
                    ?REPLY_FOUND(State2, JObj)
            end
    end;

handle_call({'restart_task', TaskId}, _From, State) ->
    lager:debug("attempting to restart ~s", [TaskId]),
    case task_by_id(TaskId, State) of
        'undefined' ->
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] -> handle_call_start_task(Task#{finished=>'undefined'}, State)
            end;
        Task ->
            'true' = kz_tasks:is_processing(Task),
            lager:info("task ~s is already running", [TaskId]),
            ?REPLY(State, {'error', 'already_started'})
    end;

%% This used to be cast but would race with worker process' EXIT signal.
handle_call({'worker_finished', TaskId, TotalSucceeded, TotalFailed}, _From, State) ->
    lager:debug("worker finished ~s: ~p:~p", [TaskId, TotalSucceeded, TotalFailed]),
    Task = task_by_id(TaskId, State),
    Task1 = Task#{finished => kz_time:now_s()
                 ,total_rows_failed => TotalFailed
                 ,total_rows_succeeded => TotalSucceeded
                 },
    log_elapsed_time(Task1),
    %% This MUST happen before put_attachment or conflicts won't be resolved.
    {'ok', _JObj} = update_task(Task1),
    State1 = remove_task(TaskId, State),
    State2 = remove_last_worker_update(TaskId, State1),
    ?REPLY(State2, 'ok');

handle_call({'remove_task', TaskId}, _From, State) ->
    lager:debug("attempting to remove ~s", [TaskId]),
    case task_by_id(TaskId, State) of
        'undefined' ->
            case kz_tasks:task_by_id(TaskId) of
                [] -> ?REPLY_NOT_FOUND(State);
                [Task] ->
                    %%TODO: if app shutdown before task termination
                    %%this would be the place to attempt some cleanup
                    {'ok', _} = kz_datamgr:del_doc(?KZ_TASKS_DB, TaskId),
                    ?REPLY_FOUND(State, kz_tasks:to_public_json(Task))
            end;
        Task ->
            'true' = kz_tasks:is_processing(Task),
            ?REPLY(State, {'error', 'task_running'})
    end;

handle_call(_Request, _From, State) ->
    lager:debug("unhandled call ~p from ~p", [_Request, _From]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'worker_error', TaskId}, State) ->
    lager:debug("worker error ~s", [TaskId]),
    Task = #{total_rows := TotalRows} = task_by_id(TaskId, State),
    Task1 = Task#{finished => kz_time:now_s()
                 ,total_rows_failed => TotalRows
                 ,total_rows_succeeded => 0
                 },
    {'ok', _JObj} = update_task(Task1),
    State1 = remove_task(TaskId, State),
    State2 = remove_last_worker_update(TaskId, State1),
    {'noreply', State2};

handle_cast({worker_update_processed, TaskId, TotalSucceeded, TotalFailed}, State) ->
    #state{last_worker_update = #{TaskId := LastWorkerUpdate}} = State,
    ProcessedSoFar = (TotalSucceeded + TotalFailed),
    SendUpdate = ProcessedSoFar > (LastWorkerUpdate + ?PROGRESS_AFTER_PROCESSED),
    case {SendUpdate, task_by_id(TaskId, State)} of
        {_, 'undefined'} ->
            %% Happens when there was a stop_task right before
            {'noreply', State};
        {'true', Task} ->
            lager:info("worker update ~s: ~p/~p",
                       [TaskId, TotalSucceeded, TotalFailed]),
            Task1 = Task#{total_rows_failed => TotalFailed
                         ,total_rows_succeeded => TotalSucceeded
                         },
            {'ok', _JObj} = update_task(Task1),
            State1 = add_task(Task1, remove_task(TaskId, State)),
            State2 = set_last_worker_update(TaskId, ProcessedSoFar, State1),
            {'noreply', State2};
        _ ->
            {'noreply', State}
    end;

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'EXIT', Pid, ?STOP_REASON}, State) ->
    lager:debug("worker ~p was stopped", [Pid]),
    {'noreply', State};

handle_info({'EXIT', Pid, 'normal'}, State) ->
    lager:debug("worker ~p finished", [Pid]),
    {'noreply', State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    #{id := TaskId} = Task = task_by_pid(Pid, State),
    lager:error("worker ~p died executing ~s: ~p", [Pid, TaskId, _Reason]),
    %% Note: setting total_rows_failed to undefined here will change
    %%  status to ?STATUS_BAD but will not update total_rows_failed value in doc.
    Task1 = Task#{finished => kz_time:now_s()
                 ,total_rows_failed => 'undefined'
                 },
    {'ok', _JObj} = update_task(Task1),
    State1 = remove_task(TaskId, State),
    State2 = remove_last_worker_update(TaskId, State1),
    kz_process:spawn(fun try_to_salvage_output/1, [TaskId]),
    {'noreply', State2};

handle_info(_Info, State) ->
    lager:debug("unhandled message ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec task_by_id(kz_tasks:id(), state()) -> kz_tasks:task() | 'undefined'.
task_by_id(TaskId, #state{tasks = Tasks}) ->
    maps:get(TaskId, Tasks, 'undefined').

-spec task_by_pid(pid(), state()) -> kz_tasks:task() | 'undefined'.
task_by_pid(Pid, #state{tasks = Tasks}) ->
    F = fun (_, #{worker_pid := WorkerPid}=Task, Acc) ->
                Pid =:= WorkerPid
                    andalso throw({'task', Task}),
                Acc
        end,
    try maps:fold(F, 'undefined', Tasks)
    catch 'throw':{'task', Task} -> Task
    end.

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
    try kz_process:spawn_link(fun Worker:start/3, [TaskId, API, ExtraArgs]) of
        Pid ->
            Task1 = Task#{started => kz_time:now_s()
                         ,worker_pid => Pid
                         ,worker_node => kz_term:to_binary(node())
                         },
            {'ok', JObj} = update_task(Task1),
            State1 = add_task(Task1, State),
            %% Initialize the `last_worker_update' value for this task
            State2 = set_last_worker_update(TaskId, 0, State1),
            ?REPLY_FOUND(State2, JObj)
    catch
        _E:_R ->
            lager:error("worker failed starting ~s: ~p", [TaskId, _R]),
            ?REPLY(State, {'error', _R})
    end.

-spec remove_task(kz_tasks:id(), state()) -> state().
remove_task(TaskId, State=#state{tasks = Tasks}) ->
    State#state{tasks = maps:remove(TaskId, Tasks)}.

-spec remove_last_worker_update(kz_tasks:id(), state()) -> state().
remove_last_worker_update(TaskId, State = #state{last_worker_update = LWU}) ->
    State#state{last_worker_update = maps:remove(TaskId, LWU)}.

-spec add_task(kz_tasks:task(), state()) -> state().
add_task(Task=#{id := TaskId}, State=#state{tasks = Tasks}) ->
    State#state{tasks = maps:put(TaskId, Task, Tasks)}.

-spec update_task(kz_tasks:task()) -> {'ok', kz_json:object()} |
                                      {'error', any()}.
update_task(Task = #{id := TaskId}) ->
    Updates = kz_json:to_proplist(kz_tasks:to_json(Task)),
    UpdateOptions = [{'update', Updates}
                    ,{'ensure_saved', 'true'}
                    ],
    case kz_datamgr:update_doc(?KZ_TASKS_DB, TaskId, UpdateOptions) of
        {'ok', Doc} ->
            {'ok', kz_tasks:to_public_json(kz_tasks:from_json(Doc))};
        {'error', _R}=E ->
            lager:error("failed to update ~s in ~s: ~p", [TaskId, ?KZ_TASKS_DB, _R]),
            E
    end.

-spec set_last_worker_update(kz_tasks:id(), non_neg_integer(), state()) -> state().
set_last_worker_update(TaskId
                      ,ProcessedSoFar
                      ,#state{last_worker_update = LWU}=State
                      ) ->
    State#state{last_worker_update = LWU#{TaskId => ProcessedSoFar}}.

-spec task_api(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
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
        <<"none">> -> 'kz_task_worker_noinput';
        _TextCSV -> 'kz_task_worker'
    end.

%%% End of Module.
