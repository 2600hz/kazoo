%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%  Run tasks scheduled by kz_tasks.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_worker).

%% API
-export([start/5]).

-include("kz_tasks.hrl").

-record(state, { task_id :: kz_tasks:task_id()
               , module :: module()
               , function :: atom()
               , fassoc :: kz_csv:fassoc()
               , extra_args :: kz_proplist()
               , total_failed = 0 :: non_neg_integer()
               , total_succeeded = 0 :: non_neg_integer()
               }).
-type state() :: #state{}.

-define(IN, 'csv_in').
-define(OUT(TaskId), <<"/tmp/task_out.", (TaskId)/binary, ".csv">>).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(kz_tasks:task_id(), module(), atom(), kz_proplist(), ne_binaries()) -> 'ok'.
start(TaskId, Module, Function, ExtraArgs, OrderedFields) ->
    _ = kz_util:put_callid(TaskId),
    case init(TaskId, Module, Function, ExtraArgs, OrderedFields) of
        {'ok', State} ->
            lager:debug("worker for ~s started", [TaskId]),
            loop(State);
        {'error', _R} ->
            kz_tasks:worker_error(TaskId),
            lager:debug("worker exiting now: ~p", [_R])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec init(kz_tasks:task_id(), module(), atom(), kz_proplist(), ne_binaries()) -> {'ok', state()} |
                                                                                  {'error', any()}.
init(TaskId, Module, Function, ExtraArgs, OrderedFields) ->
    case
        kz_util:try_load_module(Module) == Module andalso
        kz_datamgr:fetch_attachment(?KZ_TASKS_DB, TaskId, ?KZ_TASKS_ATTACHMENT_NAME_IN)
    of
        'false' ->
            lager:error("failed loading module '~p' for task ~s", [Module, TaskId]),
            {'error', 'badmodule'};
        {'error', _R}=Error ->
            lager:error("failed loading attachment ~s from ~s/~s: ~p"
                       ,[?KZ_TASKS_ATTACHMENT_NAME_IN, ?KZ_TASKS_DB, TaskId, _R]),
            Error;
        {'ok', CSV} ->
            {Header, CSVRest} = kz_csv:take_row(CSV),
            case write_output_csv_header(TaskId, Module, Function, Header) of
                {'error', _R}=Error ->
                    lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
                    Error;
                'ok' ->
                    Verifier = build_verifier(Module),
                    FAssoc = kz_csv:associator(Header, OrderedFields, Verifier),
                    State = #state{ task_id = TaskId
                                  , module = Module
                                  , function = Function
                                  , fassoc = FAssoc
                                  , extra_args = ExtraArgs
                                  },
                    _ = put(?IN, CSVRest),
                    {'ok', State}
            end
    end.

%% @private
-spec build_verifier(module()) -> kz_csv:verifier().
build_verifier(Module) ->
    fun (Field, Value) ->
            case Module:Field(Value) of
                'true' -> 'true';
                'false' ->
                    lager:error("'~s' failed to validate with ~s:~s/1"
                               ,[Value, Module, Field]),
                    'false'
            end
    end.

%% @private
-spec loop(state()) -> any().
loop(State=#state{task_id = TaskId
                 ,module = Module
                 ,function = Function
                 ,fassoc = FAssoc
                 ,extra_args = ExtraArgs
                 ,total_failed = TotalFailed
                 ,total_succeeded = TotalSucceeded
                 }) ->
    case kz_csv:take_row(get(?IN)) of
        'eof' ->
            TaskRev = upload_output(TaskId),
            _ = kz_tasks:worker_finished(TaskId, TaskRev, TotalSucceeded, TotalFailed),
            _ = erase(?IN),
            'stop';
        {Row, CSVRest} ->
            NewState =
                case is_task_successful(TaskId, Module, Function, ExtraArgs, FAssoc, Row) of
                    'false' ->
                        State#state{total_failed = TotalFailed + 1
                                   };
                    'true' ->
                        State#state{total_succeeded = TotalSucceeded + 1
                                   }
                end,
            _ = maybe_send_update(NewState),
            _ = put(?IN, CSVRest),
            _ = pause(),
            loop(NewState)
    end.

%% @private
-spec is_task_successful(kz_tasks:task_id(), module(), atom(), list(), kz_csv:fassoc(), kz_csv:row()) ->
                                boolean().
is_task_successful(TaskId, Module, Function, ExtraArgs, FAssoc, RawRow) ->
    try FAssoc(RawRow) of
        {'true', Args} ->
            try
                TaskReturn = apply(Module, Function, [ExtraArgs|Args]),
                store_return(TaskId, RawRow, TaskReturn),
                'ok' == TaskReturn
            catch
                _E:_R ->
                    kz_util:log_stacktrace(),
                    store_return(TaskId, RawRow, ?WORKER_TASK_FAILED),
                    'false'
            end;
        'false' ->
            lager:error("verifier failed on ~p", [RawRow]),
            store_return(TaskId, RawRow, ?WORKER_TASK_TYPE),
            'false'
    catch
        _:_R ->
            lager:error("verifier crashed: ~p", [_R]),
            store_return(TaskId, RawRow, ?WORKER_TASK_MAYBE_OK),
            'false'
    end.

%% @private
-spec store_return(kz_tasks:task_id(), kz_csv:row(), task_return()) -> 'ok'.
store_return(TaskId, Row, Reason) ->
    Data = [kz_csv:row_to_iolist(Row), $,, reason(Reason), $\n],
    kz_util:write_file(?OUT(TaskId), Data, ['append']).

%% @private
-spec reason(task_return()) -> iodata().
reason([_|_]=Row) ->
    kz_csv:row_to_iolist(Row);
reason(?NE_BINARY=Reason) ->
    kz_csv:row_to_iolist([Reason]);
reason(_) -> <<>>.

%% @private
-spec maybe_send_update(state()) -> 'ok'.
maybe_send_update(#state{task_id = TaskId
                        ,total_failed = TotalFailed
                        ,total_succeeded = TotalSucceeded
                        })
  when (TotalFailed + TotalSucceeded) rem 1000 == 0 ->
    kz_tasks:worker_update_processed(TaskId, TotalSucceeded, TotalFailed);
maybe_send_update(_) ->
    'ok'.

%% @private
-spec upload_output(kz_tasks:task_id()) -> ne_binary().
upload_output(TaskId) ->
    {'ok', Out} = file:read_file(?OUT(TaskId)),
    {'ok', TaskRev} = kz_tasks:worker_upload_result(TaskId, Out),
    kz_util:delete_file(?OUT(TaskId)),
    TaskRev.

%% @private
-spec write_output_csv_header(kz_tasks:task_id(), module(), atom(), kz_csv:row()) -> 'ok' |
                                                                                     {'error', any()}.
write_output_csv_header(TaskId, Module, Function, HeaderRow) ->
    HeaderRHS = kz_tasks:get_output_header(Module, Function),
    Data = [kz_csv:row_to_iolist(HeaderRow ++ HeaderRHS), $\n],
    file:write_file(?OUT(TaskId), Data).

%% @private
-spec pause() -> 'ok'.
pause() ->
    lager:debug("taking a break before next row"),
    timer:sleep(?KZ_TASKS_WAIT_AFTER_ROW).

%%% End of Module.
