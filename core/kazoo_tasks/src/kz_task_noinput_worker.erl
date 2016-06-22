%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%  Run tasks without CSV input file, scheduled by kz_tasks.
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_task_noinput_worker).

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
            loop('init', State);
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
init(TaskId, Module, Function, ExtraArgs, _) ->
    case
        kz_util:try_load_module(Module) == Module andalso
        write_output_csv_header(TaskId, Module, Function)
    of
        'false' ->
            lager:error("failed loading module '~p' for task ~s", [Module, TaskId]),
            {'error', 'badmodule'};
        {'error', _R}=Error ->
            lager:error("failed to write CSV header in ~s", [?OUT(TaskId)]),
            Error;
        'ok' ->
            State = #state{ task_id = TaskId
                          , module = Module
                          , function = Function
                          , extra_args = ExtraArgs
                          },
            {'ok', State}
    end.

%% @private
-spec loop(task_iterator(), state()) -> any().
loop(IterValue, State=#state{task_id = TaskId
                            ,module = Module
                            ,function = Function
                            ,extra_args = ExtraArgs
                            ,total_failed = TotalFailed
                            ,total_succeeded = TotalSucceeded
                            }) ->
    case is_task_successful(TaskId, Module, Function, ExtraArgs, IterValue) of
        'stop' ->
            TaskRev = upload_output(TaskId),
            _ = kz_tasks:worker_finished(TaskId, TaskRev, TotalSucceeded, TotalFailed),
            'stop';
        {'false', {_PrevRow, NewIterValue}} ->
            NewState = State#state{total_failed = TotalFailed + 1
                                  },
            _ = maybe_send_update(NewState),
            _ = pause(),
            loop(NewIterValue, NewState);
        {'true', {_PrevRow, NewIterValue}} ->
            NewState = State#state{total_succeeded = TotalSucceeded + 1
                                  },
            _ = maybe_send_update(NewState),
            _ = pause(),
            loop(NewIterValue, NewState)
    end.

%% @private
-spec is_task_successful(kz_tasks:task_id(), module(), atom(), list(), task_iterator()) ->
                                {boolean(), task_iterator()} | 'stop'.
is_task_successful(TaskId, Module, Function, ExtraArgs, IterValue) ->
    try Module:Function(ExtraArgs, IterValue) of
        'stop' -> 'stop';
        {'ok', _Data}=NewIterValue ->
            %% For initialisation steps. Skeeps writing a CSV output row.
            {'true', NewIterValue};
        {[_|_]=NewRow, _Data}=NewIterValue ->
            store_return(TaskId, NewRow),
            {'true', NewIterValue};
        {?NE_BINARY=NewRow, _Data}=NewIterValue ->
            store_return(TaskId, NewRow),
            {'true', NewIterValue};
        {Error, _Data}=NewIterValue ->
            store_return(TaskId, Error),
            {'false', NewIterValue}
    catch
        _E:_R ->
            kz_util:log_stacktrace(),
            store_return(TaskId, ?WORKER_TASK_FAILED),
            {'false', 'stop'}
    end.

%% @private
-spec store_return(kz_tasks:task_id(), task_return()) -> 'ok'.
store_return(TaskId, Reason) ->
    Data = [reason(Reason), $\n],
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
-spec write_output_csv_header(kz_tasks:task_id(), module(), atom()) -> 'ok' |
                                                                       {'error', any()}.
write_output_csv_header(TaskId, Module, Function) ->
    HeaderRHS = kz_tasks:get_output_header(Module, Function),
    Data = [kz_csv:row_to_iolist(HeaderRHS), $\n],
    file:write_file(?OUT(TaskId), Data).

%% @private
-spec pause() -> 'ok'.
pause() ->
    MS = ?KZ_TASKS_WAIT_AFTER_ROW,
    lager:debug("taking a ~pms break before next row", [MS]),
    timer:sleep(MS).

%%% End of Module.
